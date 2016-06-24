#lang typed/racket

(require/typed web-server/http/bindings
               [extract-binding/single (Symbol (Listof (Pairof Symbol (U String Bytes))) -> (U String Bytes))])

(require/typed web-server/http/request-structs
               [binding:file-filename (Any -> Bytes)])

(require "../storage/storage.rkt"
         "assignment-structs.rkt"
         "../base.rkt")


(provide do-submit-step)
;; If file-name or data are #f, nothing is uploaded.
(: do-submit-step (Assignment Step String (U String #f) (U Bytes #f) (Listof Step) -> (Result String)))
(define (do-submit-step assignment step uid file-name data steps)
  (let ((assignment-id (Assignment-id assignment))
        (step-id (Step-id step)))
    (let* ((assignment-record (assignment:select (class-name) assignment-id))
           (is-open (assignment:Record-open assignment-record)))
      (if (not is-open) (Failure "This assignment is currently closed.")
          (begin
            (when (not (submission:exists? assignment-id (class-name) step-id uid))
              (submission:create assignment-id (class-name) step-id uid))
            (submission:publish assignment-id (class-name) step-id uid)
            ;; Assign reviews to the student if applicable
            (let ((next (default-next-action assignment steps uid)))
              (cond
                [(MustReviewNext? next) (assign-reviews assignment-id next uid)])
              (Success "Assignment submitted.")))))))

(: assign-reviews (String MustReviewNext String -> (Listof Void)))
(define (assign-reviews assignment-id next uid)
  (let* ((step (MustReviewNext-step next))
         (reviews (Step-reviews step)))
    (map (default-ensure-assigned-review assignment-id uid step) reviews)))


(: default-next-action (Assignment (Listof Step) String -> (U MustSubmitNext MustReviewNext #t)))
(define (default-next-action assignment steps uid)
  (next-action default-check-reviewed default-ensure-assigned-review assignment steps uid))

;; Given an assignment and the list of steps to complete, returns the next-action the user must take
;; or #t if the user has completed the assignment
(provide next-action)
(: next-action ((String Step Review String -> Boolean)
                (String String Step -> (Review -> Void))
                Assignment
                (Listof Step)
                String -> (U MustSubmitNext MustReviewNext #t)))
(define (next-action check-reviewed ensure-assigned-review assignment steps uid)
  (let ((assignment-id (Assignment-id assignment)))
    (cond 
      [(null? steps) #t]
      [else 
       (let ((check-result (check-step check-reviewed ensure-assigned-review assignment-id (car steps) uid))
             (rest (cdr steps)))
         (cond
           [(eq? #t check-result) (next-action check-reviewed ensure-assigned-review assignment rest uid)]
           [else check-result]))])))


;; Returns #t if this step has been published to and all reviews have been compelted.
;; If the uid has not published for this step, returns a MustSubmitNext for this step-id along with the instructions from the assignment description
;; Otherwise, returns a MustReviewNext for this step-id
(: check-step ((String Step Review String -> Boolean)
               (String String Step -> (Review -> Void))
               String
               Step
               String -> (U MustSubmitNext MustReviewNext #t)))
(define (check-step check-reviewed ensure-assigned-review assignment-id step uid)
  (let* ((step-id (Step-id step))
         (has-published (and (submission:exists? assignment-id (class-name) step-id uid)
                             (submission:published? assignment-id (class-name) step-id uid))))
    (cond 
      [(not has-published) (MustSubmitNext step (Step-instructions step))]
      [else (check-reviews check-reviewed ensure-assigned-review assignment-id step (Step-reviews step) uid)])))

;; Returns #t if all of the reviews for the specified step are completed.
;; Otherwise, returns a MustReviewNext with the step for which reviews have not been completed
(provide check-reviews)
(: check-reviews ((String Step Review String -> Boolean)
                  (String String Step -> (Review -> Void))
                  String
                  Step
                  (Listof Review)
                  String -> (U MustSubmitNext MustReviewNext #t)))
(define (check-reviews check-reviewed ensure-assigned-review assignment-id step reviews uid)
  (cond
    [(null? reviews) #t]
    [else (let* ((next-review (car reviews))
                 (rest (cdr reviews))
                 (result (check-reviewed assignment-id step next-review uid)))
            (cond
              [result (check-reviews check-reviewed ensure-assigned-review assignment-id step rest uid)]
              [else (MustReviewNext step (get-reviews ensure-assigned-review assignment-id uid step))]))]))

(: get-reviews ((String String Step -> (Review -> Void))
                String
                String
                Step -> (Listof String)))
(define (get-reviews ensure-assigned-review assignment-id uid step)
  (let* ((reviews (Step-reviews step)))         
    (map (ensure-assigned-review assignment-id uid step) reviews)
    (review:select-assigned-reviews assignment-id (class-name) (Step-id step) uid)))

(: default-ensure-assigned-review (String String Step -> (Review -> Void)))
(define (default-ensure-assigned-review assignment-id uid step)
  (lambda (review)
    (let* ((assigned-amount (review:count-assigned-reviews (class-name) assignment-id uid (Step-id step) (Review-id review)))
           (expected-amount (Review-amount review))
           (diff (max 0 (- expected-amount assigned-amount))))
    (assign-n-reviews diff assignment-id (Step-id step) uid review))))

(: assign-n-reviews (Exact-Nonnegative-Integer String String String Review -> Void))
(define (assign-n-reviews n assignment-id step-id uid review) 
  (cond [(<= n 0) (void)]
        [else (begin
                (assign-single-review assignment-id step-id uid review)
                (assign-n-reviews (- n 1) assignment-id step-id uid review))]))

(: assign-single-review (String String String Review -> Void))
(define (assign-single-review assignment-id step-id uid review)
  (let ((review-id (Review-id review)))
  (cond [(instructor-solution? review) (review:assign-instructor-solution assignment-id (class-name) step-id (dependency-submission-name review-id 1) uid review-id)]
        [(student-submission? review) (review:assign-student-reviews assignment-id (class-name) step-id uid review-id 1)])))

; Returns #t if the review has been completed and #f otherwise
; assignment-id -> step -> review -> user-id -> bool?
(provide default-check-reviewed)
(: default-check-reviewed (String Step Review String -> Boolean))
(define (default-check-reviewed assignment-id step review uid)
  (cond
    [(instructor-solution? review) (check-instructor-solution assignment-id step review uid)]
    [(student-submission? review) (check-student-submission assignment-id step review uid)]))

;; Returns #t if the instructor solution has been reviewed and #f otherwise
(: check-instructor-solution (String Step instructor-solution String -> Boolean))
(define (check-instructor-solution assignment-id step instructor-solution uid)
  (let* ((review-id (instructor-solution-id instructor-solution))
         (count (review:completed? assignment-id (class-name) (Step-id step) uid review-id)))
    count))

(: check-student-submission (String Step student-submission String -> Boolean))
(define (check-student-submission assignment-id step student-submission uid) 
  (let* ((id (student-submission-id student-submission))
         (count (review:count-completed assignment-id (class-name) (Step-id step) uid id))
         (required-reviews (student-submission-amount student-submission)))
    (>= count required-reviews)))


;; Dependencies 
(provide default-get-dependencies)
(: default-get-dependencies (Assignment -> (Listof Dependency)))
(define (default-get-dependencies assignment)
  (cond [(not (Assignment? assignment)) (raise-argument-error 'determine-dependencies "Assignment" assignment)]
        [else (apply append (map (step-dependencies (Assignment-id assignment)) (Assignment-steps assignment)))]))     

(: step-dependencies (String -> (Step -> (Listof Dependency))))
(define (step-dependencies assignment-id)
  (lambda (step)
    (map (determine-dependency assignment-id (Step-id step)) (Step-reviews step))))

(: determine-dependency (String String -> (Review -> Dependency)))
(define (determine-dependency assignment-id step-id)
  (lambda (review)
    (let* ((review-id (Review-id review))
           (met (lambda ([n : Exact-Nonnegative-Integer]) (check-upload assignment-id step-id review-id n))))
      (cond [(instructor-solution? review) (instructor-solution-dependency (met 1) step-id (Review-id review))]
            [(student-submission? review) 
             (let ((amount (student-submission-amount review)))
               (student-submission-dependency (met amount) step-id (Review-id review) amount))]
            [else (raise (format "Unknown dependency type: ~a" review))]))))

(: check-upload (String String String Exact-Nonnegative-Integer -> Boolean))
(define (check-upload assignment-id step-id review-id n)
  (cond [(<= n 0) #t]
        [(not (submission:exists? assignment-id (class-name) step-id (dependency-submission-name review-id n))) #f]
        [else (check-upload assignment-id step-id review-id (- n 1))]))


(provide default-take-dependency)
(: default-take-dependency (String Dependency (Listof (Pairof Symbol (U Bytes String))) (Listof Any) -> (Result String)))
(define (default-take-dependency assignment-id dependency bindings raw-bindings)
  (let ((review-id (review-dependency-review-id (assert dependency review-dependency?)))
        (step-id (review-dependency-step-id (assert dependency review-dependency?))))
  (cond [(instructor-solution-dependency? dependency) (run-submissions (class-name) assignment-id step-id review-id bindings raw-bindings 1)]
        [(student-submission-dependency? dependency) (run-submissions (class-name) assignment-id step-id review-id bindings raw-bindings (student-submission-dependency-amount dependency))]
        [else (raise (format "Unknown dependency: ~a" dependency))])))



(: run-submissions (String String String String (Listof (Pairof Symbol (U Bytes String))) (Listof Any) Exact-Nonnegative-Integer -> (Result String)))
(define (run-submissions class assignment stepName review-id bindings raw-bindings amount)
  (letrec: ((helper : (Exact-Nonnegative-Integer -> (Result String))
                    (lambda ([n : Exact-Nonnegative-Integer])
                      (if (<= n 0) (Success "Dependencies uploaded.")
                          (let* ((sym (string->symbol (string-append "file-" (number->string n))))
                                 (uname (dependency-submission-name review-id n))
                                 (data (extract-binding/single sym bindings))
                                 (filename (bytes->string/utf-8 (binding:file-filename (list-ref raw-bindings (- n 1))))))
                            (let ((result (upload-dependency-solution class (dependency-submission-name review-id n) assignment stepName filename data)))
                              (cond [(Failure? result) result]
                                    [else (helper (- n 1))])))))))
    (helper amount)))

(provide default-assignment-handler)
(: default-assignment-handler AssignmentHandler)
(define default-assignment-handler (AssignmentHandler default-next-action do-submit-step default-get-dependencies default-take-dependency "default"))
