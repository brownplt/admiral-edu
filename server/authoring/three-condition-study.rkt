#lang racket

(require web-server/http/bindings
         db
         (planet esilkensen/yaml:3:1)
         json
         "../base.rkt"
         "../email/email.rkt"
         "assignment-structs.rkt"
         (prefix-in default: "next-action.rkt")
         "util.rkt"
         "../database/mysql/common.rkt")


;; 3 different groups

;; assignment-id -> uid -> group
(define (lookup-group assignment-id uid)
  (let* ((yaml-string (retrieve-file (dependency-file-name assignment-id)))
         (yaml (string->yaml yaml-string))
         (does-reviews (hash-ref yaml "does-reviews"))
         (gets-reviewed (hash-ref yaml "gets-reviewed"))
         (no-reviews (hash-ref yaml "no-reviews")))
    (cond [(member uid does-reviews) 'does-reviews]
          [(member uid gets-reviewed) 'gets-reviewed]
          [(member uid no-reviews) 'no-reviews]
          [else (error (format "Could not find uid ~a in configuration file.\n\nYAML:~a\n\n" uid yaml-string))])))

(provide three-next-action)
(define (three-next-action assignment steps uid)
  (let ((assignment-id (Assignment-id assignment)))
    (let ((group (lookup-group assignment-id uid)))
      (cond 
        [(null? steps) #t]
        [else (three-check-step assignment steps uid group)]))))

(define (three-check-step assignment steps uid group)
  (let ((assignment-id (Assignment-id assignment))
        (step (first steps))
        (tail (rest steps)))
    (let* ((step-id (Step-id step))
           (has-submitted (> (submission:count assignment-id class-name step-id uid) 0))
           (result (cond 
                     [(not has-submitted) (MustSubmitNext step (Step-instructions step))]
                     [(eq? group 'no-reviews) (default:next-action (three-check-reviewed group) three-ensure-assigned-review assignment tail uid)]
                     [(eq? group 'does-reviews) (handle-does-reviews assignment steps uid group)]
                     [(eq? group 'gets-reviewed) (default:next-action (three-check-reviewed group) three-ensure-assigned-review assignment tail uid)]
                     [else (error (format "Unknown group type ~a" group))])))
      (cond
        ;; If this is the final-submission, check to see if they have completed their reflection
        [(eq? #t result) (check-final-review assignment (last steps) uid group)]
        [else result]))))

(define (handle-does-reviews assignment steps uid group)
  (let* ((assignment-id (Assignment-id assignment))
         (step (first steps))
         (tail (rest steps))
         (result (default:check-reviews (three-check-reviewed group) three-ensure-assigned-review assignment-id step (Step-reviews step) uid)))
    (if (eq? #t result) 
          (default:next-action (three-check-reviewed group) three-ensure-assigned-review assignment tail uid)
          result)))

; Returns #t if the review has been completed and #f otherwise
; assignment-id -> step -> review -> user-id -> bool?
(provide three-check-reviewed)
(define (three-check-reviewed group)
  (lambda (assignment-id step review uid)
    (cond
      [(instructor-solution? review) 
           (let* ((review-id (Review-id review)))
             (cond [(string=? (symbol->string group) review-id) (check-instructor-solution assignment-id uid step)]
                   [else #t]))]
      [(student-submission? review) (default:default-check-reviewed assignment-id step review uid)])))

;; assignment -> step -> user-id -> group-symbol -> Either #t MustReviewNext
;; Checks to see if this is the final step. If it is, ensures that students are assigned
;; an instructor solution review.
(define (check-final-review assignment step uid group)
  (let* ((assignment-id (Assignment-id assignment))
         (step-id (Step-id step))
         (match-group-id (lambda (review) (string=? (symbol->string group) (Review-id review))))
         (reviews (filter match-group-id (Step-reviews step)))
         (check-review (lambda (review) (not (default:default-check-reviewed assignment-id step review uid)))))
    (map (three-ensure-assigned-review assignment-id uid step) reviews)
    ;; Filter out completed reviews. If the list is empty, there are no reviews left
    (cond [(null? (filter check-review reviews)) #t]
          [else (MustReviewNext step (review:select-assigned-reviews assignment-id class-name step-id uid))])))

(provide three-do-submit-step)
(define (three-do-submit-step assignment step uid file-name data steps)
  (let ((assignment-id (Assignment-id assignment))
        (step-id (Step-id step)))
    ;(upload-submission class user assignment step data)
    (upload-submission class-name uid assignment-id step-id file-name data)
    (let* ((group (lookup-group assignment-id uid))
           (extra-message (cond [(eq? group 'gets-reviewed) "You don't need to do any reviewing.  But, you will receive reviews, so you can wait for feedback if you want before you submit your final implementation and tests."]
                                [(eq? group 'does-reviews) "You have been assigned reviews for this assignment, and must complete them.  If enough reviews aren't available right now, you'll receive notifications as they are assigned to you."]
                                [(eq? group 'no-reviews) "You don't need to do any reviewing, and you won't receive any reviews for this assignment.  Submit your implementation and final tests at any time."])))
      ;; look to see if there are any pending reviewers
      (if (eq? group 'gets-reviewed) (maybe-assign-reviewers assignment-id step uid) (printf "Skipping maybe-assign review. uid: ~a, group: ~a\n" uid group))
      (send-email uid "Your submission has been received." (string-append "Your submission to '" assignment-id "' has been received. Note: " extra-message))
    ;; Assign reviews to the student if applicable
      (let ((next (three-next-action assignment steps uid)))
        (cond
          [(MustReviewNext? next) (three-assign-reviews assignment-id (MustReviewNext-step next) uid)])
        (Success (string-append "Assignment submitted. <b>Note : " extra-message "</b>"))))))

(define (maybe-assign-reviewers assignment-id step uid)
  (let* ((reviews (Step-reviews step))
         (step-id (Step-id step)))
    (map (maybe-assign-reviewers-helper assignment-id step-id uid) reviews)))

(define (maybe-assign-reviewers-helper assignment-id step-id uid)
  (lambda (review)
    (cond [(student-submission? review) (let* ((amount (student-submission-amount review))
                                               (pending-reviews (get-pending-reviews assignment-id step-id amount)))
                                          (map (assign-reviewer assignment-id step-id uid) (map (lambda (v) (vector-ref v 0)) pending-reviews)))]
          [(instructor-solution? review) #f]
          [else (error "Ooops should not be here.")])))

(define (assign-reviewer assignment-id step-id uid)
  (lambda (hash)
    (let* ((review (review:select-by-hash hash))
           (reviewer (review:Record-reviewer-id review))
           (assignment-id (review:Record-assignment-id review))
           (q (merge "UPDATE" review:table
                     "SET" review:reviewee-id "=?"
                     "WHERE" review:hash "=?"))
           (result (run query-exec q uid hash)))
      (send-email reviewer "Captain Teach: A review is ready." 
                  (string-join 
                   (list "A review has been assigned to you."
                         (string-append "Assignment-id: " assignment-id)
                         "You can access the review at the following URL:"
                         (string-append "https://" sub-domain server-name "/" class-name "/next/" assignment-id "/"))
                   "\n"))
      result)))

(define (get-pending-reviews assignment-id step-id amount)
  (let* ((q (merge "SELECT " review:hash ", count(*) as C"
                       "FROM" review:table
                       "WHERE" review:assignment-id "=? AND"
                               review:class-id "=? AND"
                               review:step-id "=? AND"
                               review:reviewee-id "='HOLD'"
                       "GROUP BY" review:reviewer-id
                       "ORDER BY" "C DESC,"
                       review:time-stamp "ASC"
                       "LIMIT ?"))
         (result (run query-rows q assignment-id class-name step-id amount)))
    (printf "results of query: ~a\n" result)
    result))

;; For a given assignment-id, step, and uid, assigns reviews that are not currently assigned.
(define (three-assign-reviews assignment-id step uid)
  (let* ((reviews (Step-reviews step)))
    (map (three-ensure-assigned-review assignment-id uid step) reviews)))

(define (three-ensure-assigned-review assignment-id uid step)
  (lambda (review)
    (check-single-assigned assignment-id step uid review)))

(define (check-single-assigned assignment-id step uid review)
  (let ((result (cond [(student-submission? review) ((check-student-submission assignment-id uid step) review)]
                      [(instructor-solution? review) ((check-instructor-solution assignment-id uid step) review)])))
    (when result ((three-assign-review assignment-id (Step-id step) uid) result))))

(define (check-student-submission assignment-id uid step)
  (lambda (review)
    (let* ((step-id (Step-id step))
           (review-id (Review-id review))
           (expected (student-submission-amount review))
           (rubric (student-submission-rubric review))
           (actual  (review:count-assigned-reviews class-name assignment-id uid step-id review-id))
           (diff (max 0 (- expected actual))))
      (student-submission review-id diff rubric))))

; (assignment-id -> uid -> step-id -> (review -> Either review #f))
; Checks to see if an instructor review needs to be completed.
; Returns the specified review if the review has not been completed. Otherwise, returns #f
(define (check-instructor-solution assignment-id uid step)
  (lambda (review)
    (let* ((step-id (Step-id step))
           (user-group (symbol->string (lookup-group assignment-id uid)))
           (review-id (Review-id review))
           (rubric (instructor-solution-rubric review))
           (count (review:count-assigned-reviews class-name assignment-id uid step-id review-id)))
      ;; If the user-group matches the review-id, assign them to the review
      (cond [(string=? user-group review-id) (if (> count 0) #f review)]
            [else #f]))))
  
(define (three-assign-single-review assignment-id step-id uid review)
  ((three-assign-review assignment-id step-id uid) review))

;; Student is in the does-reviews group
(define (three-assign-review assignment-id step-id uid)
  (lambda (review)
    (let ((review-id (Review-id review))
          (amount (Review-amount review)))
      (cond [(instructor-solution? review) (review:assign-instructor-solution assignment-id class-name step-id (dependency-submission-name review-id 1) uid review-id)]
            [(student-submission? review) (assign-student-reviews assignment-id class-name step-id uid review-id amount)]))))

(define (gets-reviewed-list assignment-id step-id)
  (let* ((yaml-string (retrieve-file (dependency-file-name assignment-id)))
         (yaml (string->yaml yaml-string))
         (gets-reviewed (hash-ref yaml "gets-reviewed")))
    gets-reviewed))

(define (assign-student-reviews assignment-id class-name step-id uid review-id amount)
  (let* ((students (gets-reviewed-list assignment-id step-id))
         (student-commas (string-join (build-list (length students) (lambda (n) "?")) ","))
         (q (merge "SELECT" submission:user-id
                   "FROM" submission:table
                   "WHERE" submission:user-id "IN (" student-commas ") AND"
                           submission:assignment-id "=? AND"
                           submission:class-id "=? AND"
                           submission:step-id "=? AND"
                           submission:times-reviewed "<=?"
                   "ORDER BY" submission:times-reviewed "ASC," submission:time-stamp "ASC"
                   "LIMIT ?"))
         (query-list (append (list query-rows q) students (list assignment-id class-name step-id amount amount)))
         (result (apply run query-list))
         (total-found (length result))
         (hold-amount (- amount total-found)))
    (printf "Found ~a students to review, will hold ~a for later\n" total-found hold-amount)
    (map (assign-student-review assignment-id class-name step-id uid review-id) result)
    (assign-student-hold assignment-id class-name step-id uid review-id hold-amount)))

(define (assign-student-review assignment-id class-name step-id uid review-id)
  (lambda (vec)
    (let ((reviewee (vector-ref vec 0)))
      (printf "Assigning review between ~a and ~a\n" reviewee uid)
      (review:create assignment-id class-name step-id reviewee uid review-id))))

(define (assign-student-hold assignment-id class-name step-id uid review-id n)
  (printf "Assigning hold to ~a\n" uid)
  (if (<= n 0) #t
      (begin (review:create assignment-id class-name step-id "HOLD" uid review-id)
             (assign-student-hold assignment-id class-name step-id uid review-id (- n 1)))))

(provide dependency-file-name)
(define (dependency-file-name assignment-id)
  (string-append class-name "/" assignment-id "/three-condition-config.yaml"))

(define (three-get-deps assignment)
  (cons 
   (three-study-config-dependency (check-for-config-file (Assignment-id assignment)))
   (filter instructor-solution-dependency? (default:default-get-dependencies assignment))))

(define (check-for-config-file assignment-id)
  (is-file? (dependency-file-name assignment-id)))
  
  
(define (three-take-deps assignment-id dependency bindings raw-bindings)
  (cond [(three-study-config-dependency? dependency) (take-config assignment-id bindings raw-bindings)]
        [else (default:default-take-dependency assignment-id dependency bindings raw-bindings)]))

(define (take-config assignment-id bindings raw-bindings)
  (let* ((data (extract-binding/single 'three-condition-file bindings)))
    (write-file (dependency-file-name assignment-id) data)
    (Success "Configuration uploaded.")))

(provide three-condition-study-handler)
(define three-condition-study-handler
  (AssignmentHandler three-next-action three-do-submit-step three-get-deps three-take-deps))

