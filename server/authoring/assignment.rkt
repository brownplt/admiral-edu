#lang racket

(provide (except-out (all-defined-out)) (all-from-out "assignment-structs.rkt"))

(require (planet esilkensen/yaml:3:1)
         json
         "assignment-structs.rkt"
         "util.rkt"
         "../database/mysql.rkt"
         "../config.rkt"
         (prefix-in machine: "progress-machine.rkt"))

(define basic-class "BasicElement")
(define likert-class "LikertElement")
(define free-form-class "FreeFormElement")


;; Assignment
(define (yaml->assignment yaml) 
  (cond [(not (yaml? yaml)) (raise-argument-error 'yaml->assignment "yaml" yaml)]
        [(not (= 4 (hash-count yaml))) (raise-user-error "Expected record with 4 fields: `name`, `id`, `description`, and `steps`")]
        [(not (hash-has-keys? yaml "name" "id" "description" "steps")) (raise-user-error "Expected record with 4 fields: `name`, `id`, `description`, and `steps`")]
        [else (let ((id (hash-ref yaml "id"))
                    (name (hash-ref yaml "name"))
                    (description (hash-ref yaml "description"))
                    (steps (map yaml->step (hash-ref yaml "steps"))))
                (Assignment name id description steps))]))

(define (assignment->yaml assignment)
  (cond [(not (Assignment? assignment)) (raise-argument-error 'assignment->yaml "Assignment" assignment)]
        [else (let ((name (Assignment-name assignment))
                    (id (Assignment-id assignment))
                    (description (Assignment-description assignment))
                    (steps (map step->yaml (Assignment-steps assignment))))
                `#hash(("name" . ,name) ("id" . ,id) ("description" . ,description) ("steps" . ,steps)))]))

;; Step
(define (step->yaml step)
  (cond [(not (Step? step)) (raise-argument-error 'step->yaml "Step" step)]
        [else (let* ((id (Step-id step))
                     (instructions (Step-instructions step))
                     (reviews (map review->yaml (Step-reviews step)))
                     (partial `(("id" . ,id) ("instructions" . ,instructions))))
                (make-hash (if (null? reviews) partial (cons `("reviews" . ,reviews) partial))))]))

(define (yaml->step yaml)
  (cond [(not (yaml? yaml)) (raise-argument-error 'yaml->step "yaml" yaml)]
        [(not (or (= 2 (hash-count yaml)) (= 3 (hash-count yaml)))) (raise-user-error "Expected two or three fields: `id`, `instructions`, and optionally `reviews`." yaml)]
        [(not (hash-has-keys? yaml "id" "instructions")) (raise-user-error "Expected two or three fields: `id`, `instructions`, and optionally `reviews`." yaml)]
        [else (let ((id (hash-ref yaml "id"))
                    (instructions (hash-ref yaml "instructions")))
                (cond [(= 2 (hash-count yaml)) (step id instructions)]
                      [(not (hash-has-key? yaml "reviews")) (raise-user-error "Expected two or three fields: `id`, `instructions`, and optionally `reviews`." yaml)]
                      [else (let ((reviews (hash-ref yaml "reviews")))
                              (Step id instructions (map yaml->review reviews)))]))]))        

;; Reviews

;; Student Submission
(define (student-submission->yaml submission) 
  (cond [(not (student-submission? submission)) (raise-argument-error 'student-submission->yaml "student-submission" submission)]
        [else (let ((amount (student-submission-amount submission))
                    (rubric (rubric->yaml (student-submission-rubric submission)))
                    (id (student-submission-id submission)))
                `#hash(("student-submission" . #hash(("id" . ,id) ("amount" . ,amount) ("rubric" . ,rubric)))))]))

(define (yaml->student-submission yaml) 
  (cond [(not (yaml? yaml)) (raise-argument-error 'yaml->student-submission "yaml" yaml)]
        [(not (= 1 (hash-count yaml))) (raise-user-error "Expected a single record `student-submission`." yaml)]
        [(not (hash-has-key? yaml "student-submission")) (raise-user-error "Expected a single record `student-submission`." yaml)]
        [else (let ((rec (hash-ref yaml "student-submission")))
                (cond [(not (= 3 (hash-count rec))) (raise-user-error "Expected three fields `id`, `amount` and `rubric`." rec)]
                      [(not (hash-has-keys? rec "id" "amount" "rubric")) (raise-user-error "Expected three fields `id`, `amount` and `rubric`." rec)]
                      [else (let ((amount (hash-ref rec "amount"))
                                  (rubric (hash-ref rec "rubric"))
                                  (id (hash-ref rec "id")))
                              (student-submission id amount (yaml->rubric rubric)))]))]))

;; Instructor Solution
(define (instructor-solution->yaml solution)
  (cond [(not (instructor-solution? solution)) (raise-argument-error 'instructor-solution->yaml "instructor-solution" solution)]
        [else (let ((id (instructor-solution-id solution))
                    (rubric (rubric->yaml (instructor-solution-rubric solution))))
                `#hash(("instructor-solution" . #hash(("id" . ,id) ("rubric" . ,rubric)))))]))

(define (yaml->instructor-solution yaml)
  (cond [(not (yaml? yaml)) (raise-argument-error 'yaml->instructor-solution "yaml" yaml)]
        [(not (= 1 (hash-count yaml))) (raise-user-error "Expected a single record `instructor-solution`." yaml)]
        [(not (hash-has-key? yaml "instructor-solution")) (raise-user-error "Expected a single record `instructor-solution`." yaml)]
        [else (let ((rec (hash-ref yaml "instructor-solution")))
                (cond [(not (= 2 (hash-count rec))) (raise-user-error "Expected two fields `id` and `rubric`." rec)]
                      [(not (hash-has-keys? rec "id" "rubric")) (raise-user-error "Expected two fields `id` and `rubric`." rec)]
                      [else (let ((id (hash-ref rec "id"))
                                  (rubric (hash-ref rec "rubric")))
                              (instructor-solution id (yaml->rubric rubric)))]))]))

;; Generic
(define (review->yaml review)
  (cond [(not (review? review)) (raise-argument-error 'review->yaml "review" review)]
        [(instructor-solution? review) (instructor-solution->yaml review)]
        [(student-submission? review) (student-submission->yaml review)]))

(define (yaml->review yaml)
  (cond [(not (yaml? yaml)) (raise-argument-error 'yaml->review "yaml" yaml)]
        [(not (= 1 (hash-count yaml))) (raise-user-error "Expected one field either `instructor-solution` or `student-submission`." yaml)]
        [(hash-has-key? yaml "instructor-solution") (yaml->instructor-solution yaml)]
        [(hash-has-key? yaml "student-submission") (yaml->student-submission yaml)]
        [else (raise-user-error "Expected one field either `instructor-solution` or `student-submission`." yaml)]))

;; Rubric
(define (rubric->yaml rubric)
  (cond [(not (Rubric? rubric)) (raise-argument-error 'rubric->yaml "Rubric" rubric)]
        [else (let ((elements (Rubric-elements rubric)))
                (map rubric-element->yaml elements))]))

(define (yaml->rubric yaml)
  (cond [(not (yaml? yaml)) (raise-argument-error 'yaml->rubric "yaml" yaml)]
        [else (let ((elems (map yaml->element yaml)))
                (Rubric elems))]))

(define (rubric->json rubric)
  (cond [(not (Rubric? rubric)) (raise-argument-error 'rubric->json "Rubric" rubric)]
        [else (let* ((elements (Rubric-elements rubric))
                     (inner (map rubric-element->json elements)))
                `#hasheq((rubric . ,inner)))]))

;; Rubric elements

;; Instruction
(define (yaml->instruction yaml)
  (cond [(not (yaml? yaml)) (raise-argument-error 'yaml->instruction "yaml" yaml)]
        [(not (= 1 (hash-count yaml))) (raise-user-error "Expected a single record `instruction`.")]
        [(not (hash-has-key? yaml "instruction")) (raise-user-error "Expected a single record `instruction`.")]
        [else (let ((text (hash-ref yaml "instruction")))
                (instruction text))]))

(define (instruction->yaml instruction)
  (cond [(not (instruction? instruction)) (raise-argument-error 'instruction->yaml "instruction" instruction)]
        [else (let ((text (instruction-text instruction)))
                `#hash(("instruction" . ,text)))]))

(define (instruction->json instruction)
  (cond [(not (instruction? instruction)) (raise-argument-error 'instruction->json "instruction" instruction)]
        [else (let ((text (instruction-text instruction)))
                `#hasheq((class . ,basic-class)
                         (prompt . ,text)
                         (id . "prompt")))]))

;; Complex rubric-element creator
(define (yaml->rubric-element key constructor . arguments)
  (lambda (element-yaml)
    (cond 
      [(not (yaml? element-yaml)) (raise-argument-error (string->symbol (string-append "yaml->" key)) "yaml" element-yaml)]
      [(not (hash-has-key? element-yaml key)) (raise-user-error (string-append "Expected a single '" key "' field." element-yaml))]
      [(not (= 1 (hash-count element-yaml))) (raise-user-error (string-append "Expected a single '" key "' field." element-yaml))]
      [else (let* ((r (hash-ref element-yaml key))
                   (expected (length arguments))
                   (error-message (string-append "A `" key "` record should contain " (number->string expected) " fields: `" (string-join arguments "`, `") "`")))
              (cond 
                [(not (= expected (hash-count r))) (raise-user-error error-message element-yaml)]
                [else (letrec ((helper (lambda (acc args)
                                         (cond [(null? args) (apply constructor (reverse acc))]
                                               [else (cond [(not (hash-has-key? r (car args))) (raise-user-error error-message element-yaml)]
                                                           [else (let ((new-acc (cons (hash-ref r (car args)) acc)))
                                                                   (helper new-acc (cdr args)))])]))))
                        (helper '() arguments))]))])))

;; Likert

(define yaml->likert
  (yaml->rubric-element "likert" likert "id" "text" "min-label" "max-label" "granularity"))

(define (likert->yaml likert)
  (cond [(not (likert? likert)) (raise-argument-error 'likert->yaml "likert" likert)]
        [else (let ((id (likert-id likert))
                    (text (likert-text likert))
                    (min (likert-min likert))
                    (max (likert-max likert))
                    (granularity (likert-granularity likert)))
                `#hash(("likert" . #hash(("id" . ,id) ("text" . ,text) ("min-label" . ,min) ("max-label" . ,max) ("granularity" . ,granularity)))))]))

(define (likert->json likert)
  (cond [(not (likert? likert)) (raise-argument-error 'likert->json "likert" likert)]
        [else (let ((id (likert-id likert))
                    (text (likert-text likert))
                    (min (likert-min likert))
                    (max (likert-max likert))
                    (granularity (likert-granularity likert)))
                `#hasheq((class . ,likert-class)
                         (id . ,id)
                         (prompt . ,text)
                         (minLabel . ,min)
                         (maxLabel . ,max)
                         (rangeSize . ,granularity)
                         (selected . -1)))]))

;; Free Form
(define yaml->free-form
  (yaml->rubric-element "free-form" free-form "id" "text"))

(define (free-form->yaml form)
  (let ((id (free-form-id form))
        (text (free-form-text form)))
    `#hash(("free-form" . #hash(("id" . ,id) ("text" . ,text))))))

(define (free-form->json form)
  (cond [(not (free-form? form)) (raise-argument-error 'free-form->json "free-form" form)]
        [else (let ((id (free-form-id form))
                    (text (free-form-text form)))
                `#hasheq((class . ,free-form-class)
                         (id . ,id)
                         (prompt . ,text)
                         (content . "")))]))

;; Any Rubricoo Element
(define (yaml->element yaml)
  (cond [(not (yaml? yaml)) (raise-argument-error 'yaml->rubric-element "yaml" yaml)]
        [(not (= 1 (hash-count yaml))) (raise-user-error "Expected a single record `likert`, `free-form`, or `instruction`." yaml)]
        [(hash-has-key? yaml "instruction") (yaml->instruction yaml)]
        [(hash-has-key? yaml "likert") (yaml->likert yaml)]
        [(hash-has-key? yaml "free-form") (yaml->free-form yaml)]
        [else (raise-user-error "Expected a single record `likert`, `free-form`, or `instruction`." yaml)]))

(define (rubric-element->yaml el)
  (cond [(not (rubric-element? el)) (raise-argument-error 'rubric-element->yaml "rubric-element" el)]
        [else (cond
                [(instruction? el) (instruction->yaml el)]
                [(likert? el) (likert->yaml el)]
                [(free-form? el) (free-form->yaml el)])]))

(define (rubric-element->json el)
  (cond [(not (rubric-element? el)) (raise-argument-error 'rubric-element->json "rubric-element" el)]
        [else (cond
                [(instruction? el) (instruction->json el)]
                [(likert? el) (likert->json el)]
                [(free-form? el) (free-form->json el)])]))

#|
(define (repeat-id? steps)
  (cond [(not ((listof Step?) steps)) (raise-argument-error 'repeat-name? "listof Step?" steps)]
        [(null? steps) #f]
        [else (repeats? (sort (map Step-id steps) string<?))]))
|#

(define (repeat-id? getter)
  (lambda (list)
    (cond [(null? list) #f]
          [else (repeats? (sort (map getter list) string<?))])))

(define (repeats? ls)
  (if (null? ls) #f
      (letrec ((helper (lambda (head tail)
                         (cond [(null? tail) #f]
                               [else (let ((next (car tail)))
                                       (cond [(equal? head next) head]
                                             [else (helper next (cdr tail))]))]))))
        (helper (car ls) (cdr ls)))))

(define (getId review) 
  (cond [(student-submission? review) (student-submission-id review)]
        [(instructor-solution? review) (instructor-solution-id review)]
        [else (raise-user-error 'validate-step "Expected to find student-submissions / instructor-solution.")]))

(define (getRubric review) 
  (cond [(student-submission? review) (student-submission-rubric review)]
        [(instructor-solution? review) (instructor-solution-rubric review)]
        [else (raise-user-error 'validate-step "Expected to find student-submissions / instructor-solution.")]))

(define (validate-step step)
  (cond [(not (Step? step)) (raise-argument-error 'validate-step "Step" step)]
        [(validate-id (Step-id step)) (validate-id (Step-id step))]
        [else (let* ((reviews (Step-reviews step))
                     (ids (map getId reviews))
                     (sorted (sort ids string<?)))
                (let ((repeat (repeats? sorted))
                      (invalid-ids (filter (lambda (x) x) (map validate-id ids))))
                  (cond [repeat (string-append "Each review id must be unique for the step it is in. Found duplicate review id '" repeat "' in the step '" (Step-id step) "'.")]
                        [(not (null? invalid-ids)) (string-join invalid-ids "\n")]
                        [else #f])))]))

(define (validate-id id)
  (let ((try-match (regexp-match "[a-zA-Z0-9-]*" id)))
    (cond [(null? try-match) "Id can only contain letters, numbers, and '-' characters. Rejected '" id "'."]
          [(equal? (car try-match) id) #f]
          [else (let* ((drop-amount (string-length (car try-match)))
                       (illegal-char (substring id drop-amount (+ drop-amount 1))))
                  (string-append "Id can only contain letters, numbers, and '-' characters. Rejected '" id "' because it contained '" illegal-char "'."))])))
        
(define (validate-assignment assignment)
  (cond [(not (Assignment? assignment)) (raise-argument-error 'validate-assignment "Assignment" assignment)]
        [(assignment:exists? (Assignment-id assignment) class-name) (string-append "The specified assignment id '" (Assignment-id assignment) "' already exists.")]
        [else (let* ((check-steps ((repeat-id? Step-id) (Assignment-steps assignment)))
                     (check-review-ids (filter (lambda (x) x) (map (repeat-id? getId) (map Step-reviews (Assignment-steps assignment)))))
                     (valid-id (validate-id (Assignment-id assignment)))
                     (ids (append (map Step-id (Assignment-steps assignment)) (flatten (map (lambda (step) (map getId (Step-reviews step))) (Assignment-steps assignment)))))
                     (valid-step-ids (filter (lambda (x) (not (eq? #f x))) (map validate-id ids))))
                (cond [valid-id valid-id]
                      [(not (null? check-review-ids)) (string-append "Found duplicate review-ids: " (string-join check-review-ids ", "))]
                      [check-steps (string-append "Assignment may not have multiple steps with the same id. Found multiple instances of '" check-steps "'")]
                      [(not (null? valid-step-ids)) (string-join valid-step-ids "")]
                      [else #f]))]))

;;(with-handlers ([exn:fail? could-not-create-user]) (create-new-user (ct-session-class session) new-uid new-role))
(define (yaml-bytes->create-assignment bytes)
  (let ((yaml-string (bytes->string/utf-8 bytes)))
    (let ((yaml (with-handlers ([exn:fail? could-not-parse]) (string->yaml yaml-string))))
      (cond [(Failure? yaml) (Failure-message yaml)]
            [else (let ((assignment (with-handlers ([exn:fail? invalid-yaml]) (yaml->assignment yaml))))
                    (print assignment) (newline)
                    (cond [(Failure? assignment) (Failure-message assignment)]
                          [else (let ((result (create-assignment assignment)))                                  
                                  (cond [(eq? #t result) (save-assignment-description class-name (Assignment-id assignment) yaml-string) "Success"]
                                        [else result]))]))]))))

(define (could-not-parse exn)
  (failure "Could not parse as YAML."))

(define (invalid-yaml exn)
  (failure "YAML did not contain a valid assignment description."))


(define (create-assignment assignment)
  (cond [(not (Assignment? assignment)) (raise-argument-error 'create-assignment "Assignment" assignment)]
        [else (let ((validation (validate-assignment assignment)))
                (cond [validation validation]
                      [else (create-database-entries assignment)
                            (create-base-rubrics assignment) #t]))]))

(define (create-database-entries assignment)
  (let ((id (Assignment-id assignment)))
    (assignment:create id class-name)))

;;(create-default-rubric class assignment stepName rubric review-id)

(define (create-base-rubrics assignment)
  (let* ((class class-name)
         (assign (Assignment-id assignment))
         (steps (Assignment-steps assignment))
         (create (lambda (step) 
                   (let* ((stepName (Step-id step))
                          (reviews (Step-reviews step))
                          (create (lambda (review)
                                    (let ((review-id (getId review))
                                          (rubric (jsexpr->string (rubric->json (getRubric review)))))
                                      (create-default-rubric class assign stepName rubric review-id)))))
                     (map create reviews)))))
    (map create steps)))

(define (assignment->progress-machine assignment)
  (cond [(Assignment? assignment) (raise-argument-error 'assignment->progress-machine "Assignment" assignment)]
        [else (let ((steps (Assignment-steps assignment)))
                #f)]))

(struct Success (message))
(struct Failure (message))
(define (failure . messages)
  (Failure (apply string-append messages)))

(define (next-step assignment-id uid)
  (let ((assignment (yaml->assignment (string->yaml (retrieve-assignment-description class-name assignment-id)))))
    (next-action (Assignment-id assignment) (Assignment-steps assignment) uid)))

;; Attempts to submit for the specified uid, assignment, and step-id. If this is not the next expected action,
;; This returns a failure with a message describing what the user should do next.
(define (submit-step assignment-id step-id uid data)
  ;; Assignment must exist
  (cond 
    [(not (assignment:exists? assignment-id class-name)) (failure "The specified assignment '" assignment-id "' does not exists.")]
    [else (let* ((assignment (yaml->assignment (string->yaml (retrieve-assignment-description class-name assignment-id))))
                 (assignment-id (Assignment-id assignment))
                 (steps (Assignment-steps assignment))
                 (next (next-action assignment-id steps uid)))
            (cond
              [(and (MustSubmitNext? next) (equal? (Step-id (MustSubmitNext-step next)) step-id)) (do-submit-step assignment-id step-id uid data steps)]
              [else (failure "Could not submit to the step '" step-id "'." (next-action-error next))]))]))

(define (do-submit-step assignment-id step-id uid data steps)
  ;(upload-submission class user assignment step data)
  (upload-submission class-name uid assignment-id step-id data)
  ;; Assign reviews to the student if applicable
  (let ((next (next-action assignment-id steps uid)))
    (cond
      [(MustReviewNext? next) (assign-reviews assignment-id next uid)])))

(define (assign-reviews assignment-id next uid)
  (let* ((step (MustReviewNext-step next))
         (reviews (Step-reviews step)))
    (map (assign-review assignment-id (Step-id step) uid) reviews)))

;(assign-student-reviews assignment class step uid review-id amount)
;(assign-instructor-solution assignment class step uid review-id)

(define (assign-review assignment-id step-id uid)
  (lambda (review)
    (let ((review-id (getId review)))
      (cond [(instructor-solution? review) (review:assign-instructor-solution assignment-id class-name step-id uid review-id)]
            [(student-submission? review) (review:assign-student-reviews assignment-id class-name step-id uid review-id (student-submission-amount review))]))))

(define (next-action-error next)
  (cond [(MustSubmitNext? next) (string-append "Your next action is to submit to on '" (Step-id (MustSubmitNext-step next)) "'.")]
        [(MustReviewNext? next) (string-append "Your next action is to complete reviews for '" (Step-id (MustReviewNext-step next)) "'.")]
        [(eq? #t next) "You have completed this assignment."]
        [else ""]))
 

;; Given an assignment-id and the list of steps to complete, returns the next-action the user must take
;; or #t if the user has completed the assignment
(define (next-action assignment-id steps uid)
  (cond 
    [(null? steps) #t]
    [else 
     (let ((check-result (check-step assignment-id (car steps) uid))
           (rest (cdr steps)))
       (cond
         [(eq? #t check-result) (next-action assignment-id rest uid)]
         [else check-result]))]))

;; Returns #t if this step has been submitted to and all reviews have been compelted.
;; If the uid has not submitted for this step, returns a MustSubmitNext for this step-id along with the instructions from the assignment description
;; Otherwise, returns a MustReviewNext for this step-id
(struct MustSubmitNext (step instructions) #:transparent)
(define (check-step assignment-id step uid)
  (let* ((step-id (Step-id step))
         (has-submitted (> (submission:count assignment-id class-name step-id uid) 0)))
    (cond 
      [(not has-submitted) (MustSubmitNext step (Step-instructions step))]
      [else (check-reviews assignment-id step (Step-reviews step) uid)])))

;; Returns #t if all of the reviews for the specified step are completed.
;; Otherwise, returns a MustReviewNext with the step for which reviews have not been completed
(struct MustReviewNext (step reviews) #:transparent)  
(define (check-reviews assignment-id step reviews uid)
  (cond
    [(null? reviews) #t]
    [else (let* ((next-review (car reviews))
                 (rest (cdr reviews))
                 (result (cond
                           [(instructor-solution? next-review) (check-instructor-solution assignment-id step next-review uid)]
                           [(student-submission? next-review) (check-student-submission assignment-id step next-review uid)])))
            (cond
              [result (check-reviews assignment-id step rest uid)]
              [else (MustReviewNext step (review:select-assigned-reviews assignment-id class-name (Step-id step) uid))]))]))

;; Returns #t if the instructor solution has been reviewed and #f otherwise
(define (check-instructor-solution assignment-id step instructor-solution uid)
  (let* ((id (instructor-solution-id instructor-solution))
         (count (review:completed? assignment-id class-name (Step-id step) uid id)))
    count))

(define (check-student-submission assignment-id step student-submission uid)
  (let* ((id (student-submission-id student-submission))
         (count (review:count-completed assignment-id class-name (Step-id step) uid id))
         (required-reviews (student-submission-amount student-submission)))
    (>= count required-reviews)))

(define (assignment-id->assignment-dependencies id)
  (assignment-dependencies (assignment-id->assignment id)))

(define (assignment-dependencies assignment)
  (cond [(not (Assignment? assignment)) (raise-argument-error 'determine-dependencies "Assignment" assignment)]
        [else (flatten (map step-dependencies (Assignment-steps assignment)))]))

(define (step-dependencies step)
  (map (determine-dependency (Step-id step)) (Step-reviews step)))

(define (determine-dependency step-id)
  (lambda (review)
    (cond [(instructor-solution? review) (dependency step-id (getId review) 1 #t)]
          [(student-submission? review) (dependency step-id (getId review) (student-submission-amount review) #f)])))
                  
(struct dependency (step-id review-id amount instructor-solution) #:transparent)
                
(define (assignment-id->assignment id)
  (yaml->assignment (string->yaml (retrieve-assignment-description class-name id))))
                 
(define test-assignment
  (assignment "Clocks"
              "clock"
              "Students develop functions representing an alarm clock."
              
               (step "tests"
                     "Submit your test cases. Do not submit any clock implementation."
                     (instructor-solution "poor-tests"
                                          (rubric
                                           (likert "correctness"
                                                   "These tests are correct."
                                                   "Disagree"
                                                   "Agree"
                                                   9)
                                           
                                           (instruction "Provide feedback on tests that are not correct by clicking on the line number and adding a comment.")
                                           
                                           (likert "coverage"
                                                   "These tests cover the possible inputs."
                                                   "Disagree"
                                                   "Agree"
                                                   9)
                                           
                                           (free-form "not-covered"
                                                      "If applicable, provide inputs that are not covered by the tests.")))
                     
                     (instructor-solution "good-tests"
                                          (rubric
                                           (likert "correctness"
                                                   "These tests are correct."
                                                   "Disagree"
                                                   "Agree"
                                                   9)
                                          
                                           (instruction "Provide feedback on tests that are not correct by clicking on the line number and adding a comment.")
                                          
                                           (likert "coverage"
                                                   "These tests cover the possible inputs."
                                                   "Disagree"
                                                   "Agree"
                                                   9)
                                           
                                           (free-form "not-covered"
                                                      "If applicable, provide inputs that are not covered by the tests.")))
                     
                     (student-submission "student-review"
                                         1
                                         (rubric
                                          (likert "correctness"
                                                  "These tests are correct."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                          (instruction "Provide feedback on tests that are not correct by clicking on the line number and adding a comment.")
                                          
                                          (likert "coverage"
                                                  "These tests cover the possible inputs."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                          (free-form "not-covered"
                                                     "If applicable, provide inputs that are not covered by the tests."))))
               
               (step "implementation"
                     "Submit all of your test cases and your clock implementation."
                     
                     (instructor-solution "poor-implementation"
                                          (rubric
                                          (likert "behavior"
                                                  "This code correctly implements the desired behavior."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                          (instruction "If applicable, leave inline feedback where the incorrect behaviors exist.")
                                          
                                          (likert "structure"
                                                  "This code is structured well."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                          (instruction "If applicable, leave inline feedback where the code is not structured well.")
                                          
                                          (free-form "feedback"
                                                     "Additional Comments")))
                     
                     (instructor-solution "good-implementation"
                                          (rubric
                                          (likert "behavior"
                                                  "This code correctly implements the desired behavior."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                          (instruction "If applicable, leave inline feedback where the incorrect behaviors exist.")
                                          
                                          (likert "structure"
                                                  "This code is structured well."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                          (instruction "If applicable, leave inline feedback where the code is not structured well.")
                                          
                                          (free-form "feedback"
                                                     "Additional Comments")))
                     
                     (student-submission "student-review"
                                         1
                                         (rubric
                                         (likert "behavior"
                                                  "This code correctly implements the desired behavior."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                         (instruction "If applicable, leave inline feedback where the incorrect behaviors exist.")
                                          
                                         (likert "structure"
                                                  "This code is structured well."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                         (instruction "If applicable, leave inline feedback where the code is not structured well.")
                                          
                                         (free-form "feedback"
                                                     "Additional Comments"))))))

(define test-step (step "implementation"
                     "Submit all of your test cases and your clock implementation."
                     
                     (instructor-solution "poor-implementation"
                                          (rubric
                                          (likert "behavior"
                                                  "This code correctly implements the desired behavior."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                          (instruction "If applicable, leave inline feedback where the incorrect behaviors exist.")
                                          
                                          (likert "structure"
                                                  "This code is structured well."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                          (instruction "If applicable, leave inline feedback where the code is not structured well.")
                                          
                                          (free-form "feedback"
                                                     "Additional Comments")))
                     
                     (instructor-solution "good-implementation"
                                          (rubric
                                          (likert "behavior"
                                                  "This code correctly implements the desired behavior."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                          (instruction "If applicable, leave inline feedback where the incorrect behaviors exist.")
                                          
                                          (likert "structure"
                                                  "This code is structured well."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                          (instruction "If applicable, leave inline feedback where the code is not structured well.")
                                          
                                          (free-form "feedback"
                                                     "Additional Comments")))
                     
                     (student-submission "student-review"
                                         1
                                         (rubric
                                         (likert "behavior"
                                                  "This code correctly implements the desired behavior."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                         (instruction "If applicable, leave inline feedback where the incorrect behaviors exist.")
                                          
                                         (likert "structure"
                                                  "This code is structured well."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                         (instruction "If applicable, leave inline feedback where the code is not structured well.")
                                          
                                         (free-form "feedback"
                                                     "Additional Comments")))))