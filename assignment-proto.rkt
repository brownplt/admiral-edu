#lang racket

(require "db-proto.rkt"
         web-server/lang/serial-lambda)

;; Initializes the database, initializes students and class,
;; and initializes the assignment
(provide init-sample)
(define (init-sample)
  (init-db)
  (init-class)
  (init-assignment))

;; The assignment is called "tic-tac-toe"
(define assignment_id "tic-tac-toe")
;; It will be registered to the class "cs220"
(define class_id "cs220")

;; There are two groups for this assignment 
(define instructor-reviews "instructor-reviews")
(define no-instructor-reviews "no-instructor-reviews")

;; When we initialize the class we create a record
;; for the class, 4 students, and register each student
;; on the class
(define (init-class)
  (create-class class_id)
  (create-student "bwayne")
  (create-student "bbanner")
  (create-student "pparker")
  (create-student "tstark")
  (register-student "bwayne" class_id)
  (register-student "bbanner" class_id)
  (register-student "pparker" class_id)
  (register-student "tstark" class_id))

;; Initialization function for this assignment to be run once at start up
;; Create the assignment specifying its id, iterator, and generator
;; Create two groups for the assignment
;; Then ask the iterator to initialize each student in the class
(define (init-assignment)
  (create-assignment assignment_id iterator generator)
  (create-group instructor-reviews assignment_id)
  (create-group no-instructor-reviews assignment_id)
  (iterator "bwayne" 'init 'init)
  (iterator "bbanner" 'init 'init)
  (iterator "pparker" 'init 'init)
  (iterator "tstark" 'init 'init)
  #t)

(provide result result-message result-next_step fail fail-message finished success)
(struct result (message next_step) #:transparent)
(struct success (message) #:transparent)
(struct fail (message) #:transparent)
(struct finished (message) #:transparent)


;; Sample assignment iterator
(define iterator
  (serial-lambda
   (student_id step_id resource) 
   (cond
     ;; If the student is not registered for this class, fail
   [(not (is-registered? student_id class_id))
    (fail "The provided student id is not registed in the specified class")]
   
   ;; If the student has not started, select a group for the student
   [(not (has-started? student_id assignment_id)) (select-group student_id)]
   
   ;; If the student has finished the assignment, tell them they are done
   [(is-finished? student_id assignment_id) (finished "Final Submission successful.")]
   
   ;; Otherwise, if the student is allowed to submit for this particular assignment and step
   ;; try to validate their submission
   [(can-submit student_id assignment_id step_id) (process-submission student_id step_id resource)]
   [else (fail "Could not submit on this step")])))

;; Selects a group for a student. This alternates each time
;; a student initializes. Once initialized, the student is assigned
;; the task "submit_data_definition"
(define (select-group student_id)
  (let* ((inst-size (group-size instructor-reviews assignment_id))
         (no-inst-size (group-size no-instructor-reviews assignment_id))
         (group (if (> inst-size no-inst-size) no-instructor-reviews instructor-reviews)))
    (assign-group group assignment_id student_id)
    (set-status student_id assignment_id "submit_data_definition")
    (result "Assignment started." "submit_data_definition")))



;; For this assignment, we always accept a submission.
;; We create a record of the submission, then we determine
;; the next step for the student. If this was the final submission,
;; we return the finished message. Othewise, we say the submission
;; was successful and find the next step.
(define (process-submission student step resource)
  (if (hash-has-key? handlers step)
      (let ((handler (hash-ref handlers step)))
        (handler student step resource))
      (fail (string-append "Invalid step: " step))))

;; The next step is dependent on which group the student
;; is in
(define (determine-next-step student step)
  (let ((group (get-students-group student assignment_id)))
    (cond
      [(string=? group instructor-reviews) (next-inst-step student step)]
      [(string=? group no-instructor-reviews) (next-no-inst-step student step)])))

;; If the student is in the instructor review group
;; they get three reviews, 1 good instructor, 1 bad instructor
;; and 1 student review
(define (next-inst-step student step)
  (cond
    [(string=? step "submit_data_definition") "review_good_test"]
    [(string=? step "review_good_test") "review_bad_test"]
    [(string=? step "review_bad_test") "review_student_test"]
    [(string=? step "review_student_test") "final_submission"]
    [(string=? step "final_submission") "finished"]))

;; If the student is in the no-instructor review group
;; they get three reviews all of which come from other students.
(define (next-no-inst-step student step)
  (cond
    [(string=? step "submit_data_definition") "review_student_test0"]
    [(string=? step "review_student_test0") "review_student_test1"]
    [(string=? step "review_student_test1") "review_student_test2"]
    [(string=? step "review_student_test2") "final_submission"]
    [(string=? step "final_submission") "finished"]))
    
(provide serv-request serv-request-target serv-request-resource)
(struct serv-request (target resource))


(define (always-valid contents) #t)

(define (git-handler root-url filename validator)
  (lambda (student-id step resource)
    (let ((result (checkout root-url student-id resource filename)))
      (match result
        [(success contents) (let ((validation (validator contents)))
                              (if validation
                                  (process-contents contents student-id step)
                                  (fail "Submission could not be validated.")))]
        [(fail message) (fail (string-append "Submission failed: " message))]))))
      
(define (process-contents contents student-id step)
  (let ((next-step (determine-next-step student-id step)))
    (create-submission student-id assignment_id step contents)
    (set-status student-id assignment_id next-step)
    (assign-review student-id next-step)
    (result "Submission received." next-step)))

(define (assign-review student review_step)
  (cond
    [(string=? review_step "review_student_test") (choose-student-review student "submit_data_definition" review_step)]
    [(string=? review_step "review_student_test0") (choose-student-review student "submit_data_definition" review_step)]
    [(string=? review_step "review_student_test1") (choose-student-review student "submit_data_definition" review_step)]
    [(string=? review_step "review_student_test2") (choose-student-review student "submit_data_definition" review_step)]
    [else (void)]))

;; Not concurrently safe
(define (choose-student-review student step review_step)
  (let ((reviewee (get-least-reviewed assignment_id step)))
    (create-review student reviewee assignment_id step review_step)))

(define (checkout root-url student-id hash filename)
  (if (system (string-append "./checkout.sh " root-url " " student-id " " hash))
      (let ((contents (get-contents student-id filename)))
        (system (string-append "rm -rf " student-id))
        contents)
      (fail (string-append "Could not locate commit hash: " hash))))

(define (get-contents student-id filename)
  (let ((path (string-append student-id "/" filename)))
  (if (file-exists? path)
      (let* ((input (open-input-file path))
             (contents (port->string input))
             (closed (close-input-port input)))
        (success contents))
      (fail (string-append "Could not locate file: " filename)))))

(define (rubric-handler student-id step resource)
  (let ((next-step (determine-next-step student-id step)))
    (set-status student-id assignment_id next-step)
    (result "Rubric submitted." next-step)))

;; Sample request generator
(define generator 
  (serial-lambda 
   (student_id step_id)
   (cond
     [(string=? step_id "submit_data_definition") (serv-request "assignment/submit" "Please submit a commit hash.")]
     [(string=? step_id "review_good_test") (serv-request "assignment/submit" "Some predefined good resource to review.")]
     [(string=? step_id "review_bad_test") (serv-request "assignment/submit" "Some predefined bad resource to review")]
     [(string=? step_id "review_student_test") (serv-request "assignment/submit" (get-review-resource student_id assignment_id step_id))]
     [(string=? step_id "review_student_test0") (serv-request "assignment/submit" (get-review-resource student_id assignment_id step_id))]
     [(string=? step_id "review_student_test1") (serv-request "assignment/submit" (get-review-resource student_id assignment_id step_id))]
     [(string=? step_id "review_student_test2") (serv-request "assignment/submit" (get-review-resource student_id assignment_id step_id))]
     [(string=? step_id "final_submission") (serv-request "assignment/submit" "Please submit a commit hash.")]
     [(string=? step_id "finished") (serv-request "" "")]
     [else 'invalid_step_id])))



(define handlers
  (hash  "submit_data_definition" (git-handler "https://github.com/jcollard/" "submission.scala" always-valid)
        "review_good_test" rubric-handler
        "review_bad_test" rubric-handler
        "review_student_test" rubric-handler
        "review_student_test0" rubric-handler
        "review_student_test1" rubric-handler
        "review_student_test2" rubric-handler
        "final_submission" (git-handler "https://github.com/jcollard/" "submission.scala" always-valid)))