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

(provide result result-message result-next_step fail fail-message finished)
(struct result (message next_step) #:transparent)
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
   [(can-submit student_id assignment_id step_id) (validate-resource student_id step_id resource)]
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
(define (validate-resource student step resource)
  (create-submission student assignment_id step resource)
  (let ((next-step (determine-next-step student step)))
    (set-status student assignment_id next-step)
    (cond 
      [(string=? step "final_submission") (finished "Final Submission successful.")]
      [else (result "Submission successful." next-step)])))

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
    
(provide serv-request serv-request-target serv-request-resource-type)
(struct serv-request (target resource-type))

;; Sample request generator
(define generator 
  (serial-lambda 
   (step_id)
   (cond
     [(string=? step_id "submit_data_definition") (serv-request "assignment/submit" "url")]
     [(string=? step_id "review_good_test") (serv-request "assignment/submit" "rubric")]
     [(string=? step_id "review_bad_test") (serv-request "assignment/submit" "rubric")]
     [(string=? step_id "review_student_test") (serv-request "assignment/submit" "rubric")]
     [(string=? step_id "review_student_test0") (serv-request "assignment/submit" "rubric")]
     [(string=? step_id "review_student_test1") (serv-request "assignment/submit" "rubric")]
     [(string=? step_id "review_student_test2") (serv-request "assignment/submit" "rubric")]
     [(string=? step_id "final_submission") (serv-request "assignment/submit" "url")]
     [(string=? step_id "finished") (serv-request "" "")]
     [else 'invalid_step_id])))
   