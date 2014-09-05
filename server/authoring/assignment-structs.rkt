#lang racket

;; Assignment Description
(provide (struct-out Assignment))
(struct Assignment (name id description assignment-handler steps) #:transparent)

(provide assignment)
(define (assignment name id description assignment-handler . steps)
  (Assignment name id description assignment-handler steps))

;; Step Description
(provide (contract-out
         [struct Step ((id string?) (instructions string?) (reviews (listof review?)))]))
(struct Step (id instructions reviews) #:transparent)

;; Review Description
(provide step)
(define (step id instructions . reviews)
  (Step id instructions reviews))

(provide Review-id)
(define (Review-id review)
  (cond [(student-submission? review) (student-submission-id review)]
        [(instructor-solution? review) (instructor-solution-id review)]))

(provide Review-amount)
(define (Review-amount review)
  (cond [(student-submission? review) (student-submission-amount review)]
        [(instructor-solution? review) 1]))

(provide Review-rubric)
(define (Review-rubric review) 
  (cond [(student-submission? review) (student-submission-rubric review)]
        [(instructor-solution? review) (instructor-solution-rubric review)]
        [else (raise-user-error 'validate-step "Expected to find student-submissions / instructor-solution.")]))

;; Student Submission Review
(provide (contract-out
          [struct student-submission ((id string?) (amount integer?) (rubric Rubric?))]))
(struct student-submission (id amount rubric) #:transparent)

;; Instructor Solution Review
(provide (contract-out
          [struct instructor-solution ((id string?) (rubric Rubric?))]))
(struct instructor-solution (id rubric) #:transparent)

;; Rubric Description
(provide (contract-out 
          [struct Rubric ((elements (non-empty-listof rubric-element?)))]))
(struct Rubric (elements) #:transparent)

(provide rubric)
(define (rubric . elements)
  (Rubric elements))

;; Rubric Element Descriptions

;; Instruction
(provide (contract-out
          [struct instruction ((text string?))]))
(struct instruction (text) #:transparent)

;; Likert
(provide (contract-out
          [struct likert ((id string?) (text string?) (min string?) (max string?) (granularity integer?))]))
(struct likert (id text min max granularity) #:transparent)

;; Free Form
(provide (contract-out
          [struct free-form ((id string?) (text string?))]))
(struct free-form (id text) #:transparent)

(provide rubric-element?)
(define rubric-element? 
  (or/c instruction? likert? free-form?))

(provide review?)
(define review?
  (or/c student-submission? instructor-solution?))



;; Used to state a students next action is to submit to the step
(provide (struct-out MustSubmitNext))
(struct MustSubmitNext (step instructions) #:transparent)

;; Used to state a students next action is to review on a step
(provide (struct-out MustReviewNext))
(struct MustReviewNext (step reviews) #:transparent) 

;; Assignment Handler
;; next-action: (assignment -> steps -> (Either MustSubmitNext MustReviewNext #t))
;;   Given an assignment-id and the list of steps to complete, returns the next-action the user must take
;;   or #t if the user has completed the assignment
;; do-submit-step: (assignment -> step -> uid -> file-name -> data -> steps -> (Either Success Failure))
;;   Given an assignment, a step, user id, file name being submitted, the data of the file, and a list of assignment steps
;;   attempts to submit the data for the specified assignment user and step.
(provide (struct-out AssignmentHandler))
(struct AssignmentHandler (next-action do-submit-step))
         

(provide (struct-out Success))
(struct Success (message))

(provide (struct-out Failure))
(struct Failure (message))

(provide failure)
(define (failure . messages)
  (Failure (apply string-append messages)))

(provide dependency-submission-name)
(define (dependency-submission-name review-id n)
  (string-append "default-submission-" review-id "-" (number->string n)))