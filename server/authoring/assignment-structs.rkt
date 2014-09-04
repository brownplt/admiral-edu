#lang racket

;; Assignment Description
(provide (contract-out
          [struct Assignment ((name string?) (id string?) (description string?) (steps (non-empty-listof Step?)))]))

;; TODO(3 study): Add next-action-function to Assignments
(struct Assignment (name id description steps) #:transparent)

(provide assignment)
(define (assignment name id description . steps)
  (Assignment name id description steps))

;; Step Description
(provide (contract-out
         [struct Step ((id string?) (instructions string?) (reviews (listof review?)))]))
(struct Step (id instructions reviews) #:transparent)

;; Review Description
(provide step)
(define (step id instructions . reviews)
  (Step id instructions reviews))

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

