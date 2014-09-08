#lang racket

(require "assignment-structs.rkt"
         "../base.rkt")

(provide get-dependencies)
(define (get-dependencies assignment)
  (cond [(not (Assignment? assignment)) (raise-argument-error 'determine-dependencies "Assignment" assignment)]
        [else (flatten (map (step-dependencies (Assignment-id assignment)) (Assignment-steps assignment)))]))     

(define (step-dependencies assignment-id)
  (lambda (step)
    (map (determine-dependency assignment-id (Step-id step)) (Step-reviews step))))

(define (determine-dependency assignment-id step-id)
  (lambda (review)
    (let* ((review-id (Review-id review))
           (met (lambda (n) (check-upload assignment-id step-id review-id n))))
      (cond [(instructor-solution? review) (instructor-solution-dependency (met 1) step-id (Review-id review))]
            [(student-submission? review) 
             (let ((amount (student-submission-amount review)))
               (student-submission-dependency (met amount) step-id (Review-id review) amount))]
            [else (raise (format "Unknown dependency type: ~a" review))]))))

(define (check-upload assignment-id step-id review-id n)
  (cond [(<= n 0) #t]
        [(not (submission:exists? assignment-id class-name step-id (dependency-submission-name review-id n))) #f]
        [else (check-upload assignment-id step-id review-id (- n 1))]))

