#lang racket

(require "../../base.rkt"
         "../../authoring/assignment.rkt"
         (prefix-in action: "action.rkt"))

(provide load)
(define (load session url message [post #f])
  (let* ((assignment-id (first url))
         (assignment (assignment-id->assignment assignment-id)))
    (append
     `((h1 (action:assignments "Assignments"))
       (h2 (action:dashboard assignment-id assignment-id))
       (h3 "Step Statistics"))
     (apply append (map (step->statistic assignment-id) (Assignment-steps assignment))))))

(define (step->statistic assignment-id)
  (lambda (step)
    (let ((step-id (Step-id step))
          (reviews (Step-reviews step)))
      ;struct Step (id instructions reviews)
      `((h4 ,(string-append "Step : " step-id))
        ,(cons 'ul 
               (cons
                `(li ,(string-append "Submissions: " (number->string (submission:count-step assignment-id class-name step-id))))
                (map (review->statistic assignment-id step-id) reviews)))))))

(define (review->statistic assignment-id step-id)
  (lambda (review)
    (let ((review-id (Review-id review)))
      `(li ,(string-append "Review : " review-id)
        (ul 
         (li ,(string-append "Completed: " (number->string (review:count-completed-reviews assignment-id class-name step-id review-id)))))))))
      

  
  