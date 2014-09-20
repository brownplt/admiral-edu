#lang racket


(provide (struct-out Failure))
(struct Failure (message) #:transparent)

(provide wrap-failure)
(define (wrap-failure procedure)
  (let ((wrap-failure-prime (lambda (exn) (Failure (format "An exception was raised: ~a\n" (log-exception exn))))))
    (with-handlers [(exn? wrap-failure-prime)] (procedure))))
  

(provide log-exception)
(define (log-exception exn)
  (cond [(not (exn? exn)) "Not an exception."]
        [else 
         (let* ((message (exn-message exn)))
                ;(marks (exn-continuation-marks exn))
                ;(stack (if (continuation-mark-set? marks) (continuation-mark-set->context marks) #f))
                ;(stack-output (if stack (map log-stack-elem stack) '())))'
           
           (apply string-append 
                  (append `(,(format "Caught Exception: ~a\n" exn)
                            ,(format "Message: ~a\n" message)))))]))
                          ;stack-output)))]))

(define (log-stack-elem elem) 
  (let ((label (if (null? elem) "function-name???" (car elem)))
        (srcloc (if (and (not (null? elem)) (srcloc? (cdr elem))) (srcloc->string (cdr elem)) "No Source Location")))
  (format "~a - ~a\n" label srcloc)))