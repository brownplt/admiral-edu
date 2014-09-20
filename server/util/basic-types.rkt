#lang typed/racket


(provide (struct-out Failure))
(struct Failure ([message : String]) #:transparent)

(provide Result)
(define-type (Result A) (U (Success A) Failure))

(provide (struct-out Success))
(struct: (A) Success ([result : A]) #:transparent)

(provide failure)
(: failure (String * -> Failure))
(define (failure . messages)
  (Failure (apply string-append messages)))


