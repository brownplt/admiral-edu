#lang typed/racket

(provide Result)
(define-type (Result a) (U (Success a) Failure))

(provide (struct-out Success))
(struct: (a) Success ([result : a]) #:transparent)

(provide (struct-out Failure))
(struct: Failure ([message : String]) #:transparent)

(provide failure)
(: failure (String * -> Failure))
(define (failure . messages)
  (Failure (apply string-append messages)))