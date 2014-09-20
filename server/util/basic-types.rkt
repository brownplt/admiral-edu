#lang typed/racket

(require/typed "basic-types-untyped.rkt"
               [#:struct Failure ([message : String])]
               [wrap-failure (All (A) (-> A) -> (U A Failure))])

; (struct Failure (message) #:transparent)
(provide (struct-out Failure))

; (All (A) ((-> A) -> (U A Failure))))
; Given a thunk, attempts to evaluate it. If an exception is raised
; returns a Failure with diagnostic information about the exception.
; Otherwise returns A.
(provide wrap-failure)

(provide Result)
(define-type (Result A) (U (Success A) Failure))

(provide (struct-out Success))
(struct: (A) Success ([result : A]) #:transparent)

(provide failure)
(: failure (String * -> Failure))
(define (failure . messages)
  (Failure (apply string-append messages)))


