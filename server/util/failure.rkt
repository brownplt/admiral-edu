#lang typed/racket

(require "basic-types.rkt")

(require/typed "failure-untyped.rkt"
               [untyped-wrap-failure (All (A) (-> A) -> (U A Failure))])

; (All (A) ((-> A) -> (U A Failure))))
; Given a thunk, attempts to evaluate it. If an exception is raised
; returns a Failure with diagnostic information about the exception.
; Otherwise returns A.
(provide wrap-failure)
(define wrap-failure untyped-wrap-failure)