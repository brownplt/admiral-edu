#lang racket

(require "ct-session.rkt")
  
(provide req->session)
(define (req->session req)
  (ct-session "cmpsci220" "arjunguha"))
