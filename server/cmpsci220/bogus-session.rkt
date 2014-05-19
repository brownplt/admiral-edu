#lang racket

(require "../ct-session.rkt")
  
(provide req->session)

;; Instructor account
(define (req->session req)
  (ct-session "cmpsci220" "jcollard"))

;; Student account
;;(define (req->session req)
;;  (ct-session "cmpsci220" "jcollard"))

;; None existent account
;;(define (req->session req)
;;  (ct-session "cmpsci220" "no-one"))


