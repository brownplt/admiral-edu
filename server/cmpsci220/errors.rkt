#lang racket

(require "../ct-session.rkt")

(provide not-registered)
(define (not-registered session)
  `(html 
    (body 
     (h1 "Error") 
     (p "You don't appear to be registered for this class.")
     (p , (string-append "User ID: " (ct-session-uid session)))
     (p , (string-append "Class ID: " (ct-session-class session))))))