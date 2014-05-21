#lang racket

(require "../ct-session.rkt")

(provide error-not-registered)
(define (error-not-registered session)
  `(html 
    (body 
     (h1 "Error") 
     (p "You don't appear to be registered for this class.")
     (p , (string-append "User ID: " (ct-session-uid session)))
     (p , (string-append "Class ID: " (ct-session-class session))))))

(provide error-invalid-session)
(define error-invalid-session
  '(html
    (body
     (h1 "An Error Occurred")
     (p "This session is not valid. Try to log out and then log in again."))))