#lang racket

(require web-server/servlet)
(require "../ct-session.rkt")
(require web-server/http/bindings
         web-server/templates
         web-server/http/response-structs
         xml
         json)

(provide not-authorized)
(define (not-authorized)
  (let ([display-message "You are not authorized to access this page."])
    (include-template "html/error.html")))

(provide assignment-closed)
(define (assignment-closed)
  "<p>The assignment you were attempting to access is currently closed.</p>")

(provide exception-occurred)
(define (exception-occurred exn)
  (printf "Exception Caught: ~a\n\n" exn)
  ;; TODO Recursively print out exception information
  ;; TODO Send email with exception output to self.
  (response/full
   200 #"Okay"
   (current-seconds) TEXT/HTML-MIME-TYPE
   empty
   (list (string->bytes/utf-8 "An error occurred while processing your request. This has been reported. Please try again later."))))

(provide response-error)
(define (response-error message)
  (response/full
   200 #"Okay"
   (current-seconds) TEXT/HTML-MIME-TYPE
   empty
   (list (string->bytes/utf-8 (error message)))))

(provide error)
(define (error message)
  (let ([display-message message])
    (include-template "html/error.html")))

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

(provide four-oh-four)
(define (four-oh-four)
  (response/xexpr
   '(html (body (p "404")))))

(provide error-page)
(define (error-page . message)
  (let ([display-message (apply string-append message)])
    (include-template "html/error.html")))
    