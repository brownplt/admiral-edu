#lang racket
(require web-server/servlet
         web-server/servlet-dispatch
         web-server/web-server
         web-server/dispatch
         "bogus-session.rkt"
         "ct-session.rkt"
         "database/mysql.rkt")


(define (get-session req)
  (req->session req))

(define-values (start mk-url)
  (dispatch-rules
   [("") index]))

;; Returns #f if the session is not valid
;; Returns 0 if the session is for a student
;; Returns 1 if the session is for an instructor
(define (role session)
  (let* ((class (ct-session-class session))
         (uid (ct-session-uid session))
         (result (select-role class uid)))
    (match result
      ['() #f]
      ['(#(0)) 0]
      ['(#(1)) 1])))

(define (index req)
  (let* ((session (get-session req))
         (uid (ct-session-uid session))
         (valid-role (role session))
         (class (ct-session-class session)))
    (response/xexpr
     (if (not valid-role) `(html (body (h1 "Error") (p , (string-append "Could not find " uid " in " class))))
     `(html
       (body
        (h1 ,class)
        (p ,(string-append "Logged in as: " uid))
        (p ,(string-append "Your role is " (if (= 1 valid-role) "Instructor" "Student")))))))))
  
(init-db)
(create-class "cmpsci220")
(create-user "arjunguha")
(create-user "jcollard")
(create-role "cmpsci220" "arjunguha" 1)
(create-role "cmpsci220" "jcollard" 0)

(serve #:dispatch (dispatch/servlet start)
       #:port 8080)

(define-values (in out) (make-pipe))

(read-line in)
