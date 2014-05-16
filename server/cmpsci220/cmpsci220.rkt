#lang racket

(require web-server/servlet
         web-server/servlet-dispatch
         web-server/web-server
         web-server/dispatch)

(require "shibboleth/umass-shib.rkt"
         "../ct-session.rkt"
         "../database/mysql.rkt")

(require "index.rkt"
         "errors.rkt")

(provide start)
(define-values (start mk-url)
  (dispatch-rules
   [("") (dispatch index)]
   [("") #:method "post" (post->dispatch post->index)]))

(define (get-session req)
  (req->session req))

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

(define (render session page)
  (let ((valid-role (role session)))
    (response/xexpr
     (if (not valid-role) 
         (not-registered session)
         (page session valid-role)))))

(define (dispatch page)
  (lambda (req)
    (let ((session (get-session req)))
      (render session page))))

(define (post->render session page bindings)
  (let ((valid-role (role session)))
    (response/xexpr
     (if (not valid-role) 
         (not-registered session)
         (page session valid-role bindings)))))

(define (post->dispatch page)
  (lambda (req)
    (let ((session (get-session req))
          (bindings (request-bindings req)))
      (post->render session page bindings))))

(init-db)
(create-class "cmpsci220")
(create-user "arjunguha")
(create-user "jcollard")
(create-role "cmpsci220" "arjunguha" 1)
(create-role "cmpsci220" "jcollard" 0)