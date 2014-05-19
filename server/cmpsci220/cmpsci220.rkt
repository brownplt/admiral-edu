#lang racket

(require web-server/servlet
         web-server/servlet-dispatch
         web-server/web-server
         web-server/dispatch)

(require "bogus-session.rkt"
         "config.rkt"
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
;; otherwise returns a role-record
(define (role session)
  (let* ((class (ct-session-class session))
         (uid (ct-session-uid session))
         (result (select-role class uid)))
    (if (eq? result '()) 
        #f
        (get-role-record (vector-ref (car result) 0)))))

(define (render session page)
  (let ((valid-role (role session)))
    (response/xexpr
     (if (not valid-role) 
         (error-not-registered session)
         (page session valid-role)))))

(define (dispatch page)
  (lambda (req)
    (let ((session (get-session req)))
      (if (eq? session 'invalid-session) 
          (response/xexpr error-invalid-session)
          (render session page)))))

(define (post->render session page bindings)
  (let ((valid-role (role session)))
    (response/xexpr
     (if (not valid-role) 
         (error-not-registered session)
         (page session valid-role bindings)))))

(define (post->dispatch page)
  (lambda (req)
    (let ((session (get-session req))
          (bindings (request-bindings req)))
      (if (eq? session 'invalid-session)
          (response/xexpr error-invalid-session)
          (post->render session page bindings)))))

(initialize)