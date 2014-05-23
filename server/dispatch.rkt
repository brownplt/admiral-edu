#lang racket
(require web-server/servlet
         web-server/servlet-dispatch
         web-server/web-server
         web-server/dispatch)

(require 
  "cmpsci220/initialize.rkt"
  "auth/google-openidc.rkt"
  "config.rkt"
  "ct-session.rkt"
  "database/mysql.rkt")

;; Resets the database to a fresh configuration
(initialize)

(require "pages/index.rkt"
         "pages/errors.rkt")

;; Defines how to process incomming requests are handled
(provide ct-rules)
(define-values (ct-rules mk-url)
  (dispatch-rules
   [("") (dispatch index)]
   [("") #:method "post" (post->dispatch post->index)]))

;; Defines how a session is created
;; request -> ct-session
(define (get-session req)
  (ct-session class-name (req->uid req)))

;; Returns #f if the session is not valid
;; otherwise returns a role-record
(define (role session)
  (let* ((class (ct-session-class session))
         (uid (ct-session-uid session))
         (result (role:select class uid)))
    result))

;; If the session has a valid role, renders the specified page. Otherwise,
;; this displays an error message
(define (render session page)
  (let ((valid-role (role session)))
    (response/xexpr
     (if (not valid-role) 
         (error-not-registered session)
         (page session valid-role)))))

;; If the session is valid, tries to render the specified page. Othewise,
;; this responds with an invalid session error
(define (dispatch page)
  (lambda (req)
    (let ((session (get-session req)))
      (if (eq? session 'invalid-session) 
          (response/xexpr error-invalid-session)
          (render session page)))))

;; If the session has a valid role, renders the specified page with the specified bindings. 
;; Otherwise, this displays an error message
(define (post->render session page bindings)
  (let ((valid-role (role session)))
    (response/xexpr
     (if (not valid-role) 
         (error-not-registered session)
         (page session valid-role bindings)))))

;; If the session is valid, tries to render the specified page with any 
;; request-bindings. Othewise, this responds with an invalid session error.
(define (post->dispatch page)
  (lambda (req)
    (let ((session (get-session req))
          (bindings (request-bindings req)))
      (if (eq? session 'invalid-session)
          (response/xexpr error-invalid-session)
          (post->render session page bindings)))))