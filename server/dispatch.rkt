#lang racket
(require web-server/servlet
         web-server/servlet-dispatch
         web-server/web-server
         web-server/dispatch)

(require 
  "auth/google-openidc.rkt"
  "config.rkt"
  "ct-session.rkt"
  "database/mysql.rkt")

(define erase-directory
    (lambda (assignment)
      (lambda (stepName)
        (lambda (reviewee)
          (let ((path (string-append "reviews/" (cadr assignment) "/" (car assignment) "/" stepName "/" reviewee )))
            (delete-file path))))))

(define (applicative xs ls)
  (map (lambda (f) (map f xs)) ls))

(require "pages/index.rkt"
         (prefix-in review: "pages/review.rkt")
         "pages/errors.rkt"
         (prefix-in author: "pages/author.rkt")
         "pages/next.rkt"
         "pages/assignments.rkt"
         (prefix-in submit: "pages/submit.rkt")
         (prefix-in dep: "pages/dependencies.rkt")
         (prefix-in feedback: "pages/feedback.rkt"))

;; Defines how to process incomming requests are handled
(provide ct-rules)
(define-values (ct-rules mk-url)
  (dispatch-rules
   [((string-arg) ...) (handler #f)]
   [((string-arg) ...) #:method "post" (handler #t)]
   [else four-oh-four]))

(define (handler post)
  (lambda (req path)
    (let ((session (get-session req))
          (bindings (request-bindings req))
          (post-data (request-post-data/raw req))
          (clean-path (filter (lambda (x) (not (equal? "" x))) path)))
      (handlerPrime post post-data session bindings clean-path))))

(define (handlerPrime post post-data session bindings path)
  (print (list post path)) (newline)
  (match path
    ['() (if post (post->render session post->index bindings) (render session index))]
    [(list "") (if post (post->render session post->index bindings) (render session index))]
    [(cons "review" rest) (if post (review:post->review session post-data rest) (render-html session review:load rest))]
    [(cons "file-container" rest) (if post (review:push->file-container session post-data rest) (render-html session review:file-container rest))]
    [(cons "su" (cons uid rest)) (with-sudo post post-data uid session bindings rest)]
    [(cons "author" rest) (if post (author:post->validate session post-data rest) (render-html session author:load rest))]
    [(cons "next" rest) (render-html session next rest)]
    [(cons "assignments" rest) (render-html session assignments rest)]
    [(cons "dependencies" rest) (if post (dep:post session rest bindings) (render-html session dep:dependencies rest))]
    [(cons "submit" rest) (if post (submit:submit session role rest bindings) #f)] ;;TODO Handle correctly when not a post
    [(cons "feedback" rest) (if post (feedback:post session role rest post-data) (render-html session feedback:load rest))]
    [else (four-oh-four)]))

(define (with-sudo post post-data uid session bindings path)
  (let* ((user-role (role session))
         (can-sudo (if user-role (roles:role-can-edit user-role) #f))
         (new-session (ct-session (ct-session-class session) uid)))
    (if (not can-sudo) (four-oh-four)
        (handlerPrime post post-data new-session bindings path))))


(define (initialization session role [message '()])
  `(html
      (p "The service has been initialized to a fresh state.")))

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

(define (render-html session page rest)
  (let ((valid-role (role session)))
    (if (not valid-role)
        (response/xexpr (error-not-registered session))
        (response/full
         200 #"Okay"
         (current-seconds) TEXT/HTML-MIME-TYPE
         empty
         (list (string->bytes/utf-8 (page session valid-role rest)))))))

;; If the session is valid, tries to render the specified page. Othewise,
;; this responds with an invalid session error
(define (dispatch-html page)
  (lambda (req . rest)
    (let ((session (get-session req)))
      (if (eq? session 'invalid-session) 
          (response/xexpr error-invalid-session)
          (render-html session page rest)))))
    
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
