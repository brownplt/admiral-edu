#lang racket
(require web-server/servlet
         web-server/dispatch
         racket/date)

(require 
  "auth/google-openidc.rkt"
  "base.rkt")

(require "pages/index.rkt"
         (prefix-in review: "pages/review.rkt")
         (prefix-in error: "pages/errors.rkt")
         (prefix-in author: "pages/author.rkt")
         "pages/next.rkt"
         (prefix-in assignments: "pages/assignments.rkt")
         (prefix-in export: "pages/export.rkt")
         (prefix-in submit: "pages/submit.rkt")
         (prefix-in dep: "pages/dependencies.rkt")
         (prefix-in feedback: "pages/feedback.rkt")
         (prefix-in roster: "pages/roster.rkt")
         (prefix-in browse: "pages/browse.rkt")
         (prefix-in typed: "dispatch-typed.rkt"))

(define (any? x)
  #t)

;; Defines how to process incomming requests are handled
(provide ct-rules)
(define-values (ct-rules mk-url)
  (dispatch-rules
   [((string-arg) ...) (handler #f)]
   [((string-arg) ...) #:method "post" (handler #t)]
   [else error:four-oh-four]))

(define (handler post)
  (lambda (req path)
    (let* ((raw-bindings (request-bindings/raw req))
           (bindings (request-bindings req))
           (post-data (request-post-data/raw req))
           (clean-path (filter (lambda (x) (not (equal? "" x))) path))
           (start-rel-url (ensure-trailing-slash (string-append "/" class-name "/" (string-join path "/"))))
           (session (get-session req (make-table start-rel-url bindings)))
           (result (with-handlers ([any? error:exception-occurred]) (handlerPrime post post-data session bindings raw-bindings clean-path))))
      result)))


(define (ensure-trailing-slash candidate)
  (let ((len (string-length candidate)))
    (cond [(= 0 len) "/"]
          [else (let ((last-char (string-ref candidate (- len 1))))
                  (cond [(eq? #\/ last-char) candidate]
                        [else (string-append candidate "/")]))])))

(define (handlerPrime post post-data session bindings raw-bindings path)
  (printf "[~a] ~a ~a - ~a ~a [~a]\n" (date->string (current-date) #t) (ct-session-class session) (ct-session-uid session) (if post "POST" "GET") path session) (flush-output)
  (match path
    ['() (render session index)]
    [(list "") (render session index)]
    [(cons "review" rest) (cond [post (review:post->review session post-data rest)]                                
                                [else (render-html session review:load rest)])]
    [(cons "file-container" rest) (cond [post (review:push->file-container session post-data rest)]
                                        [(and (> (length rest) 1)
                                              (string=? "download" (list-ref rest (- (length rest) 2)))) (render-any session review:check-download rest)]
                                        [(render-html session review:file-container rest)])]
    [(cons "su" (cons uid rest)) (with-sudo post post-data uid session bindings raw-bindings rest)]
    [(cons "author" rest) (if post (author:post->validate session post-data rest) (render-html session author:load rest))]
    [(cons "next" rest) (render-html session next rest)]
    [(cons "dependencies" rest) (if post (dep:post session rest bindings raw-bindings) (render-html session dep:dependencies rest))]
    [(cons "submit" rest) (if post (submit:submit session role rest bindings raw-bindings) (error:response-error 
                                                                                            (error:error (string-append "<p>You've accessed this page in an invalid way.</p>"
                                                                                                                  "<p>Try returning to <a href='https://" sub-domain server-name "/" class-name "/'>Class Home</a> and trying again.</p>"))))]
    [(cons "feedback" rest) (if post (feedback:post session role rest bindings post-data) (render-html session feedback:load rest))]
    [(cons "export" rest) (export:load session (role session) rest)]
    [(cons "exception" rest) (error "Test an exception occurring.")]
    [(cons "roster" rest) (if post (render-html session (roster:post post-data bindings) rest) (render-html session roster:load rest))]
    [(cons "browse" rest) (cond [(and (> (length rest) 1)
                                      (string=? "download" (list-ref rest (- (length rest) 2)))) (render-any session browse:download rest)]
                                [else (render-html session browse:load rest)])]
    [else (typed:handlerPrime post post-data session bindings raw-bindings path)]))

(define (require-auth session f)
  (let* ((user-role (role session))
         (can-sudo (if user-role (roles:Record-can-edit user-role) #f)))
    (if can-sudo (f) (error:not-authorized))))

(define (with-sudo post post-data uid session bindings raw-bindings path)
  (let* ((user-role (role session))
         (can-sudo (if user-role (roles:Record-can-edit user-role) #f))
         (new-session (ct-session (ct-session-class session) uid (ct-session-table session))))
    (if (not can-sudo) (error:four-oh-four)
        (handlerPrime post post-data new-session bindings raw-bindings path))))


(define (initialization session role [message '()])
  `(html
      (p "The service has been initialized to a fresh state.")))

;; Defines how a session is created
;; request -> ct-session
(define (get-session req trailing-slash)
  (ct-session class-name (req->uid req) trailing-slash))

;; Returns #f if the session is not valid
;; otherwise returns a role-record
(define (role session)
  (let* ((class (ct-session-class session))
         (uid (ct-session-uid session)))
    (cond [(role:exists? class uid) (role:select class uid)]
          [else #f])))

;; If the session has a valid role, renders the specified page. Otherwise,
;; this displays an error message
(define (render session page)
  (let ((valid-role (role session)))
    (response/xexpr
     (if (not valid-role) 
         (error:error-not-registered session)
         (page session valid-role)))))

(define (render-html session page rest)
  (let ((valid-role (role session)))
    (if (not valid-role)
        (response/xexpr (error:error-not-registered session))
        (response/full
         200 #"Okay"
         (current-seconds) TEXT/HTML-MIME-TYPE
         empty
         (list (string->bytes/utf-8 (page session valid-role rest)))))))

(define (render-any session page rest)
    (let ((valid-role (role session)))
    (if (not valid-role)
        (response/xexpr (error:error-not-registered session))
        (page session valid-role rest))))



;; If the session has a valid role, renders the specified page with the specified bindings. 
;; Otherwise, this displays an error message
(define (post->render session page bindings)
  (let ((valid-role (role session)))
    (response/xexpr
     (if (not valid-role) 
         (error:error-not-registered session)
         (page session valid-role bindings)))))
