#lang racket

(require web-server/http/bindings)
(require web-server/templates)
(require web-server/http/response-structs)

(require "../ct-session.rkt"
         "../database/mysql.rkt"
         "../config.rkt")

(provide load)
(define (load session role rest [message '()])
  (let* ((assignment (car rest))
         (step (cadr rest))
         [file-container (string-append "../../file-container/" assignment "/" step "/")])
    (include-template "html/review.html")))

(provide file-container)
(define (file-container session role rest [message '()])
  (let ((assignment (car rest))
        (step (cadr rest)))
    (string-append (include-template "html/file-container-header.html")
                   (retrieve-submission-file (ct-session-class session) (ct-session-uid session) assignment step "0" "sample.scala")
                   (include-template "html/file-container-footer.html"))))

