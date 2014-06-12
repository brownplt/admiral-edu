#lang racket

(require web-server/http/bindings
         web-server/templates
         web-server/http/response-structs
         xml)

(require "../ct-session.rkt"
         "../database/mysql.rkt"
         "../config.rkt")

(define (repeat val n)
  (cond
    [(<= n 0) '()]
    [else (cons val (repeat val (- n 1)))]))

(provide load)
(define (load session role rest [message '()])
  (let* ((assignment (car rest))
         (step (cadr rest))
         (updir (apply string-append (repeat "../" (length rest))))
         [file-container (string-append updir "file-container/" (to-path rest))])
    (include-template "html/review.html")))

(provide file-container)
(define (file-container session role rest [message '()])
  (let* ([assignment (xexpr->string (car rest))]
         (stepName (cadr rest))
         [step (to-step-link stepName (- (length rest) 2))]
         [path (to-path-html (cddr rest))]
         (r (review:select-review assignment (ct-session-class session) stepName (ct-session-uid session)))
         (reviewee (car r))
         (version (cdr r)))
    (string-append (include-template "html/file-container-header.html")
                   (retrieve-submission-file (ct-session-class session) reviewee assignment stepName version (to-path (cddr rest)))
                   (include-template "html/file-container-footer.html"))))

(define (to-step-link step depth)
  (if (<= depth 0) (xexpr->string step)
      (let ((updepth (string-append (apply string-append (repeat "../" depth)) (xexpr->string step))))
        (string-append "<a href=\"" updepth "\">" (xexpr->string step) "</a>"))))

(define (to-path ls)
  (letrec ((helper (lambda (acc ls)
                     (match ls
                       ['() (apply string-append (reverse acc))]
                       [(cons head '()) (let ((new-acc (cons head acc)))
                                          (helper new-acc '()))]
                       [(cons head tail) (let ((new-acc (cons "/" (cons head acc))))
                           
                                           
                                           (helper new-acc tail))]))))
    (helper '() ls)))

(define (to-path-html input)
  (letrec ((helper (lambda (acc ls)
                     (match ls
                       ['() (apply string-append (reverse acc))]
                       [(cons head '()) (let ((new-acc (cons head acc)))
                                          (helper new-acc '()))]
                       [(cons head tail) (let* ((url (string-append (apply string-append (repeat "../" (- (length input) (+ (length acc) 1)))) (xexpr->string head) "/"))
                                                (link (string-append " <a href=\"" url "\">" (xexpr->string head) "</a> / "))
                                                (new-acc (cons link acc)))
                                           (helper new-acc tail))]))))
    (helper '() input)))