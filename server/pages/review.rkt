#lang racket

(require web-server/http/bindings
         web-server/templates
         web-server/http/response-structs
         xml
         json)

(require "../ct-session.rkt"
         "../database/mysql.rkt"
         "../config.rkt"
         "errors.rkt")

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

(provide push->file-container)
(define (push->file-container session post-data rest)
  (cond 
    [(equal? (last rest) "save") (push->save session post-data rest)]
    [(equal? (last rest) "load") (push->load session rest)]
    [else (four-oh-four)]))

(define (push->save session post-data rest)
  (let* ((json (jsexpr->string (bytes->jsexpr post-data)))
         (save-path (get-save/load-path session rest)))
    (write-file save-path json)     
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 "Success")))))

(define (push->load session rest)
  (let* ((load-path (get-save/load-path session rest))
         (data (if (file-exists? load-path) (retrieve-file load-path) "{\"comments\" : {}}")))
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 data)))))

(define (get-save/load-path session rest)
  (let* ((reviewer (ct-session-uid session))
         (class (ct-session-class session))
         (stepName (cadr rest))
         (assignment (car rest))
         (r (review:select-review assignment class stepName reviewer))
         (reviewee (car r))
         (path-to-file (to-path (take (cddr rest) (- (length rest) 3))))
         (path (string-append "reviews/" class "/" assignment "/" stepName "/" reviewee "/" reviewer "/" path-to-file)))
    path))
  
(provide file-container)
(define (file-container session role rest [message '()])
  (let* ([assignment (xexpr->string (car rest))]
         [save-url (prepare-save-url rest)]
         [load-url (prepare-load-url rest)]
         (stepName (cadr rest))
         [step (to-step-link stepName (- (length rest) 2))]
         (last-path (last rest))
         (prefix (if (equal? last-path "") "" (string-append last-path "/")))
         [path (to-path-html (cddr rest))]
         (r (review:select-review assignment (ct-session-class session) stepName (ct-session-uid session)))
         (reviewee (car r))
         (version (cdr r))
         (file-path (submission-file-path (ct-session-class session) reviewee assignment stepName version (to-path (cddr rest))))
         (contents (if (is-directory? file-path) (render-directory prefix file-path) (render-file file-path))))
    (string-append (include-template "html/file-container-header.html")
                   contents
                   (include-template "html/file-container-footer.html"))))

(define (prepare-url word rest)
  (let* ((last-el (last rest))
         (prefix (if (equal? last-el "") "" (string-append last-el "/"))))
    (string-append "\"" prefix word "\"")))

(define (prepare-load-url rest)
  (prepare-url "load" rest))

(define (prepare-save-url rest)
  (prepare-url "save" rest))
   

(define (render-directory prefix dir-path)
  (let ((dirs (sub-directories-of dir-path))
        (files (list-files dir-path)))
    (string-append
     "<div id=\"directory\" class=\"browser\">"
     "<ul>"
     (apply string-append (map (html-directory prefix) dirs))
     (apply string-append (map (html-file prefix) files))
     "</ul>"
     "</div>")))

(define (html-directory prefix)
  (lambda (dir)
    (string-append "<li class=\"directory\"><a href=\"" prefix dir "\">" dir "</a></li>")))

(define (html-file prefix)
  (lambda (file)
    (string-append "<li class=\"file\"><a href=\"" prefix file "\">" file "</a></li>")))

(define (render-file file-path)
  (string-append "<textarea id=\"file\" class=\"file\">" (retrieve-file file-path) "</textarea>"))

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
                       [(cons head tail) (let* ((url (string-append (apply string-append (repeat "../" (- (length input) (+ (length acc) 1)))) (xexpr->string head)))
                                                (link (string-append " <a href=\"" url "\">" (xexpr->string head) "</a> / "))
                                                (new-acc (cons link acc)))
                                           (helper new-acc tail))]))))
    (helper '() input)))