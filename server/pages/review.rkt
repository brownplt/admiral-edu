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

;;TODO: Ensure that the hash being loaded is assigned to the person who is logged into
;; this session
(provide load)
(define (load session role rest [message '()])
  (let ((submit? (equal? "submit" (car rest))))
    (if submit? 
        (do-submit-review session role rest message)
        (do-load session role rest message))))

(define (do-load session role rest message)
  (let* ((r-hash (car rest))
         (review (review:select-by-hash r-hash))
         (assignment (review:record-assignment-id review))
         (step (review:record-step-id review))
         (updir (apply string-append (repeat "../" (+ (length rest) 1))))
         (root-url updir)
         [submit-url (string-append root-url "review/submit/" r-hash "/")]
         (updir-rubric (apply string-append (repeat "../" (- (length rest) 1))))
         [file-container (string-append updir "file-container/" (to-path rest))]
         [save-url (xexpr->string (string-append "\"" updir-rubric step "/save\""))]
         [load-url (xexpr->string (string-append "\"" updir-rubric step "/load\""))]
         (reviewer (ct-session-uid session))
         (class (ct-session-class session))
         (r (review:select-by-hash r-hash)))
    (if (equal? r 'no-reviews)
        (let ([display-message "There are no reviews available for you at this time."])
          (include-template "html/message.html"))
        (include-template "html/review.html"))))

(define (do-submit-review session role rest message)
  (let* ((r-hash (cadr rest))
         (review (review:select-by-hash r-hash))
         (assignment (review:record-assignment-id review))
         (step (review:record-step-id review))
         (updir (apply string-append (repeat "../" (+ (length rest) 1))))
         (root-url updir))
    (review:mark-complete r-hash)
    (string-append "<p>Review Submitted</p>"
                   "<p><a href='" root-url "next/" assignment "/'>Continue</a></p>")))

(provide post->review)
(define (post->review session post-data rest)
  (let* ((r-hash (car rest))
         (review (review:select-by-hash r-hash)))
    (cond
      [(equal? (last rest) "save") (post->save-rubric session post-data review)]
      [(equal? (last rest) "load") (post->load-rubric session review)]
      [else (four-oh-four)])))

(define (post->save-rubric session post-data review)
  (let ((data (jsexpr->string (bytes->jsexpr post-data)))
        (class (ct-session-class session))
        (assignment (review:record-assignment-id review))
        (stepName (review:record-step-id review))
        (reviewee (review:record-reviewee-id review))
        (reviewer (ct-session-uid session))
        (review-id (review:record-review-id review)))
    (save-rubric class assignment stepName review-id reviewer reviewee data)
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 "Success")))))
  
(define (post->load-rubric session review)
  (let* ((class (ct-session-class session))
         (assignment (review:record-assignment-id review))
         (stepName (review:record-step-id review))
         (reviewee (review:record-reviewee-id review))
         (reviewer (ct-session-uid session))
         (review-id (review:record-review-id review))
         (data (retrieve-rubric class assignment stepName review-id reviewer reviewee)))
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 data)))))

(provide push->file-container)
(define (push->file-container session post-data rest)
  (let* ((r-hash (car rest))
         (review (review:select-by-hash r-hash)))
    (cond 
      [(equal? (last rest) "save") (push->save session post-data review)]
      [(equal? (last rest) "load") (push->load session review)]
      [else (four-oh-four)])))

(define (push->save session post-data review)
  (let ((data (jsexpr->string (bytes->jsexpr post-data)))
        (class (ct-session-class session))
        (assignment (review:record-assignment-id review))
        (stepName (review:record-step-id review))
        (reviewee (review:record-reviewee-id review))
        (reviewer (ct-session-uid session))
        (review-id (review:record-review-id review)))
    (save-review-comments class assignment stepName review-id reviewer reviewee data)
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 "Success")))))

(define (push->load session review)
  (let* ((class (ct-session-class session))
         (assignment (review:record-assignment-id review))
         (stepName (review:record-step-id review))
         (reviewee (review:record-reviewee-id review))
         (reviewer (ct-session-uid session))
         (review-id (review:record-review-id review))
         (data (load-review-comments class assignment stepName review-id reviewer reviewee)))
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 data)))))
  
(provide file-container)
(define (file-container session role rest [message '()])
  (print "in file-container") (newline)
  (print (list "rest" rest)) (newline)
  (let* ((r-hash (car rest))
         (review (review:select-by-hash r-hash))
         (class (ct-session-class session))
         [assignment (review:record-assignment-id review)]
         (stepName (review:record-step-id review))
         (reviewee (review:record-reviewee-id review))
         [save-url (prepare-save-url rest)]
         [load-url (prepare-load-url rest)]
         [step (to-step-link stepName (- (length rest) 2))]
         (last-path (last rest))
         (prefix (if (equal? last-path "") "" (string-append last-path "/")))
         [path (to-path-html (cdr rest))]
         (file (to-path (cdr rest)))
         (temp (list (print (list class assignment reviewee stepName file)) (newline)))
         (test (print (list "test")))
         (test-prime (newline))
         (file-path (submission-file-path class reviewee assignment stepName file))
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