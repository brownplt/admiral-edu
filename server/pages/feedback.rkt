#lang racket
(require web-server/http/bindings
         web-server/templates
         web-server/http/response-structs
         xml
         json
         (planet esilkensen/yaml:3:1))

(require "../ct-session.rkt"
         "../database/mysql.rkt"
         "../config.rkt"
         (prefix-in error: "errors.rkt")
         "../authoring/assignment.rkt")

(define (repeat val n)
  (cond
    [(<= n 0) '()]
    [else (cons val (repeat val (- n 1)))]))

(provide load)
(define (load session role rest [message '()])
  (let ((action (car rest)))
    (cond [(equal? "view" action) (do-view session role (cdr rest) message)]
          [(equal? "file-container" action) (do-file-container session role (cdr rest) message)]
          [else (do-default session role rest message)])))

(provide post)
(define (post session role rest post-data)
  (let ((action (car rest)))
    (cond [(equal? "file-container" action) (post->do-file-container session role (cdr rest) post-data)]
          [(equal? "view" action) (post->do-view session role (cdr rest) post-data)]
          [else (error "This shouldn't happen.")])))



(define (do-default session role rest message)
  (let* ((uid (ct-session-uid session))
         (assignment (car rest))
         (reviews (review:select-feedback class-name assignment uid))
         (results (if (null? reviews) "<p>You have no reviews at this time.</p>" (gen-reviews reviews))))
    (string-append "<h1>" assignment "</h1>"
                   results)))

(define (gen-reviews reviews) (gen-reviews-helper reviews 1))

(define (gen-reviews-helper reviews cur)
  (if (null? reviews) ""
      (let* ((review (car reviews))
             (hash (review:record-hash review))
             (step (review:record-step-id review))
             (rest (cdr reviews)))
        (string-append "<p><a href='../view/" hash "/'>Review #" (number->string cur) ": " step "</a></p>" (gen-reviews-helper rest (+ 1 cur))))))
         
(define (do-view session role rest message)
  (let* ((r-hash (car rest))
         (review (review:select-by-hash r-hash))
         (assignment (review:record-assignment-id review))
         (step (review:record-step-id review))
         (updir (apply string-append (repeat "../" (+ (length rest) 1))))
         (root-url updir)
         [submit-url (string-append root-url "review/submit/" r-hash "/")]
         (updir-rubric (apply string-append (repeat "../" (- (length rest) 1))))
         [file-container (string-append updir "file-container/" (to-path rest))]
         [load-url (xexpr->string (string-append "\"" updir-rubric step "/load\""))]
         (reviewer (ct-session-uid session))
         (class (ct-session-class session)))
    (if (not (validate review session)) (error:error "You are not authorized to see this page.")
        (include-template "html/feedback.html"))))

(define (validate review session)
  (let ((uid (ct-session-uid session))
        (reviewee (review:record-reviewee-id review)))
    (equal? uid reviewee)))

(define (do-file-container session role rest [message '()])
  (let* ((r-hash (car rest))
         (review (review:select-by-hash r-hash))
         (class (ct-session-class session))
         [assignment (review:record-assignment-id review)]
         (stepName (review:record-step-id review))
         (reviewee (review:record-reviewee-id review))
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
    (if (not (validate review session)) (error:error "You are not authorized to see this page.")
        (string-append (include-template "html/feedback-file-container-header.html")
                       contents
                       (include-template "html/file-container-footer.html")))))

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

(define (to-step-link step depth)
  (if (<= depth 0) (xexpr->string step)
      (let ((updepth (string-append (apply string-append (repeat "../" depth)) (xexpr->string step))))
        (string-append "<a href=\"" updepth "\">" (xexpr->string step) "</a>"))))

(define (prepare-url word rest)
  (let* ((last-el (last rest))
         (prefix (if (equal? last-el "") "" (string-append last-el "/"))))
    (string-append "\"" prefix word "\"")))

(define (prepare-load-url rest)
  (prepare-url "load" rest))

(define (prepare-save-url rest)
  (prepare-url "save" rest))


;;TODO: Also in pages/review.rkt Should abstract to common function place
(define (to-path ls)
  (letrec ((helper (lambda (acc ls)
                     (match ls
                       ['() (apply string-append (reverse acc))]
                       [(cons head '()) (let ((new-acc (cons head acc)))
                                          (helper new-acc '()))]
                       [(cons head tail) (let ((new-acc (cons "/" (cons head acc))))
                           
                                           
                                           (helper new-acc tail))]))))
    (helper '() ls)))

(define (render-directory prefix dir-path)
  (let ((dirs (sub-directories-of dir-path))
        (files (list-only-files dir-path)))
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


(define (post->do-file-container session role rest post-data)
  (let* ((hash (car rest))
         (path (string-join (take (cdr rest) (- (length rest) 2))  "/"))
         (review (review:select-by-hash hash)))
    (if (not (validate review session)) (error:error "You are not authorized to see this page.")
        (post->load session path review))))

(define (post->load session path review)
  (let* (
         (class (ct-session-class session))
         (assignment (review:record-assignment-id review))
         (stepName (review:record-step-id review))
         (reviewer (review:record-reviewer-id review))
         (reviewee (ct-session-uid session))
         (review-id (review:record-review-id review))
         (data (load-review-comments class assignment stepName review-id reviewer reviewee path)))
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 data)))))


(define (post->do-view session role rest post-data)
  (let* ((hash (car rest))
         (review (review:select-by-hash hash)))
    (if (not (validate review session)) (error:error "You are not authorized to see this page.")
        (post->load-rubric session review))))
  
(define (post->load-rubric session review)
  (let* ((class (ct-session-class session))
         (assignment (review:record-assignment-id review))
         (stepName (review:record-step-id review))
         (reviewer (review:record-reviewer-id review))
         (reviewee (ct-session-uid session))
         (review-id (review:record-review-id review))
         (data (retrieve-rubric class assignment stepName review-id reviewer reviewee)))
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 data)))))