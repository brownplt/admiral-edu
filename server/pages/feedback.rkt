#lang racket
(require web-server/http/bindings
         web-server/templates
         web-server/http/response-structs
         xml
         json
         web-server/http/bindings
         (planet esilkensen/yaml:3:1))

(require "../storage/storage.rkt"
         "../base.rkt"
         (prefix-in error: "errors.rkt")
         "../util/file-extension-type.rkt"
         "../authoring/assignment.rkt")

(define (repeat val n)
  (cond
    [(<= n 0) '()]
    [else (cons val (repeat val (- n 1)))]))

(provide load)
(define (load session role rest [message '()])
  (let ((action (car rest)))
    (cond [(equal? "view" action) (do-view session (cdr rest) message)]
          [(equal? "file-container" action) (do-file-container session role (cdr rest) message)]
          [else (do-default session role rest message)])))

(provide post)
(define (post session role rest bindings post-data)
  (let ((action (car rest))
        (submit? (exists-binding? 'feedback bindings)))
    (cond [submit? (post->do-feedback-submit session (cadr rest) bindings )]
          [(equal? "file-container" action) (post->do-file-container session role (cdr rest) post-data)]
          [(equal? "view" action) (post->do-view session (cdr rest) post-data)]
          [else (error "You are not authorized to perform this action.")])))

(define (post->do-feedback-submit session review-hash bindings)
  (let* ((review (review:select-by-hash review-hash))
         (uid (ct-session-uid session))
         (reviewee (review:Record-reviewee-id review))
         (match (equal? uid reviewee))
         (feedback (if (exists-binding? 'feedback bindings) (extract-binding/single 'feedback bindings) ""))
         (flag (if (exists-binding? 'flag bindings) #t #f)))
    (cond [(not match) (response-with (error:not-authorized))]
          [else (begin
                  (review:set-flagged review-hash flag)
                  (save-review-feedback review feedback)
                  (response-with (do-view session (list review-hash) "<p>Feedback submitted.</p>")))])))

(define (response-with resp)
  (response/full
   200 #"Okay"
   (current-seconds) TEXT/HTML-MIME-TYPE
   empty
   (list (string->bytes/utf-8 resp))))

(define (do-default session role rest message)
  (let* ((uid (ct-session-uid session))
         (start-url (hash-ref (ct-session-table session) 'start-url))
         (assignment (car rest))
         (reviews (review:select-feedback class-name assignment uid))
         (submissions (submission:select-from-assignment assignment class-name uid))
         (results (if (and (empty? reviews)
                           (empty? submissions)) "<p>You have no reviews at this time.</p>" 
                                                 (string-append (gen-submissions submissions start-url)
                                                                "<h2>Review Feedback</h2>"
                                                                (gen-reviews reviews start-url)))))
    (string-append "<h1>" assignment "</h1>"
                   results)))

(define (gen-submissions submissions start-url)
  (string-join
   (map xexpr->string
        (append '((h2 "Browse Submission")) (map (gen-submission start-url) submissions)))))

(define (gen-submission start-url)
  (lambda (record)
    (let ((assignment-id (submission:Record-assignment record))
          (step-id (submission:Record-step record)))
    `(p (a ((href ,(string-append start-url "../../browse/" assignment-id "/" step-id "/"))) ,step-id)))))

(define (gen-reviews reviews start-url) (gen-reviews-helper reviews 1 start-url))

(define (gen-reviews-helper reviews cur start-url)
  (if (null? reviews) ""
      (let* ((review (car reviews))
             (hash (review:Record-hash review))
             (step (review:Record-step-id review))
             (rest (cdr reviews)))
        (string-append "<p><a href='" start-url "../view/" hash "/'>Review #" (number->string cur) ": " step "</a></p>" (gen-reviews-helper rest (+ 1 cur) start-url)))))
         
(define (do-view session rest message)
  (let* ((start-url (hash-ref (ct-session-table session) 'start-url))
         (r-hash (car rest))
         (review (review:select-by-hash r-hash))
         (assignment (review:Record-assignment-id review))
         (step (review:Record-step-id review))
         (updir (apply string-append (repeat "../" (+ (length rest) 1))))
         (root-url updir)
         [display-message message]
         [review-feedback (load-review-feedback review)]
         [review-flagged (if (review:Record-flagged review) "CHECKED" "")]
         [submit-url (string-append start-url root-url "review/submit/" r-hash "/")]
         (updir-rubric (apply string-append (repeat "../" (- (length rest) 1))))
         [file-container (string-append start-url updir "file-container/" (to-path rest))]
         [load-url (xexpr->string (string-append "\"" start-url updir-rubric step "/load\""))]
         (reviewer (ct-session-uid session))
         (class (ct-session-class session)))
    (review:mark-feedback-viewed r-hash)
    (if (not (validate review session)) (error:error "You are not authorized to see this page.")
        (include-template "html/feedback.html"))))

(define (validate review session)
  (let ((uid (ct-session-uid session))
        (reviewee (review:Record-reviewee-id review)))
    (equal? uid reviewee)))

(define (do-file-container session role rest [message '()])
  (let* ((start-url (hash-ref (ct-session-table session) 'start-url))
         (r-hash (car rest))
         (review (review:select-by-hash r-hash))
         (class (ct-session-class session))
         [assignment (review:Record-assignment-id review)]
         (stepName (review:Record-step-id review))
         (reviewee (review:Record-reviewee-id review))
         [default-mode (determine-mode-from-filename (last rest))]
         [load-url (string-append "'" start-url "load" "'")]
         [step (to-step-link stepName (- (length rest) 2))]
         (last-path (last rest))
         (prefix (if (equal? last-path "") "" (string-append last-path "/")))
         [path (to-path-html (cdr rest))]
         (file (to-path (cdr rest)))
         (test-prime (newline))
         (file-path (submission-file-path class assignment reviewee stepName file))
         (contents (if (is-directory? file-path) (render-directory file-path start-url) (render-file file-path))))
    (if (not (validate review session)) (error:error "You are not authorized to see this page.")
        (string-append (include-template "html/feedback-file-container-header.html")
                       contents
                       (include-template "html/file-container-footer.html")))))

(define (determine-mode-from-filename filename)
  (let* ((split (string-split filename "."))
         (ext (if (null? split) "" (last split))))
    (extension->file-type ext)))

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
  (if (< depth 0) (xexpr->string step)
      (let ((updepth (string-append (apply string-append (repeat "../" depth)) "./")))
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

(define (render-directory dir-path start-url)
  (let ((dirs (list-dirs dir-path))
        (files (list-files dir-path)))
    (string-append
     "<div id=\"directory\" class=\"browser\">"
     "<ul>"
     (apply string-append (map (html-directory start-url) dirs))
     (apply string-append (map (html-file start-url) files))
     "</ul>"
     "</div>")))

(define (html-directory start-url)
  (lambda (dir)
    (string-append "<li class=\"directory\"><a href=\"" start-url dir "\">" dir "</a></li>")))

(define (html-file start-url)
  (lambda (file)
    (string-append "<li class=\"file\"><a href=\"" start-url file "\">" file "</a></li>")))

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
         (assignment (review:Record-assignment-id review))
         (stepName (review:Record-step-id review))
         (reviewer (review:Record-reviewer-id review))
         (reviewee (ct-session-uid session))
         (review-id (review:Record-review-id review))
         (data (load-review-comments class assignment stepName review-id reviewer reviewee path)))
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 data)))))


(define (post->do-view session rest post-data)
  (let* ((hash (car rest))
         (review (review:select-by-hash hash)))
    (if (not (validate review session)) (error:error "You are not authorized to see this page.")
        (post->load-rubric session review))))
  
(define (post->load-rubric session review)
  (let* ((class (ct-session-class session))
         (assignment (review:Record-assignment-id review))
         (stepName (review:Record-step-id review))
         (reviewer (review:Record-reviewer-id review))
         (reviewee (ct-session-uid session))
         (review-id (review:Record-review-id review))
         (data (retrieve-rubric class assignment stepName review-id reviewer reviewee)))
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 data)))))