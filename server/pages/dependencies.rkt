#lang racket

(require web-server/http/bindings
         web-server/templates
         web-server/http/response-structs
         xml
         json)

(require "../base.rkt"
         (prefix-in assign: "../authoring/assignment.rkt")
         (prefix-in error: "errors.rkt"))

(define base-url (string-append "/" class-name "/dependencies/"))

(define (repeat val n)
  (cond
    [(<= n 0) '()]
    [else (cons val (repeat val (- n 1)))]))

(provide dependencies)
(define (dependencies session role rest [message '()])
  (let ((len (length rest)))
    (cond [(= len 1) (assignment-dependencies (car rest))]
          [(= len 2) (assignment-dependencies (car rest))]
          [(> len 2) (dependencies-form (car rest) (cadr rest) (caddr rest) rest)])))

(define (assignment-dependencies assignment-id [message #f])
  (cond [(not (assignment:exists? assignment-id class-name)) (error:error-page "The assignment id '" assignment-id "' was not found.")]
        [else (let* ((deps (assign:assignment-id->assignment-dependencies assignment-id))
                     [assignment-id assignment-id]
                     [dependency-list (string-append "<p><a href='/" class-name "/assignments/'>Back to Assignments</a></p>"
                                                     (string-join (map (dep->html assignment-id) deps) "\n"))]
                     [message-text (if message message "")])
                (include-template "html/dependency-list.html"))]))

;(struct dependency (step-id review-id amount instructor-solution) #:transparent)
(define (dep->html assignment-id)
  (lambda (dep)
    (let* ((sid (assign:dependency-step-id dep))
           (rid (assign:dependency-review-id dep))
           (inst (if (assign:dependency-instructor-solution dep) " - <b>Instructor Solution</b>" ""))
           (a-start (string-append "<a href=\"" base-url assignment-id "/" sid "/" rid "/\">"))
           (a-end (if (assign:dependency-met dep) " - Ready" " - Dependencies Missing")))
      (string-append "<li>" 
                     a-start
                     sid ":" rid inst  "</a>"
                     a-end 
                     "</li>"))))
  
(define (dependencies-form assignment step review-id rest)
  (let* ((dep (car (assign:find-dependencies assignment step review-id)))
         (met (assign:dependency-met dep))
         [load-url (xexpr->string (string-append "\"" base-url (string-join rest "/") "/load\""))]
         [dependency-form (generate-dependency-form assignment step review-id)]) ;;(if met (dependency-met assignment step review-id) (generate-dependency-form assignment step review-id))])
    (include-template "html/dependency.html")))

(define (dependency-met assignment-id step-id review-id)
  (let* ((dep (car (assign:find-dependencies assignment-id step-id review-id)))
         (amount (assign:dependency-amount dep))
         (instructor-solution (assign:dependency-instructor-solution dep)))
    (string-append "<p>Assignment id:" assignment-id "</p>"
                   "<p>Submission Step id:" step-id "</p>"
                   "<p>Review id:" review-id "</p>"
                   "<p>This review step requires " (number->string amount) " default solution(s).</p>"
                   "<p>Dependencies for this review step have already been provided.</p>"
                   "<p>TODO: Allow user to upload replacement dependencies</p>")))
;;TODO Allow user to upload new dependencies

(provide post)
(define (post session rest bindings)
  (let* ((class class-name)
         (assignment (car rest))
         (stepName (cadr rest))
         (review-id (caddr rest))
         (action (cadddr rest)))
    (cond [(equal? action "load") (load-rubric class assignment stepName review-id)]
          [(equal? action "upload") (upload-submissions class assignment stepName review-id bindings)])))

(define (load-rubric class assignment stepName review-id)
  (let ((data (retrieve-default-rubric class assignment stepName review-id)))
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 data)))))

(define (upload-submissions class assignment-id step-id review-id bindings)
  (let* ((dep (car (assign:find-dependencies assignment-id step-id review-id)))
         (amount (assign:dependency-amount dep))
         (result (run-submissions class assignment-id step-id review-id bindings amount) ))
    (response/full
     200 #"Okay"
     (current-seconds) TEXT/HTML-MIME-TYPE
     empty
     (list (string->bytes/utf-8 result)))))

(define (run-submissions class assignment stepName review-id bindings amount)
  (letrec ((helper (lambda (n)
                     (if (<= n 0) (assignment-dependencies assignment "<p>Dependencies uploaded.</p>")
                         (let* ((sym (string->symbol (string-append "file-" (number->string n))))
                                (uname (assign:default-submission review-id n))
                                (data (extract-binding/single sym bindings)))
                           (let ((result (upload-instructor-solution class uname assignment stepName data)))
                             (if (not result) (error:error (string-append "Failed to upload dependency for " stepName ". This is most likely because the file provided was not a zip archive."))
                                 (begin
                                   (assign:check-ready assignment)
                                   (helper (- n 1))))))))))
    (helper amount)))

;(struct dependency (step-id review-id amount instructor-solution) #:transparent)
(define (generate-dependency-form assignment-id step-id review-id)
  (let* ((dep (car (assign:find-dependencies assignment-id step-id review-id)))
         (amount (assign:dependency-amount dep))
         (instructor-solution (assign:dependency-instructor-solution dep)))
    (string-append "<p>Assignment id:" assignment-id "</p>"
                   "<p>Submission Step id:" step-id "</p>"
                   "<p>Review id:" review-id "</p>"
                   "<p>This review step requires " (number->string amount) " default solution(s).</p>"
                   "<form action='" 
                   (string-append base-url assignment-id "/" step-id "/" review-id "/upload/")
                   "' method='post' enctype='multipart/form-data'>"
                   (generate-form-string amount)
                   "<input type='submit' value='Upload'>"
                   "</form>")))

(define (generate-form-string n)
  (letrec ((helper (lambda (acc n)
                     (let ((ns (number->string n)))
                       (cond 
                         [(<= n 0) (apply string-append acc)]
                         [else (helper 
                                (cons 
                                 (string-append "<p style='font-weight:bold;'>Solution #" ns "</p>"
                                                "<p style='margin-left:10px;'>"
                                                "<input name='file-" ns "' type='file' id='file-" ns "'>"
                                                "</p>\n") acc)
                                (- n 1))])))))
    (helper '() n)))