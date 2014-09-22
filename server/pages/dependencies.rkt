#lang racket

(require web-server/http/bindings
         web-server/http/request-structs
         web-server/templates
         web-server/http/response-structs
         xml
         json)

(require "../storage/storage.rkt"
         "../base.rkt"
         (prefix-in assign: "../authoring/assignment.rkt")
         (prefix-in error: "errors.rkt"))

(define THREE-STUDY-ACTION "three-study")
(define LIST-DEPENDENCIES "list-dependencies")
(define base-url (string-append "/" class-name "/dependencies/"))

(define (repeat val n)
  (cond
    [(<= n 0) '()]
    [else (cons val (repeat val (- n 1)))]))

(provide dependencies)
(define (dependencies session role rest [message '()])
  (let* ((len (length rest)))
    (cond [(= len 1) (assignment-dependencies (car rest))]
          [(and (= len 2) (string=? THREE-STUDY-ACTION (cadr rest))) (three-study-form (car rest))]
          [(> len 2) (dependencies-form (car rest) (cadr rest) (caddr rest) rest)])))

(define (assignment-dependencies assignment-id [message ""])
  (cond [(not (assignment:exists? assignment-id class-name)) (error:error-page "The assignment id '" assignment-id "' was not found.")]
        [else (let* ((deps (assign:assignment-id->assignment-dependencies assignment-id))
                     [header (string-append "<a href='/" class-name "/assignments/'>Assignments</a>")]
                     (dependency-list (string-append (string-join (map (dep->html assignment-id) deps) "\n")))
                     [extra-message ""]
                     [body (string-append "<h2><a href='/" class-name "/assignments/dashboard/" assignment-id "/'>" assignment-id "</a></h2>"
                                          "<p>" message "</p>"
                                          "<p>The links below allow you to preview each rubric and upload file dependencies.</p>"
                                          "<ul>" dependency-list "</ul>"
                                          )])
                (include-template "html/basic.html"))]))

(define (three-study-form assignment-id [message #f])
  (cond [(not (assignment:exists? assignment-id class-name)) (error:error-page "The assignment id '" assignment-id "' was not found.")]
        [else 
         (let* ([header assignment-id]
                [extra-message ""]
                [body (render-three-study-form assignment-id)])
           (include-template "html/basic.html"))]))

(define (render-three-study-form assignment-id)
  (string-append "<p>You are uploading the 3 condition study yaml file.</p>"
                 "<form method='post' action='" base-url assignment-id "/" THREE-STUDY-ACTION "/" "' enctype='multipart/form-data'>"
                 "<input type='file' name='three-condition-file'>"
                 "<input type='submit' value='Upload'>"
                 "</form>"))

;(struct dependency (step-id review-id amount instructor-solution) #:transparent)
(define (dep->html assignment-id)
  (lambda (dep)
    (cond [(assign:review-dependency? dep) (begin
                                      (let* ((sid (assign:review-dependency-step-id dep))
                                             (rid (assign:review-dependency-review-id dep))
                                             (inst (if (assign:instructor-solution-dependency? dep) " - <b>Instructor Solution</b>" ""))
                                             (a-start (string-append "<a href=\"" base-url assignment-id "/" sid "/" rid "/\">"))
                                             (a-end (if (assign:dependency-met dep) " - Ready" " - Dependencies Missing")))
                                        (string-append "<li>" 
                                                       a-start
                                                       sid ":" rid inst  "</a>"
                                                       a-end 
                                                       "</li>")))]
          [(assign:three-study-config-dependency? dep) (begin
                                                         (let ((ready (if (assign:dependency-met dep) " - Ready" " - Dependency Missing")))
                                                           (string-append "<li>"
                                                                          "<a href='" base-url assignment-id "/" THREE-STUDY-ACTION "/'>"
                                                                          "Three Study Configuration File" ready
                                                                          "</a>"
                                                                          "</li>")))]

          [else (raise "Unknown dependency")])))
  
(define (dependencies-form assignment step review-id rest)
  (let* ((dep (car (assign:find-dependencies assignment step review-id)))
         (met (assign:dependency-met dep))
         [load-url (xexpr->string (string-append "\"" base-url (string-join rest "/") "/load\""))]
         [dependency-form (generate-dependency-form assignment step review-id)]) ;;(if met (dependency-met assignment step review-id) (generate-dependency-form assignment step review-id))])
    (include-template "html/dependency.html")))

(provide post)
(define (post session rest bindings raw-bindings)
  (let* ((class class-name)
         (assignment (car rest))
         (action (last rest)))
    (cond [(equal? action "load") (let ((stepName (cadr rest))
                                        (review-id (caddr rest)))
                                    (load-rubric class assignment stepName review-id))]
          [(equal? action "upload") (let ((stepName (cadr rest))
                                          (review-id (caddr rest)))
                                      (upload-submissions class assignment stepName review-id bindings raw-bindings))]
          [(string=? action THREE-STUDY-ACTION) (upload-three-condition assignment bindings raw-bindings)])))

(define (load-rubric class assignment stepName review-id)
  (let ((data (retrieve-default-rubric class assignment stepName review-id)))
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 data)))))

(define (upload-three-condition assignment-id bindings raw-bindings)
  (let* ((dep (car (filter assign:three-study-config-dependency? (assign:assignment-id->assignment-dependencies assignment-id))))
         (result (assign:handle-dependency assignment-id dep bindings raw-bindings)))
        (assign:check-ready assignment-id)
    (response/full
     200 #"Okay"
     (current-seconds) TEXT/HTML-MIME-TYPE
     empty
     (list (string->bytes/utf-8 (render-result assignment-id result))))))

(define (upload-submissions class assignment-id step-id review-id bindings raw-bindings)
  (let* ((dep (car (assign:find-dependencies assignment-id step-id review-id)))
         (result (assign:handle-dependency assignment-id dep bindings raw-bindings)))
    (assign:check-ready assignment-id)
    (response/full
     200 #"Okay"
     (current-seconds) TEXT/HTML-MIME-TYPE
     empty
     (list (string->bytes/utf-8 (render-result assignment-id result))))))

(define (render-result assignment-id result)
  (cond [(Success? result) (assignment-dependencies assignment-id (string-append "<p>" (Success-result result) "</p>"))]
        [(Failure? result) (error:error (Failure-message result))]
        [else (raise (format "Unknown result: ~a" result))]))



;(struct dependency (step-id review-id amount instructor-solution) #:transparent)
(define (generate-dependency-form assignment-id step-id review-id)
  (let* ((dep (car (assign:find-dependencies assignment-id step-id review-id)))
         (amount (if (assign:student-submission-dependency? dep) (assign:student-submission-dependency-amount dep) 1))
         (instructor-solution (assign:instructor-solution-dependency? dep)))
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