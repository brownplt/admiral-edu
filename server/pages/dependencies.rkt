#lang racket

(require web-server/http/bindings
         web-server/templates
         web-server/http/response-structs
         xml
         json)

(require "../ct-session.rkt"
         "../database/mysql.rkt"
         "../config.rkt"
         (prefix-in assign: "../authoring/assignment.rkt")
         "errors.rkt")

(define base-url "/ct/dependencies/")

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

(define (assignment-dependencies assignment-id)
  (cond [(not (assignment:exists? assignment-id class-name)) (error-page "The assignment id '" assignment-id "' was not found.")]
        [else (let* ((deps (assign:assignment-id->assignment-dependencies assignment-id))
                     [assignment-id assignment-id]
                     [dependency-list (string-join (map (dep->html assignment-id) deps) "\n")])
                (include-template "html/dependency-list.html"))]))

;(struct dependency (step-id review-id amount instructor-solution) #:transparent)
(define (dep->html assignment-id)
  (lambda (dep)
    (let ((sid (assign:dependency-step-id dep))
          (rid (assign:dependency-review-id dep))
          (inst (if (assign:dependency-instructor-solution dep) " - <b>Instructor Solution</b>" "")))
      (string-append "<li>" 
                     "<a href=\"" base-url assignment-id "/" sid "/" rid "/\">" 
                     sid ":" rid inst 
                     "</a></li>"))))
  
(define (dependencies-form assignment step review-id rest)
  (let* ([dependency-form (generate-dependency-form)]
         [load-url (xexpr->string (string-append "\"" base-url (string-join rest "/") "/load\""))])
    (include-template "html/dependency.html")))

(provide post->load-rubric)
(define (post->load-rubric session rest)
  (let* ((class class-name)
         (assignment (car rest))
         (stepName (cadr rest))
         (review-id (caddr rest))
         (data (retrieve-default-rubric class assignment stepName review-id)))
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 data)))))

(define (generate-dependency-form)
  "<p>Insert Dependency Form Here</p>")