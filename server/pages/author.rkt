#lang racket

(require web-server/http/bindings
         web-server/templates
         web-server/http/response-structs
         xml
         json
         yaml)

(require "../storage/storage.rkt"
         "../base.rkt"
         (prefix-in error: "errors.rkt")
         "../authoring/assignment.rkt")

(define (repeat val n)
  (cond
    [(<= n 0) '()]
    [else (cons val (repeat val (- n 1)))]))

(define NEW-ACTION "new")
(define EDIT-ACTION "edit")
(define VALIDATE-ACTION "validate")
(define VALIDATE-AND-SAVE-ACTION "validate-save")

(define warning-message
  (string-join 
   '("<p><b>Warning:</b> You are editing an existing assignment."
     "In general it is safe to change instructions and add steps."
     "However, if students have started this assignment, changing ids"
     "and rubric structures may cause inconsistencies in the exported"
     "assignment data.</p>")))

(provide load)
(define (load session role rest [message '()])
  (if (not (roles:Record-can-edit role)) (error:not-authorized)
      (let* ((len (length rest))
             (action (if (= 0 len) NEW-ACTION (car rest))))
        (cond [(equal? NEW-ACTION action) (authoring session role rest "")]
              [(equal? EDIT-ACTION action) (edit session role (cdr rest) warning-message)]))))

(define (authoring session role rest [message '()])
  (page session role rest message "" (string-append "'" VALIDATE-ACTION "'") "test"))

(define (edit session role rest [message '()])
  (if (< (length rest) 1) (error:error "Invalid URL. Expected /author/edit/assignment-id/")
      (let ((assignment-id (car rest)))
        (if (not (assignment:exists? assignment-id class-name)) (error:error (string-append "No such assignment: " assignment-id))
            (let* ((contents (retrieve-assignment-description class-name assignment-id)))
              (page session role rest message contents (string-append "'" VALIDATE-AND-SAVE-ACTION "'") "test"))))))

(define (page session role rest message contents validate load)
  (let* ([save-url validate]
         [load-url load]
         [class-name class-name])
    (string-append (include-template "html/authoring-header.html")
                   contents
                   (include-template "html/authoring-footer.html"))))



(provide post->validate)
(define (post->validate session post-data rest)
  (let ((action (last rest)))
    (cond
      [(equal? VALIDATE-ACTION action) (validate session post-data)]
      [(equal? VALIDATE-AND-SAVE-ACTION action) (validate-and-save session post-data rest)])))

(define (validate session post-data)
  (let ((result (yaml-bytes->create-assignment post-data)))
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 result)))))

(define (validate-and-save session post-data rest)
  (let ((assignment-id (cadr rest)))
    (let ((result (yaml-bytes->save-assignment post-data)))
      (response/full
       200 #"Okay"
       (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 result))))))

  
