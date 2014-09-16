#lang racket

(require web-server/http/response-structs
          web-server/templates
         xml)

(require "../base.rkt"
         (prefix-in error: "errors.rkt")
         (prefix-in dashboard: "assignments/dashboard.rkt")
         (prefix-in list: "assignments/list.rkt")
         (prefix-in action: "assignments/action.rkt")
         (prefix-in student-view: "assignments/student-view.rkt"))

(provide load)
(define (load post)
  (lambda (session role rest [message #f])
    (let ([title "Assignments"]
          [body (string-join
                 (map xexpr->string
                      (filter (compose not void?)
                              (cond [(not (roles:role-can-edit role)) (student-view:load session rest message post)]
                                    [else (show-instructor-view session rest message post)]))))])
      (include-template "html/plain.html"))))

(define (show-instructor-view session url message [post #f])
  (let ((action (if (empty? url) action:LIST (first url)))
        (rest-url (if (empty? url) '() (rest url))))
    ((lookup-action-function action) session rest-url message post)))

(provide export)
(define (export session role rest)
  (let ((assignment-id (car rest)))
    (if (not (roles:role-can-edit role)) (fail-auth)
        (let ((data (export-assignment class-name assignment-id)))
          (response/full
           200 #"Okay"
           (current-seconds) #"application/octet-stream; charset=ISO-8859-1"
           empty
           (list data))))))

(define (fail-auth)
  (response/full
         200 #"Okay"
         (current-seconds) TEXT/HTML-MIME-TYPE
         empty
         (list (string->bytes/utf-8 (error:error "You are not authorized to see this page.")))))

(define (no-such-action action)
  (lambda args
    (error:error (format "The requested action ~a is not valid here." action))))

(define action-functions
  (hash action:LIST list:load
        action:DASHBOARD dashboard:load
        action:OPEN dashboard:open
        action:CLOSE dashboard:close
        action:DELETE dashboard:delete))

(define (lookup-action-function action)
  (cond [(hash-has-key? action-functions action) (hash-ref action-functions action)]
        [else (no-such-action action)]))

