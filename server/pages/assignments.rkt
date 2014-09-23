#lang racket

(require web-server/http/response-structs
          web-server/templates
         xml)

(require "../base.rkt"
         (prefix-in error: "errors.rkt")
         (prefix-in dashboard: "assignments/dashboard.rkt")
         (prefix-in list: "assignments/list.rkt")
         (prefix-in action: "assignments/action.rkt")
         (prefix-in student-view: "assignments/student-view.rkt")
         (prefix-in status: "assignments/status.rkt"))

(provide load)
(define (load post)
  (lambda (session role rest [message #f])
    (let ([title "Assignments"]
          [body (string-join
                 (map xexpr->string
                      (filter (compose not void?)
                              (cond [(not (roles:Record-can-edit role)) (student-view:load session rest message post)]
                                    [else (show-instructor-view session rest message post)]))))])
      (include-template "html/plain.html"))))

(define (show-instructor-view session url message [post #f])
  (let ((action (if (empty? url) action:LIST (first url)))
        (rest-url (if (empty? url) url (rest url))))
    ((lookup-action-function action) session rest-url message post)))

(define (no-such-action action)
  (lambda args
    (error:error (format "The requested action ~a is not valid here." action))))

(define action-functions
  (hash action:LIST list:load
        action:DASHBOARD dashboard:load
        action:OPEN dashboard:open
        action:CLOSE dashboard:close
        action:DELETE dashboard:delete
        "status" status:load))

(define (lookup-action-function action)
  (cond [(hash-has-key? action-functions action) (hash-ref action-functions action)]
        [else (no-such-action action)]))

