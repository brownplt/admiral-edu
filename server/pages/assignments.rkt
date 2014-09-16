#lang racket

(require web-server/http/bindings
         web-server/templates
         web-server/http/response-structs
         xml
         json
         (planet esilkensen/yaml:3:1))

(require "../base.rkt"
         (prefix-in error: "errors.rkt")
         "../authoring/assignment.rkt"
         (prefix-in dashboard: "assignments/dashboard.rkt")
         (prefix-in list: "assignments/list.rkt")
         (prefix-in action: "assignments/action.rkt"))


(define (repeat val n)
  (cond
    [(<= n 0) '()]
    [else (cons val (repeat val (- n 1)))]))

(provide load)
(define (load post)
  (lambda (session role rest [message #f])
    (let ((start-url (hash-ref (ct-session-table session) 'start-url)))
      (cond [(not (roles:role-can-edit role)) (show-open-assignments start-url)]
            [else (show-instructor-view session rest message post)]))))

(define (show-instructor-view session url message [post #f])
  (let ((action (if (empty? url) action:LIST (first url)))
        (start-url (hash-ref (ct-session-table session) 'start-url))
        (rest-url (if (empty? url) '() (rest url))))
    (apply string-append 
           (map xexpr->string
                (filter xexpr?
                        ((lookup-action-function action) start-url session rest-url message post))))))


(define (show-open-assignments start-url)
  (let* ((assign-list (filter assignment:record-open (assignment:list class-name)))
         (assign-rendered (if (null? assign-list) 
                              "<p>There are currently no open assignments.</p>"
                              (apply string-append (map (open-record->html start-url) assign-list)))))
                             
    (string-append "<h1>Assignments</h1>"
                   assign-rendered)))

(define (open-record->html start-url)
  (lambda (record)
    (let ((id (assignment:record-id record))
          (open (assignment:record-open record)))
      (cond [open (what-next-link id start-url)]
            [else ""]))))

;; TODO: This should be slightly more sophisticated so that closed assignments still show up
(define (what-next-link id start-url)
  (string-append "<p>" id " : <a href='" start-url "../next/" id "/'>Next Step</a> - <a href='" start-url "../feedback/" id "/'>Assignment Feedback</a></p>"))


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
  (lambda (start-url session url message)
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

