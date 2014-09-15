#lang racket

(require web-server/http/bindings
         web-server/templates
         web-server/http/response-structs
         xml
         json
         (planet esilkensen/yaml:3:1))

(require "../base.rkt"
         (prefix-in error: "errors.rkt")
         "../authoring/assignment.rkt")

(define OPEN-ACTION "open")
(define CLOSE-ACTION "close")
(define DASHBOARD-ACTION "dashboard")
(define DELETE-ACTION "delete")
(define LIST-ACTION "")


(define (repeat val n)
  (cond
    [(<= n 0) '()]
    [else (cons val (repeat val (- n 1)))]))

(provide load)
(define (load post)
  (lambda (session role rest [message ""])
    (let ((start-url (hash-ref (ct-session-table session) 'start-url)))
      (cond [(not (roles:role-can-edit role)) (show-open-assignments start-url)]
            [else (show-instructor-view session rest message post)]))))

(define (show-instructor-view session url message [post #f])
  (let ((action (if (empty? url) LIST-ACTION (first url)))
        (start-url (hash-ref (ct-session-table session) 'start-url))
        (rest-url (if (empty? url) '() (rest url))))
    ((lookup-action-function action) start-url session rest-url message post)))

(define (do-list-action start-url session url message [post #f])
    (let* ((assign-list (assignment:list class-name))
           (open-assignments (filter assignment:record-open assign-list))
           (closed-assignments (filter (lambda (x) (not (assignment:record-open x))) assign-list)))
      (string-append "<h1>Assignments</h1>" 
                     message
                     "<p><a href='/" class-name "/author/'>New Assignment</a></p>"
                     "<p>Click an assignment to view more details.</p>"
                     "<h2>Open Assignments</h2>"
                     "<ul>"
                     (apply string-append (map (record->html start-url) open-assignments))
                   "</ul>"
                   "<h2>Closed Assignments</h2>"
                   "<ul>"
                   (apply string-append (map (record->html start-url) closed-assignments))
                   "</ul>")))

(define (record->html start-url)
  (lambda (record)
    (let ((id (assignment:record-id record))
          (open (if (assignment:record-open record) "Open" "Closed")))
      (string-append "<li><a href='/" class-name "/assignments/"  DASHBOARD-ACTION "/" id "/'>" id "</a></li>"))))


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


#|" - " (edit-assignment-link id) " - <a href=\"/" class-name "/assignments/" action "/" id "/\">" link-text "</a> - "
                   "<a href='/" class-name "/export/" id "/" id ".zip'>Export Assignment Data</a>"
                   "</p>")))
|#

(define (do-dashboard-action start-url session url message [post #f])
  (let ((assignment-id (first url)))
    (check-ready assignment-id)
    (let* ((assignment (assignment:select class-name assignment-id))
           (open (assignment:record-open assignment))
           (ready (assignment:record-ready assignment))
           (status (if ready (if open "Open" "Closed") "Missing Dependencies")))
        
    (string-append "<h1><a href='/" class-name "/assignments/'>Assignments</a></h1>"
                   "<h2>"assignment-id "</h2>"
                   "<p>Status: " status "</p>"
                   (cond [(and ready open) (string-append "<p><a href='/" class-name "/assignments/" CLOSE-ACTION "/" assignment-id  "/'>Close Assignment</a></p>")]
                         [(and ready (not open)) (string-append "<p><a href='/" class-name "/assignments/" OPEN-ACTION "/" assignment-id "/'>Open Assignment</a></p>")]
                         [else ""])
                   "<p><a href='/" class-name "/dependencies/" assignment-id "/'>Upload Dependencies</a></p>"
                   "<p><a href='/" class-name "/author/edit/" assignment-id "/'>Edit Assignment Description</a></p>"
                   "<p><a href='/" class-name "/export/" assignment-id "/" assignment-id ".zip'>Export Assignment Data</a></p>"
                   "<p><a href='/" class-name "/assignments/" DELETE-ACTION "/" assignment-id "/'>Delete Assignment</a></p>")
  )))

(define (do-open-action start-url session url message [post #f])
  (let ((assignment-id (first url)))
    (assignment:open assignment-id class-name)
    (do-dashboard-action start-url session url message)))

(define (do-close-action start-url session url message [post #f])
  (let ((assignment-id (first url)))
    (assignment:close assignment-id class-name)
    (do-dashboard-action start-url session url message)))

(define (do-delete-action start-url session url message [post #f])
  (let ((assignment-id (first url)))
    (cond [post (run-delete start-url session url message assignment-id)]
          [else (string-append "<h1><a href='/" class-name "/assignments/'>Assignments</a></h1>"
                               "<h2><a href='/" class-name "/assignments/" DASHBOARD-ACTION "/" assignment-id "'>" assignment-id "</a></h2>"
                               "<p><b>You are about to delete this assignment. This action is irreversable. Click Submit below to proceed.</b></p>"
                               "<form action='/" class-name "/assignments/delete/" assignment-id "/' method='post'>"
                               "<input type='submit'>"
                               "</form>")])))

(define (run-delete start-url session url message assignment-id)
  (delete-assignment assignment-id)
  (do-list-action start-url session url (string-append "<p><b>Assignment " assignment-id " deleted.</b></p>")))

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
  (hash LIST-ACTION do-list-action
        DASHBOARD-ACTION do-dashboard-action
        OPEN-ACTION do-open-action
        CLOSE-ACTION do-close-action
        DELETE-ACTION do-delete-action))

(define (lookup-action-function action)
  (cond [(hash-has-key? action-functions action) (hash-ref action-functions action)]
        [else (no-such-action action)]))

