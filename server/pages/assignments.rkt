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
         "errors.rkt"
         "../authoring/assignment.rkt")

(define (repeat val n)
  (cond
    [(<= n 0) '()]
    [else (cons val (repeat val (- n 1)))]))

(provide assignments)
(define (assignments session role rest [message '()])
  (cond [(not (roles:role-can-edit role)) (show-open-assignments)]
        [else (show-instructor-view session rest message)]))

(define (show-instructor-view session rest [message '()])
  (let ((no-care (check-action rest))
        (assign-list (assignment:list class-name)))    
    (string-append "<h1>Assignments</h1>" 
                   "<p><a href='/" class-name "/author/'>New Assignment</a></p>"
                   (apply string-append (map record->html assign-list)))))

(define (check-action rest)
  (cond [(null? rest) #f]
        [(equal? "open" (car rest)) (let ((id (cadr rest)))
                                      (assignment:open id class-name))]
        [(equal? "close" (car rest)) (let ((id (cadr rest)))
                                       (assignment:close id class-name))]))

(define (show-open-assignments)
  (let ((assign-list (assignment:list class-name)))
    (string-append "<h1>Assignments</h1>"
                   (apply string-append (map open-record->html assign-list)))))

(define (open-record->html record)
  (let ((id (assignment:record-id record))
        (ready (assignment:record-ready record))
        (open (assignment:record-open record)))
    (cond [open (what-next-link id)]
          [else ""])))

(define (what-next-link id)
  (string-append "<a href='../next/" id "/'/'>" id "</a>"))

(define (record->html record)
  (let ((id (assignment:record-id record))
        (ready (assignment:record-ready record))
        (open (assignment:record-open record)))
    (cond [ready (with-open-link id open)]
          [else (with-ready-link id)])))

(define (with-open-link id open)
  (let ((link-text (if open "Close" "Open"))
        (action (if open "close" "open")))
    (string-append "<p>" id " - <a href=\"/" class-name "/assignments/" action "/" id "/\">" link-text "</a></p>")))

(define (with-ready-link id)
  (string-append "<p>" id ": This assignment is missing dependencies. <a href=\"/" class-name "/dependencies/" id "/\">Upload Dependencies</a></p>"))