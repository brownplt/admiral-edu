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
  (let ((assign-list (assignment:list class-name)))
    (string-append "<h1>Assignments</h1>" 
                   (apply string-append (map record->html assign-list)))))

(define (record->html record)
  (let ((id (assignment:record-id record))
        (ready (assignment:record-ready record))
        (open (assignment:record-ready record)))
    (cond [ready (with-open-link id open)]
          [else (with-ready-link id)])))

(define (with-open-link id open)
  (let ((link-text (if open "Close" "Open")))
    (string-append "<p>" id "<a href=\"#\">" link-text "</a></p>")))

(define (with-ready-link id)
  (string-append "<p>" id ": This assignment is missing dependencies. <a href=\"#\">Upload Dependencies</a></p>"))