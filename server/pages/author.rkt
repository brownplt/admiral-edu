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

(provide authoring)
(define (authoring session role rest [message '()])
  (let* ((last-path (last rest))
         (prefix (if (equal? last-path "") "" (string-append last-path "/")))
         [save-url "\"validate\""]
         [load-url "test"]
         (contents "Test Contents"))
    (string-append (include-template "html/authoring-header.html")
                   contents
                   (include-template "html/authoring-footer.html"))))

(provide post->validate)
(define (post->validate session post-data rest)
  (let ((result (yaml-bytes->create-assignment post-data)))
    (print "Recieved new assignment. Responding with: ") (print response) (newline)
    (response/full
     200 #"Okay"
     (current-seconds) #"application/json; charset=utf-8"
     empty
     (list (string->bytes/utf-8 result)))))


  
