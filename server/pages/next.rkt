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

(provide next)
(define (next session role rest [message '()])
  (let* ((uid (ct-session-uid session))
         (assignment (car rest))
         (test (list (print uid) (newline) (print assignment) (newline) (print "Calling next-step") (newline)))         
         (do-next (next-step assignment uid)))
    (cond 
      [(MustSubmitNext? do-next) (handle-submit-next assignment do-next)]
      [(MustReviewNext? do-next) (handle-review-next do-next)]
      [(eq? #t do-next) (assignment-completed)])))

(define (handle-submit-next assignment-id action)
  (let* ((step (MustSubmitNext-step action))
         (instruction (Step-instructions step))
         (step-id (Step-id step)))
    (string-append "<p>You must submit to '" step-id "'.</p>" 
                   "<p>Instructions: "instruction"</p>"
                   "<form action='../../submit/" assignment-id "/" step-id "/' method='post' enctype='multipart/form-data'>"
                   "<p>File:</p>"
                   "<p><input type='file' id='file' name='file'></p>"
                   "<p><input type='submit' value='Upload'></p>")))

(define (handle-review-next action)
  (let* ((step (MustReviewNext-step action))
         (step-id (Step-id step))
         (reviews (MustReviewNext-reviews action)))
    (string-append 
     "<p>You must complete the following reviews: </p>"
     "<p>TODO: List review links here</p>"))) ;;TODO: List Reviews

(define (assignment-completed)
  "You have completed this assignment.")
                      