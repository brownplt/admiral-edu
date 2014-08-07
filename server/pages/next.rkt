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
      [(MustSubmitNext? do-next) (handle-submit-next do-next)]
      [(MustReviewNext? do-next) (handle-review-next do-next)]
      [(eq? #t do-next) (assignment-completed)])))

(define (handle-submit-next action)
  (let* ((step (MustSubmitNext-step action))
         (instruction (Step-instructions step))
         (step-id (Step-id step)))
    (string-append "You must submit to '" step-id "'." instruction)))

(define (handle-review-next action)
  (let* ((step (MustReviewNext-step action))
         (step-id (Step-id step))
         (reviews (MustReviewNext-reviews action)))
    "You must complete the following reviews: ")) ;;TODO: List Reviews

(define (assignment-completed)
  "You have completed this assignment.")
                      