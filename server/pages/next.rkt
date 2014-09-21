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

(define (repeat val n)
  (cond
    [(<= n 0) '()]
    [else (cons val (repeat val (- n 1)))]))

(provide next)
(define (next session role rest [message '()])
  (let* ((assignment-id (car rest))
         (assignment-record (assignment:select class-name assignment-id))
         (is-open (assignment:Record-open assignment-record))
         (start-url (hash-ref (ct-session-table session) 'start-url)))
    (if (not is-open) (error:assignment-closed)
        (let* ((uid (ct-session-uid session))
               (assignment (car rest))     
               (do-next (next-step assignment-id uid)))
          (cond 
            [(MustSubmitNext? do-next) (handle-submit-next assignment do-next start-url)]
            [(MustReviewNext? do-next) (handle-review-next do-next start-url)]
            [(eq? #t do-next) (assignment-completed)]
            [else (error "Unknown next-action.")])))))

(define (handle-submit-next assignment-id action start-url)
  (let* ((step (MustSubmitNext-step action))
         (instruction (Step-instructions step))
         (step-id (Step-id step)))
    (string-append "<p>You must submit to '" step-id "'.</p>" 
                   "<p>Instructions: "instruction"</p>"
                   "<form action='" start-url "../../submit/" assignment-id "/" step-id "/' method='post' enctype='multipart/form-data'>"
                   "<p>File:</p>"
                   "<p><input type='file' id='file' name='file'></p>"
                   "<p><input type='submit' value='Upload'></p>")))

(define (handle-review-next action start-url)
  (let* ((step (MustReviewNext-step action))
         (step-id (Step-id step))
         (reviews (MustReviewNext-reviews action))
         (result  (string-append 
                   "<p>You must complete the following reviews: </p>"
                   (apply string-append (map (review-link start-url) reviews)))))
    result))

(define (review-link start-url)
  (lambda (hash)
    (let* ((review (review:select-by-hash hash))
           (completed (review:Record-completed review))
           (reviewee (review:Record-reviewee-id review)))
      (cond [completed ""]
            [(string=? reviewee "HOLD") (string-append "<p>This review is on hold. You will be notified when this review is assigned.</p>")]
            [else (string-append "<p><a href='" start-url "../../review/" hash "/'>Review</a></p>")]))))

(define (assignment-completed)
  "You have completed this assignment.")
                      