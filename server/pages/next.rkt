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
         (start-url (hash-ref (ct-session-table session) 'start-url))
         (user-id (ct-session-uid session)))
    (if (not is-open) (error:assignment-closed)
        (let* ((uid (ct-session-uid session))
               (assignment (car rest))     
               (do-next (next-step assignment-id uid)))
          (cond 
            [(MustSubmitNext? do-next) (handle-submit-next assignment user-id do-next start-url)]
            [(MustReviewNext? do-next) (handle-review-next do-next start-url)]
            [(eq? #t do-next) (assignment-completed)]
            [else (error "Unknown next-action.")])))))

(define (handle-submit-next assignment-id user-id action start-url)
  (let* ((step (MustSubmitNext-step action))
         (instruction (Step-instructions step))
         (step-id (Step-id step))
         (exists (submission:exists? assignment-id class-name step-id user-id)))
    (cond [exists (view-publish step-id instruction start-url assignment-id)]
          [else (view-upload step-id instruction start-url assignment-id)])))

(define (view-publish step-id instruction start-url assignment-id)
  (let ((submit-url (string-append start-url "../../submit/" assignment-id  "/" step-id "/"))
        (browse-url (string-append start-url "../../browse/" assignment-id "/" step-id "/")))
    (string-append "<p>Below is your current submission to '" step-id "'. It has not yet been published. You may make changes until you are ready to publish.</p>"
                   "<iframe width='800px' height='600px' style='border: none;' src='" browse-url "' scrolling='no'></iframe>"
                   "<h3>Upload new submission</h3>"
                   "<p><b>Warning:</b> This will over write your current submission.</p>"
                   "<form action='" submit-url "' method='post' enctype='multipart/form-data'>"
                   "<p>Instructions: " instruction "</p>"
                   "<p>File:</p>"
                   "<p><input type='file' id='file' name='file'></p>"
                   "<p><input type='submit' value='Upload'></p>"
                   "</form>"
                   "<h3>Publish Current Submission</h3>"
                   "<p><b>Warning:</b> After publishing, you may not make any changes to this submission step. "
                   "Make sure all files you would like to submit for this step are present in the preview above "
                   "before clicking the button below.</p>"
                   "<form action='" submit-url "' method='post'>"
                   "<input type='hidden' name='action' value='submit'>"
                   "<input type='submit' value='Publish Submission'>"
                   "</form>")))
        
(define (view-upload step-id instruction start-url assignment-id)
    (string-append "<p>You are uploading a submission to '" step-id "'.</p>" 
                   "<p>Instructions: "instruction"</p>"
                   "<form action='" start-url "../../submit/" assignment-id "/" step-id "/' method='post' enctype='multipart/form-data'>"
                   "<p>File:</p>"
                   "<p><input type='file' id='file' name='file'></p>"
                   "<p><input type='submit' value='Upload'></p>"
                   "</form>"))

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
                      