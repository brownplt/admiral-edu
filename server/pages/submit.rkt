#lang racket

(require web-server/http/bindings
         web-server/http/request-structs
         web-server/templates
         web-server/http/response-structs
         xml
         json
         yaml)

(require "../base.rkt"
         "errors.rkt"
         "../storage/storage.rkt"
         "../authoring/assignment.rkt"
         "next.rkt")

(provide submit)
(define (submit session role rest bindings raw-bindings)
  (let ((table (ct-session-table session))
        (uid (ct-session-uid session))
        (assignment (car rest))
        (step (cadr rest)))
    (cond [(and (hash-has-key? table 'action)
                (string=? (hash-ref table 'action) "submit")) (handle-submit session role rest uid assignment step)]
          [else
           (let* ((data (extract-binding/single 'file bindings))
                  (filename (bytes->string/utf-8 (binding:file-filename (car raw-bindings)))))
             (if (check-okay-to-submit uid assignment step)
                 (preview-upload session role rest uid assignment step filename data)
                 (render-html "<p>Could not submit to the specified step.</p>")))])))

(define (render-html data)
  (response/full
     200 #"Okay"
     (current-seconds) TEXT/HTML-MIME-TYPE
     empty
     (list (string->bytes/utf-8 data))))

(define (check-okay-to-submit uid assignment step)
  (let ((do-next (next-step assignment uid)))
    (cond 
      [(MustSubmitNext? do-next) (equal? (Step-id (MustSubmitNext-step do-next)) step)]
      [else #f])))


(define (preview-upload session role rest uid assignment step filename data)
  (let ((result  (upload-submission class-name uid assignment step filename data))
        (start-url (hash-ref (ct-session-table session) 'start-url)))
    (cond [(Success? result) 
           ;; We have uploaded the file successfully, we now have a browser and a confirm submission button
           (render-html 
            (xexpr->string
             `(html
               (title "Captain Teach - Submission Uploaded")
               (body
                (p "Submission uploaded successfully. " (b "Note:") " Your submission has not yet been published.")
                (p (a ((href ,(string-append  start-url "../../../next/" assignment "/"))) "View Submission"))))))]
          [(Failure? result)
           (let ((message (Failure-message result)))
             (render-html (string-append "<p>" message "</p>"
                                         "<p><a href='" start-url "../../../next/" assignment "/'>Back</a></p>" )))])))




(define (handle-submit session role rest uid assignment step)
  (let ((result (submit-step assignment step uid))
        (start-url (hash-ref (ct-session-table session) 'start-url)))
    (cond [(Success? result) 
           (let ((message (Success-result result)))
             (render-html (string-append "<p>" message "</p>"
                                         "<p><a href='" start-url "../../../feedback/" assignment "/'>Continue</a></p>" )))]
          [(Failure? result)
           (let ((message (Failure-message result)))
             (render-html (string-append "<p>" message "</p>"
                                         "<p><a href='" start-url "../../../next/" assignment "/'>Back</a></p>" )))])))
