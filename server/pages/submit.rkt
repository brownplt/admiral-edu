#lang racket

(require web-server/http/bindings
         web-server/http/request-structs
         web-server/templates
         web-server/http/response-structs
         xml
         json
         (planet esilkensen/yaml:3:1))

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
               (title "Captain Teach - Submission Preview")
               (body
                (p "Below is a preview of your submission. Please review it. 
                   When you are sure it is correct. Click the submit button below.")
                (iframe ((width "800px") 
                         (height "600px")
                         ;; TODO: Load CSS rather than using inline style
                         (style "border: none;")
                         (src ,(string-append start-url "../../../browse/" assignment "/" step "/")) 
                         (scrolling "no")))
                (form ((method "post") (action ""))
                      (input ((type "hidden") (name "action") (value "submit")))
                      (p (b "Warning:") "Once you submit, you will not be able to make any changes to your work."
                         (input ((type "submit") (value "Submit")))))
                (form ((method "get") (action ,(string-append  start-url "../../../next/" assignment "/")))
                      (input ((type "submit") (value "Cancel"))))))))]
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
                                         "<p><a href='" start-url "../../../next/" assignment "/'>Continue</a></p>" )))]
          [(Failure? result)
           (let ((message (Failure-message result)))
             (render-html (string-append "<p>" message "</p>"
                                         "<p><a href='" start-url "../../../next/" assignment "/'>Back</a></p>" )))])))
