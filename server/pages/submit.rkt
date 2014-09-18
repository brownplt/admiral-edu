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
         "../authoring/assignment.rkt"
         "next.rkt")

(provide submit)
(define (submit session role rest bindings raw-bindings)
  (let* ((uid (ct-session-uid session))
         (assignment (car rest))
         (step (cadr rest))
         (data (extract-binding/single 'file bindings))
         (filename (bytes->string/utf-8 (binding:file-filename (car raw-bindings)))))
    (if (check-okay-to-submit uid assignment step)
        (handle-submit session role rest uid assignment step filename data)
        (render-html "<p>Could not submit to the specified step.</p>"))))

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

(define (handle-submit session role rest uid assignment step filename data)
  (let ((result (submit-step assignment step uid filename data))
        (start-url (hash-ref (ct-session-table session) 'start-url)))
    (cond [(Success? result) 
           (let ((message (Success-result result)))
             (render-html (string-append "<p>" message "</p>"
                                         "<p><a href='" start-url "../../../next/" assignment "/'>Continue</a></p>" )))]
          [(Failure? result)
           (let ((message (Failure-message result)))
             (render-html (string-append "<p>" message "</p>"
                                         "<p><a href='" start-url "../../../next/" assignment "/'>Back</a></p>" )))])))