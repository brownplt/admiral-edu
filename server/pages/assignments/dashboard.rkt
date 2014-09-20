#lang racket

(require (prefix-in action: "action.rkt")
         (prefix-in list: "list.rkt")
         "../../base.rkt"
         "../../authoring/assignment.rkt")

(provide load)
(define (load session url message [post #f])
  (let ((assignment-id (first url)))
    (check-ready assignment-id)
    (let* ((assignment (assignment:select class-name assignment-id))
           (open (assignment:Record-open assignment))
           (ready (assignment:Record-ready assignment))
           (status (if ready (if open "Open" "Closed") "Missing Dependencies")))
           `((h1 ,(action:assignments "Assignments"))
             (h2 ,assignment-id)
             ,(when message message)
             (h3 ,(action:status assignment-id "Status") " : " ,status)
             ,(when ready
                (cond [open `(p ,(action:close assignment-id "Close Assignment"))]
                      [else `(p ,(action:open assignment-id "Open Assignment"))]))
             (p ,(action:dependencies assignment-id "Upload Dependencies"))
             (p ,(action:edit assignment-id "Edit Assignment Description"))
             (p ,(action:export assignment-id "Export Assignment Data"))
             (p ,(action:delete assignment-id "Delete Assignment"))))))

(provide open)
(define (open session url message [post #f])
  (let ((assignment-id (first url)))
    (assignment:open assignment-id class-name)
    (load session url message)))

(provide close)
(define (close session url message [post #f])
  (let ((assignment-id (first url)))
    (assignment:close assignment-id class-name)
    (load session url message)))

(provide delete)
(define (delete session url message [post #f])
  (let ((assignment-id (first url)))
    (cond [post (run-delete session url message assignment-id)]
          [else 
           `((h1 ,(action:assignments "Assignments"))
             (h2 ,(action:dashboard assignment-id assignment-id))
             (p (b "You are about to delete this assignment. This action is irreversible. Click the submit button below to proceed."))
             (form ((action ,(string-append "/" class-name "/assignments/delete/" assignment-id "/"))
                    (method "POST"))
                   (input ((type "submit")))))])))

(define (run-delete session url message assignment-id)
  (delete-assignment assignment-id)
  (list:load session url `(p (b ,(string-append "Assignment '" assignment-id "' deleted.")))))