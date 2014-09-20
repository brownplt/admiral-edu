#lang racket

(require xml)
(require "../../base.rkt"
         (prefix-in action: "action.rkt"))

(provide load)
(define (load session url message [post #f])
    (let* ((assign-list (assignment:list class-name))
           (open-assignments (filter assignment:Record-open assign-list))
           (closed-assignments (filter (lambda (x) (not (assignment:Record-open x))) assign-list)))
      `((h1 "Assignments")
        ,(when message message)
        (p (a ((href ,(string-append "/" class-name "/author/"))) "New Assignment"))
        (h2 "Open Assignments")
        ,(cons 'ul (map record->html  open-assignments))
        (h2 "Closed Assignments")
        ,(cons 'ul (map record->html closed-assignments)))))


(define (record->html record)
    (let ((id (assignment:Record-id record)))
      `(li ,(action:dashboard id id))))
      