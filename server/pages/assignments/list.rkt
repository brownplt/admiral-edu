#lang racket

(require xml)
(require "../../base.rkt"
         (prefix-in action: "action.rkt"))

(provide load)
(define (load start-url session url message [post #f])
    (let* ((assign-list (assignment:list class-name))
           (open-assignments (filter assignment:record-open assign-list))
           (closed-assignments (filter (lambda (x) (not (assignment:record-open x))) assign-list)))
      `((h1 "Assignments")
        ,(when message message)
        (p (a ((href ,(string-append "/" class-name "/author/"))) "New Assignment"))
        (h2 "Open Assignments")
        ,(cons 'ul (map (record->html start-url) open-assignments))
        (h2 "Closed Assignments")
        ,(cons 'ul (map (record->html start-url) closed-assignments)))))


(define (record->html start-url)
  (lambda (record)
    (let ((id (assignment:record-id record)))
      `(li ,(action:dashboard id id)))))
      