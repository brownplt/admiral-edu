#lang racket

(require "../../base.rkt")

; ct-session -> (listof string) -> (#f xexpr?) -> bool? -> (listof xexpr? void?)
(provide load)
(define (load  session url message [post #f])
  (let* ((start-url (hash-ref (ct-session-table session) 'start-url)))
    `((h1 "Assignments")
      ,(when message message)
      (h2 "Open Assignments")
      ,(cons 'ul (list-open-assignments start-url))
      (h2 "Closed Assignments")
      ,(cons 'ul (list-closed-assignments session start-url)))))


; string-url -> (listof xexpr? void?)
(define (list-open-assignments start-url)
  (let* ((assignments (assignment:list class-name))
         (open-assignments (filter assignment:Record-open assignments)))
    (cond [(empty? open-assignments) '((p "There are currently no open assignments."))]
          [else (map (open-assignment-element start-url) open-assignments)])))

; string-url -> (listof xexpr? void?)
(define (open-assignment-element start-url)
  (lambda (record)
    (let ((assignment-id (assignment:Record-id record)))
    `(li ,assignment-id " : "
        (a ((href ,(string-append start-url "../next/" assignment-id "/"))) "Next Step") " - "
        (a ((href ,(string-append start-url "../feedback/" assignment-id "/"))) "Assignment Feedback")))))

; ct-session -> (listof xexpr? void?)
(define (list-closed-assignments session start-url)
    (let* ((uid (ct-session-uid session))
           (assignment-list (assignment:list class-name))
           (closed-assignments (filter (show-closed? uid) assignment-list)))
      (cond [(empty? closed-assignments) '((p "There are currently no closed assignments."))]
            [else (map (closed-assignment-element start-url) closed-assignments)])))

(define (show-closed? uid)
  (lambda (record)
    (let* ((assignment-id (assignment:Record-id record))
           (closed (not (assignment:Record-open record)))
           (has-submitted (submission:has-submitted? assignment-id class-name uid)))
      (and closed has-submitted))))

(define (closed-assignment-element start-url)
  (lambda (record)
    (let ((assignment-id (assignment:Record-id record)))
      `(li ,assignment-id " : "
           (a ((href ,(string-append start-url "../feedback/" assignment-id "/"))) "Assignment Feedback")))))