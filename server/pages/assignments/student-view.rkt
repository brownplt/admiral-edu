#lang typed/racket

(require "../../base.rkt"
         "../typed-xml.rkt")

; ct-session -> (listof string) -> (#f xexpr?) -> bool? -> (listof xexpr? void?)
(provide load)
(: load (ct-session (Listof String) (U XExpr #f) Boolean -> (Listof (U XExpr Void))))
(define (load  session url message [post #f])
  (let*: ((start-url (hash-ref (ct-session-table session) 'start-url))
          [open : XExpr (cons 'ul (list-open-assignments start-url))]
          [closed : XExpr (cons 'ul (list-closed-assignments session start-url))])
    `((h1 "Assignments")
      ,(when message message)
      (h2 "Open Assignments")
      ,open
      (h2 "Closed Assignments")
      ,closed)))

; string-url -> (listof xexpr? void?)
(: list-open-assignments (String -> (Listof XExpr)))
(define (list-open-assignments start-url)
  (let*: ([assignments : (U 'no-such-class (Listof assignment:Record)) (assignment:list class-name)]
          (open-assignments (filter assignment:Record-open (cast assignments (Listof assignment:Record)))))
    (cond [(empty? open-assignments) '((p "There are currently no open assignments."))]
          [else (map (open-assignment-element start-url) open-assignments)])))


; string-url -> (listof xexpr? void?)
(: open-assignment-element (String -> (assignment:Record -> XExpr)))
(define (open-assignment-element start-url)
  (lambda (record)
    (let ((assignment-id (assignment:Record-id record)))
    `(li () ,assignment-id " : "
        (a ((href ,(string-append start-url "../next/" assignment-id "/"))) "Next Step") " - "
        (a ((href ,(string-append start-url "../feedback/" assignment-id "/"))) "Assignment Feedback")))))



; ct-session -> (listof xexpr? void?)
(: list-closed-assignments (ct-session String -> (Listof XExpr)))
(define (list-closed-assignments session start-url)
    (let*: ((uid (ct-session-uid session))
            [assignment-list : (U 'no-such-class (Listof assignment:Record)) (assignment:list class-name)]
            (closed-assignments (filter (show-closed? uid) (cast assignment-list (Listof assignment:Record)))))
      (cond [(empty? closed-assignments) '((p "There are currently no closed assignments."))]
            [else (map (closed-assignment-element start-url) closed-assignments)])))



(: show-closed? (String -> (assignment:Record -> Boolean)))
(define (show-closed? uid)
  (lambda (record)
    (let* ((assignment-id (assignment:Record-id record))
           (closed (not (assignment:Record-open record)))
           (has-submitted (submission:has-submitted? assignment-id class-name uid)))
      (and closed has-submitted))))


(: closed-assignment-element (String -> (assignment:Record -> XExpr)))
(define (closed-assignment-element start-url)
  (lambda (record)
    (let ((assignment-id (assignment:Record-id record)))
      `(li () ,assignment-id " : "
           (a ((href ,(string-append start-url "../feedback/" assignment-id "/"))) "Assignment Feedback")))))
