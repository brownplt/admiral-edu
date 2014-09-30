#lang typed/racket

(require/typed 
 (prefix-in error: "errors.rkt")
 [error:error (String -> (Listof (U XExpr Void)))])

(require "../base.rkt"
         "typed-xml.rkt"
         (prefix-in dashboard: "assignments/dashboard.rkt")
         (prefix-in list: "assignments/list.rkt")
         (prefix-in action: "assignments/action.rkt")
         (prefix-in student-view: "assignments/student-view.rkt")
         (prefix-in status: "assignments/status.rkt"))

(provide load)
(: load (->* (ct-session (Listof String) Boolean) ((U XExpr #f)) (Listof (U XExpr Void))))
(define (load session url post [message #f])
    (cond [(can-edit? session) (show-instructor-view session url post message)]
          [else  (student-view:load session url message post)]))

(: can-edit? (ct-session -> Boolean))
(define (can-edit? session)
  (let ((session-role (role session)))
    (roles:Record-can-edit session-role)))

(: role (ct-session -> roles:Record))
(define (role session)
  (let* ((class (ct-session-class session))
         (uid (ct-session-uid session))
         (result (role:select class uid)))
    result))

(: show-instructor-view (ct-session (Listof String) Boolean (U XExpr #f) -> (Listof (U XExpr Void))))
(define (show-instructor-view session url post message)
  (let ((action (if (empty? url) action:LIST (first url)))
        (rest-url (if (empty? url) url (rest url))))
    ((lookup-action-function action) session rest-url message post)))

(: no-such-action (Any -> (->* (ct-session (Listof String) (U XExpr #f)) (Boolean) (Listof (U XExpr Void)))))
(define (no-such-action action)
  (lambda (_ __ ___ [____ #f])
    (error:error (format "The requested action ~a is not valid here." action))))

(: action-functions (HashTable String (->* (ct-session (Listof String) (U XExpr #f)) (Boolean) (Listof (U XExpr Void)))))
(define action-functions
  (make-hash (list
              (cons action:LIST list:load)
              (cons action:DASHBOARD dashboard:load)
              (cons action:OPEN dashboard:open)
              (cons action:CLOSE dashboard:close)
              (cons action:DELETE dashboard:delete)
              (cons "status" status:load))))

(: lookup-action-function (String -> (->* (ct-session (Listof String) (U XExpr #f)) (Boolean) (Listof (U XExpr Void)))))
(define (lookup-action-function action)
  (cond [(hash-has-key? action-functions action) (hash-ref action-functions action)]
        [else (no-such-action action)]))

