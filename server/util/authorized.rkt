#lang typed/racket

(require "../base.rkt"
         "../pages/typed-xml.rkt")

(require/typed (prefix-in error: "../pages/errors.rkt")
               [error:not-authorized (-> Any)])


(: role (ct-session -> roles:Record))
(define (role session)
  (let* ((class (ct-session-class session))
         (uid (ct-session-uid session))
         (result (role:select class uid)))
    result))


; TODO: Eventually this should have the following type
;(: can-edit (All (a) (-> ct-session (-> a * (Listof (U XExpr Void))) a * (Listof (U XExpr Void)))))
(provide can-edit)
(: can-edit (All (a b) (-> ct-session (-> a * b) a * Any)))
(define (can-edit session f . args)
  (cond [(can-edit? session) (apply f args)]
        [else (error:not-authorized)]))


(provide can-edit?)
(: can-edit? (ct-session -> Boolean))
(define (can-edit? session)
  (let ((session-role (role session)))
    (roles:Record-can-edit session-role)))