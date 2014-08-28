#lang racket

(require "../base.rkt"
         (prefix-in error: "../pages/errors.rkt"))


(define (role session)
  (let* ((class (ct-session-class session))
         (uid (ct-session-uid session))
         (result (role:select class uid)))
    result))

(provide can-edit)
(define (can-edit session f . args)
  (let* ((session-role (role session))
         (can-edit (roles:role-can-edit session-role)))
    (cond [can-edit (apply f args)]
          [else (error:not-authorized)])))
  
  