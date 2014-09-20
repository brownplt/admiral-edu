#lang racket

(require db)
(require "../../configuration.rkt")

(provide (all-from-out db))


(define (connect)
  (let ((new-conn (mysql-connect #:user db-user-name
                                 #:database db-name
                                 #:password db-password
                                 #:server (get-db-address))))
    new-conn))

(provide run)
;(: run (All (A) ((QueryFunction A) String QueryArgument * -> A)))
(define (run query-func q args)
  (let* ((conn (virtual-connection connect))
         (query-args (prepare-statement conn q args))
         (func (cond [(eq? query-func 'query-rows) query-rows]
                     [(eq? query-func 'query-row) query-row]
                     [(eq? query-func 'query-exec) query-exec]
                     [(eq? query-func 'query-value) query-value]))
         (result (apply func query-args)))
    (disconnect conn)
    result))

;(: prepare-statement (Any String (Listof QueryArgument) -> (Listof Any)))
(define (prepare-statement conn q args)
    (append (list conn (virtual-statement q)) args))