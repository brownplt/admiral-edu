#lang racket

(require db)
(require "../../configuration.rkt"
         "../../ct-session.rkt")

;; Helper functions


(provide merge)
;(: merge (String * -> String))
(define (merge . strings)
  (string-join strings " "))

(provide release)
(define (release conn)
  (if (connected? conn) 
      (begin
        (disconnect conn))
      #f))

(provide make-sql-conn)
(define (make-sql-conn)
  (mysql-connect #:user (db-user-name)
                 #:database (db-name)
                 #:password (db-password)
                 #:server (db-address)))

(provide run)
(define (run query-func q . args)
  (let* ((conn (virtual-connection make-sql-conn))
         (query-args (prepare-statement conn q args))
         (result (apply query-func query-args)))
    (release conn)
    result))

(define (prepare-statement conn q args)
    (append (list conn (virtual-statement q)) args))

(provide order->string)
(define (order->string order)
  (match order
    ['asc "ASC"]
    ['desc "DESC"]
    [_ (error (format "Could not convert ~a to order." order))]))

; ((Listof String) Symbol -> (ct-session -> Symbol))
(provide common:get-sort-by)
(define (common:get-sort-by valid-columns default)
  (lambda (session)
    (let ((table (ct-session-table session)))
      (cond [(not (hash-has-key? table 'sort-by)) default]
            [else (let ((dir-string (hash-ref table 'sort-by)))
                    (cond [(valid-column valid-columns dir-string) (string->symbol dir-string)]
                          [else default]))]))))

;((Listof String) -> (Symbol -> Boolean))
(provide common:sort-by?)
(define (common:sort-by? valid-columns)
  (lambda (symbol)
    (valid-column valid-columns (symbol->string symbol))))

;((Listof String) String -> Boolean)
(define (valid-column valid-columns string)
  (let ((result (member string valid-columns string=?)))
    (if result #t #f)))

