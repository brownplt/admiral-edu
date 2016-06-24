#lang racket

(require db)
(require "../../configuration.rkt")

(provide (all-from-out db))

(provide (struct-out Null))
(struct Null () #:transparent)

(provide (struct-out TimeStamp))
(struct TimeStamp (year
                   month
                   day
                   hour
                   minute
                   second) #:transparent)

;(struct: TimeStamp ([year : Nonnegative-Integer] 
;                    [month : Nonnegative-Integer]
;                    [day : Nonnegative-Integer]
;                    [hour : Nonnegative-Integer]
;                    [minute : Nonnegative-Integer]
;                    [second : Nonnegative-Integer]) #:transparent)




(define (connect)
  (let ((new-conn (mysql-connect #:user (db-user-name)
                                 #:database (db-name)
                                 #:password (db-password)
                                 #:server (db-address))))
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
    (cond [(list? result) (map safe-vector result)]
          [(vector? result) (safe-vector result)]
          [(void? result) (void)]
          [else (->query-result result)])))

;; Converts a Vectorof Any to a Vectorof QueryResults
;(: safe-vector ((Vectorof Any) -> (Vectorof QueryResult)))
(define (safe-vector vec)
  (let* ((ls (vector->list vec))
         (safe (map ->query-result ls)))
    (list->vector safe)))

;(: ->query-result (Any -> QueryResult))
(provide ->query-result)
(define (->query-result any)
  (cond [(string? any) any]
        [(number? any) any]
        [(sql-timestamp? any) (TimeStamp 
                                      (sql-timestamp-year any)
                                      (sql-timestamp-month any)
                                      (sql-timestamp-day any)
                                      (sql-timestamp-hour any)
                                      (sql-timestamp-minute any)
                                      (sql-timestamp-second any))]
        [(sql-null? any) (Null)]
        [else (error "Invalid type.")]))



;(: prepare-statement (Any String (Listof QueryArgument) -> (Listof Any)))
(define (prepare-statement conn q args)
    (append (list conn (virtual-statement q)) args))