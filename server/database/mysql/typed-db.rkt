#lang typed/racket

(require "../../configuration.rkt"
         "../../ct-session.rkt"
         "../../util/basic-types.rkt"
         "../../util/failure.rkt")

(provide (all-from-out "../../util/basic-types.rkt")
         (all-from-out "../../util/failure.rkt"))

(require/typed (prefix-in native: "untyped-db.rkt")
               [native:sql-timestamp? (Any -> Boolean)]
               [native:sql-timestamp-year (Any -> Nonnegative-Integer)]
               [native:sql-timestamp-month (Any -> Nonnegative-Integer)]
               [native:sql-timestamp-day (Any -> Nonnegative-Integer)]
               [native:sql-timestamp-hour (Any -> Nonnegative-Integer)]
               [native:sql-timestamp-minute (Any -> Nonnegative-Integer)]
               [native:sql-timestamp-second (Any -> Nonnegative-Integer)]
               [native:run ((U 'query-rows 'query-row 'query-exec 'query-value) String (Listof QueryArgument) -> Any)])

(provide (struct-out TimeStamp))
(struct: TimeStamp ([year : Nonnegative-Integer] 
                    [month : Nonnegative-Integer]
                    [day : Nonnegative-Integer]
                    [hour : Nonnegative-Integer]
                    [minute : Nonnegative-Integer]
                    [second : Nonnegative-Integer]) #:transparent)

(provide merge)
(: merge (String * -> String))
(define (merge . strings)
  (string-join strings " "))

(provide QueryArgument)
(define-type QueryArgument (U String Number))

(provide QueryResult)
(define-type QueryResult (U String Number TimeStamp))

(: query-exec (String QueryArgument * -> Void))
(provide query-exec)
(define (query-exec query . args)
  (cast (native:run 'query-exec query args) Void))

(: query-row (String QueryArgument * -> (Vectorof QueryResult)))
(provide query-row)
(define (query-row query . args)
  (safe-vector (cast (native:run 'query-row query args) (Vectorof Any))))

(: query-row-list (String (Listof QueryArgument) -> (Vectorof QueryResult)))
(provide query-row-list)
(define (query-row-list query args)
  (safe-vector (cast (native:run 'query-row query args) (Vectorof Any))))

(provide query-rows)
(: query-rows (String QueryArgument * -> (Listof (Vectorof QueryResult))))
(define (query-rows query . args)
  (map safe-vector (cast (native:run 'query-rows query args) (Listof (Vectorof Any)))))

(provide query-rows-list)
(: query-rows-list (String (Listof QueryArgument) -> (Listof (Vectorof QueryResult))))
(define (query-rows-list query args)
  (map safe-vector (cast (native:run 'query-rows query args) (Listof (Vectorof Any)))))

(provide query-value)
(: query-value (String QueryArgument * -> QueryResult))
(define (query-value query . args)
  (->query-result (native:run 'query-value query args)))

;; Converts a Vectorof Any to a Vectorof QueryResults
(: safe-vector ((Vectorof Any) -> (Vectorof QueryResult)))
(define (safe-vector vec)
  (let* ((ls (vector->list vec))
         (safe (map ->query-result ls)))
    (list->vector safe)))

;; Casts Any to a QueryResult. An exception is raised if the value passed in cannot be cast
(: ->query-result (Any -> QueryResult))
(define (->query-result any)
  (cond [(string? any) (cast any String)]
        [(number? any) (cast any Number)]
        [(native:sql-timestamp? any) (TimeStamp 
                                      (native:sql-timestamp-year any)
                                      (native:sql-timestamp-month any)
                                      (native:sql-timestamp-day any)
                                      (native:sql-timestamp-hour any)
                                      (native:sql-timestamp-minute any)
                                      (native:sql-timestamp-second any))]
        [else (error "Invalid type.")]))

(provide common:get-sort-by)
(: common:get-sort-by ((Listof String) Symbol -> (ct-session -> Symbol)))
(define (common:get-sort-by valid-columns default)
  (lambda (session)
    (let ((table (ct-session-table session)))
      (cond [(not (hash-has-key? table 'sort-by)) default]
            [else (let ((dir-string (hash-ref table 'sort-by)))
                    (cond [(valid-column valid-columns dir-string) (string->symbol dir-string)]
                          [else default]))]))))

(: common:sort-by? ((Listof String) -> (Symbol -> Boolean)))
(provide common:sort-by?)
(define (common:sort-by? valid-columns)
  (lambda (symbol)
    (valid-column valid-columns (symbol->string symbol))))

(: valid-column ((Listof String) String -> Boolean))
(define (valid-column valid-columns string)
  (let ((result (member string valid-columns string=?)))
    (if result #t #f)))

(provide order->string)
(: order->string ((U 'asc 'desc) -> String))
(define (order->string order)
  (match order
    ['asc "ASC"]
    ['desc "DESC"]))