#lang typed/racket

(require "../../configuration.rkt"
         "../../ct-session.rkt"
         "../../util/basic-types.rkt"
         "../../util/failure.rkt")

(provide (all-from-out "../../util/basic-types.rkt"))

(require/typed (prefix-in native: "untyped-db.rkt")
               [native:sql-timestamp? (Any -> Boolean)]
               [native:sql-timestamp-year (Any -> Nonnegative-Integer)]
               [native:sql-timestamp-month (Any -> Nonnegative-Integer)]
               [native:sql-timestamp-day (Any -> Nonnegative-Integer)]
               [native:sql-timestamp-hour (Any -> Nonnegative-Integer)]
               [native:sql-timestamp-minute (Any -> Nonnegative-Integer)]
               [native:sql-timestamp-second (Any -> Nonnegative-Integer)]
               [native:run ((U 'query-rows 'query-row 'query-exec) String (Listof QueryArgument) -> Any)])

(provide TimeStamp)
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

(provide query-rows)
(: query-rows (String QueryArgument * -> (Listof (Vectorof QueryResult))))
(define (query-rows query . args)
  (map safe-vector (cast (native:run 'query-rows query args) (Listof (Vectorof Any)))))

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


    
