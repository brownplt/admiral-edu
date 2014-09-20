#lang typed/racket

(require "../../configuration.rkt"
         "../../ct-session.rkt"
         "../../util/basic-types.rkt"
         "../../util/failure.rkt")

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

(define-type QueryArgument (U String Number))
(define-type QueryResult (U String Number TimeStamp))

;; Attempts to execute a query.
(: query-exec (String QueryArgument * -> (Result Void)))
(provide query-exec)
(define (query-exec query . args)
  (let ((result (wrap-failure (lambda () (cast (native:run 'query-exec query args) Void)))))
    (cond [(Failure? result) (cast result Failure)]
          [else (Success (void))])))

;; Attempts to Query a single row.
(: query-row (String QueryArgument * -> (Result (Vectorof QueryResult))))
(provide query-row)
(define (query-row query . args)
  (let*: ([thunk : (-> (Vectorof Any)) (lambda () (cast (native:run 'query-row query args) (Vectorof Any)))]
          (try (wrap-failure thunk)))
    (cond [(Failure? try) (cast try Failure)]
          [else (Success (safe-vector (cast try (Vectorof Any))))])))

;; Attempts to Query rows
(provide query-rows)
(: query-rows (String QueryArgument * -> (Result (Listof (Vectorof QueryResult)))))
(define (query-rows query . args)
  (let ((result (wrap-failure (lambda () (map safe-vector (cast (native:run 'query-rows query args) (Listof (Vectorof Any))))))))
    (cond [(Failure? result) (cast result Failure)]
          [else (cast (Success result) (Success (Listof (Vectorof QueryResult))))])))

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


    
