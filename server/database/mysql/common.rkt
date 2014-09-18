#lang racket

(require db)
(require "../../configuration.rkt")

(provide intercalate merge)

 ;; Helper functions
(define (intercalate v ls)
  (cdr 
   (foldr 
    (lambda (x xs) 
      (cons v (cons x xs))) 
    '() ls)))

(define (merge . strings)
  (foldr string-append ""
         (intercalate " " strings)))

;; Database Information
(define username "captain_teach")
(define password "captain_teach")
(define database "captain_teach")

(provide make-sql-conn)
(define (make-sql-conn)
  (connect))

(provide release)
(define (release conn)
  
  (if (connected? conn) 
      (begin
        (disconnect conn))
      #f))

(provide connect)
(define (connect)
  (let ((new-conn (mysql-connect #:user username
                                 #:database password
                                 #:password database
                                 #:server (get-db-address))))

    new-conn))

;;TODO: We should really be using a connection pool. However,
;; this eventually causes the system to hang after being idle and results
;; in the annoying 502 proxy errors
(define pool (connection-pool connect))

(provide try-with-default)
(define (try-with-default default f . args)
  (with-handlers ([exn:fail? (lambda (exn) default)]) (apply f args)))

(provide run)
(define (run query-func q . args)
  (let* ((conn (virtual-connection connect))
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

