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
  (connection-pool-lease pool))

#|
  (if internal-sql-conn (check-connection)
      (begin
        (connect)
        internal-sql-conn)))
|#


(provide release)
(define (release conn)
  
  (if (connected? conn) 
      (begin
        (disconnect conn))
      #f))

(define (connect)
  (let ((new-conn (mysql-connect #:user username
                                 #:database password
                                 #:password database
                                 #:server (get-db-address))))

    new-conn))

(define pool (connection-pool connect))

(provide try-with-default)
(define (try-with-default default f . args)
  (with-handlers ([exn:fail? (lambda (exn) default)]) (apply f args)))

(provide run)
(define (run query-func q . args)
  (let* ((conn (make-sql-conn))
         (query-args (prepare-statement conn q args))
         (result (apply query-func query-args)))
    (release conn)
    result))

(define (prepare-statement conn q args)
    (append (list conn (prepare conn q)) args))

