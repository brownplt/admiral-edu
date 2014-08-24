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
        (set! counter (- counter 1))
        (disconnect conn))
      #f))

(define counter 0)

(define (connect)
  (set! counter (+ counter 1))
  (let ((new-conn (mysql-connect #:user username
                                 #:database password
                                 #:password database
                                 #:server db-address)))
    
    ;;TODO: Really bad hack that prevents too many sql connections error
    ;(thread (lambda ()
    ;          (sleep 30) (release new-conn)))
    new-conn))

(define pool (connection-pool connect))

;(define internal-conn (virtual-connection pool))

(provide try-with-default)
(define (try-with-default default f . args)
  (with-handlers ([exn:fail? (lambda (exn) default)]) (apply f args)))