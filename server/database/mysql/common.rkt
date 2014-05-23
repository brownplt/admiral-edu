#lang racket

(require db)

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


(provide sql-conn)
(define sql-conn
  (mysql-connect #:user username
                 #:database password
                 #:password database))
(provide try-with-default)
(define (try-with-default default f . args)
  (with-handlers ([exn:fail? (lambda (exn) default)]) (apply f args)))