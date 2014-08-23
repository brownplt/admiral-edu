#lang racket

(require db
         "common.rkt")

;; Class Table
(provide table id id-type)
(define table "class")
(define id "id")
(define id-type "varchar(255)")

;; Initializes the class table.
(provide init)
(define (init)
  (let ((drop (prepare (sql-conn) (merge "DROP TABLE IF EXISTS" table)))
        (create (prepare (sql-conn) (merge "CREATE TABLE" table "(" id id-type "unique)"))))
    (query-exec (sql-conn) drop)
    (query-exec (sql-conn) create)))

;; Retrieve all classes
(provide all)
(define (all)
  (let ((query (prepare (sql-conn) (merge "SELECT * FROM" table))))
    (query-rows (sql-conn) query)))

;; Creates a record in the class table
(provide create)
(define (create id)
  (let ((create (prepare (sql-conn) (merge "INSERT INTO" table "values(?)"))))
    (query-exec (sql-conn) create id)))

;; Checks that a class exists
(provide exists?)
(define (exists? class)
  (let* ((query (merge "SELECT COUNT(*) FROM" table "WHERE" id "=? LIMIT 1"))
         (prepped (prepare (sql-conn) query))
         (result (vector-ref (query-row (sql-conn) prepped class) 0)))
    (= 1 result)))
         