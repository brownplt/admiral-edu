#lang racket

(require db
         "common.rkt")

;; Class Table
(provide table id-column id-type)
(define table "class")
(define id-column "id")
(define id-type "varchar(255)")

;; Initializes the class table.
(provide init)
(define (init sql-conn)
  (let ((drop (prepare sql-conn (merge "DROP TABLE IF EXISTS" table)))
        (create (prepare sql-conn (merge "CREATE TABLE" table "(" id-column id-type "unique)"))))
    (query-exec sql-conn drop)
    (query-exec sql-conn create)))

;; Retrieve all classes
(provide select-classes)
(define (select-classes sql-conn)
  (let ((query (prepare sql-conn (merge "SELECT * FROM" table))))
    (query-rows sql-conn query)))

;; Creates a record in the class table
(provide create-class)
(define (create-class sql-conn id)
  (let ((create (prepare sql-conn (merge "INSERT INTO" table "values(?)"))))
    (query-exec sql-conn create id)))