#lang racket

(require db
         "common.rkt")

;; Class Table
(provide class-table class-table-id class-table-id-type)
(define class-table "class")
(define class-table-id "id")
(define class-table-id-type "varchar(255)")

;; Initializes the class table.
(provide init-class-table)
(define (init-class-table sql-conn)
  (let ((drop (prepare sql-conn (merge "DROP TABLE IF EXISTS" class-table)))
        (create (prepare sql-conn (merge "CREATE TABLE" class-table "(" class-table-id class-table-id-type "unique)"))))
    (query-exec sql-conn drop)
    (query-exec sql-conn create)))

;; Retrieve all classes
(provide internal-select-classes)
(define (internal-select-classes sql-conn)
  (let ((query (prepare sql-conn (merge "SELECT * FROM" class-table))))
    (query-rows sql-conn query)))

;; Creates a record in the class table
(provide internal-create-class)
(define (internal-create-class sql-conn id)
  (let ((create (prepare sql-conn (merge "INSERT INTO" class-table "values(?)"))))
    (query-exec sql-conn create id)))