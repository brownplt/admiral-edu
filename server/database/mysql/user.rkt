#lang racket
(require db
         "common.rkt")

;; User Table
(provide table uid uid-type)
(define table "user")
(define uid "uid")
(define uid-type "varchar(255)")

;; Initializes the user table.
(provide init)
(define (init)
  (let ((drop (prepare sql-conn (merge "DROP TABLE IF EXISTS" table)))
        (create (prepare sql-conn (merge "CREATE TABLE" table "(" uid uid-type "unique)"))))
    (query-exec sql-conn drop)
    (query-exec sql-conn create)))

;; Creates a record in the user table
(provide create)
(define (create username)
  (let ((query (prepare sql-conn (merge "INSERT INTO" table "values (?)"))))
    (query-exec sql-conn query username)))

;; Retrieves all users
(provide all)
(define (all)
  (let ((query (prepare sql-conn (merge "SELECT * FROM" table))))
    (query-rows sql-conn query)))

(provide exists?)
(define (exists? s-uid)
  (let* ((query (merge "SELECT COUNT(" uid ") FROM" table "WHERE" uid "=?"))
         (prep (prepare sql-conn query))
         (result (vector-ref (query-row sql-conn prep s-uid) 0)))
    (> result 0)))