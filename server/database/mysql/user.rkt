#lang racket
(require db
         "common.rkt")

;; User Table
(provide user-table user-table-uid user-table-uid-type)
(define user-table "user")
(define user-table-uid "uid")
(define user-table-uid-type "varchar(255)")

;; Initializes the user table.
(provide init-user-table)
(define (init-user-table sql-conn)
  (let ((drop (prepare sql-conn (merge "DROP TABLE IF EXISTS" user-table)))
        (create (prepare sql-conn (merge "CREATE TABLE" user-table "(" user-table-uid user-table-uid-type "unique)"))))
    (query-exec sql-conn drop)
    (query-exec sql-conn create)))

;; Creates a record in the user table
(provide internal-create-user)
(define (internal-create-user sql-conn username)
  (let ((query (prepare sql-conn (merge "INSERT INTO" user-table "values (?)"))))
    (query-exec sql-conn query username)))

;; Retrieves all users
(provide internal-select-users)
(define (internal-select-users sql-conn)
  (let ((query (prepare sql-conn (merge "SELECT * FROM" user-table))))
    (query-rows sql-conn query)))

(provide internal-exists-user)
(define (internal-exists-user sql-conn uid)
  (let* ((query (merge "SELECT COUNT(" user-table-uid ") FROM" user-table "WHERE" user-table-uid "=?"))
         (prep (prepare sql-conn query))
         (result (vector-ref (query-row sql-conn prep uid) 0)))
    (> result 0)))