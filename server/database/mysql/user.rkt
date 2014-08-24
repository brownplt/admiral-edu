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
  (let* ((conn (make-sql-conn))
         (drop (prepare conn (merge "DROP TABLE IF EXISTS" table)))
         (create (prepare conn (merge "CREATE TABLE" table "(" uid uid-type "unique)"))))
    (query-exec conn drop)
    (query-exec conn create)
    (release conn)))

;; Creates a record in the user table
(provide create)
(define (create username)
  (let* ((conn (make-sql-conn))
         (query (prepare conn (merge "INSERT INTO" table "values (?)"))))
    (query-exec conn query username)
    (release conn)))

;; Retrieves all users
(provide all)
(define (all)
  (let* ((conn (make-sql-conn))
         (query (prepare conn (merge "SELECT * FROM" table)))
         (result (query-rows conn query)))
    (release conn)
    result))

(provide exists?)
(define (exists? s-uid)
  (let* ((conn (make-sql-conn))
         (query (merge "SELECT COUNT(" uid ") FROM" table "WHERE" uid "=?"))
         (prep (prepare conn query))
         (result (vector-ref (query-row conn prep s-uid) 0)))
    (release conn)
    (> result 0)))