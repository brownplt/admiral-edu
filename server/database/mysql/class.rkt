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
  (let* ((conn (make-sql-conn))
        (drop (prepare conn (merge "DROP TABLE IF EXISTS" table)))
        (create (prepare conn (merge "CREATE TABLE" table "(" id id-type "unique)"))))
    (query-exec conn drop)
    (query-exec conn create)
    (release conn)))

;; Retrieve all classes
(provide all)
(define (all)
  (let* ((conn (make-sql-conn))
         (query (prepare conn (merge "SELECT * FROM" table)))
         (result (query-rows conn query)))
    (release conn)
    result))

;; Creates a record in the class table
(provide create)
(define (create id)
  (let* ((conn (make-sql-conn))
         (create (prepare conn (merge "INSERT INTO" table "values(?)"))))
    (query-exec conn create id)
    (release conn)))

;; Checks that a class exists
(provide exists?)
(define (exists? class)
  (let* ((conn (make-sql-conn))
         (query (merge "SELECT COUNT(*) FROM" table "WHERE" id "=? LIMIT 1"))
         (prepped (prepare conn query))
         (result (vector-ref (query-row conn prepped class) 0)))
    (release conn)
    (= 1 result)))
         