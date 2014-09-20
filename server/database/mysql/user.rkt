#lang typed/racket
(require "typed-db.rkt")

;; User Table
(provide table uid uid-type)

(define table "user")
(define uid "uid")
(define uid-type "VARCHAR(255) UNIQUE")

;; Creates the user table removing any previously existing table.
(provide init)
(: init (-> Void))
(define (init)
  (let ((drop (merge "DROP TABLE IF EXISTS" table))
        (create (merge "CREATE TABLE" table "(" uid uid-type ")")))
    (query-exec drop)
    (query-exec create)))

;; Creates a record with the specified username.
(provide create)
(: create (String -> Void))
(define (create username)
  (let ((query (merge "INSERT INTO" table "values (?)")))
    (query-exec query username)))

(provide all)
(: all (-> (Listof (Vectorof QueryResult))))
(define (all)
  (let ((query (merge "SELECT * FROM" table)))
    (query-rows query)))

(provide exists?)
(: exists? (String -> Boolean))
(define (exists? s-uid)
  (let* ((query (merge "SELECT COUNT(" uid ") FROM" table "WHERE" uid "=?"))
         (result (query-row query s-uid))
         (count (vector-ref result 0)))
    (> (cast count Exact-Nonnegative-Integer) 0)))