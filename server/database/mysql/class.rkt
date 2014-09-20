#lang typed/racket

(require "typed-db.rkt")

;; Class Table
(provide table id id-type)
(define table "class")
(define id "id")
(define id-type "VARCHAR(255) UNIQUE")

(provide init)
(: init (-> Void))
(define (init)
  (let ((drop (merge "DROP TABLE IF EXISTS" table))
        (create (merge "CREATE TABLE" table "(" id id-type ")")))
    (query-exec drop)
    (query-exec create)))

(provide all)
(: all (-> (Listof (Vectorof QueryResult))))
(define (all)
  (query-rows (merge "SELECT * FROM" table)))

(provide create)
(: create (String -> Void))
(define (create id)
  (query-exec (merge "INSERT INTO" table "values(?)") id))

(provide exists?)
(: exists? (String -> Boolean))
(define (exists? class)
  (let* ((query (merge "SELECT COUNT(*) FROM" table "WHERE" id "=? LIMIT 1"))
         (result (cast (query-value query class) Exact-Nonnegative-Integer)))
    (= result 1)))