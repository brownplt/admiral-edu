#lang typed/racket

(require "typed-db.rkt")

(provide table)
(: table String)
(define table "system")

(provide version version-type)
(: version String)
(define version "version")
(: version-type String)
(define version-type "INT")

(provide id id-type)
(: id String)
(define id "id")

(: id-type String)
(define id-type "BOOL")

(: the-id 1)
(provide the-id)
(define the-id 1)

(provide current-version)
(: current-version Exact-Nonnegative-Integer)
(define current-version 3)

(provide init)
(: init (-> Void))
(define (init)
  (let ((drop (merge "DROP TABLE IF EXISTS" table))
        (create (merge "CREATE TABLE" table "("
                                      version version-type ","
                                      id id-type ","
                                      "PRIMARY KEY (" id "))"))
        (initial-version (merge "INSERT INTO" table "VALUES(?,?)")))
    (query-exec drop)
    (query-exec create)
    (query-exec initial-version current-version the-id)))

;; Returns the current version of the database configuration
(provide select-version)
(: select-version (-> Exact-Nonnegative-Integer))
(define (select-version)
  (let* ((q (merge "SELECT" version
                   "FROM" table
                   "WHERE" id "=?"
                   "LIMIT 1"))
         (result (query-row q the-id)))
    (cast (vector-ref result 0) Exact-Nonnegative-Integer)))
