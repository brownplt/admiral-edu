#lang racket

(require db
         "common.rkt")

(provide table)
(define table "system")

(provide version version-type)
(define version "version")
(define version-type "INT")

(provide id id-type)
(define id "id")
(define id-type "BOOL")

(define the-id 1)

(provide current-version)
(define current-version 1)

;; Initializes the assignment table.
(provide init)
(define (init)
  (let ((drop (merge "DROP TABLE IF EXISTS" table))
        (create (merge "CREATE TABLE" table "(" 
                       version version-type ","
                       id id-type ","
                       "PRIMARY KEY (" id "))"))
        (initial-version (merge "INSERT INTO" table " VALUES(?,?)")))
    (run query-exec drop)
    (run query-exec create)
    (run query-exec initial-version current-version the-id)))


;; Returns the current version of the database configuration
(provide select-version)
(define (select-version)
  (let* ((q (merge "SELECT" version
                   "FROM" table
                   "WHERE" id "=?"
                   "LIMIT 1"))
         (result (run query-row q the-id)))
    (vector-ref result 0)))