#lang racket

(require db
         "common.rkt"
         (prefix-in class: "class.rkt"))

;; Assignment Table
(provide table assignment-id assignment-id-type class-id class-id-type)
(define table "assignment")

(define assignment-id "assignment_id")
(define assignment-id-type "VARCHAR(255)")

(define class-id "class_id")
(define class-id-type class:id-type)

(define assignment-open "assignment_open")
(define assignment-open-type "BOOLEAN")

;; Initializes the assignment table.
(provide init)
(define (init)
  (let ((drop (prepare sql-conn (merge "DROP TABLE IF EXISTS" table)))
        (create (prepare sql-conn (merge "CREATE TABLE" table "(" 
                                         assignment-id assignment-id-type ","
                                         class-id class-id-type ","
                                         assignment-open assignment-open-type ","
                                         "PRIMARY KEY (" assignment-id "," class-id "))"))))
    (query-exec sql-conn drop)
    (query-exec sql-conn create)))

;; Tries to create the specified assignment for the specified class
;; Returns one of the following
;;   #t - on success
;;   'no-such-class - if the specified class does not exist
;;   'duplicate-assignment - if the assignment id already exists for this class
(provide create)
(define (create assignment class)
  (cond
    [(not (class:exists? class)) 'no-such-class]
    [(exists? assignment class) 'duplicate-assignment]
    [else
     (let* ((query (merge "INSERT INTO" table "VALUES(?,?,0)"))
            (prep (prepare sql-conn query)))
       (query-exec sql-conn prep assignment class) 
       #t)]))

;; Checks if a particular assignment, class pair exists
(provide exists?)
(define (exists? assignment class)
  (let* ((query (merge "SELECT COUNT(*) FROM" table "WHERE" assignment-id "=? AND" class-id "=? LIMIT 1"))
         (prep (prepare sql-conn query))
         (result (vector-ref (query-row sql-conn prep assignment class) 0)))
    (= 1 result)))

(provide all)
(define (all)
  (let* ((query (merge "SELECT * FROM" table))
         (prep (prepare sql-conn query)))
    (query-rows sql-conn prep)))

(provide open)
(define (open assignment class)
  (let* ((query (merge "UPDATE" table "SET" assignment-open "=1 WHERE" assignment-id "=? AND" class-id "=?"))
         (prep (prepare sql-conn query)))
    (query-exec sql-conn prep assignment class)))

(provide close)
(define (close assignment class)
  (let* ((query (merge "UPDATE" table "SET" assignment-open "=0 WHERE" assignment-id "=? AND" class-id "=?"))
         (prep (prepare sql-conn query)))
    (query-exec sql-conn prep assignment class)))
         
    
(provide list)
(define (list class)
  (cond
    [(not (class:exists? class)) 'no-such-class]
    [else
     (let* ((query (merge "SELECT * FROM" table "WHERE" class-id "=?"))
            (prep (prepare sql-conn query)))
       (query-rows sql-conn prep class))]))