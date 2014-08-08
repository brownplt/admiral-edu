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

(define assignment-ready "assignment_ready")
(define assignment-ready-type "BOOLEAN")

;; Initializes the assignment table.
(provide init)
(define (init)
  (let ((drop (prepare sql-conn (merge "DROP TABLE IF EXISTS" table)))
        (create (prepare sql-conn (merge "CREATE TABLE" table "(" 
                                         assignment-id assignment-id-type ","
                                         class-id class-id-type ","
                                         assignment-open assignment-open-type ","
                                         assignment-ready assignment-ready-type ","
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
     (let* ((query (merge "INSERT INTO" table "VALUES(?,?,0,0)"))
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

(define (set-column column-name value)
  (lambda (assignment class)
    (let* ((query (merge "UPDATE" table "SET" column-name "=? WHERE" assignment-id "=? AND" class-id "=?"))
           (prep (prepare sql-conn query)))
      (query-exec sql-conn prep value assignment class))))

(provide open)
(define open (set-column assignment-open 1))

(provide close)
(define close (set-column assignment-open 0))

(provide mark-ready)
(define mark-ready (set-column assignment-ready 1))

(provide mark-not-ready)
(define mark-not-ready (set-column assignment-ready 0))
  
(provide record-class record-id record-open record-ready record?)
(struct record (class id open ready) #:transparent)
    
(provide list)
(define (list class)
  (cond
    [(not (class:exists? class)) 'no-such-class]
    [else
     (let* ((query (merge "SELECT" class-id "," assignment-id "," assignment-open "," assignment-ready "FROM" table "WHERE" class-id "=? ORDER BY" assignment-id "ASC"))
            (prep (prepare sql-conn query))
            (results (query-rows sql-conn prep class)))
       (map result->record results))]))

(define (result->record result)
  (let ((class (vector-ref result 0))
        (id (vector-ref result 1))
        (open (= 1 (vector-ref result 2)))
        (ready (= 1 (vector-ref result 3))))
    (record class id open ready)))