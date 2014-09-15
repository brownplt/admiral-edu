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
  (let* ((drop (merge "DROP TABLE IF EXISTS" table))
         (create (merge "CREATE TABLE" table "(" 
                        assignment-id assignment-id-type ","
                        class-id class-id-type ","
                        assignment-open assignment-open-type ","
                        assignment-ready assignment-ready-type ","
                        "PRIMARY KEY (" assignment-id "," class-id "))")))
    (run query-exec drop)
    (run query-exec create)))

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
     (let ((query (merge "INSERT INTO" table "VALUES(?,?,0,0)")))
       (run query-exec query assignment class)
       #t)]))

;; Checks if a particular assignment, class pair exists
(provide exists?)
(define (exists? assignment class)
  (let* ((query (merge "SELECT COUNT(*) FROM" table "WHERE" assignment-id "=? AND" class-id "=? LIMIT 1"))
         (result (vector-ref (run query-row query assignment class) 0)))
    (= 1 result)))

(provide all)
(define (all)
  (let ((query (merge "SELECT * FROM" table)))
    (run query-rows query)))

(define (set-column column-name value)
  (lambda (assignment class)
    (let ((query (merge "UPDATE" table "SET" column-name "=? WHERE" assignment-id "=? AND" class-id "=?")))
      (run query-exec query value assignment class))))

(provide open)
(define open (set-column assignment-open 1))

(provide close)
(define close (set-column assignment-open 0))

(provide mark-ready)
(define mark-ready (set-column assignment-ready 1))

(provide mark-not-ready)
(define mark-not-ready (set-column assignment-ready 0))
  
(provide (struct-out record))
(struct record (class id open ready) #:transparent)

(define record-details (merge class-id "," assignment-id "," assignment-open "," assignment-ready))
(define (row->record vec)
  (let ((class-id (vector-ref vec 0))
        (assignment-id (vector-ref vec 1))
        (open (= 1(vector-ref vec 2)))
        (ready (= 1 (vector-ref vec 3))))
    (record class-id assignment-id open ready)))

(provide select)
(define (select class assignment)
  (let ((q (merge "SELECT" record-details
                  "FROM" table
                  "WHERE" class-id "=? AND"
                          assignment-id "=?"
                  "LIMIT 1")))
    (row->record (run query-row q class assignment))))
    
(provide list)
(define (list class)
  (cond
    [(not (class:exists? class)) 'no-such-class]
    [else
     (let* ((q (merge "SELECT" class-id "," assignment-id "," assignment-open "," assignment-ready "FROM" table "WHERE" class-id "=? ORDER BY" assignment-id "ASC"))
            (results (run query-rows q class)))
       (map result->record results))]))

(provide delete-assignment)
(define (delete-assignment class assignment)
  (let ((query (merge "DELETE FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=?")))
    (run query-exec query assignment class)))


(define (result->record result)
  (let ((class (vector-ref result 0))
        (id (vector-ref result 1))
        (open (= 1 (vector-ref result 2)))
        (ready (= 1 (vector-ref result 3))))
    (record class id open ready)))