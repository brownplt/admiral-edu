#lang typed/racket

(require "typed-db.rkt"
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
(: init (-> Void))
(define (init)
  (let* ((drop (merge "DROP TABLE IF EXISTS" table))
         (create (merge "CREATE TABLE" table "(" 
                        assignment-id assignment-id-type ","
                        class-id class-id-type ","
                        assignment-open assignment-open-type ","
                        assignment-ready assignment-ready-type ","
                        "PRIMARY KEY (" assignment-id "," class-id "))")))
    (query-exec drop)
    (query-exec create)))


;; Tries to create the specified assignment for the specified class
;; Returns one of the following
;;   #t - on success
;;   'no-such-class - if the specified class does not exist
;;   'duplicate-assignment - if the assignment id already exists for this class
(provide create)
(: create (String String -> (U 'no-such-class 'duplicate-assignment #t)))
(define (create assignment class)
  (cond
    [(not (class:exists? class)) 'no-such-class]
    [(exists? assignment class) 'duplicate-assignment]
    [else
     (let ((query (merge "INSERT INTO" table "VALUES(?,?,0,0)")))
       (query-exec query assignment class)
       #t)]))


;; Checks if a particular assignment, class pair exists
(provide exists?)
(: exists? (String String -> Boolean))
(define (exists? assignment class)
  (let* ((query (merge "SELECT COUNT(*) FROM" table "WHERE" assignment-id "=? AND" class-id "=? LIMIT 1"))
         (result (cast (query-value query assignment class) Integer)))
    (= 1 result)))


(provide all)
(: all (-> (Listof (Vectorof QueryResult))))
(define (all)
  (let ((query (merge "SELECT * FROM" table)))
    (query-rows query)))

(: set-column (String QueryArgument -> (String String -> Void)))
(define (set-column column-name value)
  (lambda (assignment class)
    (let ((query (merge "UPDATE" table "SET" column-name "=? WHERE" assignment-id "=? AND" class-id "=?")))
      (query-exec query value assignment class))))

(provide open)
(: open (String String -> Void))
(define open (set-column assignment-open 1))

(provide close)
(: close (String String -> Void))
(define close (set-column assignment-open 0))

(provide mark-ready)
(: mark-ready (String String -> Void))
(define mark-ready (set-column assignment-ready 1))

(provide mark-not-ready)
(: mark-not-ready (String String -> Void))
(define mark-not-ready (set-column assignment-ready 0))


(provide (struct-out Record))
(struct: Record ([class : String] [id : String] [open : Boolean] [ready : Boolean]) #:transparent)


(define record-details (merge class-id "," assignment-id "," assignment-open "," assignment-ready))
(define-type Vector-Record (Vector String String (U 0 1) (U 0 1)))

(: row->Record (Vector-Record -> Record))
(define (row->Record vec)
  (let ((class-id (vector-ref vec 0))
        (assignment-id (vector-ref vec 1))
        (open (= 1(vector-ref vec 2)))
        (ready (= 1 (vector-ref vec 3))))
    (Record class-id assignment-id open ready)))


(provide select)
(: select (String String -> Record))
(define (select class assignment)
  (let* ((q (merge "SELECT" record-details
                   "FROM" table
                   "WHERE" class-id "=? AND"
                           assignment-id "=?"
                   "LIMIT 1"))
         (result (cast (query-row q class assignment) Vector-Record)))
    (row->Record result)))


(provide list)
(: list (String -> (U 'no-such-class (Listof Record))))
(define (list class)
  (cond
    [(not (class:exists? class)) 'no-such-class]
    [else
     (let* ((q (merge "SELECT" record-details "FROM" table "WHERE" class-id "=? ORDER BY" assignment-id "ASC"))
            (results (cast (query-rows q class) (Listof Vector-Record))))
       (map row->Record results))]))


(provide delete-assignment)
(: delete-assignment (String String -> Void))
(define (delete-assignment class assignment)
  (let ((query (merge "DELETE FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=?")))
    (query-exec query assignment class)))