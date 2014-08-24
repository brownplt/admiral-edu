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
  (let* ((conn (make-sql-conn))
        (drop (prepare conn (merge "DROP TABLE IF EXISTS" table)))
        (create (prepare conn (merge "CREATE TABLE" table "(" 
                                         assignment-id assignment-id-type ","
                                         class-id class-id-type ","
                                         assignment-open assignment-open-type ","
                                         assignment-ready assignment-ready-type ","
                                         "PRIMARY KEY (" assignment-id "," class-id "))"))))
    (query-exec conn drop)
    (query-exec conn create)
    (release conn)))

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
     (let* ((conn (make-sql-conn))
            (query (merge "INSERT INTO" table "VALUES(?,?,0,0)"))
            (prep (prepare conn query)))
       (query-exec conn prep assignment class) 
       (release conn)
       #t)]))

;; Checks if a particular assignment, class pair exists
(provide exists?)
(define (exists? assignment class)
  (let* ((conn (make-sql-conn))
         (query (merge "SELECT COUNT(*) FROM" table "WHERE" assignment-id "=? AND" class-id "=? LIMIT 1"))
         (prep (prepare conn query))
         (result (vector-ref (query-row conn prep assignment class) 0)))
    (release conn)
    (= 1 result)))

(provide all)
(define (all)
  (let* ((conn (make-sql-conn))
         (query (merge "SELECT * FROM" table))
         (prep (prepare conn query))
         (result (query-rows conn prep)))
    (release conn)
    result))

(define (set-column column-name value)
  (lambda (assignment class)
    (let* ((conn (make-sql-conn))
           (query (merge "UPDATE" table "SET" column-name "=? WHERE" assignment-id "=? AND" class-id "=?"))
           (prep (prepare conn query)))
      (query-exec conn prep value assignment class)
      (release conn))))

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
     (let* ((conn (make-sql-conn))
            (query (merge "SELECT" class-id "," assignment-id "," assignment-open "," assignment-ready "FROM" table "WHERE" class-id "=? ORDER BY" assignment-id "ASC"))
            (prep (prepare conn query))
            (results (query-rows conn prep class)))
       (release conn)
       (map result->record results))]))

(define (result->record result)
  (let ((class (vector-ref result 0))
        (id (vector-ref result 1))
        (open (= 1 (vector-ref result 2)))
        (ready (= 1 (vector-ref result 3))))
    (record class id open ready)))