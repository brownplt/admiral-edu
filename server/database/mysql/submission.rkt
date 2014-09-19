#lang racket

(require db
         "common.rkt"
         "../../ct-session.rkt"
         (prefix-in class: "class.rkt")
         (prefix-in assignment: "assignment.rkt")
         (prefix-in user: "user.rkt")
         (prefix-in role: "role.rkt"))

;; Submission Table
(provide table)
(define table "submission")

(provide assignment-id assignment-id-type)
(define assignment-id "assignment_id")
(define assignment-id-type assignment:assignment-id-type)

(provide class-id class-id-type)
(define class-id "class_id")
(define class-id-type class:id-type)

(provide step-id step-id-type)
(define step-id "step_id")
(define step-id-type "VARCHAR(255)")

(provide user-id user-id-type)
(define user-id "user_id")
(define user-id-type user:uid-type)

(provide time-stamp time-stamp-type time-stamp-type-1)
(define time-stamp "time_stamp")
(define time-stamp-type "TIMESTAMP DEFAULT 0")
(define time-stamp-type-0 "TIMESTAMP")
(define time-stamp-type-1 "TIMESTAMP DEFAULT 0")

(provide times-reviewed times-reviewed-type)
(define times-reviewed "times_reviewed")
(define times-reviewed-type "INT")

(define valid-columns `(,class-id ,step-id ,user-id ,time-stamp ,times-reviewed))

; ct-session -> (U 'class_id 'step_id 'user_id 'time_stamp 'times_reviewed)
(provide get-sort-by)
(define get-sort-by (common:get-sort-by valid-columns 'user_id))

(provide sort-by?)
(define sort-by? (common:sort-by? valid-columns))


;; Initializes the assignment table.
(provide init)
(define (init)
  (let* ((conn (make-sql-conn))
         (drop (prepare conn (merge "DROP TABLE IF EXISTS" table)))
        (create (prepare conn (merge "CREATE TABLE" table "(" 
                                         assignment-id assignment-id-type ","
                                         class-id class-id-type ","
                                         step-id step-id-type ","
                                         user-id user-id-type ","
                                         time-stamp time-stamp-type ","
                                         times-reviewed times-reviewed-type ","
                                         "PRIMARY KEY (" assignment-id "," class-id "," step-id "," user-id "))"))))
    (query-exec conn drop)
    (query-exec conn create)
    (release conn)))


(provide(struct-out record))
(struct record (assignment class step user time-stamp) #:transparent)

(define record-select (merge assignment-id "," class-id "," step-id "," user-id "," time-stamp))

(define (vector->record vec)
  (let ((assignment (vector-ref vec 0))
        (class (vector-ref vec 1))
        (step (vector-ref vec 2))
        (user (vector-ref vec 3))
        (time-stamp (vector-ref vec 4)))
    (record assignment class step user time-stamp)))


;;TODO Add Instructor-solution field and mark true
(provide create-instructor-solution)
(define (create-instructor-solution assignment class step user)
     (let* ((conn (make-sql-conn))
            (query (merge "INSERT INTO" table " VALUES(?,?,?,?,NOW(),9001)"))
            (prep (prepare conn query)))
       (query-exec conn prep assignment class step user)
       (release conn)
       #t))

;; Creates a record for the specified assignment, class, step, and user.
;; This function returns one of the following:
;; #t - If successful, a timestamp and version number are generated and returns #t
;; 'no-such-class - if the specified class doesn't exist
;; 'no-such-assignment - if the specified class doesn't exist for the class
;; 'no-such-user - if the specified user does not exist
;; 'no-such-user-in-class - if the specified user exists but is not registered for the class
(provide create)
(define (create assignment class step user)
  (cond
    [(not (class:exists? class)) 'no-such-class]
    [(not (assignment:exists? assignment class)) 'no-such-assignment]
    [(not (user:exists? user)) 'no-such-user]
    [(not (role:exists? class user)) 'no-such-user-in-class]
    [else
     (let* ((conn (make-sql-conn))
            (query (merge "INSERT INTO" table " VALUES(?,?,?,?,NOW(),0)"))
            (prep (prepare conn query)))
       (query-exec conn prep assignment class step user)
       (release conn)
       #t)]))

;; Given an assignment, class, step, and user, lists all entries ordered by
;; their version number
;; This function returns one of the following:
;; listof record? - If successful, a timestamp and version number are generated and returns #t
;; 'no-such-class - if the specified class doesn't exist
;; 'no-such-assignment - if the specified class doesn't exist for the class
;; 'no-such-user - if the specified user does not exist
;; 'no-such-user-in-class - if the specified user exists but is not registered for the class
(provide list)
(define (list assignment class step user)
  (cond
    [(not (class:exists? class)) 'no-such-class]
    [(not (assignment:exists? assignment class)) 'no-such-assignment]
    [(not (user:exists? user)) 'no-such-user]
    [(not (role:exists? class user)) 'no-such-user-in-class]
    [else
     (let* ((conn (make-sql-conn))
            (query (merge "SELECT" time-stamp
                          "FROM" table 
                          "WHERE" assignment-id "=? AND"
                                  class-id "=? AND"
                                  step-id "=? AND"
                                  user-id "=?"
                          "ORDER BY" time-stamp "DESC"))
            (prep (prepare conn query))
            (result (query-rows conn prep assignment class step user))
            (to-record (lambda (vec) (record assignment class step user (vector-ref vec 0)))))
       (release conn)
       (map to-record result))]))

(provide reviewed)
(define (reviewed assignment class step user)
  (let* ((conn (make-sql-conn))
         (query (merge "SELECT" times-reviewed
                       "FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=? AND"
                               step-id "=? AND"
                               user-id "=? LIMIT 1"))
         (prep (prepare conn query))
         (result (query-row conn prep assignment class step user)))
    (release conn)
    (vector-ref result 0)))

(provide increment-reviewed)
(define (increment-reviewed assignment class step user)
  (let* ((conn (make-sql-conn))
         (current (reviewed assignment class step user))
         (plusOne (+ current 1))
         (query (merge "UPDATE" table
                       "SET" times-reviewed "=?"
                       "WHERE" assignment-id "=? AND"
                               class-id "=? AND"
                               step-id "=? AND"
                               user-id "=?"))
         (prep (prepare conn query)))
    (query-exec conn prep plusOne assignment class step user)
    (release conn)))

(provide select-least-reviewed)
(define (select-least-reviewed assignment class step not-users)
  (let* ((conn (make-sql-conn))
         (user-commas (string-join (build-list (length not-users) (lambda (n) "?")) ","))
         (query (merge "SELECT" user-id
                       "FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=? AND"
                               step-id "=? AND"
                               user-id " NOT IN (" user-commas ")"
                       "ORDER BY" times-reviewed "ASC"
                       "LIMIT 1"))
         (prep (prepare conn query))
         (arg-list (append `(,conn ,prep ,assignment ,class ,step) not-users))
         (result (apply query-row arg-list)))
    (release conn)
    (vector-ref result 0)))
         

;; Given an assignment, class, step, and user, returns the number of entries that have been created
;; This function returns one of the following:
;; number? - If successful, a timestamp and version number are generated and returns #t
;; 'no-such-class - if the specified class doesn't exist
;; 'no-such-assignment - if the specified class doesn't exist for the class
;; 'no-such-user - if the specified user does not exist
;; 'no-such-user-in-class - if the specified user exists but is not registered for the class
(provide count)
(define (count assignment class step user)
  (cond
    [(not (class:exists? class)) 'no-such-class]
    [(not (assignment:exists? assignment class)) 'no-such-assignment]
    [(not (user:exists? user)) 'no-such-user]
    [(not (role:exists? class user)) 'no-such-user-in-class]
    [else
     (let* ((conn (make-sql-conn))
            (query (merge "SELECT COUNT(*)"
                          "FROM" table 
                          "WHERE" assignment-id "=? AND"
                                  class-id "=? AND"
                                  step-id "=? AND"
                                  user-id "=?"
                          "LIMIT 1"))
            (prep (prepare conn query))
            (result (vector-ref (query-row conn prep assignment class step user) 0)))
       (release conn)
       result)]))

(provide exists?)
(define (exists? assignment class step user)
         (let* ((conn (make-sql-conn))
                (query (merge "SELECT COUNT(*)"
                              "FROM" table
                              "WHERE" assignment-id "=? AND"
                                      class-id "=? AND"
                                      step-id "=? AND"
                                      user-id "=?"
                              "LIMIT 1"))
                (prep (prepare conn query))
                (result (vector-ref (query-row conn prep assignment class step user) 0)))
           (release conn)
           (> result 0)))

(provide has-submitted)
(define (has-submitted assignment class user)
  (let* ((q (merge "SELECT COUNT(*)"
                   "FROM" table
                   "WHERE" assignment-id "=? AND"
                           class-id "=? AND"
                           user-id "=?"
                   "LIMIT 1"))
         (result (run query-row q assignment class user))
         (value (vector-ref result 0)))
    (> value 0)))

(provide delete-assignment)
(define (delete-assignment class assignment)
  (let ((query (merge "DELETE FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=?")))
    (run query-exec query assignment class)))

(provide select-all)
(define (select-all assignment class step sort-by order)
  (let* ((sort-by (if (sort-by? sort-by) (symbol->string sort-by) user-id))
         (query (merge "SELECT" record-select
                       "FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=? AND"
                               step-id "=? AND"
                               user-id "NOT LIKE \"default-submission%\""
                       "ORDER BY" sort-by (order->string order)))
         (results (run query-rows query assignment class step)))
    (map vector->record results)))

(provide count-step)
(define (count-step assignment class step)
  (let* ((q (merge "SELECT COUNT(*)"
                   "FROM" table
                   "WHERE" assignment-id "=? AND"
                           class-id "=? AND"
                           step-id "=? AND"
                           user-id "NOT LIKE \"default-submission%\""
                   "LIMIT 1"))
         (result (run query-row q assignment class step)))
    (vector-ref result 0)))