#lang typed/racket

(require "typed-db.rkt"
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

(provide published published-type)
(define published "published")
(define published-type "BOOL")

(provide last-modified last-modified-type)
(define last-modified "last_modified")
(define last-modified-type "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")

(define valid-columns `(,class-id ,step-id ,user-id ,time-stamp ,times-reviewed ,published))

; ct-session -> (U 'class_id 'step_id 'user_id 'time_stamp 'times_reviewed)
(provide get-sort-by)
(: get-sort-by (ct-session -> Symbol))
(define get-sort-by (common:get-sort-by valid-columns 'user_id))

(provide sort-by?)
(: sort-by? (Symbol -> Boolean))
(define sort-by? (common:sort-by? valid-columns))

;; Initializes the assignment table.
(provide init)
(define (init)
  (let* ((drop (merge "DROP TABLE IF EXISTS" table))
         (create (merge "CREATE TABLE" table "(" 
                                         assignment-id assignment-id-type ","
                                         class-id class-id-type ","
                                         step-id step-id-type ","
                                         user-id user-id-type ","
                                         time-stamp time-stamp-type ","
                                         times-reviewed times-reviewed-type ","
                                         published published-type ","
                                         last-modified last-modified-type ","
                                         "PRIMARY KEY (" assignment-id "," class-id "," step-id "," user-id "))")))
    (query-exec drop)
    (query-exec create)))

(provide(struct-out Record))
(struct Record ([assignment : String] [class : String] [step : String] [user : String] [time-stamp : TimeStamp] [published : Boolean] [last-modified : TimeStamp]) #:transparent)

(define record-select (merge assignment-id "," class-id "," step-id "," user-id "," time-stamp "," published "," last-modified))
(define-type Vector-Record (Vector String String String String TimeStamp Integer TimeStamp))

(: vector->record (Vector-Record -> Record))
(define (vector->record vec)
  (let ((assignment (vector-ref vec 0))
        (class (vector-ref vec 1))
        (step (vector-ref vec 2))
        (user (vector-ref vec 3))
        (time-stamp (vector-ref vec 4))
        (published (= 1 (vector-ref vec 5)))
        (last-modified (vector-ref vec 6)))
    (Record assignment class step user time-stamp published last-modified)))

;;TODO Add Instructor-solution field and mark true
(provide create-instructor-solution)
(: create-instructor-solution (String String String String -> Void))
(define (create-instructor-solution assignment class step user)
  (let ((query (merge "INSERT INTO" table " VALUES(?,?,?,?,NOW(),9001,true,NOW())")))
    (query-exec query assignment class step user)))

;; Creates a record for the specified assignment, class, step, and user.
;; This function returns one of the following:
;; #t - If successful, a timestamp and version number are generated and returns #t
;; 'no-such-class - if the specified class doesn't exist
;; 'no-such-assignment - if the specified class doesn't exist for the class
;; 'no-such-user - if the specified user does not exist
;; 'no-such-user-in-class - if the specified user exists but is not registered for the class
(provide create)
(: create (String String String String -> (U #t 'no-such-class 'no-such-assignment 'no-such-user 'no-such-user-in-class)))
(define (create assignment class step user)
  (cond
    [(not (class:exists? class)) 'no-such-class]
    [(not (assignment:exists? assignment class)) 'no-such-assignment]
    [(not (user:exists? user)) 'no-such-user]
    [(not (role:exists? class user)) 'no-such-user-in-class]
    [else
     (let ((query (merge "INSERT INTO" table " VALUES(?,?,?,?,NOW(),0,false,NOW())")))
       (query-exec query assignment class step user)
       #t)]))

;; Updates the record for the assignment, class, step, and user such that it is
;; marked as "published".
(provide publish)
(: publish (String String String String -> Void))
(define (publish assignment class step user)
  (let ((query (merge "UPDATE" table
                      "SET" published "=true"
                      "WHERE" assignment-id "=? AND"
                              class-id "=? AND"
                              step-id "=? AND"
                              user-id "=?")))
    (query-exec query assignment class step user)))

(provide unpublish)
(: unpublish (String String String String -> Void))
(define (unpublish assignment class step user)
  (let ((query (merge "UPDATE" table
                      "SET" published "=false"
                      "WHERE" assignment-id "=? AND"
                              class-id "=? AND"
                              step-id "=? AND"
                              user-id "=?")))
    (query-exec query assignment class step user)))

;; Given an assignment, class, step, and user, lists all entries ordered by
;; their version number
;; This function returns one of the following:
;; listof record? - If successful, a timestamp and version number are generated and returns #t
;; 'no-such-class - if the specified class doesn't exist
;; 'no-such-assignment - if the specified class doesn't exist for the class
;; 'no-such-user - if the specified user does not exist
;; 'no-such-user-in-class - if the specified user exists but is not registered for the class
(provide list)
(: list (String String String String -> (U (Listof Record) 'no-such-class 'no-such-assignment 'no-such-user 'no-such-user-in-class)))
(define (list assignment class step user)
  (cond
    [(not (class:exists? class)) 'no-such-class]
    [(not (assignment:exists? assignment class)) 'no-such-assignment]
    [(not (user:exists? user)) 'no-such-user]
    [(not (role:exists? class user)) 'no-such-user-in-class]
    [else
     (let* ((query (merge "SELECT" record-select
                          "FROM" table 
                          "WHERE" assignment-id "=? AND"
                                  class-id "=? AND"
                                  step-id "=? AND"
                                  user-id "=?"
                          "ORDER BY" time-stamp "DESC"))
            (result (cast (query-rows query assignment class step user) (Listof Vector-Record))))
       (map vector->record result))]))

(provide reviewed)
(: reviewed (String String String String -> Exact-Nonnegative-Integer))
(define (reviewed assignment class step user)
  (let* ((query (merge "SELECT" times-reviewed
                       "FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=? AND"
                               step-id "=? AND"
                               user-id "=? LIMIT 1"))
         (result (query-row query assignment class step user))
         (reviewed-times (vector-ref result 0)))
    (cast reviewed-times Exact-Nonnegative-Integer)))

;; TODO: Need to have a Transaction here
(provide increment-reviewed)
(: increment-reviewed (String String String String -> Void))
(define (increment-reviewed assignment class step user)
  (let* ((current (reviewed assignment class step user))
         (plusOne (+ current 1))
         (query (merge "UPDATE" table
                       "SET" times-reviewed "=?"
                       "WHERE" assignment-id "=? AND"
                               class-id "=? AND"
                               step-id "=? AND"
                               user-id "=?")))
    (query-exec query plusOne assignment class step user)))

(provide select-least-reviewed)
(: select-least-reviewed (String String String (Listof String) -> String))
(define (select-least-reviewed assignment class step not-users)
  (let* ((user-commas (string-join (build-list (length not-users) (lambda (n) "?")) ","))
         (query (merge "SELECT" user-id
                       "FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=? AND"
                               step-id "=? AND"
                               user-id " NOT IN (" user-commas ") AND"
                               published "=true"
                       "ORDER BY" times-reviewed "ASC"
                       "LIMIT 1"))
         (arg-list (append `(,assignment ,class ,step) not-users))
         (result (query-row-list query arg-list))
         (id (vector-ref result 0)))
    (cast id String)))
         
;; Given an assignment, class, step, and user, returns the number of entries that have been created
;; This function returns one of the following:
;; number? - If successful, a timestamp and version number are generated and returns #t
;; 'no-such-class - if the specified class doesn't exist
;; 'no-such-assignment - if the specified class doesn't exist for the class
;; 'no-such-user - if the specified user does not exist
;; 'no-such-user-in-class - if the specified user exists but is not registered for the class
(provide count)
(: count (String String String String -> (U Exact-Nonnegative-Integer 'no-such-class 'no-such-assignment 'no-such-user 'no-such-user-in-class)))
(define (count assignment class step user)
  (cond
    [(not (class:exists? class)) 'no-such-class]
    [(not (assignment:exists? assignment class)) 'no-such-assignment]
    [(not (user:exists? user)) 'no-such-user]
    [(not (role:exists? class user)) 'no-such-user-in-class]
    [else
     (let* ((query (merge "SELECT COUNT(*)"
                          "FROM" table 
                          "WHERE" assignment-id "=? AND"
                                  class-id "=? AND"
                                  step-id "=? AND"
                                  user-id "=?"
                          "LIMIT 1"))
            (result (query-value query assignment class step user)))
       (cast result Exact-Nonnegative-Integer))]))

(provide exists?)
(: exists? (String String String String -> Boolean))
(define (exists? assignment class step user)
         (let* ((query (merge "SELECT COUNT(*)"
                              "FROM" table
                              "WHERE" assignment-id "=? AND"
                                      class-id "=? AND"
                                      step-id "=? AND"
                                      user-id "=?"
                              "LIMIT 1"))
                (result (query-value query assignment class step user)))
           (> (cast result Exact-Nonnegative-Integer) 0)))

(provide has-submitted?)
(: has-submitted? (String String String -> Boolean))
(define (has-submitted? assignment class user)
  (let* ((q (merge "SELECT COUNT(*)"
                   "FROM" table
                   "WHERE" assignment-id "=? AND"
                           class-id "=? AND"
                           user-id "=?"
                   "LIMIT 1"))
         (result (query-row q assignment class user))
         (value (vector-ref result 0)))
    (> (cast value Exact-Nonnegative-Integer) 0)))

(provide delete-assignment)
(: delete-assignment (String String -> Void))
(define (delete-assignment class assignment)
  (let ((query (merge "DELETE FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=?")))
    (query-exec query assignment class)))

(provide select-all)
(: select-all (String String String Symbol (U 'asc 'desc) -> (Listof Record)))
(define (select-all assignment class step sort-by order)
  (let* ((sort-by (if (sort-by? sort-by) (symbol->string sort-by) user-id))
         (query (merge "SELECT" record-select
                       "FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=? AND"
                               step-id "=? AND"
                               user-id "NOT LIKE \"default-submission%\""
                       "ORDER BY" sort-by (order->string order)))
         (results (query-rows query assignment class step)))
    (map vector->record (cast results (Listof Vector-Record)))))

(provide published?)
(: published? (String String String String -> Boolean))
(define (published? assignment class step uid)
  (let* ((q (merge "SELECT" published
                   "FROM" table
                   "WHERE" assignment-id "=? AND"
                           class-id "=? AND"
                           step-id "=? AND"
                           user-id "=?"
                   "LIMIT 1"))
         (result (cast (query-value q assignment class step uid) Integer)))
    (= 1 result)))

(provide count-step)
(: count-step (String String String -> Exact-Nonnegative-Integer))
(define (count-step assignment class step)
  (let* ((q (merge "SELECT COUNT(*)"
                   "FROM" table
                   "WHERE" assignment-id "=? AND"
                           class-id "=? AND"
                           step-id "=? AND"
                           user-id "NOT LIKE \"default-submission%\""
                   "LIMIT 1"))
         (result (query-value q assignment class step)))
    (cast result Exact-Nonnegative-Integer)))

(provide select-from-assignment)
(: select-from-assignment (String String String -> (Listof Record)))
(define (select-from-assignment assignment class uid)
  (let* ((q (merge "SELECT" record-select
                   "FROM" table
                   "WHERE" assignment-id "=? AND"
                           class-id "=? AND"
                           user-id "=?"
                   "ORDER BY" time-stamp "DESC"))
         (result (cast (query-rows q assignment class uid) (Listof Vector-Record))))
    (map vector->record result)))

(provide select)
(: select (String String String String -> Record))
(define (select assignment class step uid)
  (let* ((q (merge "SELECT" record-select
                   "FROM" table
                   "WHERE" assignment-id "=? AND"
                           class-id "=? AND"
                           step-id "=? AND"
                           user-id "=?"
                   "LIMIT 1"))
         (result (cast (query-row q assignment class step uid) Vector-Record)))
    (vector->record result)))
