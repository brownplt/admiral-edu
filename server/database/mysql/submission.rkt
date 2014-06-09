#lang racket

(require db
         "common.rkt"
         (prefix-in class: "class.rkt")
         (prefix-in assignment: "assignment.rkt")
         (prefix-in user: "user.rkt")
         (prefix-in role: "role.rkt"))

;; Submission Table
(provide table)
(define table "submission")

(provide version version-type)
(define version "version")
(define version-type "SMALLINT UNSIGNED")

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

(provide time-stamp time-stamp-type)
(define time-stamp "time_stamp")
(define time-stamp-type "TIMESTAMP")

;; Initializes the assignment table.
(provide init)
(define (init)
  (let ((drop (prepare sql-conn (merge "DROP TABLE IF EXISTS" table)))
        (create (prepare sql-conn (merge "CREATE TABLE" table "(" 
                                         assignment-id assignment-id-type ","
                                         class-id class-id-type ","
                                         step-id step-id-type ","
                                         user-id user-id-type ","
                                         version version-type ","
                                         time-stamp time-stamp-type ","
                                         "PRIMARY KEY (" assignment-id "," class-id "," step-id "," user-id "," version "))"))))
    (query-exec sql-conn drop)
    (query-exec sql-conn create)))


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
     (let* ((version (count assignment class step user))
            (query (merge "INSERT INTO" table " VALUES(?,?,?,?,?,NOW())"))
            (prep (prepare sql-conn query)))
       (query-exec sql-conn prep assignment class step user version)
       #t)]))

(provide record record? record-assignment record-class record-step record-user record-time-stamp record-version)
(struct record (version assignment class step user time-stamp) #:transparent)

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
     (let* ((query (merge "SELECT" time-stamp "," version
                          "FROM" table 
                          "WHERE" assignment-id "=? AND"
                                  class-id "=? AND"
                                  step-id "=? AND"
                                  user-id "=?"
                          "ORDER BY" version "DESC"))
            (prep (prepare sql-conn query))
            (result (query-rows sql-conn prep assignment class step user))
            (to-record (lambda (vec) (record (vector-ref vec 1) assignment class step user (vector-ref vec 0)))))
       (map to-record result))]))

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
     (let* ((query (merge "SELECT COUNT(*)"
                          "FROM" table 
                          "WHERE" assignment-id "=? AND"
                                  class-id "=? AND"
                                  step-id "=? AND"
                                  user-id "=?"
                          "LIMIT 1"))
            (prep (prepare sql-conn query))
            (result (vector-ref (query-row sql-conn prep assignment class step user) 0)))
       result)]))

(provide exists?)
(define (exists? assignment class step user version)
         (let* ((query (merge "SELECT COUNT(*)"
                              "FROM" table
                              "WHERE" assignment-id "=? AND"
                                      class-id "=? AND"
                                      step-id "=? AND"
                                      user-id "=? AND"
                                      version "=?"
                              "LIMIT 1"))
                (prep (prepare sql-conn query))
                (result (vector-ref (query-row sql-conn prep assignment class step user version) 0)))
           (> result 0)))