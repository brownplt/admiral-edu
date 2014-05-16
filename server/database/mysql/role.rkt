#lang racket

(require db
         "common.rkt"
         "user.rkt"
         "class.rkt")

;; The role table defines a relation between users and classes
(provide role-table role-table-class-id role-table-user-uid role-table-role)
(define role-table "role")
(define role-table-class-id "class_id")
(define role-table-class-id-type class-table-id-type)

(define role-table-user-uid "user_uid")
(define role-table-user-uid-type user-table-uid-type)

(define role-table-role "role")
(define role-table-role-type "INT(4)")

;; Initializes the role table.
(provide init-role-table)
(define (init-role-table sql-conn)
  (let ((drop (prepare sql-conn (merge "DROP TABLE IF EXISTS" role-table)))
        (create (prepare sql-conn (merge "CREATE TABLE" role-table "(" 
                                         role-table-class-id role-table-class-id-type ","
                                         role-table-user-uid role-table-user-uid-type ","
                                         role-table-role role-table-role-type ","
                                         "PRIMARY KEY (" role-table-class-id "," role-table-user-uid "))"))))
    (query-exec sql-conn drop)
    (query-exec sql-conn create)))

;; Retrieve a role for a class/user
(provide internal-select-role)
(define (internal-select-role sql-conn class uid)
  (let ((query (prepare sql-conn (merge "SELECT" role-table-role "FROM" role-table "WHERE" role-table-class-id "=? AND" role-table-user-uid "=? LIMIT 1"))))
    (query-rows sql-conn query class uid)))

;; Creates a record in the role table
(provide internal-create-role)
(define (internal-create-role sql-conn class uid instructor)
  (let ((create (prepare sql-conn (merge "INSERT INTO" role-table "values(?,?,?)"))))
    (query-exec sql-conn create class uid instructor)))

;; Retrieve all students for a class
(provide internal-select-students-in-class)
(define (internal-select-students-in-class sql-conn class limit page)
  (let* ((lower (* limit page))
         (upper (+ lower limit))
         (query (merge "SELECT" role-table-user-uid "FROM" role-table "WHERE" role-table-class-id "=? AND" role-table-role "=0 LIMIT ?, ?"))
         (prepped (prepare sql-conn query))
         (to-record (lambda (result) (user-record (vector-ref result 0) class 0))))
    (map to-record (query-rows sql-conn query class lower upper))))

(provide internal-select-instructors-in-class)
(define (internal-select-instructors-in-class sql-conn class limit page)
  (let* ((lower (* limit page))
         (upper (+ lower limit))
         (query (merge "SELECT" role-table-user-uid "FROM" role-table "WHERE" role-table-class-id "=? AND" role-table-role "=1 LIMIT ?, ?"))
         (prepped (prepare sql-conn query))
         (to-record (lambda (result) (user-record (vector-ref result 0) class 0))))
    (map to-record (query-rows sql-conn query class lower upper))))

(provide internal-select-tas-in-class)
(define (internal-select-tas-in-class sql-conn class limit page)
  (let* ((lower (* limit page))
         (upper (+ lower limit))
         (query (merge "SELECT" role-table-user-uid "FROM" role-table "WHERE" role-table-class-id "=? AND" role-table-role "=2 LIMIT ?, ?"))
         (prepped (prepare sql-conn query))
         (to-record (lambda (result) (user-record (vector-ref result 0) class 0))))
    (map to-record (query-rows sql-conn query class lower upper))))

(provide user-record user-record-uid user-record-class user-record-role)
(struct user-record (uid class role) #:transparent)

;; (define (select-students-in-class class limit page) (internal-select-students-in-class class limit page))