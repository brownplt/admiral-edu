#lang racket

(require db
         "common.rkt"
         (prefix-in user: "user.rkt")
         (prefix-in class: "class.rkt"))

;; The role table defines a relation between users and classes
(provide table class-id-column class-id-type user-id-column user-id-type role-table-role)
(define table "role")
(define class-id-column "class_id")
(define class-id-type class:id-type)

(define user-id-column "user_uid")
(define user-id-type user:uid-type)

(define role-table-role "role")
(define role-table-role-type "INT(4)")

;; Initializes the role table.
(provide init)
(define (init sql-conn)
  (let* ((drop-query (merge "DROP TABLE IF EXISTS" table))
         (drop (prepare sql-conn drop-query))
         (create-query (merge "CREATE TABLE" table "(" 
                                         class-id-column class-id-type ","
                                         user-id-column user-id-type ","
                                         role-table-role role-table-role-type ","
                                         "PRIMARY KEY (" class-id-column "," user-id-column "))"))
         (create (prepare sql-conn create-query)))
    (query-exec sql-conn drop)
    (query-exec sql-conn create)))

;; Retrieve a role for a class/user
(provide select-role)
(define (select-role sql-conn class uid)
  (let ((query (prepare sql-conn (merge "SELECT" role-table-role "FROM" table "WHERE" class-id-column "=? AND" user-id-column "=? LIMIT 1"))))
    (query-rows sql-conn query class uid)))

;; Creates a record in the role table
(provide associate-role)
(define (associate-role sql-conn class uid instructor)
  (let ((create (prepare sql-conn (merge "INSERT INTO" table "values(?,?,?)"))))
    (query-exec sql-conn create class uid instructor)))

;; Retrieve all students for a class
(provide select-users-in-class)
(define (select-users-in-class sql-conn class role limit page)
  (let* ((lower (* limit page))
         (upper (+ lower limit))
         (query (merge "SELECT" user-id-column "FROM" table "WHERE" class-id-column "=? AND" role-table-role "=? LIMIT ?, ?"))
         (prepped (prepare sql-conn query))
         (to-record (lambda (result) (user-record (vector-ref result 0) class 0))))
    (map to-record (query-rows sql-conn query class role lower upper))))

(provide user-record user-record-uid user-record-class user-record-role)
(struct user-record (uid class role) #:transparent)

;; (define (select-students-in-class class limit page) (internal-select-students-in-class class limit page))