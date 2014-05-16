#lang racket

(require db
         "mysql/common.rkt"
         "mysql/class.rkt" 
         "mysql/user.rkt"
         "mysql/role.rkt") 

;; Database Information
(define username "captain_teach")
(define password "captain_teach")
(define database "captain_teach")


(define sql-conn
  (mysql-connect #:user username
                 #:database password
                 #:password database))

;; Initializes the database.
(provide init-db)
(define (init-db)
  (init-user-table sql-conn)
  (init-class-table sql-conn)
  (init-role-table sql-conn))

;; User Table
(provide select-users create-user)
(define (select-users) (internal-select-users sql-conn))
(define (create-user uid) (internal-create-user sql-conn uid))


;; Class Table
(provide select-classes create-class)
(define (select-classes) (internal-select-classes sql-conn))
(define (create-class id) (internal-create-class sql-conn id))

;; Role Table
(provide select-role create-role)
(define (select-role class uid) (internal-select-role sql-conn class uid))
(define (create-role class uid instructor) (internal-create-role sql-conn class uid instructor))
