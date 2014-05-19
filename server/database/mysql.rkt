#lang racket

(require db
         "mysql/common.rkt"
         "mysql/class.rkt" 
         "mysql/user.rkt"
         "mysql/role.rkt"
         "mysql/roles.rkt") 

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
  (init-role-table sql-conn)
  (init-roles-table sql-conn))

;; User Table
(provide select-users create-user exists-user)
(define (select-users) (internal-select-users sql-conn))
(define (create-user uid) (internal-create-user sql-conn uid))
(define (exists-user uid) (internal-exists-user sql-conn uid))

;; Class Table
(provide select-classes create-class)
(define (select-classes) (internal-select-classes sql-conn))
(define (create-class id) (internal-create-class sql-conn id))

;; Role Table
(provide select-role associate-role select-users-in-class user-record user-record-uid user-record-uid user-record-role)
(define (select-role class uid) (internal-select-role sql-conn class uid))
(define (associate-role class uid instructor) (internal-associate-role sql-conn class uid instructor))
(define (select-users-in-class class role limit page) (internal-select-users-in-class sql-conn class role limit page))

;; Roles Table
(provide create-role get-role-record role-record role-record-id role-record-role role-record-can-edit all-roles)
(define (create-role id role can-edit) (internal-create-role sql-conn id role can-edit))
(define (get-role-record id) (internal-get-role-record sql-conn id))
(define (all-roles) (internal-all-roles sql-conn))