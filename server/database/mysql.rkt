#lang racket

(require db
         "mysql/common.rkt"
         (prefix-in class: "mysql/class.rkt") 
         (prefix-in user: "mysql/user.rkt")
         (prefix-in role: "mysql/role.rkt")
         (prefix-in roles: "mysql/roles.rkt"))

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
  (user:init sql-conn)
  (class:init sql-conn)
  (role:init sql-conn)
  (roles:init sql-conn))

;; User Table
(provide select-users create-user exists-user?)
(define (select-users) (user:select-users sql-conn))
(define (create-user uid) (user:create-user sql-conn uid))
(define (exists-user? uid) (user:exists-user? sql-conn uid))

;; Class Table
(provide select-classes create-class)
(define (select-classes) (class:select-classes sql-conn))
(define (create-class id) (class:create-class sql-conn id))

;; Role Table
(provide select-role associate-role select-users-in-class user-record-uid user-record-class user-record-role)
(define (user-record-uid record) (role:user-record-uid record))
(define (user-record-class record) (role:user-record-class record))
(define (user-record-role record) (role:user-record-class record))
(define (select-role class uid) (role:select-role sql-conn class uid))
(define (associate-role class uid instructor) (role:associate-role sql-conn class uid instructor))
(define (select-users-in-class class role limit page) (role:select-users-in-class sql-conn class role limit page))

;; Roles Table
(provide create-role get-role-record role-record-id role-record-role role-record-can-edit all-roles)
(define (role-record-id record) (roles:role-record-id record))
(define (role-record-can-edit record) (roles:role-record-can-edit record))
(define (role-record-role record) (roles:role-record-role record))
(define (create-role id role can-edit) (roles:create-role sql-conn id role can-edit))
(define (get-role-record id) (roles:get-role-record sql-conn id))
(define (all-roles) (roles:all-roles sql-conn))