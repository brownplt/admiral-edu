#lang typed/racket

(require "database/mysql.rkt"
         "configuration.rkt"
         "ct-session.rkt"
         "util/basic-types.rkt")

(provide (all-from-out "configuration.rkt"))
(provide (all-from-out "database/mysql.rkt"))
(provide (all-from-out "ct-session.rkt"))
(provide (all-from-out "util/basic-types.rkt"))



;; Define Roles
(provide instructor-role ta-role student-role)
(define instructor-role 0)
(define ta-role 1)
(define student-role 2)

;; ( -> Result void?)
;; Initializes the database, creating tables and migrating if necessary.
(provide initialize)
(: initialize (-> (Result Void)))
(define (initialize)
  (unless (db-has-a-table?)
    (db-init))
  (migrate:check-migrated))

;; create tables, populate with class, instructor, and roles
(provide db-init)
(: db-init (-> Void))
(define (db-init)
  (db-init-tables)
  (class:create (class-name))
  (user:create (master-user))
  (roles:create instructor-role "Instructor" 1)
  (roles:create ta-role "Teaching Assistant" 1)
  (roles:create student-role "Student" 0)
  (role:associate (class-name) (master-user) instructor-role)
  (void))
      