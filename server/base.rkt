#lang typed/racket

(require "database/mysql.rkt"
         "configuration.rkt"
         "util/config-file-reader.rkt"
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
  (current-configuration
   (read-conf "/conf/captain-teach.config"))
  (if (init-db?) #t
      (force-initialize))
  (migrate:check-migrated))

(provide force-initialize)
(: force-initialize (-> Void))
(define (force-initialize)
  (current-configuration
   (read-conf "/conf/captain-teach.config"))
  (init-db)
  (class:create (class-name))
  (user:create (master-user))
  (roles:create instructor-role "Instructor" 1)
  (roles:create ta-role "Teaching Assistant" 1)
  (roles:create student-role "Student" 0)
  (role:associate (class-name) (master-user) instructor-role)
  (void))
      