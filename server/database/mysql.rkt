#lang typed/racket

(require "mysql/typed-db.rkt"
         (prefix-in class: "mysql/class.rkt") 
         (prefix-in user: "mysql/user.rkt")
         (prefix-in role: "mysql/role.rkt")
         (prefix-in roles: "mysql/roles.rkt")
         (prefix-in assignment: "mysql/assignment.rkt")
         (prefix-in submission: "mysql/submission.rkt")
         (prefix-in review: "mysql/review.rkt")
         (prefix-in system: "mysql/system.rkt")
         (prefix-in migrate: "mysql/migrate.rkt"))

;; User Table
(provide (all-from-out "mysql/user.rkt"))

;; Class Table
(provide (all-from-out "mysql/class.rkt"))

;; Role Table
(provide (all-from-out "mysql/role.rkt"))

;; Roles Table
(provide (all-from-out "mysql/roles.rkt"))

;; Assignment Table
(provide (all-from-out "mysql/assignment.rkt"))

;; Submission Table
(provide (all-from-out "mysql/submission.rkt"))

;; Review Table
(provide (all-from-out "mysql/review.rkt"))

;; System Table
(provide (all-from-out "mysql/system.rkt"))

;; Migration functions
(provide (all-from-out "mysql/migrate.rkt"))


;; Drops and re-creates tables
(provide db-init-tables)
(: db-init-tables (-> Void))
(define (db-init-tables)
  (user:init)
  (class:init)
  (role:init)
  (roles:init)
  (assignment:init)
  (submission:init)
  (review:init)
  (system:init))

;; is there at least one table in the captain_teach database?
;; (this isn't really a good way to check whether it's been
;; initialized)
(provide db-has-a-table?)
(: db-has-a-table? (-> Boolean))
(define (db-has-a-table?)
  (let* ((query (merge "SELECT COUNT(*)"
                       "FROM information_schema.tables "
                       "WHERE table_schema = 'captain_teach'"
                       "AND table_name = ?;"))
         (result (query-value query review:table)))
    (> (cast result Natural) 0)))

;; Permanently removes all references to an assignment from the database
(provide database:delete-assignment)
(: database:delete-assignment (String String -> Void))
(define (database:delete-assignment class-id assignment-id)
  (review:delete-assignment class-id assignment-id)
  (assignment:delete-assignment class-id assignment-id)
  (submission:delete-assignment class-id assignment-id))
