#lang racket

(require "mysql/common.rkt"
         db
         (prefix-in class: "mysql/class.rkt") 
         (prefix-in user: "mysql/user.rkt")
         (prefix-in role: "mysql/role.rkt")
         (prefix-in roles: "mysql/roles.rkt")
         (prefix-in assignment: "mysql/assignment.rkt")
         (prefix-in submission: "mysql/submission.rkt")
         (prefix-in review: "mysql/review.rkt")
         (prefix-in system: "mysql/system.rkt")
         (prefix-in migrate: "mysql/migrate.rkt"))

;; Initializes the database.
(provide init-db)
(define (init-db)
  (user:init)
  (class:init)
  (role:init)
  (roles:init)
  (assignment:init)
  (submission:init)
  (review:init)
  (system:init))

;; Returns #t if init-db has been called and #f otherwise
(provide init-db?)
(define (init-db?)
  (let* ((conn (make-sql-conn))
         (query (merge "SELECT COUNT(*)"
                       "FROM information_schema.tables "
                       "WHERE table_schema = 'captain_teach'"
                       "AND table_name = ?;"))
         (prep (prepare conn query))
         (result (query-row conn prep review:table))
         (count (vector-ref result 0)))
    (release conn)
    (> count 0)))



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

;; Permanently removes all references to an assignment from the database
(provide database:delete-assignment)
(define (database:delete-assignment class-id assignment-id)
  (review:delete-assignment class-id assignment-id)
  (assignment:delete-assignment class-id assignment-id)
  (submission:delete-assignment class-id assignment-id))