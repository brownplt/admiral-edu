#lang racket

(require "mysql/common.rkt"
         db
         (prefix-in class: "mysql/class.rkt") 
         (prefix-in user: "mysql/user.rkt")
         (prefix-in role: "mysql/role.rkt")
         (prefix-in roles: "mysql/roles.rkt")
         (prefix-in assignment: "mysql/assignment.rkt")
         (prefix-in submission: "mysql/submission.rkt")
         (prefix-in review: "mysql/review.rkt"))

;; Initializes the database.
(provide init-db)
(define (init-db)
  (user:init)
  (class:init)
  (role:init)
  (roles:init)
  (assignment:init)
  (submission:init)
  (review:init))

;; Returns #t if init-db has been called and #f otherwise
(provide init-db?)
(define (init-db?)
  (let* ((query (merge "SELECT COUNT(*)"
                       "FROM information_schema.tables "
                       "WHERE table_schema = 'captain_teach'"
                       "AND table_name = ?;"))
         (prep (prepare (sql-conn) query))
         (result (query-row (sql-conn) prep review:table))
         (count (vector-ref result 0)))
    (> count 0)))


;; User Table
(provide user:all user:create user:exists?)

;; Class Table
(provide class:all class:create class:exists?)

;; Role Table
(provide role:select role:associate role:in-class role:user-uid role:user-class role:user-role role:exists?)

;; Roles Table
(provide roles:create roles:get-role roles:role-id roles:role-name roles:role-can-edit roles:all)

;; Assignment Table
(provide assignment:exists? assignment:create assignment:all assignment:list
         assignment:open assignment:close assignment:mark-ready assignment:mark-not-ready
         assignment:record-class assignment:record-id assignment:record-open assignment:record-ready
         assignment:record?)

;; Submission Table
(provide submission:table 
         submission:record submission:record?
         submission:assignment-id submission:assignment-id-type submission:record-assignment
         submission:class-id submission:class-id-type submission:record-class
         submission:step-id submission:step-id-type submission:record-step
         submission:user-id submission:user-id-type submission:record-user
         submission:time-stamp submission:time-stamp-type submission:record-time-stamp
         submission:create submission:list submission:count
         submission:exists?
         submission:create-instructor-solution
         submission:times-reviewed)


;; Review Table
(provide review:table
         review:create
         review:record review:record? review:record-class-id 
         review:record-assignment-id review:record-step-id review:record-reviewee-id 
         review:record-reviewer-id review:record-completed review:record-review-id
         review:record-hash
         review:assignment-id review:assignment-id-type
         review:step-id review:step-id-type
         review:class-id review:class-id-type
         review:reviewee-id review:reviewee-id-type
         review:reviewer-id review:reviewer-id-type
         review:time-stamp review:time-stamp-type
         review:completed? review:count-completed
         review:select-by-hash review:select-reviews
         review:assign-student-reviews
         review:assign-instructor-solution
         review:select-reviews
         review:select-assigned-reviews
         review:mark-complete
         review:hash
         review:select-feedback)
