#lang racket

(require "mysql/common.rkt"
         (prefix-in class: "mysql/class.rkt") 
         (prefix-in user: "mysql/user.rkt")
         (prefix-in role: "mysql/role.rkt")
         (prefix-in roles: "mysql/roles.rkt")
         (prefix-in assignment: "mysql/assignment.rkt")
         (prefix-in submission: "mysql/submission.rkt"))

;; Initializes the database.
(provide init-db)
(define (init-db)
  (user:init)
  (class:init)
  (role:init)
  (roles:init)
  (assignment:init)
  (submission:init))


;; User Table
(provide user:all user:create user:exists?)

;; Class Table
(provide class:all class:create class:exists?)

;; Role Table
(provide role:select role:associate role:in-class role:user-uid role:user-class role:user-role role:exists?)

;; Roles Table
(provide roles:create roles:get-role roles:role-id roles:role-name roles:role-can-edit roles:all)

;; Assignment Table
(provide assignment:exists? assignment:create assignment:all assignment:list)