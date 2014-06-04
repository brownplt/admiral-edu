#lang racket

(require "../config.rkt"
         "../database/mysql.rkt")

;; Initializes the database.
;; Creates a class called cmpsci220 with two users: arjunguha and jcollard. 
;; Creates three roles: Instructor, Teaching Assistant, and Student. Instructors
;;   and teaching assistants have the can-edit field set to true. Students have
;;   the can-edit field set to false.
;; Create two role associations for cmpsci220: arjunguha/instructor, jcollard/ta
(provide initialize)
(define (initialize)
  (init-db)
  (class:create "cmpsci220")
  (user:create "arjunguha@umass.edu")
  (user:create "jcollard@umass.edu")
  (roles:create instructor-role "Instructor" 1)
  (roles:create ta-role "Teaching Assistant" 1)
  (roles:create student-role "Student" 0)
  (role:associate "cmpsci220" "arjunguha@umass.edu" instructor-role)
  (role:associate "cmpsci220" "jcollard@umass.edu" ta-role)
  (assignment:create "clock" "cmpsci220"))