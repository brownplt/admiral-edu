#lang racket

(require "../database/mysql.rkt")

;; Define Roles
(provide instructor-role ta-role student-role)
(define instructor-role 0)
(define ta-role 1)
(define student-role 2)

;; Initializes the database.
;; Creates a class called cmpsci220 with two users: arjunguha and jcollard. 
;; Creates three roles: Instructor, Teaching Assistant, and Student. Instructors
;;   and teaching assistants have the can-edit field set to true. Students have
;;   the can-edit field set to false.
;; Create two role associations for cmpsci220: arjunguha/instructor, jcollard/ta
(provide initialize)
(define (initialize)
  (init-db)
  (create-class "cmpsci220")
  (create-user "arjunguha")
  (create-user "jcollard")
  (create-role instructor-role "Instructor" 1)
  (create-role ta-role "Teaching Assistant" 1)
  (create-role student-role "Student" 0)
  (associate-role "cmpsci220" "arjunguha" instructor-role)
  (associate-role "cmpsci220" "jcollard" ta-role))