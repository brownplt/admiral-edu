#lang racket

(require "../database/mysql.rkt")

(provide double-quotes)
(define (double-quotes str)
  (string-append (string #\") str (string #\")))

(provide instructor-role ta-role student-role)
(define instructor-role 0)
(define ta-role 1)
(define student-role 2)

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