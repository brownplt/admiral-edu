#lang racket

(require "storage/local.rkt")
(require "database/mysql.rkt")

(provide ct-port)
(define ct-port 8080)

(provide class-name)
(define class-name "umass-cmpsci220")

(provide upload-submission)
(provide retrieve-submission-file retrieve-file submission-file-path)
(provide (all-from-out "storage/local.rkt"))
(provide sub-directories-of list-files)
(provide is-directory?)
(provide is-file?)
(provide write-file)
(provide delete-file)


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
  (if (init-db?) #t
      (begin
        (init-db)
        (class:create class-name)
        (user:create "arjunguha@umass.edu")
        (user:create "jcollard@umass.edu")
        (user:create "shriram@gmail.com")
        (user:create "joe.politz@gmail.com")
        (user:create "joseph_politz@brown.edu")
        (user:create "test@student.edu")
        (user:create "test2@student.edu")
        (roles:create instructor-role "Instructor" 1)
        (roles:create ta-role "Teaching Assistant" 1)
        (roles:create student-role "Student" 0)
        (role:associate class-name "arjunguha@umass.edu" instructor-role)
        (role:associate class-name "jcollard@umass.edu" ta-role)
        (role:associate class-name "shriram@gmail.com" instructor-role)
        (role:associate class-name "joe.politz@gmail.com" instructor-role)
        (role:associate class-name "joseph_politz@brown.edu" student-role)
        (role:associate class-name "test@student.edu" student-role)
        (role:associate class-name "test2@student.edu" student-role))))
      