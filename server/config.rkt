#lang racket

(require "database/mysql.rkt")
(require "storage/local.rkt")

(provide ct-port)
(define ct-port 8080)

(provide class-name)
(define class-name "cmpsci220")

(provide upload-submission)
(provide retrieve-submission-file retrieve-file submission-file-path)
(provide sub-directories-of list-files)
(provide is-directory?)
(provide is-file?)
(provide write-file)


;; Define Roles
(provide instructor-role ta-role student-role)
(define instructor-role 0)
(define ta-role 1)
(define student-role 2)
