#lang racket

(require "database/mysql.rkt")

(provide ct-port)
(define ct-port 8080)

(provide class-name)
(define class-name "cmpsci220")

;; Define Roles
(provide instructor-role ta-role student-role)
(define instructor-role 0)
(define ta-role 1)
(define student-role 2)
