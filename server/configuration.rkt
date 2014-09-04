#lang racket

(provide (all-defined-out))

(provide set-db-address!)
(define (set-db-address! address)
  (set! db-address address))

(provide get-db-address)
(define (get-db-address)
  db-address)

(define db-address "173.194.254.129")
(define s3-keys "/home/admiral-edu/.cloud-keys")

;; File containing:
;; username=XXXX
;; password=XXX
(define server-name "captain-teach.org")
(define sub-domain "www.")
(define smtp:credentials-file "/home/admiral-edu/.smtp-credentials")
(define smtp:server-address "email-smtp.us-east-1.amazonaws.com")
(define smtp:port 465)
(define smtp:tls #t)

(define bucket "test-class/")
(define class-name "test-class")
(define ct-port 8080)
