#lang racket

(require "util/config-file-reader.rkt")

(provide (all-defined-out))

(provide set-db-address!)
(define (set-db-address! address)
  (set! db-address address))

(provide get-db-address)
(define (get-db-address)
  db-address)

(define db-address 'nil)
(define server-name 'nil)
(define sub-domain 'nil)

(define mail-server 'nil)
(define mail-port 'nil)
(define mail-username 'nil)
(define mail-password 'nil)

(define storage-mode 'nil)
(define bucket 'nil)
(define cloud-access-key-id 'nil)
(define cloud-secret-key 'nil)
(define cloud-host 'nil)

(define class-name 'nil)

(define ct-port 'nil)

(define configuration-file "/home/admiral-edu/config")

(let* ((conf (read-conf configuration-file))
       (ref (lambda (key) (hash-ref conf key))))
  (set! db-address (ref "db-address"))
  (set! server-name (ref "server-name"))
  (set! sub-domain (ref "sub-domain"))
  (set! mail-server (ref "mail-server"))
  (set! mail-port (string->number (ref "mail-port")))
  (set! mail-username (ref "mail-username"))
  (set! mail-password (ref "mail-password"))
  (set! class-name (ref "class-name"))
  (set! ct-port (string->number (ref "ct-port")))
  (set! bucket (ref "bucket"))
  (set! cloud-access-key-id (ref "cloud-access-key-id"))
  (set! cloud-secret-key (ref "cloud-secret-key"))
  (set! cloud-host (ref "cloud-host"))
  (set! storage-mode (ref "storage-mode")))

