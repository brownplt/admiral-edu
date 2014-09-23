#lang typed/racket

(require "util/config-file-reader.rkt")

(provide (all-defined-out))

(: set-db-address! (String -> Void))
(define (set-db-address! address)
  (set! db-address address))

(: db-address (U 'nil String))
(define db-address 'nil)

(: get-db-address (-> (U 'nil String)))
(define (get-db-address)
  db-address)

(: db-user-name (U 'nil String))
(define db-user-name 'nil)

(: db-password (U 'nil String))
(define db-password 'nil)

(: db-name (U 'nil String))
(define db-name 'nil)

(: server-name (U 'nil String))
(define server-name 'nil)

(: sub-domain (U 'nil String))
(define sub-domain 'nil)

(: mail-server (U 'nil String))
(define mail-server 'nil)

(: mail-port (U 'nil Integer))
(define mail-port 'nil)

(: mail-username (U 'nil String))
(define mail-username 'nil)

(: mail-password (U 'nil String))
(define mail-password 'nil)

(: storage-mode (U 'nil String))
(define storage-mode 'nil)

(: bucket (U 'nil String))
(define bucket 'nil)

(: cloud-access-key-id (U 'nil String))
(define cloud-access-key-id 'nil)

(: cloud-secret-key (U 'nil String))
(define cloud-secret-key 'nil)

(: cloud-host (U 'nil String))
(define cloud-host 'nil)

(: class-name String)
(define class-name "")

(: ct-port (U 'nil Integer))
(define ct-port 'nil)

(: configuration-file Path-String)
(define configuration-file "/home/admiral-edu/config")


(let*: ([conf : (HashTable String String) (read-conf configuration-file)]
        [ref : (String -> String) (lambda (key) (hash-ref conf key))])
  (set! db-address (ref "db-address"))
  (set! server-name (ref "server-name"))
  (set! sub-domain (ref "sub-domain"))
  (set! mail-server (ref "mail-server"))
  (set! mail-port (assert (string->number (ref "mail-port")) exact-integer?))
  (set! mail-username (ref "mail-username"))
  (set! mail-password (ref "mail-password"))
  (set! class-name (ref "class-name"))
  (set! ct-port (assert (string->number (ref "ct-port")) exact-integer?))
  (set! bucket (ref "bucket"))
  (set! cloud-access-key-id (ref "cloud-access-key-id"))
  (set! cloud-secret-key (ref "cloud-secret-key"))
  (set! cloud-host (ref "cloud-host"))
  (set! storage-mode (ref "storage-mode"))
  (set! db-name (ref "db-name"))
  (set! db-user-name (ref "db-user-name"))
  (set! db-password (ref "db-password")))

