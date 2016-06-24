#lang typed/racket

(require "util/config-file-reader.rkt")

(provide (all-defined-out))


;; a configuration is a hash table with the required fields
;; (specified below)
(: current-configuration (Parameterof (HashTable String String)))
(define current-configuration
  (make-parameter (ann (hash) (HashTable String String))
                  (λ ([t : (HashTable String String)])
                    (check-conf-hash t))))

;; given a spec (sadly must appear below macro definition),
;; define a structure containing the specified fields,
;; and a function to read those values from a given file.
(define-syntax define-configuration-file
  (syntax-rules ()
    [(_ checker [field ty] ...)
     (begin
       (begin
         (: field (-> ty))
         (define (field)
           (maybe-convert
            (hash-ref (current-configuration)
                      (symbol->string (quote field)))
            ty)))
       ...
       (: checker ((HashTable String String) -> (HashTable String String)))
       (define (checker table)
         (let ([field-str (symbol->string (quote field))])
           (unless (hash-has-key? table field-str)
             (error 'check-conf-hash
                    "configuration missing required field: ~v"
                    field-str))
           (define field-val (hash-ref table field-str))
           (unless (check-ty field-val ty)
             (error 'check-configuration
                    "expected value for key ~v convertible to ~a, got: ~v"
                    field-str (quote ty) field-val)))
         ...
         table))]))

(define-syntax maybe-convert
  (syntax-rules (String Natural)
    [(_ v String) v]
    [(_ v Natural) (cast (string->number v) Natural)]))

(define-syntax check-ty
  (syntax-rules (String Natural)
    [(_ v String) #t]
    [(_ v Natural) (exact-nonnegative-integer?
                    (string->number v))]))

(define-configuration-file
  check-conf-hash
  [db-address String]
  [db-user-name String]
  [db-password String]
  [db-name String]
  [server-name String]
  [sub-domain String]
  [mail-server String]
  [mail-port Natural]
  [mail-username String]
  [mail-password String]
  [storage-mode String]
  [bucket String]
  [cloud-access-key-id String]
  [cloud-secret-key String]
  [cloud-host String]
  [class-name String]
  [ct-port Natural]
  [master-user String])


(module+ test
  (require typed/rackunit
           racket/runtime-path)

  
  
  ;; a simple regression test: make sure we can parse
  ;; the given example configuration. If this test gets
  ;; out of sync, just paste the new value in.
  (define-runtime-path example-conf "../conf/")

  (define test-conf
    '#hash(("ct-port" . "8080")
           ("mail-port" . "2525")
           ("mail-server" . "smtp.sendgrid.net")
           ("server-name" . "yoursite.com")
           ("cloud-access-key-id" . "YOUR-ACCESS-ID")
           ("cloud-host" . "storage.googleapis.com")
           ("mail-username" . "username")
           ("bucket" . "some-bucket-name/")
           ("class-name" . "test-class")
           ("storage-mode" . "cloud-storage")
           ("db-user-name" . "captain_teach")
           ("cloud-secret-key" . "YOUR-SECRET-KEY")
           ("db-password" . "captain_teach")
           ("sub-domain" . "www.")
           ("mail-password" . "password")
           ("db-name" . "captain_teach")
           ("master-user" . "youremail@domain.com")
           ("db-address" . "localhost")))
  
  (check-equal? (check-conf-hash test-conf) test-conf))

