#lang racket

(require net/smtp
         net/head
         openssl)

(require "../configuration.rkt"
         "../util/config-file-reader.rkt")

(define username 'nil)
(define password 'nil)

;; Checks to see if the credentials are loaded. If they are returns #t. Otherwise, attempts
;; to load them raising an exception if they can't be loaded.
(define (check-credentials)
  (cond [(not (eq? username 'nil)) #t]
        [else
         (let ((conf (read-conf smtp:credentials-file)))
           (cond [(not (hash-has-key? conf "username")) (raise (string-append "No username credential found in " smtp:credentials-file " while loading smtp configuration."))]
                 [(not (hash-has-key? conf "password")) (raise (string-append "No password credential found in " smtp:credentials-file " while loading smtp configuration."))]
                 [else
                  (begin
                    (set! username (hash-ref conf "username"))
                    (set! password (hash-ref conf "password"))
                    #t)]))]))

(check-credentials)

(define (lines string)
  (string-split string "\n"))

(provide send-email)
(define (send-email uid subject message)
  (let* ((from (string-append "tech@" server-name))
         (to (list uid))
         (header (standard-message-header from to '() '() subject))
         (split-message (lines message)))
    (cond [(email-okay uid) (begin
                              (check-credentials)
                              (smtp-send-message 
                               smtp:server-address 
                               from 
                               to 
                               header 
                               split-message
                               #:auth-user username
                               #:auth-passwd password
                               #:tls-encode ports->ssl-ports)
                              #t)]
          [else #f])))

;; TODO: Provide file / database entries
(define (email-okay uid)
  (let* ((no-email-regexp "[.]*@student.edu")
         (check (regexp-match no-email-regexp uid)))
    (not check)))
