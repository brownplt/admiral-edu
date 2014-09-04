#lang racket

(require net/smtp
         net/head
         openssl)

(require "../configuration.rkt"
         "../util/config-file-reader.rkt")

(define (lines string)
  (string-split string "\n"))

(provide send-email)
(define (send-email uid subject message)
  (let* ((from (string-append "tech@" server-name))
         (to (list uid))
         (header (standard-message-header from to '() '() subject))
         (split-message (lines message)))
    (cond [(email-okay uid) (begin
                              (smtp-send-message 
                               mail-server 
                               from 
                               to 
                               header 
                               split-message
                               #:auth-user mail-username
                               #:auth-passwd mail-password
                               #:tls-encode ports->ssl-ports
                               mail-port)
                              #t)]
          [else #f])))

;; TODO: Provide file / database entries
(define (email-okay uid)
  (let* ((no-email-regexp "([.]*@student.edu|default-submission[.]*)")
         (check (regexp-match no-email-regexp uid)))
    (not check)))
