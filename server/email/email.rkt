#lang typed/racket

(require/typed net/smtp
               [smtp-send-message (->* (String String (Listof String) String String)
                                       (#:port-no Integer
                                        #:auth-user (U String #f)
                                        #:auth-passwd (U String #f)
                                        #:tls-encode EncodeFunc)
                                       Void)])

;                                      [#:auth-user (U String #f)]
;                                      [#:auth-passwd (U String #f)]
;                                      [#:tls-encode Any]
;                                      [Exact-Nonnegative-Integer]
;                                       Void)])

(require/typed net/head
               [standard-message-header (String (Listof String) (Listof String) (Listof String) String -> String)])

(define-type EncodeFunc (->* (Input-Port Output-Port)
                             (#:mode Symbol
                              #:context Any
                              #:encrypt Any
                              #:close-original? Boolean
                              #:shutdown-on-close? Boolean
                              #:error/ssl Procedure
                              #:hostname (U String #f))
                             (Values Input-Port Output-Port)))

(require/typed openssl
               [ports->ssl-ports EncodeFunc])

(require "../configuration.rkt")

(: lines (String -> (Listof String)))
(define (lines string)
  (string-split string "\n"))

(provide send-email)
(: send-email (String String String -> Boolean))
(define (send-email uid subject message)
  (let* ((from (string-append "tech@" (server-name)))
         (to (list uid))
         (header (standard-message-header from to '() '() subject))
         (split-message (lines message)))
    (cond [(email-okay uid) (begin
                              (smtp-send-message 
                               (mail-server)
                               from 
                               to 
                               header 
                               split-message
                               #:auth-user (mail-username)
                               #:auth-passwd (mail-password)
                               #:tls-encode ports->ssl-ports
                               #:port-no (mail-port))
                              #t)]
          [else #f])))

;; TODO: Provide file / database entries

;; Extremely basic. Ensures that there are no spaces, a receipient, and a domain.
(define okay-email-regexp "[^ ]+@[^ ]+\\.[^ ]+")

;; Blacklist @student.edu email addresses and default-submission addresses
(define black-list-email-regexp "([.]*@student.edu|default-submission[.]*)")

(: email-okay (String -> Boolean))
(define (email-okay uid)
  (let ((black-listed (regexp-match black-list-email-regexp uid))
        (okay (regexp-match-exact? okay-email-regexp uid)))
    (and (not black-listed) okay)))


