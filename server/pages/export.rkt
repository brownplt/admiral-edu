#lang racket

(require web-server/http/response-structs)

(require "../storage/storage.rkt"
         "../base.rkt"
         (prefix-in error: "errors.rkt"))

(provide load)
(define (load session role rest)
  (let ((assignment-id (car rest)))
    (if (not (roles:Record-can-edit role)) (fail-auth)
        (let ((data (export-assignment (class-name) assignment-id)))
          (response/full
           200 #"Okay"
           (current-seconds) #"application/octet-stream; charset=ISO-8859-1"
           empty
           (list data))))))

(define (fail-auth)
  (response/full
         200 #"Okay"
         (current-seconds) TEXT/HTML-MIME-TYPE
         empty
         (list (string->bytes/utf-8 (error:error "You are not authorized to see this page.")))))