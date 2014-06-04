#lang racket

(require "../database/mysql.rkt")
(require "common.rkt")



(provide upload-submission)
(define (upload-submission class user assignment step data)
  (let* ((version (number->string (submission:count assignment class step user)))
         (path (create-directory class user assignment step version))
         (out (open-output-file (string-append path "/submission.tar") #:exists 'replace)))
    (display data out)
    (close-output-port out)
    (unarchive path)
    (submission:create assignment class step user)
    (delete-file (string-append path "/submission.tar"))))