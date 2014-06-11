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

(provide retrieve-submission-file)
(define (retrieve-submission-file class user assignment step version file)
  (file->string (string-append (submission-path class user assignment step version) "/" file)))

(provide is-directory?)
(define (is-directory? path)
  (directory-exists? path))

(provide is-file?)
(define (is-file? path)
  (file-exists? path))
         