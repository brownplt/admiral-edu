#lang racket

(require (planet gh/aws:1:5))
(require "../database/mysql.rkt"
         "common.rkt")

(read-keys "/home/admiraledu/aws-credentials")
(ensure-have-keys)
(define bucket-name "cmpsci220-f14")

(provide upload-submission)
(define (upload-submission class user assignment step data)
  (let* ((version (number->string (submission:count assignment class step user)))
         (path (create-directory class user assignment step version))
         (out (open-output-file (string-append path "/submission.tar") #:exists 'replace)))
    (display data out)
    (close-output-port out)
    (unarchive path)
    (delete-file (string-append path "/submission.tar"))
    (push-contents path)
    (submission:create assignment class step user)
    (delete-directory/files path)))

;; Deletes all files in the bucket
(define (clear-bucket)
  (let* ((files (ls (string-append bucket-name "/")))
         (paths (lambda (name) (string-append bucket-name "/" name)))
         (bucket+paths (map paths files))
         (remove (lambda (path) (delete path))))
    (map remove bucket+paths))) 


(define (push-contents path)
  (let ((files (find-files (lambda (file) (not (directory-exists? file))) path))
        (put (lambda (path) 
               (put/file (string-append bucket-name (path->string path)) path))))
    (map put files)))
    
