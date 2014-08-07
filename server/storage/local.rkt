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

(provide delete-file)
(define (delete-file path)
  (delete-directory/files path #:must-exist? #f))

(provide write-file)
(define (write-file path contents)
  (ensure-path-exists path)
  (let ((out (open-output-file path #:exists 'replace)))
    (display contents out)
    (close-output-port out)))

(define (ensure-path-exists path)
  (let* ((split (string-split path "/"))
         (withoutFile (take split (- (length split) 1)))
         (to-make (apply string-append (intercalate "/" withoutFile))))
    (make-directory* to-make)))

(provide retrieve-submission-file)
(define (retrieve-submission-file class user assignment step version file)
  (let ((path (string-append (submission-path class user assignment step version) "/" file)))
    (file->string path)))

(provide retrieve-file)
(define (retrieve-file path)
    (file->string path))

(provide submission-file-path)
(define (submission-file-path class user assignment step version file)
  (let ((path (string-append (submission-path class user assignment step version) "/" file)))
    path))

(provide sub-directories-of)
(define (sub-directories-of path)
  (filter (lambda (p) (is-directory? (string-append (add-slash path) p))) (map path->string (directory-list path))))

(provide add-slash)
(define (add-slash path)
  (if (equal? (string-ref path (- (string-length path) 1)) #\/) path (string-append path "/")))

(provide list-files)
(define (list-files path)
  (filter (lambda (p) (is-file? (string-append (add-slash path) p))) (map path->string (directory-list path))))

(provide is-directory?)
(define (is-directory? path)
  (directory-exists? path))

(provide is-file?)
(define (is-file? path)
  (file-exists? path))

(provide retrieve-default-rubric)
(define (retrieve-default-rubric class assignment stepName review-id)
  (let ((path (string-append class "/" assignment "/reviews/"  stepName "/" review-id "/rubric.json")))
    (retrieve-file path)))

(provide create-default-rubric)
(define (create-default-rubric class assignment stepName rubric review-id)
  (let ((path (string-append class "/" assignment "/reviews/" stepName "/" review-id "/rubric.json")))
    (write-file path rubric)))

(provide save-assignment-description)
(define (save-assignment-description class assignment description)
  (let ((path (string-append class "/" assignment "/description.yaml")))
    (write-file path description)))

(provide retrieve-assignment-description)
(define (retrieve-assignment-description class assignment)
  (let ((path (string-append class "/" assignment "/description.yaml")))
    (retrieve-file path)))
         