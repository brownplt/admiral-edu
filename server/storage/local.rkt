#lang racket

(require "../database/mysql.rkt")
(require "common.rkt")



(provide upload-instructor-solution)
(define (upload-instructor-solution class user assignment step data)
  (let* ((path (create-directory class assignment user step))
         (files (map (lambda (x) (string-append path "/" x)) (list-files path)))
         (subs (map (lambda (x) (string-append path "/" x)) (sub-directories-of path))))
    (printf "Files: ~a\n\n" subs)
    (map delete-file files)
    (map delete-file subs)
    (printf "After delete: ~a\n\n" (map (lambda (x) (string-append path "/" x)) (sub-directories-of path)))
    (let ((out (open-output-file (string-append path "/submission.tar") #:exists 'replace)))
      (print (list "Creating instructor submission" path)) (newline)
      (display data out)
      (close-output-port out)
      (unarchive path)
      (when (not (submission:exists? assignment class step user)) (submission:create-instructor-solution assignment class step user))
      (delete-file (string-append path "/submission.tar")))))

(provide upload-submission)
(define (upload-submission class user assignment step data)
  (let* ((path (create-directory class assignment user step))
         (out (open-output-file (string-append path "/submission.tar") #:exists 'replace)))
    (display data out)
    (close-output-port out)
    (unarchive path)
    ;;TODO This should be abstracted
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
  (let ((path (string-append (submission-path class assignment user step) "/" file)))
    (file->string path)))

(provide retrieve-file)
(define (retrieve-file path)
    (file->string path))

(provide submission-file-path)
(define (submission-file-path class user assignment step file)
  (let ((path (string-append (submission-path class assignment user step) "/" file)))
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

;; If the review comments exist, return that file. Otherwise return a default json which has no comments.
(provide load-review-comments)
(define (load-review-comments class assignment stepName review-id reviewer reviewee)
  (let ((path (string-append class "/" assignment "/reviews/" stepName "/" review-id "/" reviewer "/" reviewee "/comments.json")))
    (if (file-exists? path)
        (retrieve-file path)
        "{\"comments\" : {}}")))

(provide save-review-comments)
(define (save-review-comments class assignment stepName review-id reviewer reviewee data)
  (let ((path (string-append class "/" assignment "/reviews/" stepName "/" review-id "/" reviewer "/" reviewee "/comments.json")))
    (write-file path data)))

(provide save-rubric)
(define (save-rubric class assignment stepName review-id reviewer reviewee data)
  (let ((path (string-append class "/" assignment "/reviews/"  stepName "/" review-id "/" reviewer "/" reviewee "/rubric.json")))
    (write-file path data)))

;;TODO Maybe store by hash-value?

;; If a rubric exists, returns the rubric otherwise returns the default rubric
(provide retrieve-rubric)
(define (retrieve-rubric class assignment stepName review-id reviewer reviewee)
  (let ((path (string-append class "/" assignment "/reviews/"  stepName "/" review-id "/" reviewer "/" reviewee "/rubric.json")))
    (if (file-exists? path) (retrieve-file path) (retrieve-default-rubric class assignment stepName review-id))))
         

(provide retrieve-default-rubric)
(define (retrieve-default-rubric class assignment stepName review-id)
  (print "Retrieving default rubric.") (newline)
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
         