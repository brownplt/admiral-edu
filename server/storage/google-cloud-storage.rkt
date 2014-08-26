#lang racket

(require (planet gh/aws:1:5))

(require "../database/mysql.rkt")
(require "../configuration.rkt")
(require "common.rkt")
(require (prefix-in local: "local.rkt"))

(s3-host "storage.googleapis.com")
(read-keys s3-keys)


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

(provide save-rubric)
(define (save-rubric class assignment stepName review-id reviewer reviewee data)
  (let ((path (string-append class "/" assignment "/reviews/"  stepName "/" review-id "/" reviewer "/" reviewee "/rubric.json")))
    (write-file path data)))

(provide save-review-comments)
(define (save-review-comments class assignment stepName review-id reviewer reviewee file-path data)
  (let ((path (string-append class "/" assignment "/reviews/" stepName "/" review-id "/" reviewer "/" reviewee "/" (remove-leading-slash file-path) ".comments.json")))
    (write-file path data)))

(provide submission-file-path)
(define (submission-file-path class user assignment step file)
  (let ((path (string-append (submission-path class assignment user step) "/" file)))
    path))

;;;;; Everything above didn't need any modification
; Returns #t if successful and #f if failute

; Returns #t if successful and #f if failute
(provide upload-instructor-solution)
(define (upload-instructor-solution class user assignment step data)
  (let* ((path (create-directory class assignment user step))
         (files (map (lambda (x) (string-append path "/" x)) (list-files path)))
         (local-files (map (lambda (x) (string-append path "/" x)) (local:list-files path)))
         (subs (map (lambda (x) (string-append path "/" x)) (local:sub-directories-of path))))
    (map local:delete-file local-files)
    ;; Remove any previous files
    (map delete-file files)
    (map local:delete-file subs)
    (let ((out (open-output-file (string-append path "/submission.zip") #:exists 'replace)))
      (display data out)
      (close-output-port out)
      (let ((result (unarchive path)))
        (local:delete-file (string-append path "/submission.zip"))
        (when result   
          (let ((files (list-all-sub-files path))
                ;; remove-leading-slash is a hack fix
                (upload-f (lambda (p) (put/file (string-append bucket (remove-leading-slash p)) (string->path p)))))
            (when (not (submission:exists? assignment class step user)) (submission:create-instructor-solution assignment class step user))
            (map upload-f files)            
            (local:delete-file path)))
        result))))

(define (remove-leading-slash p)
  (let* ((fc (string-ref p 0))
         (pp (if (eq? #\/ fc) (substring p 1) p)))
    pp))

(provide upload-submission)
(define (upload-submission class user assignment step data)
  (let* ((path (create-directory class assignment user step))
         (file (string-append path "/submission.zip"))
         (out (open-output-file file #:exists 'replace)))
    (display data out)
    (close-output-port out)
    
    (let ((result (unarchive path)))
      (local:delete-file file)
      (when result 
        (begin
          (let ((files (list-all-sub-files path))
                (upload-f (lambda (p) (put/file (string-append bucket (remove-leading-slash p)) (string->path p)))))
            (map upload-f files)
            (submission:create assignment class step user)
            (local:delete-file path))))
      result)))

(provide export-assignment)
(define (export-assignment class assignment)
  (let* ((path (string-append class "/" assignment))
         (archive (string-append path "/" assignment ".zip"))
         (files (ls (string-append bucket path))))
    (map store-local-file files)
    
    (system (string-append "rm " archive " -f"))
    
    (system (string-append "zip -r " archive " " path))
     (let ((data (file->bytes archive)))
       (system (string-append "rm " archive " -f"))
       (map local:delete-file files)
       data)))

(define (store-local-file path)
  (local:ensure-path-exists path)
  (let* ((bytes (get/bytes (string-append bucket path)))
         (out (open-output-file path #:exists 'replace)))
    (write-bytes bytes out)
    (close-output-port out)))
  
    

(provide delete-file)
(define (delete-file path)
  (let* ((files (ls (string-append bucket path)))
         (delete-f (lambda (p) (delete (string-append bucket p)))))
    (map delete-f files)))

(provide retrieve-submission-file)
(define (retrieve-submission-file class user assignment step version file)
  (let ((path (string-append (submission-path class assignment user step) "/" file)))
    (retrieve-file path)))

(provide sub-directories-of)
(define (sub-directories-of path)
  (let* ((len (string-length path))
         (lc (string-ref path (- len 1)))
         (pathPrime (if (eq? #\/ lc) path (string-append path "/"))))
  (filter (lambda (p) (is-directory? (string-append pathPrime p))) (list-files pathPrime))))

(provide list-files)
(define (list-files path)
  (let* ((files (ls (string-append bucket path)))
         (split-path (string-split path "/"))
         (split (lambda (x) (string-split x "/")))
         (split-files (map split files))
         (at-len (length split-path))
         (at-path (map last (remove-duplicates (map (lambda (x) (take-up-to x (+ at-len 1))) split-files)))))
    at-path))

(provide list-only-files)
(define (list-only-files path)
  (let* ((files (ls (string-append bucket path)))
         (split-path (string-split path "/"))
         (split (lambda (x) (string-split x "/")))
         (split-files (map split files))
         (at-len (length split-path))
         (at-path (map last (filter (lambda (x) (= (length x) (+ at-len 1))) split-files))))
    at-path))

(define (take-up-to ls n)
  (cond [(> (- (length ls) n) 0) (take ls n)]
        [else ls]))

(provide is-directory?)
(define (is-directory? path) (not (is-file? path)))

(provide is-file?)
(define (is-file? path)
  (file-exists-in-cloud? path))

(define (get-review-path review)
  (let* ((class (review:record-class-id review))
         (assignment (review:record-assignment-id review))
         (step (review:record-step-id review))
         (review-id (review:record-review-id review))
         (reviewer (review:record-reviewer-id review))
         (reviewee (review:record-reviewee-id review)))
    (string-append class "/" assignment "/reviews/" step "/" review-id "/" reviewer "/" reviewee "/")))

(provide save-review-feedback)
(define (save-review-feedback review feedback)
  (let* ((file-name "feedback.txt")
         (path (string-append (get-review-path review) file-name)))
    (write-file path feedback)))

(provide load-review-feedback)
(define (load-review-feedback review)
  (let* ((file-name "feedback.txt")
         (path (string-append (get-review-path review) file-name)))
    (if (file-exists-in-cloud? path) (retrieve-file path) "")))

(provide load-review-comments)
(define (load-review-comments class assignment stepName review-id reviewer reviewee file-path)
  (let ((path (string-append class "/" assignment "/reviews/" stepName "/" review-id "/" reviewer "/" reviewee "/" (remove-leading-slash file-path) ".comments.json")))
    (if (file-exists-in-cloud? path)
        (retrieve-file path)
        "{\"comments\" : {}}")))

(provide retrieve-rubric)
(define (retrieve-rubric class assignment stepName review-id reviewer reviewee)
  (let ((path (string-append class "/" assignment "/reviews/"  stepName "/" review-id "/" reviewer "/" reviewee "/rubric.json")))
    (if (file-exists-in-cloud? path) (retrieve-file path) (retrieve-default-rubric class assignment stepName review-id))))

(define (file-exists-in-cloud? path)
  (let* ((files (ls (string-append bucket path)))
         (member? (filter (lambda (x) (equal? path x)) files)))
    (and (= (length files) 1) (= (length member?) 1))))

(provide write-file)
(define (write-file path contents)
  (local:ensure-path-exists path)
  (let ((out (open-output-file path #:exists 'replace)))
    (display contents out)
    (close-output-port out)
    ;; Push the file to the cloud
    (put/file (string-append bucket (remove-leading-slash path)) (string->path path))
    ;; Remove local copy
    (local:delete-file path)))

(provide retrieve-file)
(define (retrieve-file path)
    (bytes->string/locale (get/bytes (string-append bucket path))))

(define (upload-path path)
  (let ((all-files (list-all-sub-files path))
        (helper (lambda (file-path) (put/file (string-append bucket (remove-leading-slash file-path)) (string->path file-path)))))
    (map helper all-files)))

(define (list-all-sub-files path)
  (cond 
    [(local:is-file? path) '()]
    [(local:is-directory? path) 
     (let* ((files (map (lambda (x) (string-append path "/" x)) (local:list-files path)))
            (sub-dirs (map (lambda (x) (string-append path "/" x)) (local:sub-directories-of path)))
            (sub-contents (map list-all-sub-files sub-dirs)))
       (flatten (append files sub-contents)))]))
         