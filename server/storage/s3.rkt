#lang racket

(require (planet gh/aws:1:5))
(read-keys "/home/admiraledu/aws-credentials")
(ensure-have-keys)
(define bucket-name "cmpsci220-f14")

(define path-delim "/")

(define (to-path ls)
  (foldr string-append "" (cons path-delim (intercalate path-delim ls))))

 ;; Helper functions
(define (intercalate v ls)
  (if (null? ls) ls
      (cdr 
       (foldr 
        (lambda (x xs) 
          (cons v (cons x xs))) 
        '() ls))))

(define root '("home" "admiraledu" "files"))

(define (create-path-list class user assignment step version)
  (append root (list class user assignment step version)))

(provide create-directory)
(define (create-directory class user assignment step version)
  (letrec ((path-list (list class user assignment step version))
           (helper (lambda (acc rest)
                          ;; This is O(n^2) where n is the number of sub-directories. 
                          ;; Could be improved to O(n). However, n is a constant so in practice
                          ;; this shouldn't be an issue.
                     (let ((path (to-path (reverse acc))))
                       (if (not (directory-exists? path)) 
                           (make-directory path) '())
                       (if (null? rest) path (helper (cons (car rest) acc) (cdr rest)))))))
    (helper (reverse root) path-list)))

(provide upload-submission)
(define (upload-submission class user assignment step version data)
  (let* ((path (create-directory class user assignment step version))
         (out (open-output-file (string-append path "/submission.tar") #:exists 'replace)))
    (display data out)
    (close-output-port out)
    (unarchive path)
    (delete-file (string-append path "/submission.tar"))
    (push-contents path)
    (delete-directory/files path)))

(define (push-contents path)
  (let ((files (find-files (lambda (file) (not (directory-exists? file))) path))
        (put (lambda (path) 
               (put/file (string-append bucket-name (path->string path)) path))))
    (map put files)))
    

(define (unarchive path)
  (system (string-append "tar -xf " path "/submission.tar -C " path)))