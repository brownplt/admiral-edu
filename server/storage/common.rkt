#lang racket

 ;; Helper functions
(provide intercalate)
(define (intercalate v ls)
  (if (null? ls) ls
      (cdr 
       (foldr 
        (lambda (x xs) 
          (cons v (cons x xs))) 
        '() ls))))

(provide root)
(define root '())

(define path-delim "/")

(provide to-path)
(define (to-path ls)
  (substring (foldr string-append "" (cons path-delim (intercalate path-delim ls))) 1))

(define (create-path-list class assignment user step)
  (append root (list class assignment user step)))

(provide submission-path)
(define (submission-path class assignment user step)
  (to-path (create-path-list class assignment user step)))

(provide create-directory)
(define (create-directory class user assignment step)
  (letrec ((path-list (list class user assignment step))
           (helper (lambda (acc rest)
                          ;; This is O(n^2) where n is the number of sub-directories. 
                          ;; Could be improved to O(n). However, n is a constant so in practice
                          ;; this shouldn't be an issue.
                     (let ((path (to-path (reverse acc))))
                       (when (not (equal? "" path))
                         (if (not (directory-exists? path)) 
                             (make-directory path) '()))
                       (if (null? rest) path (helper (cons (car rest) acc) (cdr rest)))))))
    (helper (reverse root) path-list)))

(provide ensure-path-exists)
(define (ensure-path-exists path)
  (let* ((split (string-split path "/"))
         (withoutFile (take split (- (length split) 1)))
         (to-make (apply string-append (intercalate "/" withoutFile))))
    (make-directory* to-make)))

;; Checks if a file-name is acceptable
;; Acceptable filenames include alphanumeric characters, underscore, dash, and period
;; that is, they must meet the regular expression #rx[a-zA-Z0-9_.-]*
;; Returns #t if the file-name is acceptable and #f otherwise.
(provide check-file-name)
(define (check-file-name file-name)
  (let* ((okay-chars #rx"[a-zA-Z0-9_.-]*"))
    (regexp-match-exact? okay-chars file-name)))

;; TODO: Eventually we want to detect the archive type and choose the correct program to extract it.
;; This also needs to be able to handle invalid file types
(provide unarchive)
(define (unarchive path file-name)
  (system (string-append "unzip " file-name " -d " path)))