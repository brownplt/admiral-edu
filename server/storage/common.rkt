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
(define root '("home" "admiraledu" "files"))

(define path-delim "/")

(provide to-path)
(define (to-path ls)
  (foldr string-append "" (cons path-delim (intercalate path-delim ls))))

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
                       (if (not (directory-exists? path)) 
                           (make-directory path) '())
                       (if (null? rest) path (helper (cons (car rest) acc) (cdr rest)))))))
    (helper (reverse root) path-list)))


;; TODO: Eventually we want to detect the archive type and choose the correct program to extract it.
;; This also needs to be able to handle invalid file types
(provide unarchive)
(define (unarchive path)
  (system (string-append "tar -xf " path "/submission.tar -C " path)))