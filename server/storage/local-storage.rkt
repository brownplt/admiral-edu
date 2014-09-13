#lang racket

(require "common.rkt")

(provide ensure-path-exists)

; (path -> string?)
(provide retrieve-file)
(define (retrieve-file path)
  (file->string path))

; (path -> contents -> ())
; Given a path and the contents to a file, writes that file (over writing any existing file).
; Writes the local file (over writing if necessary). Then, pushes the local file to the cloud.
(provide write-file)
(define (write-file path contents)
  (ensure-path-exists path)
  (let ((out (open-output-file path #:exists 'replace)))
    (display contents out)
    (close-output-port out)))

; Deletes the local copy and the remote copy
(provide delete-path)
(define (delete-path path)
  (when (not (eq? 'does-not-exist (path-info path)))
      (delete-directory/files path #:must-exist? #f)))

; (path -> Either 'file 'directory 'does-not-exist)
; Returns a symbol representing if the path is a file, directory, or does not exist
(provide path-info)
(define (path-info path)
  (cond [(directory-exists? path) 'directory]
        [(file-exists? path) 'file]
        [else 'does-not-exist]))

; (path -> (listof string))
; Returns all files that are at the specified path.
(provide list-files)
(define (list-files path)
  (let ((f (lambda (p) (is-file? (string-append (add-slash path) p)))))
    (filter f (map path->string (directory-list path)))))

(provide list-dirs)
(define (list-dirs path)
  (let ((f (lambda (p) (is-directory? (string-append (add-slash path) p)))))
    (filter f (map path->string (directory-list path)))))

(define (is-file? path)
  (eq? 'file (path-info path)))

(define (is-directory? path)
  (eq? 'directory (path-info path)))

(define (add-slash path)
  (if (equal? (string-ref path (- (string-length path) 1)) #\/) path (string-append path "/")))

; (path -> (listof path))
; Returns all files that are at the specified path recursively adding all sub directories
(provide list-sub-files)
(define (list-sub-files path)
  (let* ((ls (map path->string (directory-list path)))
         (full-ls (map (lambda (p) (string-append (add-slash path) p)) ls))
         (files (filter is-file? full-ls))
         (dirs (filter is-directory? full-ls)))
    (flatten (append files (map list-sub-files dirs)))))