#lang typed/racket

;;TODO: Once everything is typed, we should replace almost all instances of Path-String with Path

; (path -> string?)
(provide retrieve-file)
(: retrieve-file (String -> String))
(define (retrieve-file path)
  (file->string path))


; (path -> contents -> ())
; Given a path and the contents to a file, writes that file (over writing any existing file).
; Writes the local file (over writing if necessary). Then, pushes the local file to the cloud.
(provide write-file)
(: write-file (String Any -> Void))
(define (write-file path contents)
  (ensure-path-exists path)
  (let ((out (open-output-file path #:exists 'replace)))
    (display contents out)
    (close-output-port out)))

; Deletes the local copy and the remote copy
(provide delete-path)
(: delete-path (String -> Void))
(define (delete-path path)
  (when (not (eq? 'does-not-exist (path-info path)))
      (delete-directory/files path #:must-exist? #f)))


; (path -> Either 'file 'directory 'does-not-exist)
; Returns a symbol representing if the path is a file, directory, or does not exist
(provide path-info)
(: path-info (String -> (U 'file 'directory 'does-not-exist)))
(define (path-info path)
  (cond [(directory-exists? path) 'directory]
        [(file-exists? path) 'file]
        [else 'does-not-exist]))


; (path -> (listof string))
; Returns all files that are at the specified path.
(provide list-files)
(: list-files (String -> (Listof String)))
(define (list-files path)
  (let ((f (lambda: ([p : String]) (is-file? (string-append (add-slash path) p)))))
    (filter f (map path->string (directory-list path)))))


(provide list-dirs)
(: list-dirs (String -> (Listof String)))
(define (list-dirs path)
  (let ((f (lambda: ([p : String]) (is-directory? (string-append (add-slash path) p)))))
    (filter f (map path->string (directory-list path)))))

(: is-file? (String -> Boolean))
(define (is-file? path)
  (eq? 'file (path-info path)))


(: is-directory? (String -> Boolean))
(define (is-directory? path)
  (eq? 'directory (path-info path)))

(: add-slash (String -> String))
(define (add-slash path-string)
  (let ((path (cond [(string? path-string) (assert path-string string?)]
                    [else (path->string (assert path-string path?))])))
    (if (equal? (string-ref path (- (string-length path) 1)) #\/) path (string-append path "/"))))


; (path -> (listof path))
; Returns all files that are at the specified path recursively adding all sub directories
(provide list-sub-files)
(: list-sub-files (String -> (Listof String)))
(define (list-sub-files path)
  (let* ((ls (map path->string (directory-list path)))
         (full-ls (map (lambda: ([p : String]) (string-append (add-slash path) p)) ls))
         (files (filter is-file? full-ls))
         (dirs (filter is-directory? full-ls))
         (sub-files (apply append (map list-sub-files dirs))))
    (append files sub-files)))


(provide ensure-path-exists)
(: ensure-path-exists (String -> Void))
(define (ensure-path-exists path-string)
  (let* ((path (cond [(string? path-string) (assert path-string string?)]
                     [else (path->string (assert path-string path?))]))
         (split (string-split path "/"))
         (withoutFile (take split (- (length split) 1)))
         (to-make (apply string-append (add-between withoutFile "/"))))
    (make-directory* to-make)))
