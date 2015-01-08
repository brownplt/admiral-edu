#lang racket

(require "file-system-sig.rkt"
         aws
         "../configuration.rkt"
         (prefix-in local: "local-storage.rkt"))

(provide cloud-storage@)
(define-unit cloud-storage@
  ; The following loads this unit
  ; (invoke-unit cloud-storage@)
  ; (define-values/invoke-unit/infer cloud-storage@)
  (import)
  (export file-system^)  
  
  (s3-host cloud-host)
  (public-key cloud-access-key-id)
  (private-key cloud-secret-key)
  
  ; If the file exists locally, it returns it. Otherwise, it fetches it from the cloud and then returns it
  (define (retrieve-file path)
    (let* ((info (local:path-info path)))
      (local:ensure-path-exists path)
      (when (eq? info 'does-not-exist) (get/file (string-append bucket path) (string->path path))))
    (local:retrieve-file path))
  
  (define (retrieve-file-bytes path)
    (let* ((info (local:path-info path)))
      (local:ensure-path-exists path)
      (when (eq? info 'does-not-exist) 
        (begin (printf "Syncing: ~a\n" (string-append bucket path))
               (get/file (string-append bucket path) (string->path path))
               (printf "Done.\n"))))
    (local:retrieve-file-bytes path))
    

  ; Writes the local file (over writing if necessary). Then, pushes the local file to the cloud.
  (define (write-file path contents)
    (let ((clean-path (clean path)))
      (local:write-file clean-path contents)
      (let ((bucket+path (string-append bucket clean-path))
            (pathname (string->path clean-path)))
        (put/file bucket+path pathname))
    (void)))

  ;; Conversts all spaces to underscores. This is a hack but the (put/file API
  ;; dies when you pass in a path with a space. This bug has been reported here:
  ;; https://github.com/greghendershott/aws/issues/35
  (define (clean to-clean)
    (let* ((string-list (string->list to-clean))
           (to-safe (lambda (c) (cond [(eq? #\  c) #\_]
                                      [else c]))))
      (apply string (map to-safe string-list))))
  
  
  ; Deletes the local copy and the remote copy
  (define (delete-path path)
    (let* ((files (ls (string-append bucket path)))
           (delete-f (lambda (p) (delete (string-append bucket p)))))
      (map delete-f files))
    (local:delete-path path))
  
  ; (path -> Either 'file 'directory 'does-not-exist)
  ; Returns a symbol representing if the path is a file, directory, or does not exist
  (define (path-info path)
    (cond [(file-exists-in-cloud? path) 'file]
          [else 'directory]))
  
  (define (file-exists-in-cloud? path)
    (let* ((files (ls (string-append bucket path)))
           (member? (filter (lambda (x) (equal? path x)) files)))
      (= (length member?) 1)))
  
  ; (path -> (listof path))
  ; Returns all files that are at the specified path.
  (define (list-files path)
    (printf "Listing files at ~a\n" path)
    (let* ((files (ls (string-append bucket path)))
           (split-path (string-split path "/"))
           (split (lambda (x) (string-split x "/")))
           (split-files (map split files))
           (at-len (length split-path))
           (at-path (map last (filter (lambda (x) (= (length x) (+ at-len 1))) split-files))))
      (printf "at-path:~a\n" at-path)
      at-path))
  
  ; (path -> (listof path))
  ; Returns all directories that are at the specified path.
  (define (list-dirs path)
    (let* ((len (string-length path))
           (lc (if (= len 0) "" (string-ref path (- len 1))))
           (pathPrime (if (eq? #\/ lc) path (string-append path "/")))
           (result (filter (lambda (p) (is-directory? (string-append pathPrime p))) (list-path pathPrime))))
      result))
  
  (define (list-path path)
    (let* ((files (ls (string-append bucket path)))
           (split-path (string-split path "/"))
           (split (lambda (x) (string-split x "/")))
           (split-files (map split files))
           (at-len (length split-path))
           (at-path (map last (remove-duplicates (map (lambda (x) (take-up-to x (+ at-len 1))) split-files)))))
      at-path))
  
  (define (take-up-to ls n)
    (cond [(> (- (length ls) n) 0) (take ls n)]
          [else ls]))
  
  (define (is-directory? path)
    (eq? 'directory (path-info path)))
  
  ; (path -> (listof path))
  ; Returns all files that are at the specified path recursively adding all sub directories
  (define (list-sub-files path)
    (ls (string-append bucket path)))
  
  )
