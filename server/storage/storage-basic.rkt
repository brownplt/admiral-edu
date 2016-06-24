#lang racket

;; NOTE: This module is not typeable because it uses Units

(require "../configuration.rkt"
         "cloud-storage-unit.rkt"
         "local-storage-unit.rkt")

(provide storage-init)

;; signal an error, must call (storage-init) first...
(define (storage-not-initialized . any)
  (error 'storage-mode
         "must call (storage-init) before using storage functions"))


; (path -> string)
; Given the path to a file, returns the contents of the retrieved file
; Otherwise retures Failure with a message.
(provide retrieve-file)
(define retrieve-file storage-not-initialized)
(define (set-retrieve-file proc) (set! retrieve-file proc))

(provide retrieve-file-bytes)
(define retrieve-file-bytes storage-not-initialized)
(define (set-retrieve-file-bytes proc) (set! retrieve-file-bytes proc))

; (path -> string -> ())
; Given a path and the contents to a file, writes that file (over writing any existing file).
(provide write-file)
(define write-file storage-not-initialized)
(define (set-write-file proc) (set! write-file proc))

; (path -> ())
; Deletes the specified path. If it is a file, removes the specified file. If it is a directory
; removes the directory recursively deleting all files
(provide delete-path)
(define delete-path storage-not-initialized)
(define (set-delete-path proc) (set! delete-path proc))

; (path -> Either 'file 'directory 'does-not-exist)
; Returns a symbol representing if the path is a file, directory, or does not exist
(provide path-info)
(define path-info storage-not-initialized)
(define (set-path-info proc) (set! path-info proc))


; (path -> (listof path))
; Returns all files that are at the specified path.
(provide list-files)
(define list-files storage-not-initialized)
(define (set-list-files proc) (set! list-files proc))

; (path -> (listof path))
; Returns all files that are at the specified path recursively adding all files in sub directories
(provide list-sub-files)
(define list-sub-files storage-not-initialized)
(define (set-list-sub-files proc) (set! list-sub-files proc))

; (path -> (listof path))
; Returns all directories that are at the specified path.
(provide list-dirs)
(define list-dirs storage-not-initialized)
(define (set-list-dirs proc) (set! list-dirs proc))

; Loads cloud-storage bindings
(define (invoke-cloud-storage)
  (define-values/invoke-unit/infer cloud-storage@)
  (set-retrieve-file retrieve-file)
  (set-retrieve-file-bytes retrieve-file-bytes)
  (set-write-file write-file)
  (set-delete-path delete-path)
  (set-path-info path-info)
  (set-list-files list-files)
  (set-list-sub-files list-sub-files)
  (set-list-dirs list-dirs))

; Loads local storage bindings
(define (invoke-local-storage)
  (define-values/invoke-unit/infer local-storage@)
  (set-retrieve-file retrieve-file)
  (set-retrieve-file-bytes retrieve-file-bytes)
  (set-write-file write-file)
  (set-delete-path delete-path)
  (set-path-info path-info)
  (set-list-files list-files)
  (set-list-sub-files list-sub-files)
  (set-list-dirs list-dirs))


;; load the appropriate set of functions into the
;; exported functions (yuck, yuck).
(define (storage-init)
  (cond
    [(string=? (storage-mode) "cloud-storage") (invoke-cloud-storage)]
    [(string=? (storage-mode) "local") (invoke-local-storage)]
    [else (error (format "Could not load storage mode: ~a" (storage-mode)))]))
