#lang racket

(require "../configuration.rkt"
         "cloud-storage-unit.rkt"
         "local-storage-unit.rkt")

; (path -> string)
; Given the path to a file, returns the contents of the retrieved file
; Otherwise retures Failure with a message.
(provide retrieve-file)
(define retrieve-file 'nil)
(define (set-retrieve-file proc) (set! retrieve-file proc))

; (path -> string -> ())
; Given a path and the contents to a file, writes that file (over writing any existing file).
(provide write-file)
(define write-file 'nil)
(define (set-write-file proc) (set! write-file proc))

; (path -> ())
; Deletes the specified path. If it is a file, removes the specified file. If it is a directory
; removes the directory recursively deleting all files
(provide delete-path)
(define delete-path 'nil)
(define (set-delete-path proc) (set! delete-path proc))

; (path -> Either 'file 'directory 'does-not-exist)
; Returns a symbol representing if the path is a file, directory, or does not exist
(provide path-info)
(define path-info 'nil)
(define (set-path-info proc) (set! path-info proc))


; (path -> (listof path))
; Returns all files that are at the specified path.
(provide list-files)
(define list-files 'nil)
(define (set-list-files proc) (set! list-files proc))

; (path -> (listof path))
; Returns all files that are at the specified path recursively adding all files in sub directories
(provide list-sub-files)
(define list-sub-files 'nil)
(define (set-list-sub-files proc) (set! list-sub-files proc))

; (path -> (listof path))
; Returns all directories that are at the specified path.
(provide list-dirs)
(define list-dirs 'nil)
(define (set-list-dirs proc) (set! list-dirs proc))

; Loads cloud-storage bindings
(define (invoke-cloud-storage)
  (define-values/invoke-unit/infer cloud-storage@)
  (set-retrieve-file retrieve-file)
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
  (set-write-file write-file)
  (set-delete-path delete-path)
  (set-path-info path-info)
  (set-list-files list-files)
  (set-list-sub-files list-sub-files)
  (set-list-dirs list-dirs))

(cond
  [(string=? storage-mode "cloud-storage") (invoke-cloud-storage)]
  [(string=? storage-mode "local") (invoke-local-storage)]
  [else (error (format "Could not load storage mode: ~a" storage-mode))])

;; Ensures the required functions are set
(when (not (procedure? retrieve-file)) (error "retrieve-file not set."))
(when (not (procedure? write-file)) (error "write-file not set."))
(when (not (procedure? delete-path)) (error "delete-path not set."))
(when (not (procedure? path-info)) (error "path-info not set."))
(when (not (procedure? list-files)) (error "list-files not set."))
(when (not (procedure? list-sub-files)) (error "list-sub-files not set."))
(when (not (procedure? list-dirs)) (error "list-dirs not set."))