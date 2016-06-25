#lang racket

;; NOTE: This module is not typeable because it uses Units

(require "../configuration.rkt"
         "cloud-storage-unit.rkt"
         "local-storage-unit.rkt")

(provide storage-init)

;; signal an error, must call (storage-init) first...
(define ((storage-not-initialized proc) . any)
  (error proc
         "must call (storage-init) before using storage functions"))


(define-syntax fun-maker
  (syntax-rules ()
    [(_ name)
     (begin (provide name)
            (define name (storage-not-initialized (quote name))))]))

; (path -> string)
; Given the path to a file, returns the contents of the retrieved file
; Otherwise retures Failure with a message.
(fun-maker retrieve-file)
(fun-maker retrieve-file-bytes)

; (path -> string -> ())
; Given a path and the contents to a file, writes that file (over writing any existing file).
(fun-maker write-file)

; (path -> ())
; Deletes the specified path. If it is a file, removes the specified file. If it is a directory
; removes the directory recursively deleting all files
(fun-maker delete-path)

; (path -> Either 'file 'directory 'does-not-exist)
; Returns a symbol representing if the path is a file, directory, or does not exist
(fun-maker path-info)

; (path -> (listof path))
; Returns all files that are at the specified path.
(fun-maker list-files)

; (path -> (listof path))
; Returns all files that are at the specified path recursively adding all files in sub directories
(fun-maker list-sub-files)

; (path -> (listof path))
; Returns all directories that are at the specified path.
(fun-maker list-dirs)

; Loads cloud-storage bindings
(define (invoke-cloud-storage)
  (define-values/invoke-unit/infer cloud-storage@)
  (set! retrieve-file retrieve-file)
  (set! retrieve-file-bytes retrieve-file-bytes)
  (set! write-file write-file)
  (set! delete-path delete-path)
  (set! path-info path-info)
  (set! list-files list-files)
  (set! list-sub-files list-sub-files)
  (set! list-dirs list-dirs))

; Loads local storage bindings
(define (invoke-local-storage)
  (define-values/invoke-unit/infer local-storage@)
  (set! retrieve-file retrieve-file)
  (set! retrieve-file-bytes retrieve-file-bytes)
  (set! write-file write-file)
  (set! delete-path delete-path)
  (set! path-info path-info)
  (set! list-files list-files)
  (set! list-sub-files list-sub-files)
  (set! list-dirs list-dirs))


;; load the appropriate set of functions into the
;; exported functions (yuck, yuck).
(define (storage-init)
  (cond
    [(string=? (storage-mode) "cloud-storage") (invoke-cloud-storage)]
    [(string=? (storage-mode) "local") (invoke-local-storage)]
    [else (error (format "Could not load storage mode: ~a" (storage-mode)))]))
