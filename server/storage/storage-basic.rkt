#lang racket

;; NOTE: This module is not typeable because it uses Units

(require "../configuration.rkt"
         (prefix-in c: "cloud-storage.rkt")
         (prefix-in l: "local-storage.rkt"))

(define-syntax setup
  (syntax-rules ()
    [(_ [name c-version l-version] ...)
     (begin (provide name)
            ...
            (define (name . args)
              (cond
                [(string=? (storage-mode) "cloud-storage") (apply c-version args)]
                [(string=? (storage-mode) "local") (apply l-version args)]
                [else (error (format "Unrecognized storage mode: ~a" (storage-mode)))]))
            ...)]))

(setup [retrieve-file       c:retrieve-file       l:retrieve-file]
       [retrieve-file-bytes c:retrieve-file-bytes l:retrieve-file-bytes]
       [write-file          c:write-file          l:write-file]
       [delete-path         c:delete-path         l:delete-path]
       [path-info           c:path-info           l:path-info]
       [list-files          c:list-files          l:list-files]
       [list-sub-files      c:list-sub-files      l:list-sub-files]
       [list-dirs           c:list-dirs           l:list-dirs])

; retrieve-file: (path -> string)
; Given the path to a file, returns the contents of the retrieved file
; Otherwise retures Failure with a message.

; write-file: (path -> string -> ())
; Given a path and the contents to a file, writes that file (over writing any existing file).

; delete-path: (path -> ())
; Deletes the specified path. If it is a file, removes the specified file. If it is a directory
; removes the directory recursively deleting all files

; path-info: (path -> Either 'file 'directory 'does-not-exist)
; Returns a symbol representing if the path is a file, directory, or does not exist

; list-files: (path -> (listof path))
; Returns all files that are at the specified path.

; list-sub-files: (path -> (listof path))
; Returns all files that are at the specified path recursively adding all files in sub directories

; list-dirs: (path -> (listof path))
; Returns all directories that are at the specified path.



