#lang racket

(require "file-system-sig.rkt"
         (prefix-in local: "local-storage.rkt"))

(provide local-storage@)
(define-unit local-storage@
  (import)
  (export file-system^)  
  
  ; (path -> bytes?)
  ; Given the path to a file, returns (Success bytes?) where bytes is the contents of the retrieved file
  ; Otherwise retures Failure with a message.
  ; If the file exists locally, it returns it. Otherwise, it fetches it from the cloud and then returns it
  (define retrieve-file local:retrieve-file)
  
  (define retrieve-file-bytes local:retrieve-file-bytes)
  
  ; (path -> contents -> ())
  ; Given a path and the contents to a file, writes that file (over writing any existing file).
  ; Writes the local file (over writing if necessary). Then, pushes the local file to the cloud.
  (define write-file local:write-file)
  
  ; (path -> (Either Success Failure))
  ; Deletes the specified path. If it is a file, removes the specified file. If it is a directory
  ; removes the directory recursively deleting all files
  
  ; Deletes the local copy and the remote copy
  (define delete-path local:delete-path)
  
  ; (path -> Either 'file 'directory 'does-not-exist)
  ; Returns a symbol representing if the path is a file, directory, or does not exist
  (define path-info local:path-info)
  
  ; (path -> (listof path))
  ; Returns all files that are at the specified path.
  (define list-files local:list-files)
  
  ; (path -> (listof path))
  ; Returns all directories that are at the specified path.
  (define list-dirs local:list-dirs)

  ; (path -> (listof path))
  ; Returns all files that are at the specified path recursively adding all sub directories
  (define list-sub-files local:list-sub-files)
  
  )