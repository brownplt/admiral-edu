#lang racket/signature

; (path -> string)
; Given the path to a file, returns the contents of the retrieved file
; Otherwise retures Failure with a message.
Uretrieve-file 

Uretrieve-file-bytes

; (path -> contents -> ())
; TODO: Maybe have this return Success / Failure. Right now it might throw an exception but
; each file system may use a different exception.
; Given a path and the contents to a file, writes that file (over writing any existing file).
Uwrite-file

; (path -> ())
; Deletes the specified path. If it is a file, removes the specified file. If it is a directory
; removes the directory recursively deleting all files
Udelete-path   

; (path -> Either 'file 'directory 'does-not-exist)
; Returns a symbol representing if the path is a file, directory, or does not exist
Upath-info

; (path -> (listof path))
; Returns all files that are at the specified path.
Ulist-files

; (path -> (listof path))
; Returns all directories that are at the specified path.
Ulist-dirs

; (path -> (listof path))
; Returns all files that are at the specified path recursively adding all sub directories
Ulist-sub-files
   
