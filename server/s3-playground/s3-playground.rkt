#lang racket
(require (planet gh/aws:1:5))
(require "../storage/local.rkt")

(s3-host "storage.googleapis.com")
(public-key cloud-access-key-id)
(private-key cloud-secret-key)
(define bucket "test-class/")

(define test-path "/home/jcollard/test")

(ls (string-append bucket "/"))

(define (upload-path path)
  (let ((all-files (list-all-sub-files path))
        (helper (lambda (file-path) (put/file (string-append bucket file-path) (string->path file-path)))))
    (map helper all-files)))

(define (list-all-sub-files path)
  (cond 
    [(is-file? path) '()]
    [(is-directory? path) 
     (let* ((files (map (lambda (x) (string-append path "/" x)) (list-files path)))
            (sub-dirs (map (lambda (x) (string-append path "/" x)) (sub-directories-of path)))
            (sub-contents (map list-all-sub-files sub-dirs)))
       (flatten (append files sub-contents)))]))
  
      
       ;(flatten (append files sub-contents)))]))
  

;(read-keys "/home/admiraledu/captain-teach")

;(ensure-have-keys)

;(define bucket-name "cmpsci220-f14")

;(define bucket-path "test")

;(define connection-uri (bucket&path->uri bucket-name bucket-path))


;(bucket+path->bucket&path&uri connection-uri)

;(define buckets (list-buckets))
;(if (not (member bucket-name buckets)) (create-bucket bucket-name) 'do-nothing)



;;(create-bucket "cmpsci220-fall2014-test")

;;(put/file "cmpsci220-fall2014-test/some/crazy/directory/output.txt" (string->path "/home/jcollard/output.txt"))
;;(ls "cmpsci220-fall2014/")
;;(delete-bucket "cmpsci220-fall2014-test")