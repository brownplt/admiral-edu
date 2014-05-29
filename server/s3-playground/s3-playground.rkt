#lang racket
(require (planet gh/aws:1:5))

(read-keys "/home/admiraledu/captain-teach")

(ensure-have-keys)

(define bucket-name "cmpsci220-f14")

(define bucket-path "test")

(define connection-uri (bucket&path->uri bucket-name bucket-path))


(bucket+path->bucket&path&uri connection-uri)

(define buckets (list-buckets))
(if (not (member bucket-name buckets)) (create-bucket bucket-name) 'do-nothing)



;;(create-bucket "cmpsci220-fall2014-test")

;;(put/file "cmpsci220-fall2014-test/some/crazy/directory/output.txt" (string->path "/home/jcollard/output.txt"))
;;(ls "cmpsci220-fall2014/")
;;(delete-bucket "cmpsci220-fall2014-test")