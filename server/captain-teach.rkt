#lang racket
(require web-server/servlet
         web-server/servlet-dispatch
         web-server/web-server
         web-server/dispatch
         "cmpsci220/cmpsci220.rkt")

(serve #:dispatch (dispatch/servlet start)
       #:port 8080)

(define-values (in out) (make-pipe))

(read-line in)
