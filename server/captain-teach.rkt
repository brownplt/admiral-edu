#lang racket
(require web-server/servlet-dispatch
         web-server/web-server
         "dispatch.rkt"
         "config.rkt")

(define stop
  (serve #:dispatch (dispatch/servlet ct-rules)
         #:port ct-port))

(print "Server Started. Type `stop` to kill the server.")
(newline)

(define (block)
  (let ((input (read)))
    (if (equal? 'stop input) (stop) (block))))

(block)