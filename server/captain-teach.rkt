#lang racket
(require web-server/servlet-dispatch
         web-server/web-server
         "dispatch.rkt"
         "base.rkt"
         "storage/storage-basic.rkt")

(let ((result (initialize)))
  (when (Failure? result) (error (format "Could not initialize system: ~a\n"))))

(storage-init)

(define stop
  (serve #:dispatch (dispatch/servlet ct-rules)
         #:port (ct-port)))

(print "Server Started. Type `stop` to kill the server.")
(newline)
(flush-output)

(define (block)
  (let ((input (read)))
    (if (equal? 'stop input) (stop) (block))))

(block)
