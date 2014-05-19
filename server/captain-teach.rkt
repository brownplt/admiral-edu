#lang racket
(require web-server/servlet-dispatch
         web-server/web-server)

;; This is the configuration file for cs220.cs.umass.edu
;; It provides a function ct-rules which tells the server how to dispatch incomming requests
;; and ct-port specifies the port to launch the web server
(require "cmpsci220/cmpsci220.rkt")

(define stop
  (serve #:dispatch (dispatch/servlet ct-rules)
         #:port ct-port))

(print "Server Started. Type `stop` to kill the server.")
(newline)

(define (block)
  (let ((input (read)))
    (if (equal? 'stop input) (stop) (block))))

(block)