#lang racket

(require web-server/templates)

(provide get-response)
;(: get-response (String String -> (Listof Bytes)))
(define (get-response title body)
  (list (string->bytes/utf-8 (include-template "pages/html/plain.html"))))

