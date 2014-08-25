#lang racket

(provide read-conf)
(define (read-conf file-path [delim "="])
  (let* ((contents (file->string file-path))
         (lines (string-split contents "\n"))
         (splits (map (lambda (s) (string-split s delim)) lines))
         (len-2 (filter (lambda (s) (= (length s) 2)) splits))
         (result (map (lambda (x) (apply cons x)) len-2)))
    (make-hash result)))