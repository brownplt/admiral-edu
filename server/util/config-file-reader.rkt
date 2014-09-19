#lang typed/racket

;; Given a Path-String and optionally a delimeter, parses the file line by line
;; Each line that contains the delimeter is parsed as a key (delim) value pair and
;; included in the returned HashTable. The first instance of the delimiter is used as a seperator
(provide read-conf)
(: read-conf (->* (Path-String) (String) (HashTable String String)))
(define (read-conf file-path [delim "="])
  (let*: ([contents : String  (file->string file-path)]
          [lines : (Listof String) (string-split contents "\n")]
          [splits : (Listof (Listof String)) (map (lambda: ([s : String]) (string-split s delim)) lines)]
          [len-2 : (Listof (Listof String)) (filter (lambda: ([s : (Listof String)]) (> (length s) 1)) splits)]
          [result : (Listof (Pairof String String)) (map (lambda: ([x : (Listof String)]) (cons (car x) (string-join (cdr x) delim))) len-2)])
    (make-hash result)))