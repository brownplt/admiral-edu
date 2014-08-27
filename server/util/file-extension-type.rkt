#lang racket

(define extension-hash
  #hash(( "c" . "C")
        ( "cpp" . "C++")
        ( "cs" . "C#")
        ( "java" . "Java")
        ( "js" . "javascript")
        ( "json" . "json" )
        ( "md" . "Markdown")
        ( "arr" . "Pyret")
        ( "scala" . "Scala")
        ( "rkt" . "Scheme")
        ( "scm" . "Scheme")
        ( "yaml" . "YAML")))

(provide extension->file-type)
(define (extension->file-type ext)
  (if (hash-has-key? extension-hash (string-downcase ext))  (hash-ref extension-hash (string-downcase ext))
      "Markdown"))