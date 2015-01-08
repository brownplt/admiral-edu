#lang typed/racket

(require/typed yaml
               [string->yaml (String -> (HashTable String Any))])

(define-type YAML-ATOM (U 'null
                          Boolean
                          String
                          Integer
                          Inexact-Real
                          Bytes
                          date))

(provide YAML)
(define-type YAML (Rec Y (U YAML-ATOM
                            (Pairof Y Y)
                            (Listof Y)
                            (HashTable YAML-ATOM Y)
                            (Setof Y))))

                     ;(Listof YAML)
                     ;(HashTable YAML YAML)
                     ;(Setof YAML)))

(provide string->yaml)