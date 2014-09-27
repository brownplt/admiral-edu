#lang typed/racket

(provide (all-defined-out))

(: ors ((Listof Boolean) -> Boolean))
(define (ors els) 
  (match els
    ['() #f]
    [(cons head tail) (if head #t (ors tail))]))


(: ands ((Listof Boolean) -> Boolean))
(define (ands els) 
  (foldr (lambda: ([x : Boolean] [y : Boolean]) (and x y)) #t els))


(: hash-has-keys? (All (A B) ((HashTable A B) A * -> Boolean)))
(define (hash-has-keys? hash . els)
  (ands (map (lambda (x) (hash-has-key? hash x)) els)))