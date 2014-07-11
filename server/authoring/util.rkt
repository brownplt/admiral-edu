#lang racket

(provide (all-defined-out))

(define (ors els) 
  (cond
    [(not ((listof boolean?) els)) (raise-argument-error 'ors "non-empty-listof boolean?" els)]
    [else (match els
            ['() #f]
            [(cons head tail) (if head #t (ors tail))])]))

(define (ands els) (foldr (lambda (x y) (and x y)) #t els))


(define (hash-has-keys? hash . els)
  (ands (map (lambda (x) (hash-has-key? hash x)) els)))