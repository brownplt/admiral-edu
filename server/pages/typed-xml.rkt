#lang typed/racket

(provide XExpr)
(define-type XExpr (Rec X
                        (U String 
                           Symbol 
                           Char
                           (Pairof Symbol (Pairof (Listof (List Symbol String)) (Listof X)))
                           (Pairof Symbol (Listof X)))))