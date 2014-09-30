#lang typed/racket

(require/typed xml
               [xexpr->string (XExpr -> String)])

(provide xexpr->string)

(provide XExpr)
(define-type XExpr (Rec X
                        (U String 
                           Symbol 
                           Char
                           (Pairof Symbol (Pairof (Listof (List Symbol String)) (Listof X)))
                           (Pairof Symbol (Listof X)))))

;; Does not type check to XExpr when last two are swapped
;(Pairof Symbol (Listof X)))))
;(Pairof Symbol (Pairof (Listof (List Symbol String)) (Listof X)))
;
;
;
;(: action-item (String XExpr -> (Pairof Symbol (Pairof (Listof (List Symbol String)) (Listof XExpr)))))
;(define (action-item url context)
 ; `(a ((href ,url)) ,context))