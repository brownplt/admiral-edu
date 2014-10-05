#lang typed/racket

(require/typed xml
               [xexpr->string (XExpr -> String)])

(provide (all-from-out xml))

(provide XExpr->string)
(define XExpr->string xexpr->string)


(provide XExpr)
(define-type XExpr (Rec X
                        (U String 
                           Symbol 
                           Char
                           (Pairof Symbol (Pairof (Listof (List Symbol String)) (Listof X)))
                           (Pairof Symbol (Listof X)))))

(provide append-xexpr)
(: append-xexpr ((Pairof Symbol
                         (Pairof (Listof (List Symbol String))
                                 (Listof XExpr)))
                 (Listof XExpr) -> XExpr))
(define (append-xexpr outter inner)
  (let ([symbol : Symbol (first outter)]
        [attributes : (Listof (List Symbol String)) (second outter)]
        [current-inner : (Listof XExpr) (cddr outter)])
    ; FIXME: not sure why the type checker cannot solve this
    (cast (cons symbol (cons attributes (append current-inner inner))) XExpr)))

;; Does not type check to XExpr when last two are swapped
;(Pairof Symbol (Listof X)))))
;(Pairof Symbol (Pairof (Listof (List Symbol String)) (Listof X)))
;
;
;
;(: action-item (String XExpr -> (Pairof Symbol (Pairof (Listof (List Symbol String)) (Listof XExpr)))))
;(define (action-item url context)
 ; `(a ((href ,url)) ,context))