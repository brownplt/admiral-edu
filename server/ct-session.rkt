#lang typed/racket

(require "util/basic-types.rkt")

;; Captain Teach Session information
(provide (struct-out ct-session))
(struct: ct-session ((class : String) (uid : String) (table : (HashTable Symbol String))) #:transparent)


(provide Order)
(define-type Order (U 'asc 'desc))

; ct-session -> Order
; If the session has dir specified and it is 'asc or 'desc returns that dir
; otherwise returns 'asc.
;(provide get-dir)
(: get-order (ct-session -> Order))
(provide get-order)
(define (get-order session)
  (let ((table (ct-session-table session)))
    (cond [(not (hash-has-key? table 'order)) 'asc]
          [else (let ((dir-string (hash-ref table 'order)))
                  (match dir-string
                    ["asc" 'asc]
                    ["desc" 'desc]
                    [else 'asc]))])))

(: opposite-order (Order -> Order))
(provide opposite-order)
(define (opposite-order order)
  (match order
    ['asc 'desc]
    ['desc 'asc]))

(: clean-bindings ((Listof Any) -> (Listof (Pairof Symbol String))))
(define (clean-bindings ls)
  (match ls
    ['() '()]
    [(cons `(,symbol . ,string) tail) (cond [(and (symbol? symbol) (string? string)) (cons `(,symbol . ,string) (clean-bindings tail))]
                                            [else (clean-bindings tail)])]))

(: okay-binding? ((Pairof Symbol Any) -> Boolean))
(define (okay-binding? pair)
  (let ((symbol (car pair)))
    (cond [(eq? symbol 'sort-by) #t]
          [(eq? symbol 'order) #t]
          [(eq? symbol 'review-hash) #t]
          [(eq? symbol 'action) #t]
          [else #f])))

(provide get-binding)
(: get-binding (Symbol ct-session -> (Result String)))
(define (get-binding binding session)
  (let ((table (ct-session-table session)))
    (cond [(not (hash-has-key? table binding)) (Failure (format "No binding found: ~a" binding))]
          [else (Success (hash-ref table binding))])))

;(: make-table (String (Listof (Pairof Symbol String)) -> (HashTable Symbol String)))
(provide make-table)
(: make-table (String (Listof Any) -> (HashTable Symbol String)))
(define (make-table start-rel-url bindings)
  (let ((pairs (cons `(start-url . ,start-rel-url) (filter okay-binding? (clean-bindings bindings)))))
    (make-hash pairs)))
          
;  (let ((args (append (list 'start-url start-rel-url) (filter (compose not reserved?) (flatten bindings)))))
;    (apply hash args)))

(: flatten (All (a b) ((Listof (Pairof a b)) -> (Listof (U a b)))))
(define (flatten ls)
  (match ls
    ['() '()]
    [(cons `(,a . ,b) tail) (cons a (cons b (flatten tail)))]))