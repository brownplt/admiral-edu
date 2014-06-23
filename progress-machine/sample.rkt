#lang typed/racket

(require "machine.rkt")

(struct: (a) Left ([v : a]) #:transparent)
(struct: (a) Right ([v : a]) #:transparent)
(define-type (Either a b) (U (Left a) (Right b)))

(struct: Nothing () #:transparent)
(struct: (a) Just ([v : a]) #:transparent)
(define-type (Maybe a) (U Nothing (Just a)))

(: get-symbol (Any -> (Maybe Symbol)))
(define (get-symbol struct)
  (let ((sym (object-name struct)))
    (if (symbol? sym) (Just sym) (Nothing))))

(struct: Time ([time : Number]))
(struct: DB ([data : Any]))

(define bheap (Assignment "bheap"))
(define bheap-tests (Step "tests"))
(define bheap-implementation (Step "implementation"))

(: sample-assignment (State Input Time DB -> (Either (Pair State (Listof Output)) String)))
(define (sample-assignment state input time db)
  (match `(,state . ,input)
    [`(,(MustSubmitNext student course bheap bheap-tests) . ,(SubmitStep submission)) (submit-tests student course bheap bheap-tests submission time db)]
    [else (Right "Illegal State")]))

(: submit-tests (User Course Assignment Step Archive Time DB -> (Either (Pair State (Listof Output)) String)))
(define (submit-tests student course assignment step submission time db) 
  (let*-values (((reviewee reviewdata) (select-review! bheap-tests db))
                ((next-step) (MustReviewNext student course assignment reviewee))
                ((make-available) (make-available! student course assignment step submission db))
                ((get-review) (AssignReview student reviewee course assignment step reviewdata))
                ((outputs) (list make-available get-review)))
    (Left (pair next-step outputs))))


(define (pair a b)
  `(,a . ,b))

(: make-available! (User Course Assignment Step Archive DB -> AvailableForReview))
(define (make-available! student course assignment step submission db)
  (AvailableForReview student course assignment step submission))

(: select-review! (Step DB -> (Values User Review)))
(define (select-review! step db)
  (values
   (User "undefined")
   (Review)))

(: review-tests (Input Time DB -> (Either (Pair State (Listof Output)) String)))
(define (review-tests input itme db)
  (Right "Not yet implemented."))

(: submit-implementation (Input Time DB -> (Either (Pair State (Listof Output)) String)))
(define (submit-implementation input time db)
  (Right "Not yet implemented"))

(: review-implementation (Input Time DB -> (Either (Pair State (Listof Output)) String)))
(define (review-implementation input itme db)
  (Right "Not yet implemented."))