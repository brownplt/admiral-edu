#lang typed/racket
(struct: step-spec ((generate-request : (-> subreq))( check-response : (Any -> (U response-not-ok response-ok)))) #:transparent)

(struct: response-not-ok ((reason : String)) #:transparent)

(struct: response-ok ((maybe-next-step : (U (some step-spec) none))) #:transparent)

(define-type Expr (U e-sequence e-submission e-step-spec))
(struct: e-sequence ((start : Expr) (assign-parts : (Listof Expr))) #:transparent)
(struct: e-submission ((name : String) (form : Any) (validator : (Any Any -> Boolean))) #:transparent)
(struct: e-step-spec ((step : step-spec)) #:transparent)
(struct: subreq ((name : String) (form : Any)) #:transparent)
(struct: none () #:transparent)
(struct: (A)some ((thing : A)) #:transparent)

; interp :: 
(define: (interp (assign-part : Expr)) : step-spec 
  (match assign-part
    [(e-sequence start assign-parts) (handle-sequence start assign-parts) ]
    [(e-submission name form validator) (handle-submission name form validator)]
    [(e-step-spec spec) spec]))

(define: (handle-sequence (start : Expr) (assign-parts : (Listof Expr))) : step-spec
  (let [(firststep (interp start))]
   (step-spec
    (step-spec-generate-request firststep)
    (lambda (resp)
      (define start-resp ((step-spec-check-response firststep) resp))
      (match start-resp
        [(response-not-ok _) start-resp]
        [(response-ok next-step)
         (match next-step
           [(none) (response-ok 
                    (match assign-parts
                      [(cons h tail) (some (interp (e-sequence h tail)))]
                      [empty (none)]))]
           [(some step) (response-ok (some (interp (e-sequence (e-step-spec step) assign-parts))))])])))))



(define: (handle-submission (name : String) (form : Any) (validator : (Any Any -> Boolean))) : step-spec
  (step-spec
   (lambda ()
     (subreq name form))
  (lambda (resp)
    (if (validator form resp)
        (response-ok (none))
        (response-not-ok "Invalid submission"
        )))))

(define test-submission-validator
  (lambda (form resp) #t))

(define impl-submission-validator
  (lambda (form resp) #t))

(define assign1
  (e-sequence
      (e-submission "test-submission" 'test-submission-form test-submission-validator)
      `(,(e-submission "impl-submission" 'impl-submission-form impl-submission-validator))))


  
