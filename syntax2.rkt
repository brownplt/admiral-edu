#lang typed/racket

(define-type Step (U step-spec finished))
(struct: step-spec ((generate-request : (-> Request)) 
                    (check-response : (Form -> ServerResponse))) #:transparent)
(struct: finished () #:transparent)

(define-type ServerResponse (U response-fail response-success response-next))
(struct: response-success ((feedback : (Option String))) #:transparent)
(struct: response-fail ((message : String)) #:transparent)
(struct: response-next ((feedback : (Option String)) (next-step : Step)) #:transparent)

(define-type Expr (U e-sequence e-submission e-step e-review-task e-finished))

(struct: e-sequence ((start : Expr) 
                     (assign-parts : (Listof Expr))) #:transparent)

(struct: e-submission ((name : String) 
                       (form : Form) 
                       (validator : (Response -> Feedback))) #:transparent)

(struct: e-step ((step : Step)) #:transparent)

(struct: e-review-task ((target : String) 
                        (find-resource : (-> Resource))
                        (rubric : Rubric) 
                        (feedback : (RubricResponse -> Feedback))) #:transparent)

(struct: e-finished ())

(define-type Request (U submission-request review-request))
(struct: submission-request ((name : String) (form : Form)) #:transparent)
(struct: review-request ((resource : Resource) (rubric : Rubric)) #:transparent)

(define-type (Option A) (U (some A) none))
(struct: none () #:transparent)
(struct: (A)some ((thing : A)) #:transparent)

(struct: fail ((message : String)) #:transparent)
(struct: success ((feedback : (Option String))) #:transparent)
  
(define-type Feedback (U fail success))
(define-type Form Any)
(define-type Response Any)
(define-type Resource Any)
(define-type Rubric Any)
(define-type RubricResponse Any)

; interp :: 
(define: (interp (assign-part : Expr)) : Step 
  (match assign-part
    [(e-sequence start assign-parts) (handle-sequence start assign-parts) ]
    [(e-submission name form validator) (handle-submission name form validator)]
    [(e-step spec) spec]
    [(e-review-task target find-resource rubric feedback) (handle-review-task target find-resource rubric feedback)]))

(define: (handle-sequence (start : Expr) (assign-parts : (Listof Expr))) : Step
  (let ((firststep (interp start)))
    (match firststep
      [(finished) (finished)]
      [(step-spec request start-response)
       (step-spec request
        (lambda (resp)
          (match (start-response resp)
            [(response-fail message) (response-fail message)]
            [(response-success feedback) 
             (response-next feedback (match assign-parts
                                       [(cons h tail) (interp (e-sequence h tail))]
                                       [empty (interp (e-finished))]))]
            [(response-next feedback step) (response-next feedback (interp (e-sequence (e-step step) assign-parts)))])))])))



(define: (handle-submission (name : String) (form : Form) (validator : (Response -> Feedback))) : step-spec
  (step-spec
   (lambda ()
     (submission-request name form))
  (lambda (resp)
    (match (validator resp)
      [(success feedback) (response-success feedback)]
      [(fail message) (response-fail message)]))))

(define: (handle-review-task (target : String) (find-resource : (-> Resource)) (rubric : Rubric) (feedback : (RubricResponse -> Feedback))) : step-spec
  (step-spec
   (lambda ()
     (review-request (find-resource) rubric))
   (lambda (resp) 
     (match (feedback resp)
       [(success feedback) (response-success feedback)]
       [(fail message) (response-fail message)]))))

(define test-submission-validator
  (lambda (resp) (success (some "Successfully validated."))))

(define impl-submission-validator
  (lambda (resp) (fail "Implementaiton was invalid.")))

(define assign1
  (e-sequence
      (e-submission "test-submission" 'test-submission-form test-submission-validator)
      `(,(e-submission "impl-submission" 'impl-submission-form impl-submission-validator))))

(define-type Trace (U trace-request trace-feedback success fail))
(struct: trace-request ((request : Request) (next : Trace)) #:transparent)
(struct: trace-feedback ((feedback : String) (next : Trace)) #:transparent)

(define: (run (step : Step)) : Trace
  (match step
    [(finished) (success (none))]
    [(step-spec generate-request check-response)
     (let ((request (generate-request))
           (check (check-response 'some_response)))
       (trace-request request
                      (match check        
                        [(response-fail reason) (fail reason)]
                        [(response-success feedback) (success feedback)]
                        [(response-next (none) next-spec) (run next-spec)]
                        [(response-next (some feedback) next-spec) (trace-feedback feedback (run next-spec))])))]))
