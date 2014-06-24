#lang typed/racket

(require "machine.rkt")

(: make-available! (User Course Assignment Step Archive DB -> AvailableForReview))
(define (make-available! student course assignment step submission db)
  (AvailableForReview student course assignment step submission))

(: select-review! (Step DB -> (Values User Review)))
(define (select-review! step db)
  (values
   (User "undefined")
   (Review)))

(: accept-review! (User User Course Assignment Step Review -> AcceptReview))
(define (accept-review! reviewer reviewee course bheap bheap-tests reviewdata)
  (AcceptReview reviewer reviewee course bheap bheap-tests reviewdata))

(: submission-step (Assignment Step -> User Course Archive Time DB -> (U (Pairof State (Listof Output)) String)))
(define (submission-step assignment step)
  (lambda (student course submission time db)
    (let*-values (((reviewee reviewdata) (select-review! bheap-tests db))
                ((next-step) (MustReviewNext student course assignment step reviewee))
                ((make-available) (make-available! student course assignment step submission db))
                ((get-review) (AssignReview student reviewee course assignment step reviewdata))
                ((outputs) (list make-available get-review)))
    (cons next-step outputs))))

(: review-step (Assignment Step (User Course -> State) -> User Course User Review Time DB -> (U (Pairof State (Listof Output)) String)))
(define (review-step assignment step next-step)
  (lambda (reviewer course reviewee reviewdata time db)
    (let* ((next (next-step reviewer course))
           (accept (accept-review! reviewer reviewee course assignment step reviewdata))
           (outputs (list accept)))
      (cons next outputs))))

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

(: sample-assignment (State Input Time DB -> (U (Pairof State (Listof Output)) String)))
(define (sample-assignment state input time db)
  (match (cons state input)
    [(cons (MustSubmitNext student course (Assignment "bheap") (Step "tests")) (SubmitStep submission)) (submit-tests student course submission time db)]
    [(cons (MustReviewNext student course (Assignment "bheap") (Step "tests") reviewee) (SubmitReview reviewdata)) (review-tests student course reviewee reviewdata time db)]
    [(cons (MustSubmitNext student course (Assignment "bheap") (Step "implementation")) (SubmitStep submission)) (submit-implementation student course submission time db)]
    [(cons (MustReviewNext student course (Assignment "bheap") (Step "implementation") reviewee) (SubmitReview reviewdata)) (review-implementation student course reviewee reviewdata time db)]
    [(cons (GraderMustGradeNext student course (Assignment "bheap") (Step "implementation")) (GraderReview grader reviewdata)) (grade-implementation student course grader reviewdata db)]
    [else "Illegal State"]))

(: submit-tests (User Course Archive Time DB -> (U (Pairof State (Listof Output)) String)))
(define submit-tests (submission-step bheap bheap-tests))

(: review-tests (User Course User Review Time DB -> (U (Pairof State (Listof Output)) String)))
(define review-tests
  (let ((next-step (lambda ([user : User] [course : Course]) (MustSubmitNext user course bheap bheap-implementation))))
    (review-step bheap bheap-implementation next-step)))

(: submit-implementation (User Course Archive Time DB -> (U (Pairof State (Listof Output)) String)))
(define submit-implementation (submission-step bheap bheap-implementation))

(: review-implementation (User Course User Review Time DB -> (U (Pairof State (Listof Output)) String)))
(define review-implementation
  (let ((next-step (lambda ([user : User] [course : Course]) (GraderMustGradeNext user course bheap bheap-implementation))))
    (review-step bheap bheap-implementation next-step)))

(: grade-implementation (User Course User Review DB -> (U (Pairof State (Listof Output)) String)))
(define (grade-implementation student course grader reviewdata db)
  "Not yet implemented.")