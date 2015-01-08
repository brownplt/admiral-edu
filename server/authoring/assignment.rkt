#lang typed/racket

(require/typed yaml
               [string->yaml (String -> Assignment-YAML)])

(require/typed json
               [jsexpr->string (Any -> String)])

(require "../storage/storage.rkt"
         "assignment-structs.rkt"
         "assignment-parser.rkt"
         "../base.rkt")

(provide (all-from-out "assignment-structs.rkt"))

;; assignment-id -> void
;; Completely remove all trace of an assignment
(provide delete-assignment)
(: delete-assignment (String -> Void))
(define (delete-assignment assignment-id)
  (database:delete-assignment class-name assignment-id)
  (delete-path (string-append class-name "/" assignment-id)))


(: repeat-id? (All (A B) ((A -> B) -> ((Listof A) -> (U B #f)))))
(define (repeat-id? getter)
  (lambda (list)
    (cond [(null? list) #f]
          [else (repeats? (sort (map getter list) string<?))])))

(: repeats? (All (A) ((Listof A) -> (U A #f))))
(define (repeats? ls)
  (if (null? ls) #f
      (letrec ([helper : (A (Listof A) -> (U A #f))
                       (lambda (head tail)
                         (cond [(null? tail) #f]
                               [else (let ((next (car tail)))
                                       (cond [(equal? head next) head]
                                             [else (helper next (cdr tail))]))]))])
        (helper (car ls) (cdr ls)))))

(: validate-step (Step -> (U String #f)))
(define (validate-step step)
  (cond [(not (Step? step)) (raise-argument-error 'validate-step "Step" step)]
        [(validate-id (Step-id step)) (validate-id (Step-id step))]
        [else (let* ((reviews (Step-reviews step))
                     (ids (map Review-id reviews))
                     (sorted (sort ids string<?)))
                (let ((repeat (repeats? sorted))
                      ; FIXME: this filter should reduce (Listof (U String #f)) to just (Listof String) but the type checker
                      ; is unable to confirm this.
                      [invalid-ids : (Listof String) (cast (filter (lambda (x) x) (map validate-id ids)) (Listof String))])
                  (cond [repeat (string-append "Each review id must be unique for the step it is in. Found duplicate review id '" repeat "' in the step '" (Step-id step) "'.")]
                        [(not (null? invalid-ids)) (string-join invalid-ids "\n")]
                        [else #f])))]))

(: validate-id (String -> (U String #f)))
(define (validate-id id)
  (let ((try-match (regexp-match-exact? "[a-zA-Z0-9-]*" id)))
    (cond [try-match #f]
          [else "Id can only contain letters, numbers, and '-' characters. Rejected '" id "'."])))
  
(: validate-assignment (Assignment Boolean -> (U String #f)))
(define (validate-assignment assignment override)
  (cond [(not (Assignment? assignment)) (raise-argument-error 'validate-assignment "Assignment" assignment)]
        [(and (not override) (assignment:exists? (Assignment-id assignment) class-name)) (string-append "The specified assignment id '" (Assignment-id assignment) "' already exists.")]
        [else (let* ((check-steps ((repeat-id? Step-id) (Assignment-steps assignment)))
                     ; FIXME: this filter should reduce (Listof (U String #f)) to just (Listof String) but the type checker
                     ; is unable to confirm this.
                     [check-review-ids : (Listof String) (cast (filter (lambda (x) x) 
                                                                       (map (repeat-id? Review-id) 
                                                                            (map Step-reviews (Assignment-steps assignment)))) (Listof String))]
                     (valid-id (validate-id (Assignment-id assignment)))
                     (ids (append (map Step-id (Assignment-steps assignment)) (apply append (map (lambda ([step : Step]) (map Review-id (Step-reviews step))) (Assignment-steps assignment)))))
                     ; FIXME: this filter should reduce (Listof (U String #f)) to just (Listof String) but the type checker
                      ; is unable to confirm this.
                     [valid-step-ids : (Listof String) (cast 
                                                        (filter (lambda (x) (not (eq? #f x))) (map validate-id ids)) (Listof String))])
                (cond [valid-id valid-id]
                      [(not (null? check-review-ids)) (string-append "Found duplicate review-ids: " (string-join check-review-ids ", "))]
                      [check-steps (string-append "Assignment may not have multiple steps with the same id. Found multiple instances of '" check-steps "'")]
                      [(not (null? valid-step-ids)) (string-join valid-step-ids "")]
                      [else #f]))]))


(provide yaml-bytes->create-assignment)
(: yaml-bytes->create-assignment (Bytes -> (U String #t)))
(define (yaml-bytes->create-assignment bytes)
  (let ((yaml-string (bytes->string/utf-8 bytes)))
    (let ((yaml (with-handlers ([exn:fail? could-not-parse]) (string->yaml yaml-string))))
      (cond [(Failure? yaml) (Failure-message yaml)]
            [else (let ((assignment (with-handlers ([exn:fail? invalid-yaml]) (yaml->assignment yaml))))
                    (cond [(Failure? assignment) (Failure-message assignment)]
                          [else (let ((result (create-assignment assignment)))                                  
                                  (cond [(eq? #t result) (save-assignment-description class-name (Assignment-id assignment) yaml-string) "Success"]
                                        [else result]))]))]))))


(provide yaml-bytes->save-assignment)
(: yaml-bytes->save-assignment (Bytes -> (U String #t)))
(define (yaml-bytes->save-assignment bytes)
  (let ((yaml-string (bytes->string/utf-8 bytes)))
    (let ((yaml (with-handlers ([exn:fail? could-not-parse]) (string->yaml yaml-string))))
      (cond [(Failure? yaml) (Failure-message yaml)]
            [else (let ((assignment (with-handlers ([exn:fail? invalid-yaml]) (yaml->assignment yaml))))
                    (cond [(Failure? assignment) (Failure-message assignment)]
                          [else (let ((result (save-assignment assignment)))                                  
                                  (cond [(eq? #t result) (save-assignment-description class-name (Assignment-id assignment) yaml-string) "Success"]
                                        [else result]))]))]))))



(provide create-assignment)
(: create-assignment (Assignment -> (U String #t)))
(define (create-assignment assignment)
  (cond [(not (Assignment? assignment)) (raise-argument-error 'create-assignment "Assignment" assignment)]
        [else (let ((validation (validate-assignment assignment #f)))
                (cond [validation validation]
                      [else (create-database-entries assignment)
                            (create-base-rubrics assignment)
                            (check-no-reviews assignment)
                            #t]))]))


;; TODO: check to see if assignment has the same name as before
(: save-assignment (Assignment -> (U String #t)))
(define (save-assignment assignment)
  (cond [(not (Assignment? assignment)) (raise-argument-error 'save-assignment "Assignment" assignment)]
        [else (let ((validation (validate-assignment assignment #t)))
                (cond [validation validation]
                      [else ;; (create-database-entries assignment)
                            (create-base-rubrics assignment)
                            (check-no-reviews assignment)
                            #t]))]))


(: check-no-reviews (Assignment -> Void))
(define (check-no-reviews assignment)
  (let ((no-reviews (null? (filter no-reviews? (Assignment-steps assignment))))
        (assignment-id (Assignment-id assignment)))
    (if no-reviews (assignment:mark-ready assignment-id class-name) (assignment:mark-not-ready assignment-id class-name))))


(: no-reviews? (Step -> Boolean))
(define (no-reviews? step)
  (not (null? (Step-reviews step))))
    
(: create-database-entries (Assignment -> (U #t 'no-such-class 'duplicate-assignment)))
(define (create-database-entries assignment)
  (let ((id (Assignment-id assignment)))
    (assignment:create id class-name)))


(: create-base-rubrics (Assignment -> Void))
(define (create-base-rubrics assignment)
  (let* ((class class-name)
         (assign (Assignment-id assignment))
         (steps (Assignment-steps assignment))
         (create (lambda ([step : Step]) 
                   (let* ((stepName (Step-id step))
                          (reviews (Step-reviews step))
                          (create (lambda: ([review : Review])
                                    (let ((review-id (Review-id review))
                                          (rubric (jsexpr->string (rubric->json (Review-rubric review)))))
                                      (create-default-rubric class assign stepName rubric review-id)))))
                     (map create reviews)))))
    (map create steps)
    (void)))

(provide next-step)
(: next-step (String String -> (U MustReviewNext MustSubmitNext #t)))
(define (next-step assignment-id uid)
  (let* ((assignment (yaml->assignment (string->yaml (retrieve-assignment-description class-name assignment-id))))
         (handler (Assignment-assignment-handler assignment))
         (next-action (AssignmentHandler-next-action handler)))
    (next-action assignment (Assignment-steps assignment) uid)))


;; Attempts to publish for the specified uid, assignment, and step-id. If this is not the next expected action,
;; This returns a failure with a message describing what the user should do next.
;; If file-name and data are supplied and not #f, it is uploaded as a submission before creating a database record
(provide submit-step)
(: submit-step (->* (String String String) ((U String #f) (U Bytes #f)) (Result String)))
(define (submit-step assignment-id step-id uid [file-name #f] [data #f])
  ;; Assignment must exist
  (cond 
    [(not (assignment:exists? assignment-id class-name)) (failure "The specified assignment '" assignment-id "' does not exists.")]
    [else (let* ((assignment (yaml->assignment (string->yaml (retrieve-assignment-description class-name assignment-id))))
                 (steps (Assignment-steps assignment))
                 (handler (Assignment-assignment-handler assignment))
                 (next-action (AssignmentHandler-next-action handler)) 
                 (next (next-action assignment steps uid))
                 (do-submit-step (AssignmentHandler-do-submit-step handler))) 
            (cond
              [(and (MustSubmitNext? next) 
                    (equal? (Step-id (MustSubmitNext-step next)) step-id)) (do-submit-step assignment (step-id->step assignment-id step-id) uid file-name data steps)]
              [else (failure "Could not submit to the step '" step-id "'." (next-action-error next))]))]))


(: next-action-error ((U MustSubmitNext MustReviewNext #t) -> String))
(define (next-action-error next)
  (cond [(MustSubmitNext? next) (string-append "Your next action is to submit to on '" (Step-id (MustSubmitNext-step next)) "'.")]
        [(MustReviewNext? next) (string-append "Your next action is to complete reviews for '" (Step-id (MustReviewNext-step next)) "'.")]
        [(eq? #t next) "You have completed this assignment."]
        [else ""]))


(provide assignment-id->assignment-dependencies)
(: assignment-id->assignment-dependencies (String -> (Listof Dependency)))
(define (assignment-id->assignment-dependencies id)
  (let* ((assignment (assignment-id->assignment id))
         (handler (Assignment-assignment-handler assignment))
         (get-deps-f (AssignmentHandler-get-dependencies handler)))
    (get-deps-f assignment)))


(provide handle-dependency)
(: handle-dependency (String Dependency (Listof (Pairof Symbol (U String Bytes))) (Listof Any) -> (Result String)))
(define (handle-dependency assignment-id dependency bindings raw-bindings)
  (let* ((assignment (assignment-id->assignment assignment-id))
         (handler (Assignment-assignment-handler assignment))
         (take-dependency (AssignmentHandler-take-dependency handler)))
    (take-dependency assignment-id dependency bindings raw-bindings)))
         

(provide find-dependencies)
(: find-dependencies (String String String -> (Listof Dependency)))
(define (find-dependencies assignment-id step-id review-id)
  (let ((deps (assignment-id->assignment-dependencies assignment-id))
        (f (lambda (dep) (and (review-dependency? dep) 
                              (equal? (review-dependency-step-id dep) step-id) 
                              (equal? (review-dependency-review-id dep) review-id)))))
    (filter f deps)))


(provide check-ready)
(: check-ready (String -> (U Void #f)))
(define (check-ready assignment-id)
  (let ((deps (assignment-id->assignment-dependencies assignment-id))
        (filter-function (lambda ([dep : Dependency]) (not (dependency-met dep)))))
    (if (null? (filter filter-function deps)) (assignment:mark-ready assignment-id class-name) #f)))


(: assignment-id->assignment (String -> Assignment))
(provide assignment-id->assignment)
(define (assignment-id->assignment id)
  (yaml->assignment (string->yaml (retrieve-assignment-description class-name id))))


(: step-id->step (String String -> Step))
(define (step-id->step assignment-id step-id)
  (lookup-step (assignment-id->assignment assignment-id) step-id))

(: lookup-step (Assignment String -> Step))
(define (lookup-step assignment step-id)
  (let* ((steps (Assignment-steps assignment))
         (filter-f (lambda ([step : Step]) (equal? step-id (Step-id step))))
         (result (filter filter-f steps)))
    (car result)))

