#lang racket

(provide (all-from-out "assignment-structs.rkt"))

(require (planet esilkensen/yaml:3:1)
         json
         "assignment-structs.rkt"
         "assignment-parser.rkt"
         "assignment-dependencies.rkt"
         "util.rkt"
         "../base.rkt")


(define (repeat-id? getter)
  (lambda (list)
    (cond [(null? list) #f]
          [else (repeats? (sort (map getter list) string<?))])))

(define (repeats? ls)
  (if (null? ls) #f
      (letrec ((helper (lambda (head tail)
                         (cond [(null? tail) #f]
                               [else (let ((next (car tail)))
                                       (cond [(equal? head next) head]
                                             [else (helper next (cdr tail))]))]))))
        (helper (car ls) (cdr ls)))))

(define (validate-step step)
  (cond [(not (Step? step)) (raise-argument-error 'validate-step "Step" step)]
        [(validate-id (Step-id step)) (validate-id (Step-id step))]
        [else (let* ((reviews (Step-reviews step))
                     (ids (map Review-id reviews))
                     (sorted (sort ids string<?)))
                (let ((repeat (repeats? sorted))
                      (invalid-ids (filter (lambda (x) x) (map validate-id ids))))
                  (cond [repeat (string-append "Each review id must be unique for the step it is in. Found duplicate review id '" repeat "' in the step '" (Step-id step) "'.")]
                        [(not (null? invalid-ids)) (string-join invalid-ids "\n")]
                        [else #f])))]))

(define (validate-id id)
  (let ((try-match (regexp-match "[a-zA-Z0-9-]*" id)))
    (cond [(null? try-match) "Id can only contain letters, numbers, and '-' characters. Rejected '" id "'."]
          [(equal? (car try-match) id) #f]
          [else (let* ((drop-amount (string-length (car try-match)))
                       (illegal-char (substring id drop-amount (+ drop-amount 1))))
                  (string-append "Id can only contain letters, numbers, and '-' characters. Rejected '" id "' because it contained '" illegal-char "'."))])))
        
(define (validate-assignment assignment override)
  (cond [(not (Assignment? assignment)) (raise-argument-error 'validate-assignment "Assignment" assignment)]
        [(and (not override) (assignment:exists? (Assignment-id assignment) class-name)) (string-append "The specified assignment id '" (Assignment-id assignment) "' already exists.")]
        [else (let* ((check-steps ((repeat-id? Step-id) (Assignment-steps assignment)))
                     (check-review-ids (filter (lambda (x) x) (map (repeat-id? Review-id) (map Step-reviews (Assignment-steps assignment)))))
                     (valid-id (validate-id (Assignment-id assignment)))
                     (ids (append (map Step-id (Assignment-steps assignment)) (flatten (map (lambda (step) (map Review-id (Step-reviews step))) (Assignment-steps assignment)))))
                     (valid-step-ids (filter (lambda (x) (not (eq? #f x))) (map validate-id ids))))
                (cond [valid-id valid-id]
                      [(not (null? check-review-ids)) (string-append "Found duplicate review-ids: " (string-join check-review-ids ", "))]
                      [check-steps (string-append "Assignment may not have multiple steps with the same id. Found multiple instances of '" check-steps "'")]
                      [(not (null? valid-step-ids)) (string-join valid-step-ids "")]
                      [else #f]))]))

;;(with-handlers ([exn:fail? could-not-create-user]) (create-new-user (ct-session-class session) new-uid new-role))
(provide yaml-bytes->create-assignment)
(define (yaml-bytes->create-assignment bytes)
  (let ((yaml-string (bytes->string/utf-8 bytes)))
    (let ((yaml (with-handlers ([exn:fail? could-not-parse]) (string->yaml yaml-string))))
      (cond [(Failure? yaml) (Failure-message yaml)]
            [else (let ((assignment (with-handlers ([exn:fail? invalid-yaml]) (yaml->assignment yaml))))
                    (cond [(Failure? assignment) (Failure-message assignment)]
                          [else (let ((result (create-assignment assignment)))                                  
                                  (cond [(eq? #t result) (save-assignment-description class-name (Assignment-id assignment) yaml-string) "Success"]
                                        [else result]))]))]))))

;;(with-handlers ([exn:fail? could-not-create-user]) (create-new-user (ct-session-class session) new-uid new-role))
(provide yaml-bytes->save-assignment)
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
(define (create-assignment assignment)
  (cond [(not (Assignment? assignment)) (raise-argument-error 'create-assignment "Assignment" assignment)]
        [else (let ((validation (validate-assignment assignment #f)))
                (cond [validation validation]
                      [else (create-database-entries assignment)
                            (create-base-rubrics assignment)
                            (check-no-reviews assignment) #t]))]))

;; TODO: check to see if assignment has the same name as before
(define (save-assignment assignment)
  (cond [(not (Assignment? assignment)) (raise-argument-error 'save-assignment "Assignment" assignment)]
        [else (let ((validation (validate-assignment assignment #t)))
                (cond [validation validation]
                      [else ;; (create-database-entries assignment)
                            (create-base-rubrics assignment)
                            (check-no-reviews assignment) #t]))]))

(define (check-no-reviews assignment)
  (let ((no-reviews (null? (filter no-reviews? (Assignment-steps assignment))))
        (assignment-id (Assignment-id assignment)))
    (if no-reviews (assignment:mark-ready assignment-id class-name) (assignment:mark-not-ready assignment-id class-name))))

(define (no-reviews? step)
  (not (null? (Step-reviews step))))
    

(define (create-database-entries assignment)
  (let ((id (Assignment-id assignment)))
    (assignment:create id class-name)))

;;(create-default-rubric class assignment stepName rubric review-id)

(define (create-base-rubrics assignment)
  (let* ((class class-name)
         (assign (Assignment-id assignment))
         (steps (Assignment-steps assignment))
         (create (lambda (step) 
                   (let* ((stepName (Step-id step))
                          (reviews (Step-reviews step))
                          (create (lambda (review)
                                    (let ((review-id (Review-id review))
                                          (rubric (jsexpr->string (rubric->json (Review-rubric review)))))
                                      (create-default-rubric class assign stepName rubric review-id)))))
                     (map create reviews)))))
    (map create steps)))

(define (assignment->progress-machine assignment)
  (cond [(Assignment? assignment) (raise-argument-error 'assignment->progress-machine "Assignment" assignment)]
        [else (let ((steps (Assignment-steps assignment)))
                #f)]))

(provide next-step)
(define (next-step assignment-id uid)
  (let* ((assignment (yaml->assignment (string->yaml (retrieve-assignment-description class-name assignment-id))))
         (handler (Assignment-assignment-handler assignment))
         (next-action (AssignmentHandler-next-action handler)))
    (next-action assignment (Assignment-steps assignment) uid)))

;; Attempts to submit for the specified uid, assignment, and step-id. If this is not the next expected action,
;; This returns a failure with a message describing what the user should do next.
(provide submit-step)
(define (submit-step assignment-id step-id uid file-name data)
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
              [(and (MustSubmitNext? next) (equal? (Step-id (MustSubmitNext-step next)) step-id)) (do-submit-step assignment (step-id->step assignment-id step-id) uid file-name data steps)]
              [else (failure "Could not submit to the step '" step-id "'." (next-action-error next))]))]))


(define (next-action-error next)
  (cond [(MustSubmitNext? next) (string-append "Your next action is to submit to on '" (Step-id (MustSubmitNext-step next)) "'.")]
        [(MustReviewNext? next) (string-append "Your next action is to complete reviews for '" (Step-id (MustReviewNext-step next)) "'.")]
        [(eq? #t next) "You have completed this assignment."]
        [else ""]))




;; TODO(3-study): Provide a way to get dependencies based on assignment-handler
(provide assignment-id->assignment-dependencies)
(define (assignment-id->assignment-dependencies id)
  (let* ((assignment (assignment-id->assignment id))
         (handler (Assignment-assignment-handler assignment))
         (get-deps-f (AssignmentHandler-get-dependencies handler)))
    (get-deps-f assignment)))

(provide handle-dependency)
(define (handle-dependency assignment-id dependency bindings raw-bindings)
  (let* ((assignment (assignment-id->assignment assignment-id))
         (handler (Assignment-assignment-handler assignment))
         (take-dependency (AssignmentHandler-take-dependency handler)))
    (take-dependency assignment-id dependency bindings raw-bindings)))
         

;; TODO: Only find review-dependecies here
(provide find-dependencies)
(define (find-dependencies assignment-id step-id review-id)
  (let ((deps (assignment-id->assignment-dependencies assignment-id))
        (f (lambda (dep) (and (review-dependency? dep) 
                              (equal? (review-dependency-step-id dep) step-id) 
                              (equal? (review-dependency-review-id dep) review-id)))))
    (filter f deps)))

(provide check-ready)
(define (check-ready assignment-id)
  (let ((deps (assignment-id->assignment-dependencies assignment-id))
        (filter-function (lambda (dep) (not (dependency-met dep)))))
    (if (null? (filter filter-function deps)) (assignment:mark-ready assignment-id class-name) #f)))


(define (assignment-id->assignment id)
  (yaml->assignment (string->yaml (retrieve-assignment-description class-name id))))

(define (step-id->step assignment-id step-id)
  (lookup-step (assignment-id->assignment assignment-id) step-id))

(define (lookup-step assignment step-id)
  (let* ((steps (Assignment-steps assignment))
         (filter-f (lambda (step) (equal? step-id (Step-id step))))
         (result (filter filter-f steps)))
    (car result)))

