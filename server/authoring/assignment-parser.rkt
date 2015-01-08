#lang typed/racket

(require/typed yaml
               [yaml->string (Any -> String)])

(require "assignment-structs.rkt"
         "three-condition-study.rkt"
         (prefix-in hold: "hold-for-review-handler.rkt")
         "util.rkt"
         "next-action.rkt"
         "../base.rkt")

(define basic-class "BasicElement")
(define likert-class "LikertElement")
(define free-form-class "FreeFormElement")

(define assignment-handlers 
  (make-hash (list (cons (AssignmentHandler-key default-assignment-handler) default-assignment-handler)
                   (cons (AssignmentHandler-key three-condition-study-handler) three-condition-study-handler)
                   (cons (AssignmentHandler-key hold:hold-for-review-handler) hold:hold-for-review-handler))))

;; Assignment
;; TODO(3 study): Parse next-action-function
(provide yaml->assignment)
(: yaml->assignment (Assignment-YAML -> Assignment))
(define (yaml->assignment yaml) 
  (cond [(not (or (= 5 (hash-count yaml))
                  (= 4 (hash-count yaml)))) (raise-user-error "Expected record with 4 or 5 fields: `name`, `id`, `description`, `steps`, and optionally `assignment-handler`.")]
        [(not (hash-has-keys? yaml "name" "id" "description" "steps")) (raise-user-error "Expected record with fields: `name`, `id`, `description`, `steps`, and optionally `assignment-handler`.")]
        [else (let ((id (hash-ref yaml "id"))
                    (name (hash-ref yaml "name"))
                    (description (hash-ref yaml "description"))
                    (steps (map yaml->step (cast (hash-ref yaml "steps") (Listof Step-YAML))))
                    (assignment-handler (get-assignment-handler yaml)))
                (Assignment (assert name string?) 
                            (assert id string?)
                            (assert description string?)
                            assignment-handler steps))]))


(: get-assignment-handler (Assignment-YAML -> AssignmentHandler))
(define (get-assignment-handler yaml)
  (cond [(hash-has-keys? yaml "assignment-handler") (assert (hash-ref assignment-handlers (hash-ref yaml "assignment-handler")) AssignmentHandler?)]
        [else default-assignment-handler]))

(provide Assignment-YAML)
(define-type Assignment-YAML (HashTable String (U String AssignmentHandler (Listof Step-YAML))))

;; TODO(3 study): Output next-action-function
(provide assignment->yaml)
(: assignment->yaml (Assignment -> Assignment-YAML))
(define (assignment->yaml assignment)
  (cond [(not (Assignment? assignment)) (raise-argument-error 'assignment->yaml "Assignment" assignment)]
        [else (let ((name (Assignment-name assignment))
                    (id (Assignment-id assignment))
                    (description (Assignment-description assignment))
                    (steps (map step->yaml (Assignment-steps assignment)))
                    (handler (AssignmentHandler-key (Assignment-assignment-handler assignment))))
                `#hash(("name" . ,name) ("id" . ,id) ("description" . ,description) ("assignment-handler" . ,handler) ("steps" . ,steps)))]))


(define-type Step-YAML (HashTable String (U String (Listof Review-YAML))))
;; Step
(provide step->yaml)
(: step->yaml (Step -> Step-YAML))
(define (step->yaml step)
  (cond [(not (Step? step)) (raise-argument-error 'step->yaml "Step" step)]
        [else (let* ((id (Step-id step))
                     (instructions (Step-instructions step))
                     (reviews (map review->yaml (Step-reviews step)))
                     (partial `(("id" . ,id) ("instructions" . ,instructions))))
                (make-hash (if (null? reviews) partial (cons `("reviews" . ,reviews) partial))))]))


(provide yaml->step)
(: yaml->step (Step-YAML -> Step))
(define (yaml->step yaml)
  (cond [(not (or (= 2 (hash-count yaml)) (= 3 (hash-count yaml)))) (raise-user-error "Expected two or three fields: `id`, `instructions`, and optionally `reviews`." yaml)]
        [(not (hash-has-keys? yaml "id" "instructions")) (raise-user-error "Expected two or three fields: `id`, `instructions`, and optionally `reviews`." yaml)]
        [else (let ((id (hash-ref yaml "id"))
                    (instructions (hash-ref yaml "instructions")))
                (cond [(= 2 (hash-count yaml)) (step (assert id string?) (assert instructions string?))]
                      [(not (hash-has-key? yaml "reviews")) (raise-user-error "Expected two or three fields: `id`, `instructions`, and optionally `reviews`." yaml)]
                      [else (let*: ((reviews (cast (hash-ref yaml "reviews") (Listof Review-YAML)))
                                    [the-reviews : (Listof Review) (map yaml->review reviews)])
                              (Step (assert id string?) (assert instructions string?) the-reviews))]))]))

;; Reviews
;; Student Submission
(define-type student-submission-YAML (HashTable String (HashTable String (U String Nonnegative-Integer Rubric-YAML))))

(provide student-submission->yaml)
(: student-submission->yaml (student-submission -> student-submission-YAML))
(define (student-submission->yaml submission) 
  (cond [(not (student-submission? submission)) (raise-argument-error 'student-submission->yaml "student-submission" submission)]
        [else (let* ((amount (student-submission-amount submission))
                     (rubric (rubric->yaml (student-submission-rubric submission)))
                     (id (student-submission-id submission))
                     (inner `#hash(("id" . ,id) ("amount" . ,amount) ("rubric" . ,rubric))))
                `#hash(("student-submission" . ,inner)))]))

(provide yaml->student-submission)
(: yaml->student-submission (student-submission-YAML -> student-submission))
(define (yaml->student-submission yaml) 
  (cond [(not (= 1 (hash-count yaml))) (raise-user-error "Expected a single record `student-submission`." yaml)]
        [(not (hash-has-key? yaml "student-submission")) (raise-user-error "Expected a single record `student-submission`." yaml)]
        [else (let ((rec (hash-ref yaml "student-submission")))
                (cond [(not (= 3 (hash-count rec))) (raise-user-error "Expected three fields `id`, `amount` and `rubric`." rec)]
                      [(not (hash-has-keys? rec "id" "amount" "rubric")) (raise-user-error "Expected three fields `id`, `amount` and `rubric`." rec)]
                      [else (let ((amount (hash-ref rec "amount"))
                                  (rubric (hash-ref rec "rubric"))
                                  (id (hash-ref rec "id")))
                              (student-submission (assert id string?) (assert amount exact-nonnegative-integer?) (yaml->rubric (cast rubric Rubric-YAML))))]))]))


;; Instructor Solution
;(define-type student-submission-YAML (HashTable String (HashTable String (U String Nonnegative-Integer Rubric-YAML))))

;; FIXME: Without the Nonnegative-Integer here the contract for review->yaml fails on untyped modules when taking an instructor-solution
(define-type instructor-solution-YAML (HashTable String (HashTable String (U String Nonnegative-Integer Rubric-YAML))))

(provide instructor-solution->yaml)
(: instructor-solution->yaml (instructor-solution -> instructor-solution-YAML))
(define (instructor-solution->yaml solution)
  (cond [(not (instructor-solution? solution)) (raise-argument-error 'instructor-solution->yaml "instructor-solution" solution)]
        [else (let*: ([id : String (instructor-solution-id solution)]
                      [rubric : Rubric-YAML (rubric->yaml (instructor-solution-rubric solution))]
                      [inner : (HashTable String (U String Nonnegative-Integer Rubric-YAML)) `#hash(("id" . ,id) ("rubric" . ,rubric))])
                `#hash(("instructor-solution" . ,inner)))]))


(provide yaml->instructor-solution)
(: yaml->instructor-solution (instructor-solution-YAML -> instructor-solution))
(define (yaml->instructor-solution yaml)
  (cond [(not (= 1 (hash-count yaml))) (raise-user-error "Expected a single record `instructor-solution`." yaml)]
        [(not (hash-has-key? yaml "instructor-solution")) (raise-user-error "Expected a single record `instructor-solution`." yaml)]
        [else (let ((rec (hash-ref yaml "instructor-solution")))
                (cond [(not (= 2 (hash-count rec))) (raise-user-error "Expected two fields `id` and `rubric`." rec)]
                      [(not (hash-has-keys? rec "id" "rubric")) (raise-user-error "Expected two fields `id` and `rubric`." rec)]
                      [else (let ((id (assert (hash-ref rec "id") string?))
                                  (rubric (cast (hash-ref rec "rubric") Rubric-YAML)))
                              (instructor-solution id (yaml->rubric rubric)))]))]))

;; Generic
(provide Review-YAML)
(define-type Review-YAML (U student-submission-YAML
                            instructor-solution-YAML))

(provide review->yaml)
(: review->yaml (Review -> Review-YAML))
(define (review->yaml review)
  (cond [(instructor-solution? review) (instructor-solution->yaml review)]
        [(student-submission? review) (student-submission->yaml review)]))

(provide yaml->review)
(: yaml->review (Review-YAML -> Review))
(define (yaml->review yaml)
  (cond [(not (= 1 (hash-count yaml))) (raise-user-error "Expected one field either `instructor-solution` or `student-submission`." yaml)]
        [(hash-has-key? yaml "instructor-solution") (yaml->instructor-solution (cast yaml instructor-solution-YAML))]
        [(hash-has-key? yaml "student-submission") (yaml->student-submission (cast yaml student-submission-YAML))]
        [else (raise-user-error "Expected one field either `instructor-solution` or `student-submission`." yaml)]))

(define-type Rubric-YAML (Listof RubricElement-YAML))

;; Rubric
(provide rubric->yaml)
(: rubric->yaml (Rubric -> Rubric-YAML))
(define (rubric->yaml rubric)
  (cond [(not (Rubric? rubric)) (raise-argument-error 'rubric->yaml "Rubric" rubric)]
        [else (let ((elements (Rubric-elements rubric)))
                (map rubric-element->yaml elements))]))

(provide yaml->rubric)
(: yaml->rubric (Rubric-YAML -> Rubric))
(define (yaml->rubric yaml)
  (cond [else (let ((elems (map yaml->element yaml)))
                (Rubric elems))]))


(provide rubric->json)
(: rubric->json (Rubric -> (HashTable Symbol (Listof (HashTable Symbol (U String Number))))))
(define (rubric->json rubric)
  (cond [(not (Rubric? rubric)) (raise-argument-error 'rubric->json "Rubric" rubric)]
        [else (let* ((elements (Rubric-elements rubric))
                     (inner (map rubric-element->json elements)))
                `#hasheq((rubric . ,inner)))]))



;; Rubric elements

;; Instruction
(provide yaml->instruction)
(: yaml->instruction ((HashTable String String) -> instruction))
(define (yaml->instruction yaml)
  (cond [(not (= 1 (hash-count yaml))) (raise-user-error "Expected a single record `instruction`.")]
        [(not (hash-has-key? yaml "instruction")) (raise-user-error "Expected a single record `instruction`.")]
        [else (let ((text (hash-ref yaml "instruction")))
                (instruction (assert text string?)))]))

(provide instruction->yaml)
(: instruction->yaml (instruction -> (HashTable String String)))
(define (instruction->yaml instruction)
  (cond [(not (instruction? instruction)) (raise-argument-error 'instruction->yaml "instruction" instruction)]
        [else (let ((text (instruction-text instruction)))
                `#hash(("instruction" . ,text)))]))

(: instruction->json (instruction -> (HashTable Symbol (U Number String))))
(define (instruction->json instruction)
  (cond [(not (instruction? instruction)) (raise-argument-error 'instruction->json "instruction" instruction)]
        [else (let ((text (instruction-text instruction)))
                `#hasheq((class . ,basic-class)
                         (prompt . ,text)
                         (id . "prompt")))]))


;; Complex rubric-element creator
(: yaml->rubric-element (All (A) (String ((Listof (U Number String)) -> A) String * -> ((HashTable String (HashTable String (U Number String))) -> A))))
(define (yaml->rubric-element key constructor . arguments)
  (lambda (element-yaml)
    (cond 
      [(not (hash-has-key? element-yaml key)) (raise-user-error (format "Expected a single '~a' field in: ~a" key element-yaml))]
      [(not (= 1 (hash-count element-yaml))) (raise-user-error (format "Expected a single '~a' field in: ~a" key element-yaml))]
      [else (let* ((r (hash-ref element-yaml key))
                   (expected (length arguments))
                   (error-message (string-append "A `" key "` record should contain " (number->string expected) " fields: `" (string-join arguments "`, `") "`")))
              (cond 
                [(not (= expected (hash-count r))) (raise-user-error error-message element-yaml)]
                [else (letrec ([helper : ((Listof (U Number String)) (Listof String) -> A) 
                                       (lambda (acc args)
                                         (cond [(null? args) (constructor (reverse acc))]
                                               [else (cond [(not (hash-has-key? r (car args))) (raise-user-error error-message element-yaml)]
                                                           [else (let ((new-acc (cons (hash-ref r (car args)) acc)))
                                                                   (helper new-acc (cdr args)))])]))])
                        (helper '() arguments))]))])))

;; Likert

(: likert-list ((Listof (U String Number)) -> likert))
(define (likert-list args)
  (likert (assert (first args) string?) 
          (assert (second args) string?)
          (assert (third args) string?)
          (assert (fourth args) string?)
          (assert (fifth args) exact-nonnegative-integer?)))

(provide yaml->likert)
(: yaml->likert ((HashTable String (HashTable String (U Number String))) -> likert))
(define yaml->likert
  (yaml->rubric-element "likert" likert-list "id" "text" "min-label" "max-label" "granularity"))

(provide likert->yaml)
(: likert->yaml (likert -> (HashTable String (HashTable String (U Number String)))))
(define (likert->yaml likert)
  (cond [(not (likert? likert)) (raise-argument-error 'likert->yaml "likert" likert)]
        [else (let ((id (likert-id likert))
                    (text (likert-text likert))
                    (min (likert-min likert))
                    (max (likert-max likert))
                    ;; FIXME: Why do I have to cast to Number Exact-Nonnegative-Integer *should* fit into Number
                    ;; Infact, this cast is just fine
                    (granularity (cast (likert-granularity likert) Number)))
                `#hash(("likert" . #hash(("id" . ,id) ("text" . ,text) ("min-label" . ,min) ("max-label" . ,max) ("granularity" . ,granularity)))))]))

(provide likert->json)
(: likert->json (likert -> (HashTable Symbol (U String Number))))
(define (likert->json likert)
  (cond [(not (likert? likert)) (raise-argument-error 'likert->json "likert" likert)]
        [else (let ((id (likert-id likert))
                    (text (likert-text likert))
                    (min (likert-min likert))
                    (max (likert-max likert))
                    (granularity (likert-granularity likert)))
                `#hasheq((class . ,likert-class)
                         (id . ,id)
                         (prompt . ,text)
                         (minLabel . ,min)
                         (maxLabel . ,max)
                         (rangeSize . ,granularity)
                         (selected . -1)))]))

(: free-form-list ((Listof (U String Number)) -> free-form))
(define (free-form-list args)
  (free-form (assert (first args) string?)
             (assert (second args) string?)))

;; Free Form
(provide yaml->free-form)
(: yaml->free-form ((HashTable String (HashTable String (U String Number))) -> free-form))
(define yaml->free-form
  (yaml->rubric-element "free-form" free-form-list "id" "text"))

(provide free-form->yaml)
(: free-form->yaml (free-form -> (HashTable String (HashTable String (U String Number)))))
(define (free-form->yaml form)
  (let*: ((id (free-form-id form))
          (text (free-form-text form))
          [inner  : (HashTable String (U String Number)) `#hash(("id" . ,id) ("text" . ,text))])
    `#hash(("free-form" . ,inner))))

(provide free-form->json)
(: free-form->json (free-form -> (HashTable Symbol (U String Number))))
(define (free-form->json form)
  (let ((id (free-form-id form))
                    (text (free-form-text form)))
                `#hasheq((class . ,free-form-class)
                         (id . ,id)
                         (prompt . ,text)
                         (content . ""))))

(define-type RubricElement-YAML (U (HashTable String String)
                                   (HashTable String (HashTable String (U String Number)))))
;; Any Rubricoo Element
(provide yaml->element)
(: yaml->element (RubricElement-YAML -> RubricElement))
(define (yaml->element yaml)
  (cond [(not (= 1 (hash-count yaml))) (raise-user-error "Expected a single record `likert`, `free-form`, or `instruction`." yaml)]
        [(hash-has-key? yaml "instruction") (yaml->instruction (cast yaml (HashTable String String)))]
        [(hash-has-key? yaml "likert") (yaml->likert (cast yaml (HashTable String (HashTable String (U String Number)))))]
        [(hash-has-key? yaml "free-form") (yaml->free-form (cast yaml (HashTable String (HashTable String (U String Number)))))]
        [else (raise-user-error "Expected a single record `likert`, `free-form`, or `instruction`." yaml)]))


(: rubric-element->yaml (RubricElement -> RubricElement-YAML))
(provide rubric-element->yaml)
(define (rubric-element->yaml el)
  (cond [(not (rubric-element? el)) (raise-argument-error 'rubric-element->yaml "rubric-element" el)]
        [else (cond
                [(instruction? el) (instruction->yaml el)]
                [(likert? el) (likert->yaml el)]
                [(free-form? el) (free-form->yaml el)])]))

(provide rubric-element->json)
(: rubric-element->json (RubricElement -> (HashTable Symbol (U String Number))))
(define (rubric-element->json el)
  (cond [(not (rubric-element? el)) (raise-argument-error 'rubric-element->json "rubric-element" el)]
        [else (cond
                [(instruction? el) (instruction->json el)]
                [(likert? el) (likert->json el)]
                [(free-form? el) (free-form->json el)])]))

(provide could-not-parse)
(: could-not-parse (Any -> Failure))
(define (could-not-parse exn)
  (failure (format "Could not parse as YAML: ~a" exn)))

(provide invalid-yaml)
(: invalid-yaml (Any -> Failure))
(define (invalid-yaml exn)
  (failure (format "YAML did not contain a valid assignment description: ~a" exn)))
