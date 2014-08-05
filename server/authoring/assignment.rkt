#lang racket

(provide (except-out (all-defined-out)) (all-from-out "assignment-structs.rkt"))

(require (planet esilkensen/yaml:3:1)
         json
         "assignment-structs.rkt"
         "util.rkt")

(define basic-class "BasicElement")
(define likert-class "LikertElement")
(define free-form-class "FreeFormElement")


;; Assignment
(define (yaml->assignment yaml) 
  (cond [(not (yaml? yaml)) (raise-argument-error 'yaml->assignment "yaml" yaml)]
        [(not (= 4 (hash-count yaml))) (raise-user-error "Expected record with 4 fields: `name`, `id`, `description`, and `steps`")]
        [(not (hash-has-keys? yaml "name" "id" "description" "steps")) (raise-user-error "Expected record with 4 fields: `name`, `id`, `description`, and `steps`")]
        [else (let ((id (hash-ref yaml "id"))
                    (name (hash-ref yaml "name"))
                    (description (hash-ref yaml "description"))
                    (steps (map yaml->step (hash-ref yaml "steps"))))
                (Assignment name id description steps))]))

(define (assignment->yaml assignment)
  (cond [(not (Assignment? assignment)) (raise-argument-error 'assignment->yaml "Assignment" assignment)]
        [else (let ((name (Assignment-name assignment))
                    (id (Assignment-id assignment))
                    (description (Assignment-description assignment))
                    (steps (map step->yaml (Assignment-steps assignment))))
                `#hash(("name" . ,name) ("id" . ,id) ("description" . ,description) ("steps" . ,steps)))]))

;; Step
(define (step->yaml step)
  (cond [(not (Step? step)) (raise-argument-error 'step->yaml "Step" step)]
        [else (let* ((id (Step-id step))
                     (instructions (Step-instructions step))
                     (reviews (map review->yaml (Step-reviews step)))
                     (partial `(("id" . ,id) ("instructions" . ,instructions))))
                (make-hash (if (null? reviews) partial (cons `("reviews" . ,reviews) partial))))]))

(define (yaml->step yaml)
  (cond [(not (yaml? yaml)) (raise-argument-error 'yaml->step "yaml" yaml)]
        [(not (or (= 2 (hash-count yaml)) (= 3 (hash-count yaml)))) (raise-user-error "Expected two or three fields: `id`, `instructions`, and optionally `reviews`." yaml)]
        [(not (hash-has-keys? yaml "id" "instructions")) (raise-user-error "Expected two or three fields: `id`, `instructions`, and optionally `reviews`." yaml)]
        [else (let ((id (hash-ref yaml "id"))
                    (instructions (hash-ref yaml "instructions")))
                (cond [(= 2 (hash-count yaml)) (step id instructions)]
                      [(not (hash-has-key? yaml "reviews")) (raise-user-error "Expected two or three fields: `id`, `instructions`, and optionally `reviews`." yaml)]
                      [else (let ((reviews (hash-ref yaml "reviews")))
                              (Step id instructions (map yaml->review reviews)))]))]))        

;; Reviews

;; Student Submission
(define (student-submission->yaml submission) 
  (cond [(not (student-submission? submission)) (raise-argument-error 'student-submission->yaml "student-submission" submission)]
        [else (let ((amount (student-submission-amount submission))
                    (rubric (rubric->yaml (student-submission-rubric submission))))
                `#hash(("student-submission" . #hash(("amount" . ,amount) ("rubric" . ,rubric)))))]))

(define (yaml->student-submission yaml) 
  (cond [(not (yaml? yaml)) (raise-argument-error 'yaml->student-submission "yaml" yaml)]
        [(not (= 1 (hash-count yaml))) (raise-user-error "Expected a single record `student-submission`." yaml)]
        [(not (hash-has-key? yaml "student-submission")) (raise-user-error "Expected a single record `student-submission`." yaml)]
        [else (let ((rec (hash-ref yaml "student-submission")))
                (cond [(not (= 2 (hash-count rec))) (raise-user-error "Expected two fields `amount` and `rubric`." rec)]
                      [(not (hash-has-keys? rec "amount" "rubric")) (raise-user-error "Expected two fields `amount` and `rubric`." rec)]
                      [else (let ((amount (hash-ref rec "amount"))
                                  (rubric (hash-ref rec "rubric")))
                              (student-submission amount (yaml->rubric rubric)))]))]))

;; Instructor Solution
(define (instructor-solution->yaml solution)
  (cond [(not (instructor-solution? solution)) (raise-argument-error 'instructor-solution->yaml "instructor-solution" solution)]
        [else (let ((id (instructor-solution-id solution))
                    (rubric (rubric->yaml (instructor-solution-rubric solution))))
                `#hash(("instructor-solution" . #hash(("id" . ,id) ("rubric" . ,rubric)))))]))

(define (yaml->instructor-solution yaml)
  (cond [(not (yaml? yaml)) (raise-argument-error 'yaml->instructor-solution "yaml" yaml)]
        [(not (= 1 (hash-count yaml))) (raise-user-error "Expected a single record `instructor-solution`." yaml)]
        [(not (hash-has-key? yaml "instructor-solution")) (raise-user-error "Expected a single record `instructor-solution`." yaml)]
        [else (let ((rec (hash-ref yaml "instructor-solution")))
                (cond [(not (= 2 (hash-count rec))) (raise-user-error "Expected two fields `id` and `rubric`." rec)]
                      [(not (hash-has-keys? rec "id" "rubric")) (raise-user-error "Expected two fields `id` and `rubric`." rec)]
                      [else (let ((id (hash-ref rec "id"))
                                  (rubric (hash-ref rec "rubric")))
                              (instructor-solution id (yaml->rubric rubric)))]))]))

;; Generic
(define (review->yaml review)
  (cond [(not (review? review)) (raise-argument-error 'review->yaml "review" review)]
        [(instructor-solution? review) (instructor-solution->yaml review)]
        [(student-submission? review) (student-submission->yaml review)]))

(define (yaml->review yaml)
  (cond [(not (yaml? yaml)) (raise-argument-error 'yaml->review "yaml" yaml)]
        [(not (= 1 (hash-count yaml))) (raise-user-error "Expected one field either `instructor-solution` or `student-submission`." yaml)]
        [(hash-has-key? yaml "instructor-solution") (yaml->instructor-solution yaml)]
        [(hash-has-key? yaml "student-submission") (yaml->student-submission yaml)]
        [else (raise-user-error "Expected one field either `instructor-solution` or `student-submission`." yaml)]))

;; Rubric
(define (rubric->yaml rubric)
  (cond [(not (Rubric? rubric)) (raise-argument-error 'rubric->yaml "Rubric" rubric)]
        [else (let ((elements (Rubric-elements rubric)))
                (map rubric-element->yaml elements))]))

(define (yaml->rubric yaml)
  (cond [(not (yaml? yaml)) (raise-argument-error 'yaml->rubric "yaml" yaml)]
        [else (let ((elems (map yaml->element yaml)))
                (Rubric elems))]))

(define (rubric->json rubric)
  (cond [(not (Rubric? rubric)) (raise-argument-error 'rubric->json "Rubric" rubric)]
        [else (let* ((elements (Rubric-elements rubric))
                     (inner (map rubric-element->json elements)))
                `#hasheq((rubric . ,inner)))]))

;; Rubric elements

;; Instruction
(define (yaml->instruction yaml)
  (cond [(not (yaml? yaml)) (raise-argument-error 'yaml->instruction "yaml" yaml)]
        [(not (= 1 (hash-count yaml))) (raise-user-error "Expected a single record `instruction`.")]
        [(not (hash-has-key? yaml "instruction")) (raise-user-error "Expected a single record `instruction`.")]
        [else (let ((text (hash-ref yaml "instruction")))
                (instruction text))]))

(define (instruction->yaml instruction)
  (cond [(not (instruction? instruction)) (raise-argument-error 'instruction->yaml "instruction" instruction)]
        [else (let ((text (instruction-text instruction)))
                `#hash(("instruction" . ,text)))]))

(define (instruction->json instruction)
  (cond [(not (instruction? instruction)) (raise-argument-error 'instruction->json "instruction" instruction)]
        [else (let ((text (instruction-text instruction)))
                `#hasheq((class . ,basic-class)
                         (prompt . ,text)
                         (id . "prompt")))]))

;; Complex rubric-element creator
(define (yaml->rubric-element key constructor . arguments)
  (lambda (element-yaml)
    (cond 
      [(not (yaml? element-yaml)) (raise-argument-error (string->symbol (string-append "yaml->" key)) "yaml" element-yaml)]
      [(not (hash-has-key? element-yaml key)) (raise-user-error (string-append "Expected a single '" key "' field." element-yaml))]
      [(not (= 1 (hash-count element-yaml))) (raise-user-error (string-append "Expected a single '" key "' field." element-yaml))]
      [else (let* ((r (hash-ref element-yaml key))
                   (expected (length arguments))
                   (error-message (string-append "A `" key "` record should contain " (number->string expected) " fields: `" (string-join arguments "`, `") "`")))
              (cond 
                [(not (= expected (hash-count r))) (raise-user-error error-message element-yaml)]
                [else (letrec ((helper (lambda (acc args)
                                         (cond [(null? args) (apply constructor (reverse acc))]
                                               [else (cond [(not (hash-has-key? r (car args))) (raise-user-error error-message element-yaml)]
                                                           [else (let ((new-acc (cons (hash-ref r (car args)) acc)))
                                                                   (helper new-acc (cdr args)))])]))))
                        (helper '() arguments))]))])))

;; Likert

(define yaml->likert
  (yaml->rubric-element "likert" likert "id" "text" "min-label" "max-label" "granularity"))

(define (likert->yaml likert)
  (cond [(not (likert? likert)) (raise-argument-error 'likert->yaml "likert" likert)]
        [else (let ((id (likert-id likert))
                    (text (likert-text likert))
                    (min (likert-min likert))
                    (max (likert-max likert))
                    (granularity (likert-granularity likert)))
                `#hash(("likert" . #hash(("id" . ,id) ("text" . ,text) ("min-label" . ,min) ("max-label" . ,max) ("granularity" . ,granularity)))))]))

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

;; Free Form
(define yaml->free-form
  (yaml->rubric-element "free-form" free-form "id" "text"))

(define (free-form->yaml form)
  (let ((id (free-form-id form))
        (text (free-form-text form)))
    `#hash(("free-form" . #hash(("id" . ,id) ("text" . ,text))))))

(define (free-form->json form)
  (cond [(not (free-form? form)) (raise-argument-error 'free-form->json "free-form" form)]
        [else (let ((id (free-form-id form))
                    (text (free-form-text form)))
                `#hasheq((class . ,free-form-class)
                         (id . ,id)
                         (prompt . ,text)
                         (content . "")))]))

;; Any Rubricoo Element
(define (yaml->element yaml)
  (cond [(not (yaml? yaml)) (raise-argument-error 'yaml->rubric-element "yaml" yaml)]
        [(not (= 1 (hash-count yaml))) (raise-user-error "Expected a single record `likert`, `free-form`, or `instruction`." yaml)]
        [(hash-has-key? yaml "instruction") (yaml->instruction yaml)]
        [(hash-has-key? yaml "likert") (yaml->likert yaml)]
        [(hash-has-key? yaml "free-form") (yaml->free-form yaml)]
        [else (raise-user-error "Expected a single record `likert`, `free-form`, or `instruction`." yaml)]))

(define (rubric-element->yaml el)
  (cond [(not (rubric-element? el)) (raise-argument-error 'rubric-element->yaml "rubric-element" el)]
        [else (cond
                [(instruction? el) (instruction->yaml el)]
                [(likert? el) (likert->yaml el)]
                [(free-form? el) (free-form->yaml el)])]))

(define (rubric-element->json el)
  (cond [(not (rubric-element? el)) (raise-argument-error 'rubric-element->json "rubric-element" el)]
        [else (cond
                [(instruction? el) (instruction->json el)]
                [(likert? el) (likert->json el)]
                [(free-form? el) (free-form->json el)])]))