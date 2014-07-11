#lang racket

(provide (except-out (all-defined-out) struct:Rubric Rubric? Rubric-elements Rubric))

(require (planet esilkensen/yaml:3:1)
         rackunit)

(struct Assignment (name id description steps) #:transparent)

(define (assignment name id description . steps)
  (Assignment name id description steps))

(define (hash-has-keys? hash . els)
  (ands (map (lambda (x) (hash-has-key? hash x)) els)))

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
                

(struct Step (id instructions reviews) #:transparent)

(define (step id instructions . reviews)
  (Step id instructions reviews))

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
        [(not (hash-has-key? yaml "id")) (raise-user-error "Expected two or three fields: `id`, `instructions`, and optionally `reviews`." yaml)]
        [(not (hash-has-key? yaml "instructions")) (raise-user-error "Expected two or three fields: `id`, `instructions`, and optionally `reviews`." yaml)]
        [else (let ((id (hash-ref yaml "id"))
                    (instructions (hash-ref yaml "instructions")))
                (cond [(= 2 (hash-count yaml)) (step id instructions)]
                      [(not (hash-has-key? yaml "reviews")) (raise-user-error "Expected two or three fields: `id`, `instructions`, and optionally `reviews`." yaml)]
                      [else (let ((reviews (hash-ref yaml "reviews")))
                              (Step id instructions (map yaml->review reviews)))]))]))        


(struct student-submission (amount rubric) #:transparent)

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
                      [(not (hash-has-key? rec "amount")) (raise-user-error "Expected two fields `amount` and `rubric`." rec)]
                      [(not (hash-has-key? rec "rubric")) (raise-user-error "Expected two fields `amount` and `rubric`." rec)]
                      [else (let ((amount (hash-ref rec "amount"))
                                  (rubric (hash-ref rec "rubric")))
                              (student-submission amount (yaml->rubric rubric)))]))]))


(struct instructor-solution (id rubric) #:transparent)

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
                      [(not (hash-has-key? rec "id")) (raise-user-error "Expected two fields `id` and `rubric`." rec)]
                      [(not (hash-has-key? rec "rubric")) (raise-user-error "Expected two fields `id` and `rubric`." rec)]
                      [else (let ((id (hash-ref rec "id"))
                                  (rubric (hash-ref rec "rubric")))
                              (instructor-solution id (yaml->rubric rubric)))]))]))

(provide (contract-out 
          [struct Rubric ((elements (non-empty-listof rubric-element?)))]))
(struct Rubric (elements) #:transparent)

(define (rubric . elements)
  (Rubric elements))

(define (rubric->yaml rubric)
  (cond [(not (Rubric? rubric)) (raise-argument-error 'rubric->yaml "Rubric" rubric)]
        [else (let ((elements (Rubric-elements rubric)))
                (map rubric-element->yaml elements))]))

(define (yaml->rubric yaml)
  (cond [(not (yaml? yaml)) (raise-argument-error 'yaml->rubric "yaml" yaml)]
        [else (let ((elems (map yaml->element yaml)))
                (Rubric elems))]))

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

(struct instruction (text) #:transparent)

(define (instruction->yaml instruction)
  (cond [(not (instruction? instruction)) (raise-argument-error 'instruction->yaml "instruction" instruction)]
        [else (let ((text (instruction-text instruction)))
                `#hash(("instruction" . ,text)))]))


(define (yaml->instruction yaml)
  (cond [(not (yaml? yaml)) (raise-argument-error 'yaml->instruction "yaml" yaml)]
        [(not (= 1 (hash-count yaml))) (raise-user-error "Expected a single record `instruction`.")]
        [(not (hash-has-key? yaml "instruction")) (raise-user-error "Expected a single record `instruction`.")]
        [else (let ((text (hash-ref yaml "instruction")))
                (instruction text))]))

(let ((i (instruction "Test instructions")))
  (check-equal? i (yaml->instruction (instruction->yaml i))))

(struct likert (id text min max granularity) #:transparent)

(define (likert->yaml likert)
  (cond [(not (likert? likert)) (raise-argument-error 'likert->yaml "likert" likert)]
        [else (let ((id (likert-id likert))
                    (text (likert-text likert))
                    (min (likert-min likert))
                    (max (likert-max likert))
                    (granularity (likert-granularity likert)))
                `#hash(("likert" . #hash(("id" . ,id) ("text" . ,text) ("min-label" . ,min) ("max-label" . ,max) ("granularity" . ,granularity)))))]))

(struct free-form (id text) #:transparent)

(define (free-form->yaml form)
  (let ((id (free-form-id form))
        (text (free-form-text form)))
    `#hash(("free-form" . #hash(("id" . ,id) ("text" . ,text))))))

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

(define yaml->free-form
  (yaml->rubric-element "free-form" free-form "id" "text"))

(let ((form (free-form "test-id" "Test instructions")))
  (check-equal? form (yaml->free-form (free-form->yaml form))))

(define yaml->likert
  (yaml->rubric-element "likert" likert "id" "text" "min-label" "max-label" "granularity"))

(let ((l (likert "test-id" "Test instructions" "Disagree" "Agree" 9)))
  (check-equal? l (yaml->likert (likert->yaml l))))

(define (ors els) 
  (cond
    [(not ((listof boolean?) els)) (raise-argument-error 'ors "non-empty-listof boolean?" els)]
    [else (match els
            ['() #f]
            [(cons head tail) (if head #t (ors tail))])]))

(define (ands els) (foldr (lambda (x y) (and x y)) #t els))

(define (one-of? cs)
  (cond
    [(not ((listof contract?) cs)) (raise-argument-error 'one-of? "listof contract?" cs)]
    [else (lambda (el)
            (letrec ((helper (lambda (cs)
                               (match cs
                                 ['() #f]
                                 [(cons head? tail) (if (head? el) #t (helper tail))]))))
              (helper cs)))]))

(define review?
  (one-of? (list student-submission? instructor-solution?)))


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

(define rubric-element? 
  (one-of? 
   (list instruction? likert? free-form?)))

(check-true (rubric-element? (instruction "Some test instructions")))
(check-true (rubric-element? (likert "some-id" "Some likert instructions" "Disagree" "Agree" 9)))
(check-true (rubric-element? (free-form "some-id" "Some test instructions")))



(define test-assignment
  (assignment "Clocks"
              "clocks"
              "Students develop functions representing an alarm clock."
              
               (step "tests"
                     "Submit your test cases. Do not submit any clock implementation."
                     (instructor-solution "Poor Tests"
                                          (rubric
                                           (likert "correctness"
                                                   "These tests are correct."
                                                   "Disagree"
                                                   "Agree"
                                                   9)
                                           
                                           (instruction "Provide feedback on tests that are not correct by clicking on the line number and adding a comment.")
                                           
                                           (likert "coverage"
                                                   "These tests cover the possible inputs."
                                                   "Disagree"
                                                   "Agree"
                                                   9)
                                           
                                           (free-form "not-covered"
                                                      "If applicable, provide inputs that are not covered by the tests.")))
                     
                     (instructor-solution "Good Tests"
                                          (rubric
                                           (likert "correctness"
                                                   "These tests are correct."
                                                   "Disagree"
                                                   "Agree"
                                                   9)
                                          
                                           (instruction "Provide feedback on tests that are not correct by clicking on the line number and adding a comment.")
                                          
                                           (likert "coverage"
                                                   "These tests cover the possible inputs."
                                                   "Disagree"
                                                   "Agree"
                                                   9)
                                           
                                           (free-form "not-covered"
                                                      "If applicable, provide inputs that are not covered by the tests.")))
                     
                     (student-submission 1
                                         (rubric
                                          (likert "correctness"
                                                  "These tests are correct."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                          (instruction "Provide feedback on tests that are not correct by clicking on the line number and adding a comment.")
                                          
                                          (likert "coverage"
                                                  "These tests cover the possible inputs."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                          (free-form "not-covered"
                                                     "If applicable, provide inputs that are not covered by the tests."))))
               
               (step "implementation"
                     "Submit all of your test cases and your clock implementation."
                     
                     (instructor-solution "Poor Implementation"
                                          (rubric
                                          (likert "behavior"
                                                  "This code correctly implements the desired behavior."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                          (instruction "If applicable, leave inline feedback where the incorrect behaviors exist.")
                                          
                                          (likert "structure"
                                                  "This code is structured well."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                          (instruction "If applicable, leave inline feedback where the code is not structured well.")
                                          
                                          (free-form "feedback"
                                                     "Additional Comments")))
                     
                     (instructor-solution "Good Implementation"
                                          (rubric
                                          (likert "behavior"
                                                  "This code correctly implements the desired behavior."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                          (instruction "If applicable, leave inline feedback where the incorrect behaviors exist.")
                                          
                                          (likert "structure"
                                                  "This code is structured well."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                          (instruction "If applicable, leave inline feedback where the code is not structured well.")
                                          
                                          (free-form "feedback"
                                                     "Additional Comments")))
                     
                     (student-submission 1
                                         (rubric
                                         (likert "behavior"
                                                  "This code correctly implements the desired behavior."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                         (instruction "If applicable, leave inline feedback where the incorrect behaviors exist.")
                                          
                                         (likert "structure"
                                                  "This code is structured well."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                         (instruction "If applicable, leave inline feedback where the code is not structured well.")
                                          
                                         (free-form "feedback"
                                                     "Additional Comments"))))))

(define test-rubric
  (rubric
   (likert "behavior"
           "This code correctly implements the desired behavior."
           "Disagree"
           "Agree"
           9)
   
   (instruction "If applicable, leave inline feedback where the incorrect behaviors exist.")
   
   (likert "structure"
           "This code is structured well."
           "Disagree"
           "Agree"
           9)
   
   (instruction "If applicable, leave inline feedback where the code is not structured well.")
   
   (free-form "feedback"
              "Additional Comments")))

(check-equal? test-rubric (yaml->rubric (rubric->yaml test-rubric)))

(let ((submission (instructor-solution "test-id" test-rubric)))
  (check-equal? submission (yaml->instructor-solution (instructor-solution->yaml submission)))
  (check-equal? submission (yaml->review (review->yaml submission))))

(check-true (yaml? (student-submission->yaml (student-submission 1 test-rubric))))

(let ((submission (student-submission 2 test-rubric)))
  (check-equal? submission (yaml->student-submission (student-submission->yaml submission)))
  (check-equal? submission (yaml->review (review->yaml submission))))

(check-true (yaml? (step->yaml (step "test-id" "test instructions"))))
(check-true (yaml? (step->yaml (step "test-id" "test instructions" (instructor-solution "test-id" test-rubric)))))
(check-true (yaml? (step->yaml (step "test-id" "test instructions" (instructor-solution "test-id" test-rubric) (student-submission 2 test-rubric)))))

(let ((step (step "test-id" "test instructions")))
  (check-equal? step (yaml->step (step->yaml step))))

(let ((step (step "test-id" "test instructions" (instructor-solution "test-id" test-rubric))))
  (check-equal? step (yaml->step (step->yaml step))))

(let ((step (step "test-id" "test instructions" (instructor-solution "test-id" test-rubric) (student-submission 2 test-rubric))))
  (check-equal? step (yaml->step (step->yaml step))))

(check-true (yaml? (assignment->yaml test-assignment)))
(check-equal? test-assignment (yaml->assignment (assignment->yaml test-assignment)))