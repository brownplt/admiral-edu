#lang racket

(provide (except-out (all-defined-out)))

(require (planet esilkensen/yaml:3:1)
         rackunit)

(struct Assignment (name id description steps) #:transparent)

(define (assignment name id description . steps)
  (Assignment name id description steps))

(struct Step (id instructions reviews) #:transparent)

(define (step id instructions . reviews)
  (Step id instructions reviews))

(struct Student-submission (amount rubric) #:transparent)

(define (student-submission amount . elements)
  (Student-submission amount (Rubric elements)))

(struct Instructor-solution (id rubric) #:transparent)

(define (instructor-solution id . elements)
  (Instructor-solution id (Rubric elements)))

(struct Rubric (elements) #:transparent)

(define (rubric . elements)
  (Rubric elements))



(struct instruction (text) #:transparent)

(define (instruction->yaml instruction)
  (cond [(not (instruction? instruction)) (raise-argument-error 'instruction->yaml "instruction" instruction)]
        [else (let ((text (instruction-text instruction)))
                `#hash(("instruction" . #hash(("text" . ,text)))))]))

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

(define yaml->instruction
  (yaml->rubric-element "instruction" instruction "text"))

(let ((i (instruction "Test instructions")))
  (check-equal? i (yaml->instruction (instruction->yaml i))))

(define (ors els) 
  (cond
    [(not ((listof boolean?) els)) (raise-argument-error 'ors "non-empty-listof boolean?" els)]
    [else (match els
            ['() #f]
            [(cons head tail) (if head #t (ors tail))])]))

(define (one-of? cs)
  (cond
    [(not ((listof contract?) cs)) (raise-argument-error 'one-of? "listof contract?" cs)]
    [else (lambda (el)
            (letrec ((helper (lambda (cs)
                               (match cs
                                 ['() #f]
                                 [(cons head? tail) (if (head? el) #t (helper tail))]))))
              (helper cs)))]))


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
                                                     "If applicable, provide inputs that are not covered by the tests."))
                     
                     (instructor-solution "Good Tests"
                                          
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
                                                     "If applicable, provide inputs that are not covered by the tests."))
                     
                     (student-submission 1
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
               
               (step "implementation"
                     "Submit all of your test cases and your clock implementation."
                     
                     (instructor-solution "Poor Implementation"
                                          
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
                                                     "Additional Comments"))
                     
                     (instructor-solution "Good Implementation"
                                          
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
                                                     "Additional Comments"))
                     
                     (student-submission 1
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
                                                     "Additional Comments")))))