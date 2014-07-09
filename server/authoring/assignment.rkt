#lang racket

(require (planet esilkensen/yaml:3:1))

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
  (write-yaml instruction))

(struct likert (id text min max granularity) #:transparent)

(define (likert->yaml likert)
 (write-yaml likert))

(struct free-form (id text) #:transparent)

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