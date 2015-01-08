#lang racket
(require "../base.rkt"
         "assignment.rkt"
         "assignment-structs.rkt"
         "assignment-parser.rkt"
         "next-action.rkt"
         yaml
         rackunit)

(define (make-student id)
    (user:create id)
    (role:associate class-name id student-role))

(define ACE "ace")
(define AMY "amy")
(define ART "art")
(define ALF "alf")
  
(define JOE "joe")
(define JAN "jan")
(define JIM "jim")
(define JON "jon")

(define SAL "sal")
(define SAM "sam")
(define STU "stu")
(define SUE "sue")
(define SID "sid")

(set-db-address! "localhost")

(define (init-tests)
  (init-db)
  (create-assignment test-assignment))


(define (run-tests)
  (test-initial-dependencies)
  (test-met-dependencies))

(define (test-initial-dependencies)
  (let ((expected-test-assignment-dependencies (list
                                                (instructor-solution-dependency #f "tests" "poor-tests")
                                                (instructor-solution-dependency #f "tests" "good-tests")
                                                (student-submission-dependency #f "tests" "student-review" 1)
                                                (instructor-solution-dependency #f "implementation" "poor-impl")
                                                (instructor-solution-dependency #f "implementation" "good-impl")
                                                (student-submission-dependency #f "implementation" "student-review" 1))))
    (init-tests)
    (check-equal? ((AssignmentHandler-get-dependencies (Assignment-assignment-handler test-assignment)) test-assignment) expected-test-assignment-dependencies)))

(define (deps a b c d e f)
  (list
   (instructor-solution-dependency a "tests" "poor-tests")
   (instructor-solution-dependency b "tests" "good-tests")
   (student-submission-dependency c "tests" "student-review" 1)
   (instructor-solution-dependency d "implementation" "poor-impl")
   (instructor-solution-dependency e "implementation" "good-impl")
   (student-submission-dependency f "implementation" "student-review" 1)))

(define (check-test-assignment a b c d e f)
  (let ((expected (deps a b c d e f)))
    (check-equal? ((AssignmentHandler-get-dependencies (Assignment-assignment-handler test-assignment)) test-assignment) expected)))

(define (test-met-dependencies)
  (init-tests)  
  (submission:create-instructor-solution (Assignment-id test-assignment) class-name "tests" (dependency-submission-name "poor-tests" 1))
  (check-test-assignment #t #f #f #f #f #f)
  
  (submission:create-instructor-solution (Assignment-id test-assignment) class-name "tests" (dependency-submission-name "good-tests" 1)) 
  (check-test-assignment #t #t #f #f #f #f)
  
  (submission:create-instructor-solution (Assignment-id test-assignment) class-name "tests" (dependency-submission-name "student-review" 1)) 
  (check-test-assignment #t #t #t #f #f #f)
  
  (submission:create-instructor-solution (Assignment-id test-assignment) class-name "implementation" (dependency-submission-name "poor-impl" 1)) 
  (check-test-assignment #t #t #t #t #f #f)
  
  (submission:create-instructor-solution (Assignment-id test-assignment) class-name "implementation" (dependency-submission-name "good-impl" 1))  
  (check-test-assignment #t #t #t #t #t #f)
  
  (submission:create-instructor-solution (Assignment-id test-assignment) class-name "implementation" (dependency-submission-name "student-review" 1)) 
  (check-test-assignment #t #t #t #t #t #t))


(define test-assignment
  (assignment "Clocks"
              "clocks"
              "Students develop functions representing an alarm clock."
              default-assignment-handler
               (step "tests"
                     "Submit your test cases. Do not submit any clock implementation."
                     (instructor-solution "poor-tests"
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
                     
                     (instructor-solution "good-tests"
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
                     
                     (student-submission "student-review"
                                         1
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
                     
                     (instructor-solution "poor-impl"
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
                     
                     (instructor-solution "good-impl"
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
                     
                     (student-submission "student-review"
                                         1
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
