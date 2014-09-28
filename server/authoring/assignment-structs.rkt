#lang typed/racket

(require "../util/basic-types.rkt")

;; Assignment Description
(provide (struct-out Assignment))
(struct: Assignment ([name  : String]
                     [id : String]
                     [description : String]
                     [assignment-handler : AssignmentHandler]
                     [steps : (Listof Step)]) #:transparent)



(provide assignment)
(: assignment (String String String AssignmentHandler Step * -> Assignment))
(define (assignment name id description assignment-handler . steps)
  (Assignment name id description assignment-handler steps))


;; Step Description
(provide (struct-out Step))
(struct: Step ([id : String]
               [instructions : String]
               [reviews : (Listof Review)]) #:transparent)

;; Review Description
(provide step)
(: step (String String Review * -> Step))
(define (step id instructions . reviews)
  (Step id instructions reviews))

(provide Review)
(define-type Review (U student-submission instructor-solution))

(provide Review-id)
(: Review-id (Review -> String))
(define (Review-id review)
  (cond [(student-submission? review) (student-submission-id review)]
        [(instructor-solution? review) (instructor-solution-id review)]))


(provide Review-amount)
(: Review-amount (Review -> Exact-Nonnegative-Integer))
(define (Review-amount review)
  (cond [(student-submission? review) (student-submission-amount review)]
        [(instructor-solution? review) 1]))


(provide Review-rubric)
(: Review-rubric (Review -> Rubric))
(define (Review-rubric review) 
  (cond [(student-submission? review) (student-submission-rubric review)]
        [(instructor-solution? review) (instructor-solution-rubric review)]
        [else (raise-user-error 'validate-step "Expected to find student-submissions / instructor-solution.")]))

;; Student Submission Review
(provide (struct-out student-submission))
(struct: student-submission ([id : String] 
                            [amount : Exact-Nonnegative-Integer]
                            [rubric : Rubric]) #:transparent)


;; Instructor Solution Review
(provide (struct-out instructor-solution))
(struct: instructor-solution ([id : String]
                             [rubric : Rubric]) #:transparent)


;; Rubric Description
(provide (struct-out Rubric))
(struct: Rubric ([elements : (Listof RubricElement)]) #:transparent)


(provide rubric)
(: rubric (RubricElement * -> Rubric))
(define (rubric . elements)
  (Rubric elements))


;; Rubric Element Descriptions

;; Instruction
(provide (struct-out instruction))
(struct: instruction ([text : String]) #:transparent)

;; Likert
(provide (struct-out likert))
(struct: likert ([id : String]
                 [text : String]
                 [min : String]
                 [max : String]
                 [granularity : Exact-Nonnegative-Integer]) #:transparent)



;; Free Form
(provide (struct-out free-form))
(struct: free-form ([id : String]
                    [text : String]) #:transparent)


(provide RubricElement)
(define-type RubricElement (U free-form likert instruction ))

(provide rubric-element?)
(: rubric-element? (Any -> Boolean : RubricElement))
(define (rubric-element? any)
  (or (instruction? any)
      (likert? any)
      (free-form? any)))

(provide review?)
(: review? (Any -> Boolean : Review))
(define (review? any)
  (or (student-submission? any)
      (instructor-solution? any)))

;; Used to state a students next action is to submit to the step
(provide (struct-out MustSubmitNext))
(struct MustSubmitNext ([step : Step]
                        [instructions : String]) #:transparent)

;; Used to state a students next action is to review on a step
(provide (struct-out MustReviewNext))
(struct MustReviewNext ([step : Step] 
                        [reviews : (Listof String)]) #:transparent)


;; Assignment Handler
;; next-action: (assignment -> steps -> (Either MustSubmitNext MustReviewNext #t))
;;   Given an assignment-id and the list of steps to complete, returns the next-action the user must take
;;   or #t if the user has completed the assignment
;; do-submit-step: (assignment -> step -> uid -> file-name -> data -> steps -> (Either Success Failure))
;;   Given an assignment, a step, user id, file name being submitted, the data of the file, and a list of assignment steps
;;   attempts to submit the data for the specified assignment user and step.
;; get-dependencies: (assignment -> ListOf dependecy)
;; take-dependencies: assignment-id -> dependency -> bindings -> raw-bindings -> Either Success Failure
(provide (struct-out AssignmentHandler))
(struct AssignmentHandler ([next-action : (Assignment (Listof Step) String -> (U MustSubmitNext MustReviewNext #t))]
                           ;; TODO(after-typed): Remove optional (U String #f) and (U Bytes #f)
                           [do-submit-step : (Assignment Step String (U String #f) (U Bytes #f) (Listof Step) -> (Result String))]
                           [get-dependencies : (Assignment -> (Listof Dependency))] 
                           [take-dependency : (String Dependency (Listof (Pairof Symbol (U String Bytes))) (Listof Any) -> (Result String))]
                           [key : String]))


(provide dependency-submission-name)
(: dependency-submission-name (String Exact-Nonnegative-Integer -> String))
(define (dependency-submission-name review-id n)
  (string-append "default-submission-" review-id "-" (number->string n)))


;;TODO(3-study): change instructor-solution to be a message
;; A dependency for an assignment to be open
;; step-id: The step id
;; review-id: The review id
;; amount: The number of files that are needed for this dependency to be met
;; instructor-solution: If this is an instructor solution dependency
;; met: #t if this dependency has been met and #f otherwise.

(provide Dependency)
(define-type Dependency dependency)

(provide dependency-met)
(struct: dependency ([met : Boolean]) #:transparent)

(provide (struct-out review-dependency)) 
(struct: review-dependency dependency ([step-id : String]
                                       [review-id : String]) #:transparent)

(provide (struct-out student-submission-dependency))
(struct: student-submission-dependency review-dependency ([amount : Exact-Nonnegative-Integer]) #:transparent)

(provide (struct-out instructor-solution-dependency))
(struct instructor-solution-dependency review-dependency () #:transparent)

(provide (struct-out three-study-config-dependency))
(struct three-study-config-dependency dependency () #:transparent)