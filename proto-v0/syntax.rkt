#lang racket

(define-syntax-rule (finished)
  (begin '(finished)))

(define-syntax-rule (skip)
  (begin '(skip)))

(define-syntax-rule (sequence assign-part ...)
  (begin (list 'sequence assign-part ...)))

(define-syntax-rule (submission name form validator)
  (begin (list 'submission name form)))

(define-syntax-rule (review-task target find-resource rubric feedback)
  (begin (list 'review-task target rubric feedback)))

(define-syntax-rule (repeat assign-part pred)
  (begin (list 'repeat assign-part pred)))

(define-syntax-rule (shuffled-seq assign-part ...)
  (begin (list 'shuffled-seq assign-part ...)))

(define-syntax-rule (conditional pred assign-part ...)
  (begin (list 'conditional assign-part ...)))

(define-syntax-rule (assignment-desc groups resources parts)
  (begin (list 'assignment-desc groups resources parts)))

(define-syntax-rule (group-choice choice ...)
  (begin (list 'group-choice choice ...)))

(define assign-1
  (assignment-desc
   '("all-students")
   '()
   (sequence
      (submission "test-submission" 'test-submission-form 'test-submission-validator)
      (submission "impl-submission" 'impl-submission-form 'impl-submission-validator))))

(struct resource (name target))

(define assign-2-resources
  (list (resource "good-solution" "github.com/umass/tic-tac-toe/good-ddef.scala")
        (resource "bad-solution" "github.com/umass/tic-tac-toe/bad-ddef.scala")))

; given a list (or map) of resources, finds the resource with the specified name
(define (get-resource resources name)
  '())

(define (find-resource resources name)
  (lambda (student-id ref)
    (get-resource resources name)))

; select resource from submissions 
;        where ref=ref and student_id != student_id 
;        order by submit_time desc review_count asc 
;        limit 1;
(define (get-recent-review student-id ref)
  '())

(define (compute-good-feedback rubric-response)
  '())

(define (compute-bad-feedback rubric-response)
  '())

(define (compute-student-feedback rubric-response)
  '())

(define (data-definition-validator resource)
  '())

(define (test-validator resource)
  '())

(define (impl-validator resource)
  '())

(define assign-2
  (assignment-desc
   '("instructor-first", "no-instructor")
   assign-2-resources
   (sequence
     (submission "data-definition" 'data-definition-form data-definition-validator)
     (group-choice 
      `("instructor-first" . ,(sequence
                                 (review-task "data-definition"  
                                              (find-resource assign-2-resources "good-solution")
                                              'data-definition-rubric
                                              compute-good-feedback)
                                 (review-task "data-definition"
                                              (find-resource assign-2-resources "bad-solution")
                                              'data-definition-rubric
                                              compute-bad-feedback)
                                 (review-task "data-definition"
                                              get-recent-review
                                              'data-definition-rubric
                                              compute-student-feedback)))
      `("no-instructor" . ,(sequence
                             (review-task "data-definition"
                                          get-recent-review
                                          'data-definition-rubric
                                          compute-student-feedback)
                             (review-task "data-definition"
                                          get-recent-review
                                          'data-definition-rubric
                                          compute-student-feedback)
                             (review-task "data-definition"
                                          get-recent-review
                                          'data-definition-rubric
                                          compute-student-feedback))))
     (submission "test-submission" 'test-submission-form test-validator)
     (submission "impl-submission" 'impl-submission-form impl-validator))))