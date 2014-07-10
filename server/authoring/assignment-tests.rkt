#lang racket

(require "assignment.rkt"
         (planet cce/fasttest:4:2/random)
         (planet cce/fasttest:4:2/rackunit)
         rackunit
         rackunit/text-ui)

(random-seed 1)
(run-tests 
 (test-random
  ([text (random-string)])
  (check-pred rubric-element? (instruction text))))

(random-seed 1)
(run-tests 
 (test-random
  ([text (random-string)])
  (let ((i (instruction text)))
    (check-equal? i (yaml->instruction (instruction->yaml i))))))

(random-seed 1)
(run-tests
 (test-random
  ([id (random-string)]
   [text (random-string)])
  (check-pred rubric-element? (free-form id text))))

(random-seed 1)
(run-tests
 (test-random
  ([id (random-string)]
   [text (random-string)])
  (let ((form (free-form id text)))
    (check-equal? form (yaml->free-form (free-form->yaml form))))))

(random-seed 1)
(run-tests
 (test-random
  ([id (random-string)]
   [text (random-string)]
   [min (random-string)]
   [max (random-string)]
   [granularity (random-integer)])
  (check-pred rubric-element? (likert id text min max granularity))))

(random-seed 1)
(run-tests
 (test-random
  ([id (random-string)]
   [text (random-string)]
   [min (random-string)]
   [max (random-string)]
   [granularity (random-integer)])
  (let ((l (likert id text min max granularity)))
    (check-equal? l (yaml->likert (likert->yaml l))))))