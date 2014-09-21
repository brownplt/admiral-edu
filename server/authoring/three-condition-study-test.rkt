#lang racket

(require "../base.rkt"
         "../database/mysql/common.rkt"
         "assignment.rkt"
         "assignment-structs.rkt"
         "assignment-parser.rkt"
         "three-condition-study.rkt"
         (planet esilkensen/yaml:3:1)
         db
         rackunit)

(define (make-student id)
    (user:create id)
    (role:associate class-name id student-role))

;; Gets Reviews
(define ACE "ace")
(define AMY "amy")
(define ART "art")
(define ALF "alf")
(define gets-reviews (list ACE AMY ART ALF))
  
;; Does reviews
(define JOE "joe")
(define JAN "jan")
(define JIM "jim")
(define JON "jon")
(define does-reviews (list JOE JAN JIM JON))

;; No Reviews
(define SAL "sal")
(define SAM "sam")
(define STU "stu")
(define SUE "sue")
(define SID "sid")
(define no-reviews (list SAL SAM STU SUE SID))

(define all-students (list ACE AMY ART ALF JOE JAN JIM JON SAL SAM STU SUE SID))

(set-db-address! "localhost")

(define (init-tests)
  (init-db)
  (class:create class-name)
  

  
  (roles:create instructor-role "Instructor" 1)
  (roles:create ta-role "Teaching Assistant" 1)
  (roles:create student-role "Student" 0)

  (map make-student (list ACE AMY ART ALF JOE JAN JIM JON SAL SAM STU SUE SID))
  (create-assignment three-test-assignment)
  (write-file (dependency-file-name "test-assignment") (file->string "sample-yaml/test-assignment.yaml"))
  (save-assignment-description class-name "test-assignment" (file->string "sample-yaml/test-assignment-description.yaml")))

(define three-test-assignment (yaml->assignment (string->yaml (file->string "sample-yaml/test-assignment-description.yaml"))))
(define three-test-assignment-tests-step
  (let* ((steps (Assignment-steps three-test-assignment))
         (step (filter (lambda (step) (string=? (Step-id step) "tests")) steps)))
    (first step)))

(define three-test-assignment-implementation-step
  (let* ((steps (Assignment-steps three-test-assignment))
         (step (filter (lambda (step) (string=? (Step-id step) "implementation")) steps)))
    (first step)))

(define (run-tests)
  (initialize)
  (test-submit-order)
  (test-reviewers-submit-first)
  (test-reflection-assigned))

; groups:
; gets-review: ACE AMY ART ALF
; does-review: JOE JAN JIM JON
; no-review: SAL SAM STU SUE SID

(define useless-tar-file
  (file->string "sample-yaml/empty.tar"))

(define (test-submit-order-submit user)
  ;; NOTE(joe): this seems to be enough to get different timestamps so our
  ;; ordering tests work well
  (sleep 1)
  (three-do-submit-step three-test-assignment three-test-assignment-tests-step user "useless.tar" useless-tar-file (Assignment-steps three-test-assignment)))

(define (check-review-assignments submission-list expected-assignments)
  (map test-submit-order-submit submission-list)
  (define reviews (get-reviews-for-check "test-assignment" "tests"))
  (check-equal? (length reviews) (length expected-assignments))
  (define expect-set (list->set expected-assignments))
  (define actual-set (list->set reviews))
  ;; NOTE(joe): checking the difference both ways provides more useful output
  (define assigned-not-expected (set-subtract actual-set expect-set))
  (define expected-not-assigned (set-subtract expect-set actual-set))
  (check-equal? assigned-not-expected (set))
  (check-equal? expected-not-assigned (set))
  )
  

(define (test-submit-order)
  (init-tests)
  ;; No reviews yet because only gets-review submissions
  (check-review-assignments (list ACE AMY ART ALF) (list))
  (check-review-assignments (list JOE) (list (cons ACE JOE) (cons AMY JOE) (cons ART JOE)))
  (check-review-assignments (list JAN JIM JON SAL SAM STU SUE SID) (list
                                                                    (cons ACE JOE)
                                                                    (cons AMY JOE)
                                                                    (cons ART JOE)
                                                                    (cons ALF JAN)
                                                                    (cons ACE JAN)
                                                                    (cons AMY JAN)
                                                                    (cons ART JIM)
                                                                    (cons ALF JIM)
                                                                    (cons ACE JIM)
                                                                    (cons AMY JON)
                                                                    (cons ART JON)
                                                                    (cons ALF JON)))
   )
(define HOLD "HOLD")
(define (test-reviewers-submit-first)
  (init-tests)
  ;; No reviews yet because only gets-review submissions
  (check-review-assignments (list JOE JAN JIM JON)
                            (list
                             (cons HOLD JOE)
                             (cons HOLD JOE)
                             (cons HOLD JOE)
                             (cons HOLD JAN)
                             (cons HOLD JAN)
                             (cons HOLD JAN)
                             (cons HOLD JIM)
                             (cons HOLD JIM)
                             (cons HOLD JIM)
                             (cons HOLD JON)
                             (cons HOLD JON)
                             (cons HOLD JON)))
    (check-review-assignments (list AMY)
                            (list
                             (cons AMY JOE)
                             (cons HOLD JOE)
                             (cons HOLD JOE)
                             (cons AMY JAN)
                             (cons HOLD JAN)
                             (cons HOLD JAN)
                             (cons AMY JIM)
                             (cons HOLD JIM)
                             (cons HOLD JIM)
                             (cons HOLD JON)
                             (cons HOLD JON)
                             (cons HOLD JON)))
  (check-review-assignments (list ACE)
                            (list
                             (cons AMY JOE)
                             (cons ACE JOE)
                             (cons HOLD JOE)
                             (cons AMY JAN)
                             (cons ACE JAN)
                             (cons HOLD JAN)
                             (cons AMY JIM)
                             (cons HOLD JIM)
                             (cons HOLD JIM)
                             (cons ACE JON)
                             (cons HOLD JON)
                             (cons HOLD JON)))
  (check-review-assignments (list SUE)
                            (list
                             (cons AMY JOE)
                             (cons ACE JOE)
                             (cons HOLD JOE)
                             (cons AMY JAN)
                             (cons ACE JAN)
                             (cons HOLD JAN)
                             (cons AMY JIM)
                             (cons HOLD JIM)
                             (cons HOLD JIM)
                             (cons ACE JON)
                             (cons HOLD JON)
                             (cons HOLD JON)))

   )

(define (get-reviews-for-check assignment-id step-id)
  (define (vec->pair v) (cons (vector-ref v 0) (vector-ref v 1)))
  (map vec->pair (get-reviews "test-assignment" "tests")))

(define (get-reviews assignment-id step-id)
  (let* ((q (merge "SELECT" review:reviewee-id "," review:reviewer-id
                   "FROM" review:table
                   "WHERE" review:assignment-id "=? AND"
                           review:step-id "=?"
                   "ORDER BY" review:time-stamp))
         (result (run query-rows q assignment-id step-id)))
    result))

(define (get-all-reviews)
  (let* ((q (merge "SELECT *"
                   "FROM" review:table
                   "ORDER BY" review:time-stamp "ASC"))
         (result (run query-rows q)))
    result))


(define (test-reflection-assigned)
  (init-tests)
  (map test-reflection-assigned-submit-tests all-students)
  (map (test-reflection-assigned-submit-reviews "tests") does-reviews)
  (map test-must-submit-next all-students)
  (map test-reflection-assigned-submit-implementation all-students)
  (map (check-review-exists "does-reviews") does-reviews)
  (map (check-review-exists "no-reviews") no-reviews)
  (map (check-review-exists "gets-reviewed") gets-reviews)
  (map check-assignment-incomplete all-students)
  (map (test-reflection-assigned-submit-reviews "implementation") all-students)
  (map check-assignment-completed all-students))

(define (test-must-submit-next user)
  (let ((next (three-next-action three-test-assignment (Assignment-steps three-test-assignment) user)))
    (printf "Checking if ~a should submit next." user)
    (check-true (MustSubmitNext? next))))

(define (check-assignment-incomplete user)
  (let ((next (three-next-action three-test-assignment (Assignment-steps three-test-assignment) user)))
    (check-true (MustReviewNext? next))
    (check-equal? (Step-id (MustReviewNext-step next)) "implementation")
    (check-equal? (length (MustReviewNext-reviews next)) 1)))

(define (check-assignment-completed user)
  (check-equal? (three-next-action three-test-assignment (Assignment-steps three-test-assignment) user) #t))

(define (check-review-exists review-id-expected)
  (lambda (user)
    (let* ((hashes (review:select-assigned-reviews "test-assignment" class-name "implementation" user))
           (reviews (map review:select-by-hash hashes))
           (assigned-expected 1))
      (check-equal? (list user review-id-expected (length reviews)) (list user review-id-expected assigned-expected))
      (check-equal? (review:Record-review-id (first reviews)) review-id-expected))))

(define (test-reflection-assigned-submit-reviews step-id)
  (lambda (user)
    (let ((review-hashes (review:select-assigned-reviews "test-assignment" class-name step-id user)))
      (map review:mark-complete review-hashes))))

(define (test-reflection-assigned-submit-tests user)
  (sleep 1)
  (three-do-submit-step three-test-assignment three-test-assignment-tests-step user "useless.tar" useless-tar-file (Assignment-steps three-test-assignment)))

(define (test-reflection-assigned-submit-implementation user)
  (sleep 1)
  (printf "Submitting implementation for: ~a\n" user)
  (three-do-submit-step three-test-assignment three-test-assignment-implementation-step user "useless.tar" useless-tar-file (Assignment-steps three-test-assignment)))


