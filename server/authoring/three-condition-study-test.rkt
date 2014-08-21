#lang racket

(require "../database/mysql.rkt"
         "../database/mysql/common.rkt"
         "assignment.rkt"
         "three-condition-study.rkt"
         (planet esilkensen/yaml:3:1)
         "../config.rkt"
         db
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


(define (init-tests)
  (init-db)
  (class:create class-name)

  
  (roles:create instructor-role "Instructor" 1)
  (roles:create ta-role "Teaching Assistant" 1)
  (roles:create student-role "Student" 0)

  (map make-student (list ACE AMY ART ALF JOE JAN JIM JON SAL SAM STU SUE SID))
  (create-assignment three-test-assignment)
  (save-assignment-description class-name "test-assignment" (file->string "test-assignment-description.yaml")))

(define three-test-assignment (yaml->assignment (string->yaml (file->string "test-assignment-description.yaml"))))

(define (run-tests)
  (initialize))

; groups:
; gets-review: ACE AMY ART ALF
; does-review: JOE JAN JIM JON
; no-review: SAL SAM STU SUE SID

(define useless-tar-file
  (file->string "empty.tar"))

(define (test-submit-order-submit user)
  ;; NOTE(joe): this seems to be enough to get different timestamps so our
  ;; ordering tests work well
  (sleep 1)
  (three-do-submit-step (Assignment-id three-test-assignment) "tests" user useless-tar-file (Assignment-steps three-test-assignment)))

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
         (prep (prepare sql-conn q))
         (result (query-rows sql-conn prep assignment-id step-id)))
    result))

(define (get-all-reviews)
  (let* ((q (merge "SELECT *"
                   "FROM" review:table
                   "ORDER BY" review:time-stamp "ASC"))
         (prep (prepare sql-conn q))
         (result (query-rows sql-conn prep)))
    result))
