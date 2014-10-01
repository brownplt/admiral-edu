#lang racket

(require "../review.rkt"
         "../typed-db.rkt"
         "../../../base.rkt"
         rackunit)

(set-db-address! "localhost")

(define test-class "test-class")
(define test-assignment "test-assignment")
(define test-step-id "test-step-id")
(define test-review-id "test-review-id")

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

(define all-students (list ACE AMY ART ALF JOE JAN JIM JON SAL SAM STU SUE SID))


(define (make-student id)
    (user:create id)
    (role:associate class-name id student-role))


(define (init-tests)
  (init-db)
  (class:create test-class)
  
  (roles:create instructor-role "Instructor" 1)
  (roles:create ta-role "Teaching Assistant" 1)
  (roles:create student-role "Student" 0)

  (map make-student all-students)
  (assignment:create test-assignment test-class))

(define (create-test-submission user)
  (submission:create test-assignment test-class test-step-id user))

(define (assign-review user)
  (review:assign-student-review test-assignment test-class test-step-id user test-review-id))

(define (count-assigned user)
  (count-assigned-reviews test-class test-assignment user test-step-id test-review-id))

(define (check-count n)
  (lambda (m) (check-equal? m n)))

(define (check-null-feedback record)
  (let ((feedback-time (review:Record-feedback-viewed-time-stamp record)))
    (check-true (Null? feedback-time))))

(define (check-not-null-feedback record)
  (let ((feedback-time (review:Record-feedback-viewed-time-stamp record)))
    (check-false (Null? feedback-time))))

(define (test-reviews-assigned)
  (init-tests)
  
  ; Create a submission for each student
  (map create-test-submission all-students)
  
  (map (check-count 0) (map count-assigned all-students))
  
  ; Assign each student 3 reviews
  (map assign-review all-students)
  ; Each students should be assigned 1 review
  (map (check-count 1) (map count-assigned all-students))
  (map assign-review all-students)
  ; Each students should be assigned 2 reviews
  (map (check-count 2) (map count-assigned all-students))
  (map assign-review all-students)
  ; Each students should be assigned 3 reviews
  (map (check-count 3) (map count-assigned all-students))
  (let ((records (apply append 
                        (map (lambda (ls)
                               (map review:select-by-hash ls))
                             (map review:select-reviews all-students)))))
    (map check-null-feedback records))
  
  (map review:mark-feedback-viewed (apply append (map review:select-reviews all-students)))
  
  (let ((records (apply append 
                        (map (lambda (ls)
                               (map review:select-by-hash ls))
                             (map review:select-reviews all-students)))))
    (map check-not-null-feedback records)
    (sleep 1)
    (map review:mark-feedback-viewed (apply append (map review:select-reviews all-students)))
    (let* ((records-two (apply append 
                               (map (lambda (ls)
                                      (map review:select-by-hash ls))
                                    (map review:select-reviews all-students))))
           (zipped (zip records records-two)))
      (map check-feedback-viewed-equal zipped)))
  (Success "Tests Completed"))

(define (zip l0 l1)
  (letrec ((helper (lambda (acc l0 l1)
                     (cond [(or (empty? l1) (empty? l0)) (reverse acc)]
                           [else (let ((left (first l0))
                                       (right (first l1)))
                                   (helper (cons (cons left right) acc) (rest l0) (rest l1)))]))))
    (helper '() l0 l1)))

(define (check-feedback-viewed-equal pair)
  (let ((first (car pair))
        (second (cdr pair)))
    (check-equal? (review:Record-feedback-viewed-time-stamp first) (review:Record-feedback-viewed-time-stamp second))))
               
