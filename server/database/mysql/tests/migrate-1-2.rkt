#lang racket

(require db
         "../../../base.rkt"
         "../common.rkt"
         "../migrate-1-2.rkt"
         rackunit)

(set-db-address! "localhost")


(define (test-migrate)
  (init-system-table-v1)
  (init-review-table-v1)
  (check-equal? 1 (system:select-version))
  (check-true (Failure? (check-migrated)))
  (check-true (Success? (migrate)))
  (check-equal? 2 (system:select-version))
  (check-true (Success? (check-migrated)))
  (check-true (Failure? (migrate))))


(define (init-system-table-v1)
  (let ((drop (merge "DROP TABLE IF EXISTS" system:table))
        (create (merge "CREATE TABLE" system:table "("
                                      system:version system:version-type ","
                                      system:id system:id-type ","
                                      "PRIMARY KEY (" system:id "))"))
        (initial-version (merge "INSERT INTO" system:table "VALUES(?,?)")))
    (run query-exec drop)
    (run query-exec create)
    (run query-exec initial-version 1 system:the-id)))

;; Table structure for review used with version 1.
;; This is used for testing and shouldn't be called on a live database
(define (init-review-table-v1)
  (let* ((drop (merge "DROP TABLE IF EXISTS" review:table))
         (create (merge "CREATE TABLE" review:table "(" review:assignment-id review:assignment-id-type "," ; 0
                                                 review:class-id review:class-id-type "," ;1
                                                 review:step-id review:step-id-type "," ;2
                                                 review:reviewee-id review:reviewee-id-type "," ;3
                                                 review:reviewer-id review:reviewer-id-type "," ;4
                                                 review:time-stamp review:time-stamp-type "," ;5
                                                 review:completed review:completed-type "," ;6
                                                 review:hash review:hash-type "," ;7
                                                 review:review-id review:review-id-type "," ;8
                                                 review:instructor-solution review:instructor-solution-type "," ;9
                                                 review:flagged review:flagged-type "," ; 10
                                                 "PRIMARY KEY (" review:hash "))")))
    (run query-exec drop)
    (run query-exec create)))

(define test-assignment "test-assignment")
(define test-class "test-class")
(define test-step "test-step")
(define test-review-id "test-review-id")

(define (create-test-entry reviewee reviewer hash)
  (let ((query (merge "INSERT INTO" review:table "VALUES(?,?,?,?,?,NOW(),false,?,?,false,false)")))
    (run query-exec query test-assignment
                          test-class
                          test-step
                          reviewee
                          reviewer
                          hash
                          test-review-id)))