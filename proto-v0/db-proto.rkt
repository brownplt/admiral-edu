#lang racket

(require db
         racket/serialize
         racket/fasl)

;; Define a sql connection. For now, use sqlite3
(define sql-conn 
  (sqlite3-connect #:database "proto.db" #:mode 'create))

;; initialize the database
(provide init-db)
(define (init-db)
  (init-student-table)
  (init-class-table)
  (init-registered-table)
  (init-assignment-table)
  (init-groups-table)
  (init-group-assignment-table)
  (init-assign-status-table)
  (init-submissions-table)
  (init-reviews-table))

;; student table
(define students "students")
(define student_id "student_id")

(define (init-student-table)
  (query-exec sql-conn (merge "drop table if exists" students))
  (query-exec sql-conn (merge "create table" students "(" student_id "varchar(255) unique)")))

(provide create-student)
(define (create-student student)
  (query-exec sql-conn (merge "insert into" students "values ($1)") student))
  
;; classes table
(define classes "classes")
(define class_id "class_id")

(define (init-class-table)
  (query-exec sql-conn (merge "drop table if exists" classes))
  (query-exec sql-conn (merge "create table" classes "(" class_id "varchar(255) unique)")))  
  
(provide create-class)
(define (create-class class)
  (query-exec sql-conn (merge "insert into" classes "values ($1)") class))

(provide get-classes)
(define (get-classes)
  (query-rows sql-conn (merge "select * from" classes)))

;; registration table
(define registered "registered")

(define (init-registered-table)
  (query-exec sql-conn (merge "drop table if exists" registered))
  (query-exec sql-conn (merge "create table" registered "(" student_id "varchar(255)," class_id "varchar(255))")))

(provide register-student)
(define (register-student student class)
  (query-exec sql-conn (merge "insert into" registered "values ($1, $2)") student class))

(provide is-registered?)
(define (is-registered? student class)
  (let* ((q (merge "select count(*) from" registered "where" student_id "=$1 AND" class_id "=$2 limit 1"))
         (rows (query-row sql-conn q student class))
         (count (vector-ref rows 0)))
    (= count 1)))

(provide get-registered)
(define (get-registered class)
  (query-rows sql-conn (merge "select" student_id "from" registered "where" class_id "=$1") class))
         

;; Assignments table
;; When creating an assignment, one must provide an iterator, and a request_generator.

;; an assignment iterator is a function that takes in a student_id, step_id, and a resource
;; and produces a message stating success or failure and the next step_id the student
;; will have to complete (on failure this will probably be the original step_id)

;; A request_generator takes in a step_id and produces a request for that step

(define assignments "assignments")
(define assignment_id "assignment_id")
(define assignment_iterator "assignment_iterator")
(define request_generator "request_generator")

(define (init-assignment-table)
  (query-exec sql-conn (merge "drop table if exists" assignments))
  (query-exec sql-conn (merge "create table" assignments "(" assignment_id "varchar(255) unique," assignment_iterator "blob," request_generator "blob)")))

(provide create-assignment)
(define (create-assignment assignment iterator generator)
  (query-exec sql-conn (merge "insert into" assignments "values ($1, $2, $3)") assignment (s-exp->fasl (serialize iterator)) (s-exp->fasl (serialize generator))))

(provide get-assignment)
(define (get-assignment assignment)
  (let* ((q (merge "select" assignment_iterator "," request_generator "from" assignments "where" assignment_id "=$1 limit 1"))
         (row (query-row sql-conn q assignment))
         (iterator (deserialize (fasl->s-exp (vector-ref row 0))))
         (generator (deserialize (fasl->s-exp (vector-ref row 1)))))
    (list iterator generator)))

(provide is-assignment?)
(define (is-assignment? assignment)
  (let* ((q (merge "select count(*) from" assignments "where" assignment_id "=$1 limit 1"))
         (row (query-row sql-conn q assignment))
         (count (vector-ref row 0)))
    (= 1 count)))

(define groups "groups")
(define group_id "group_id")
(define (init-groups-table)
  (query-exec sql-conn (merge "drop table if exists" groups))
  (query-exec sql-conn (merge "create table" groups "(" group_id "varchar(255)," assignment_id "varchar(255))")))

(provide create-group)
(define (create-group group assignment)
  (query-exec sql-conn (merge "insert into" groups "values ($1, $2)") group assignment))

(provide get-groups)
(define (get-groups assignment)
  (query-list sql-conn (merge "select" group_id "from" groups "where" assignment_id "=$1") assignment))

;; Group assignment table
(define group_assignment "group_assignment")
(define (init-group-assignment-table)
  (query-exec sql-conn (merge "drop table if exists" group_assignment))
  (query-exec sql-conn (merge "create table" group_assignment "(" group_id "varchar(255)," assignment_id "varchar(255)," student_id "varchar(255))")))

(provide assign-group)
(define (assign-group group assignment student)
  (query-exec sql-conn (merge "insert into" group_assignment "values ($1, $2, $3)") group assignment student))

(provide group-size)
(define (group-size group assignment)
  (let* ((q (merge "select count(*) from" group_assignment "where" group_id "=$1 AND" assignment_id "=$2 limit 1"))
         (row (query-row sql-conn q group assignment)))
    (vector-ref row 0)))

(provide get-group)
(define (get-group group assignment)
  (query-list sql-conn (merge "select" student_id "from" group_assignment "where" group_id "=$1 AND" assignment_id "=$2") group assignment))

(provide get-students-group)
(define (get-students-group student assignment)
  (car (query-list sql-conn (merge "select" group_id "from" group_assignment "where" student_id "=$1 AND" assignment_id "=$2 limit 1") student assignment)))

;; Status table
(define assign_status "assign_status")
(define step_id "step_id")
(define (init-assign-status-table)
  (query-exec sql-conn (merge "drop table if exists" assign_status))
  (query-exec sql-conn (merge "create table" assign_status "(" student_id "varchar(255)," assignment_id "varchar(255)," step_id "varchar(255))")))

(provide has-started?)
(define (has-started? student assignment)
  (let* ((q (merge "select count(*) from" assign_status "where" student_id "=$1 AND" assignment_id "=$2 limit 1"))
         (row (query-row sql-conn q student assignment))
         (count (vector-ref row 0)))
    (not (= count 0))))

(provide is-finished?)
(define (is-finished? student assignment)
  (let* ((q (merge "select count(*) from" assign_status "where" student_id "=$1 AND" assignment_id "=$2 AND" step_id "='finished' limit 1"))
         (row (query-row sql-conn q student assignment))
         (count (vector-ref row 0)))
    (not (= count 0))))

(provide set-status)
(define (set-status student assignment step)
  (if (has-started? student assignment)
      (update-status student assignment step)
      (create-status student assignment step)))

(define (update-status student assignment step)
  (let ((q (merge "update" assign_status "set" step_id "=$1 where" assignment_id "=$2 AND" student_id "=$3")))
    (query-exec sql-conn q step assignment student)))
         
(define (create-status student assignment step)
  (let ((q (merge "insert into" assign_status "values($1,$2,$3)")))
    (query-exec sql-conn q student assignment step)))

(provide get-status)
(define (get-status student assignment)
  (let* ((q (merge "select" step_id "from" assign_status "where" student_id "=$1 AND" assignment_id "=$2 limit 1"))
         (row (query-row sql-conn q student assignment)))
    (vector-ref row 0)))

;; Submission table
(define submissions "submissions")
(define resource "resource")
(define times_reviewed "times_reviewed")
(define (init-submissions-table)
  (query-exec sql-conn (merge "drop table if exists" submissions))
  (query-exec sql-conn (merge "create table" submissions "(" student_id "varchar(255)," assignment_id "varchar(255)," step_id "varchar(255)," resource "blob," times_reviewed " integer)")))

(provide can-submit)
(define (can-submit student assignment step)
  (let* ((q (merge "select count(*) from" assign_status "where" student_id "=$1 AND" assignment_id "=$2 AND" step_id "=$3 limit 1"))
         (vec (query-row sql-conn q student assignment step))
         (count (vector-ref vec 0)))
    (= count 1)))

(provide create-submission)
(define (create-submission student assignment step res)
  (query-exec sql-conn (merge "insert into" submissions "values ($1, $2, $3, $4, 0)") student assignment step (s-exp->fasl(serialize res))))

(provide get-submission)
(define (get-submission student assignment step)
  (let* ((q (merge "select" resource "from" submissions "where" student_id "=$1 AND" assignment_id "=$2 AND" step_id "=$3 limit 1"))
         (row (query-row sql-conn q student assignment step)))
    (deserialize (fasl->s-exp (vector-ref row 0)))))

(provide incr-reviews)
(define (incr-reviews student assignment step)
  (let* ((q (merge "select" times_reviewed "from" submissions "where" student_id "=$1 AND" assignment_id "=$2 AND" step_id "=$3 limit 1"))
         (row (query-row sql-conn q student assignment step))
         (val (+ (vector-ref row 0) 1)))
    (query-exec sql-conn (merge "update" submissions "set" times_reviewed "=$1 where" student_id "=$2 AND" assignment_id "=$3 AND" step_id "=$4") val student assignment step)))

(provide get-least-reviewed)
(define (get-least-reviewed assignment step)
  (write (string-append "get-least-reviewed " assignment " " step))
  (let* ((q (merge "select" student_id "from" submissions "where" assignment_id "=$1 AND" step_id "=$2 order by" times_reviewed " ASC limit 1"))
         (row (query-row sql-conn q assignment step))
         (student (vector-ref row 0)))
    student))

;; Reviews table
(define reviews "reviews")
(define reviewee_id "reviewee_id")
(define review_step "review_step")
(define (init-reviews-table)
  (query-exec sql-conn (merge "drop table if exists" reviews))
  (query-exec sql-conn (merge "create table" reviews "(" student_id "varchar(255)," reviewee_id "varchar(255)," assignment_id "varchar(255)," step_id "varchar(255)," review_step "varchar(255))")))

(provide get-review-resource)
(define (get-review-resource student_id assignment_id step_id)
  (write (string-append "get-review-resource " student_id " " assignment_id " " step_id))
  (let* ((q "select resource from submissions, reviews where reviews.student_id=$1 AND reviews.assignment_id=$2 AND reviews.review_step=$3 AND reviews.reviewee_id=submissions.student_id AND reviews.assignment_id=submissions.assignment_id AND reviews.step_id=submissions.step_id limit 1")
         (row (query-row sql-conn q student_id assignment_id step_id)))
    (deserialize (fasl->s-exp (vector-ref row 0)))))

(provide create-review)
(define (create-review student reviewee assignment step review-step)
  (let ((q (merge "insert into" reviews "values ($1, $2, $3, $4, $5)")))
    (incr-reviews reviewee assignment step)
    (query-exec sql-conn q student reviewee assignment step review-step)))

;; Helper functions
(define (intercalate v ls)
  (cdr 
   (foldr 
    (lambda (x xs) 
      (cons v (cons x xs))) 
    '() ls)))

(define (merge . strings)
  (foldr string-append ""
         (intercalate " " strings)))