#lang racket

(require (planet esilkensen/yaml:3:1)
         json
         "assignment-structs.rkt"
         "util.rkt"
         "../database/mysql.rkt"
         "../config.rkt"
         (prefix-in machine: "progress-machine.rkt")
         "assignment.rkt")

(define (default-assign-review assignment-id step-id uid)
  (lambda (review)
    (let ((review-id (getId review))
          (amount (student-submission-amount review)))
      (cond [(instructor-solution? review) (review:assign-instructor-solution assignment-id class-name step-id "instructor" uid review-id)]
            [(student-submission? review) (review:assign-student-reviews assignment-id class-name step-id uid review-id amount)]))))

(provide select-least-reviewed)
(define (select-least-reviewed-gets-reviewed sql-conn assignment class step not-user)
  (let* ((query (merge "SELECT" user-id
                       "FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=? AND"
                               step-id "=? AND"
                               user-id "!=?"
                       "ORDER BY" times-reviewed "ASC"
                       "LIMIT 1"))
         (prep (prepare sql-conn query))
         (result (query-row sql-conn prep assignment class step not-user)))
    (vector-ref result 0)))
