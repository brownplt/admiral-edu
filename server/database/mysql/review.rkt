#lang racket

(require db
         "common.rkt"
         (prefix-in submission: "submission.rkt")
         (prefix-in class: "class.rkt")
         (prefix-in assignment: "assignment.rkt")
         (prefix-in user: "user.rkt")
         (prefix-in role: "role.rkt"))

(provide table)
(define table "review")

(provide assignment-id assignment-id-type)
(define assignment-id "assignment_id")
(define assignment-id-type submission:assignment-id-type)

(provide step-id step-id-type)
(define step-id "step_id")
(define step-id-type submission:step-id-type)

(provide version-type version)
(define version "version")
(define version-type submission:version-type)

(provide class-id class-id-type)
(define class-id "class_id")
(define class-id-type submission:class-id-type)

(provide reviewee-id reviewee-id-type)
(define reviewee-id "reviewee_id")
(define reviewee-id-type submission:user-id-type)

(provide reviewer-id reviewer-id-type)
(define reviewer-id "reviewer_id")
(define reviewer-id-type submission:user-id-type)

(provide time-stamp time-stamp-type)
(define time-stamp "time_stamp")
(define time-stamp-type "TIMESTAMP")

;; Initializes the review table.
(provide init)
(define (init)
  (let ((drop (prepare sql-conn (merge "DROP TABLE IF EXISTS" table)))
        (create (prepare sql-conn (merge "CREATE TABLE" table "("
                                         assignment-id assignment-id-type ","
                                         class-id class-id-type ","
                                         step-id step-id-type ","
                                         reviewee-id reviewee-id-type ","
                                         reviewer-id reviewer-id-type ","
                                         time-stamp time-stamp-type ","
                                         version version-type ","
                                         "PRIMARY KEY (" assignment-id "," class-id "," step-id "," reviewee-id "," reviewer-id "," version "))"))))
    (query-exec sql-conn drop)
    (query-exec sql-conn create)))

(provide create)
(define (create assignment class step reviewee reviewer version)
  (if (not (submission:exists? assignment class step reviewee version)) 'no-such-submission
      (let* ((query (merge "INSERT INTO" table "VALUES(?,?,?,?,?,NOW(),?)"))
             (prep (prepare sql-conn query)))
        (query-exec sql-conn prep assignment class step reviewee reviewer version))))

(provide select-least-reviewed)
(define (select-least-reviewed assignment class step)
  (let* ((query (merge "SELECT" reviewee-id
                       "FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=? AND"
                               step-id "=?"
                       "GROUP BY" reviewee-id
                       "ORDER BY COUNT(*) ASC, " time-stamp " DESC LIMIT 1"))
         (prep (prepare sql-conn query))
         (result (vector-ref (query-row sql-conn prep assignment class step) 0)))
    result))

(provide count)
(define (count assignment class step reviewee)
  (let* ((query (merge "SELECT COUNT(*)"
                       "FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=? AND"
                               step-id "=? AND"
                               reviewee-id "=?"))
         (prep (prepare sql-conn query))
         (result (vector-ref (query-row sql-conn prep assignment class step reviewee) 0)))
    result))

(provide count-version)
(define (count-version assignment class step reviewee version)
  (let* ((query (merge "SELECT COUNT(*)"
                       "FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=? AND"
                               step-id "=? AND"
                               reviewee-id "=? AND"
                               version "=?"))
         (prep (prepare sql-conn query))
         (result (vector-ref (query-row sql-conn prep assignment class step reviewee version) 0)))
    result))