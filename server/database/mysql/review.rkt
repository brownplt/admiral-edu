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

(provide completed completed-type)
(define completed "completed")
(define completed-type "BOOL")

(provide hash hash-type)
(define hash "hash")
(define hash-type "VARCHAR(255)")

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
                                         completed completed-type ","
                                         hash hash-type ","
                                         "PRIMARY KEY (" assignment-id "," class-id "," step-id "," reviewee-id "," reviewer-id "," version "))"))))
    (query-exec sql-conn drop)
    (query-exec sql-conn create)))

(provide create)
(define (create assignment class step reviewee reviewer version)
  (if (not (submission:exists? assignment class step reviewee version)) 'no-such-submission
      (let* ((query (merge "INSERT INTO" table "VALUES(?,?,?,?,?,NOW(),?,false,?)"))
             (prep (prepare sql-conn query)))
        (query-exec sql-conn prep assignment class step reviewee reviewer version (random-hash)))))

(provide select-review)
(define (select-review assignment class step reviewer)
  (let* ((temp_table (merge "(SELECT" reviewee-id  "," version 
                              "FROM" table 
                              "WHERE" assignment-id "=? AND"
                                      class-id "=? AND"
                                      step-id "=? AND"
                                      reviewer-id "=? AND"
                                      completed "=false"
                              "ORDER BY" time-stamp "DESC"
                              "LIMIT 1)" 
                              "AS temp_table"))
         (query (merge "SELECT COUNT(*)," reviewee-id "," version
                       "FROM" temp_table))
         (prep (prepare sql-conn query))
         (result (query-row sql-conn prep assignment class step reviewer))
         (new-review (= (vector-ref result 0) 0))
         (reviewee-id (vector-ref result 1))
         (version (vector-ref result 2))
         (least (if new-review (select-least-reviewed assignment class step reviewer) #f)))
    (cond
      [(not least) `(,reviewee-id . ,(number->string version))]
      [(eq? least 'no-reviews) 'no-reviews]
      [else (assign-review assignment class step reviewer least)])))

(define (assign-review assignment class step reviewer pair)
  (let ((reviewee (car pair))
        (version (cdr pair)))
     (create assignment class step reviewee reviewer version)
     `(,reviewee . ,version)))

(provide select-by-hash)
(define (select-by-hash the-hash)
  (let* ((query (merge "SELECT" class-id "," assignment-id "," step-id "," reviewee-id "," reviewer-id
                       "FROM" table
                       "WHERE" hash "=? LIMIT 1"))
         (prep (prepare sql-conn query))
         (result (query-row sql-conn prep the-hash)))
    (vector->list result)))
                       
 ;;(path (string-append "reviews/" class "/" assignment "/" stepName "/" reviewee "/" reviewer "/" path-to-file)))                       

(provide select-reviews)
(define (select-reviews reviewee)
  (let* ((query (merge "SELECT" hash "FROM" table "WHERE" reviewee-id "=?"))
         (prep (prepare sql-conn query))
         (result (query-rows sql-conn prep reviewee)))
    (flatten (map vector->list result))))

(provide select-least-reviewed)
(define (select-least-reviewed assignment class step not-user)
  (let* ((query (merge "SELECT COUNT(*)," submission:user-id "," submission:version
                       "FROM (SELECT" submission:table "." submission:user-id "," reviewer-id "," submission:table "." submission:version
                             "FROM (SELECT * FROM" submission:table
                                    "WHERE" submission:assignment-id "=? AND"
                                            submission:class-id "=? AND"
                                            submission:step-id "=? AND"
                                            submission:user-id "!=?"
                                            "ORDER BY" submission:time-stamp "DESC)"
                                            "AS " submission:table
                             "LEFT JOIN" table
                             "ON" table "." reviewee-id "=" submission:table "." submission:user-id " AND"
                                  table "." assignment-id "=" submission:table "." submission:assignment-id "AND"
                                  table "." class-id "=" submission:table "." submission:class-id "AND"
                                  table "." step-id "=" submission:table "." submission:step-id
                             "GROUP BY" submission:table "." submission:user-id
                             "ORDER BY " "COUNT(*) ASC," 
                                         submission:table "." submission:time-stamp "," 
                                         table "." time-stamp "DESC"
                             "LIMIT 1) AS temp"
                       "WHERE ISNULL(" reviewer-id ") OR"
                              reviewer-id "!=?"))
         (prep (prepare sql-conn query))
         (result (query-row sql-conn prep assignment class step not-user not-user))
         (no-reviews (= (vector-ref result 0) 0))
         (reviewee (vector-ref result 1))
         (ver (vector-ref result 2)))
    (if no-reviews 'no-reviews
        `(,reviewee . ,(number->string ver)))))

(provide completed?)
(define (completed? assignment class step reviewer)
  (let* ((query (merge "SELECT COUNT(*)"
                       "FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=? AND"
                               step-id "=? AND"
                               reviewer-id "=?"))
         (prep (prepare sql-conn query))
         (result (vector-ref (query-row sql-conn prep assignment class step reviewer) 0)))
    (> result 0)))

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

(define (random-hash)
  (for/fold ([s ""])
      ([x (in-range 32)])
    (string-append s
                   (number->string (truncate (random 15)) 16))))