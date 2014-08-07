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

(provide review-id review-id-type)
(define review-id "review_id")
(define review-id-type "VARCHAR(255)")

(provide instructor-solution instructor-solution-type)
(define instructor-solution "instructor_solution")
(define instructor-solution-type "BOOLEAN")

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
                                         completed completed-type ","
                                         hash hash-type ","
                                         review-id review-id-type ","
                                         instructor-solution instructor-solution-type ","
                                         "PRIMARY KEY (" assignment-id "," class-id "," step-id "," reviewee-id "," reviewer-id "," review-id "))"))))
    (query-exec sql-conn drop)
    (query-exec sql-conn create)))

(provide create)
(define (create assignment class step reviewee reviewer id is-instructor-solution)
  (if (not (submission:exists? assignment class step reviewee)) 'no-such-submission
      (let* ((query (merge "INSERT INTO" table "VALUES(?,?,?,?,?,NOW(),false,?,?,?)"))
             (prep (prepare sql-conn query)))
        (query-exec sql-conn prep assignment class step reviewee reviewer (random-hash) id is-instructor-solution)
        ;; TODO: This is not concurrently safe.
        (submission:increment-reviewed assignment class step reviewee))))

(define (count-assigned assignment class step uid review-id)
  (let* ((query (merge "SELECT COUNT(*)"
                       "FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=? AND"
                               step-id "=? AND"
                               reviewer-id "=?"))
         (prep (prepare sql-conn query)))
    #t))

(provide assign-student-reviews)
(define (assign-student-reviews assignment class step uid review-id amount)
  (cond [(<= amount 0) #t]
        [else (assign-student-review assignment class step uid review-id)
              (assign-student-reviews assignment class step uid review-id (- amount 1))]))

(define (assign-student-review assignment class step uid review-id)
  (let ((reviewee (submission:select-least-reviewed assignment class step uid)))
    (cond [(eq? reviewee 'no-reviews) #f]
          [else (create assignment class step reviewee uid review-id 0)])))

(provide assign-instructor-solution)
(define (assign-instructor-solution assignment class step uid review-id)
  (create assignment class step "instructor" uid review-id 1))


#|
(provide select-review)
(define (select-review assignment class step reviewer)
  (let* ((temp_table (merge "(SELECT" reviewee-id
                              "FROM" table 
                              "WHERE" assignment-id "=? AND"
                                      class-id "=? AND"
                                      step-id "=? AND"
                                      reviewer-id "=? AND"
                                      completed "=false"
                              "ORDER BY" time-stamp "DESC"
                              "LIMIT 1)" 
                              "AS temp_table"))
         (query (merge "SELECT COUNT(*)," reviewee-id
                       "FROM" temp_table))
         (prep (prepare sql-conn query))
         (result (query-row sql-conn prep assignment class step reviewer))
         (new-review (= (vector-ref result 0) 0))
         (reviewee-id (vector-ref result 1))
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
|#

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

(provide select-assigned-reviews)
(define (select-assigned-reviews assignment class step uid rid)
  (let* ((query (merge "SELECT" hash
                       "FROM" table
                       "WHERE" class-id "=? AND"
                               assignment-id "=? AND"
                               step-id "=? AND"
                               review-id "=? AND"
                               reviewer-id "=?"))
         (prep (prepare sql-conn query))
         (result (query-rows sql-conn prep class assignment step rid uid)))
    (map vector->list result)))
    

;; TODO: Re-write using counter field.
#|
(provide select-least-reviewed)
(define (select-least-reviewed assignment class step not-user rid)
  (let* ((query (merge "SELECT COUNT(*)," submission:user-id
                       "FROM (SELECT" submission:table "." submission:user-id "," reviewer-id
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
                                  table "." step-id "=" submission:table "." submission:step-id "AND"
                                  table "." review-id "=?"
                             "GROUP BY" submission:table "." submission:user-id
                             "ORDER BY " "COUNT(*) ASC," 
                                         submission:table "." submission:time-stamp "," 
                                         table "." time-stamp "DESC"
                             "LIMIT 1) AS temp"
                       "WHERE ISNULL(" reviewer-id ") OR"
                              reviewer-id "!=?"))
         (prep (prepare sql-conn query))
         (result (query-row sql-conn prep assignment class step not-user rid not-user))
         (no-reviews (= (vector-ref result 0) 0))
         (reviewee (vector-ref result 1)))
    (if no-reviews 'no-reviews reviewee)))
|#

(provide completed?)
(define (completed? assignment class step reviewer id)
  (let* ((query (merge "SELECT COUNT(*)"
                       "FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=? AND"
                               step-id "=? AND"
                               reviewer-id "=? AND"
                               review-id "=?"))
         (prep (prepare sql-conn query))
         (result (vector-ref (query-row sql-conn prep assignment class step reviewer id) 0)))
    (> result 0)))

(provide count-completed)
(define (count-completed assignment class step reviewer id)
  (let* ((query (merge "SELECT COUNT(*)"
                       "FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=? AND"
                               step-id "=? AND"
                               reviewer-id "=? AND"
                               review-id "=?"))
         (prep (prepare sql-conn query))
         (result (vector-ref (query-row sql-conn prep assignment class step reviewer id) 0)))
    result 0))

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

#|
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
|#

(define (random-hash)
  (for/fold ([s ""])
      ([x (in-range 32)])
    (string-append s
                   (number->string (truncate (random 15)) 16))))