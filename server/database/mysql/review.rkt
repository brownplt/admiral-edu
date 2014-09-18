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

(provide flagged flagged-type)
(define flagged "flagged")
(define flagged-type "BOOL")

;; Initializes the review table.
(provide init)
(define (init)
  (let* ((conn (make-sql-conn))
         (drop (prepare conn (merge "DROP TABLE IF EXISTS" table)))
         (create (prepare conn (merge "CREATE TABLE" table "("
                                         assignment-id assignment-id-type "," ; 0
                                         class-id class-id-type "," ;1
                                         step-id step-id-type "," ;2
                                         reviewee-id reviewee-id-type "," ;3
                                         reviewer-id reviewer-id-type "," ;4
                                         time-stamp time-stamp-type "," ;5
                                         completed completed-type "," ;6
                                         hash hash-type "," ;7
                                         review-id review-id-type "," ;8
                                         instructor-solution instructor-solution-type "," ;9
                                         flagged flagged-type "," ; 10
                                         "PRIMARY KEY (" hash "))"))))
    (query-exec conn drop)
    (query-exec conn create)
    (release conn)))

(provide create)
(define (ok-reviewee assignment class step reviewee)
  (or (string=? reviewee "HOLD") (submission:exists? assignment class step reviewee)))

(define (create assignment class step reviewee reviewer id)
  ;; TODO(joe): should this be an error?
  (when (not (ok-reviewee assignment class step reviewee)) 'no-such-submission)
                                                  ;0 1 2 3 4     5     6 7 8     9   10
  (let* ((conn (make-sql-conn))
         (query (merge "INSERT INTO" table "VALUES(?,?,?,?,?,NOW(),false,?,?,false, false)"))
         (prep (prepare conn query)))
                              ; 0           1     2    3           4        7         8
    (query-exec conn prep assignment class step reviewee reviewer (random-hash) id)
    (release conn)
    ;; TODO: This is not concurrently safe.
    (when (not (string=? reviewee "HOLD"))
      (submission:increment-reviewed assignment class step reviewee))
    
    #t))



(provide assign-student-reviews)
(define (assign-student-reviews assignment class step uid review-id amount)
  (cond [(<= amount 0) #t]
        [else (assign-student-review assignment class step uid review-id)
              (assign-student-reviews assignment class step uid review-id (- amount 1))]))


(define (assign-student-review assignment class step uid review-id)
  ;; TODO(joe): Probably a performance hit to run this query in this way. Would be faster to just get all of them at once.
  (let* (
         (not-users (map record-reviewee-id (map select-by-hash (select-assigned-reviews assignment class step uid)))) 
         (reviewee (submission:select-least-reviewed assignment class step (cons uid not-users))))
    (cond [(eq? reviewee 'no-reviews) #f]
          [else (create assignment class step reviewee uid review-id)])))

(provide assign-instructor-solution)
(define (assign-instructor-solution assignment class step reviewee reviewer review-id)
  (create-instructor-review assignment class step reviewee reviewer review-id))

(define (create-instructor-review assignment class step reviewee reviewer id)
  (let* ((conn (make-sql-conn))
         (query (merge "INSERT INTO" table "VALUES(?,?,?,?,?,NOW(),false,?,?,true,false)"))
         (prep (prepare conn query)))
    (query-exec conn prep assignment class step reviewee reviewer (random-hash) id)
    (release conn)))

(provide (struct-out record))
(struct record (class-id assignment-id step-id review-id reviewee-id reviewer-id completed hash flagged) #:transparent)

(define record-fields
  (string-join (list class-id assignment-id step-id review-id reviewee-id reviewer-id completed hash flagged) ", "))

(define (vector->record result)
  (let* (
         (class-id (vector-ref result 0))
         (assignment-id (vector-ref result 1))
         (step-id (vector-ref result 2))
         (review-id (vector-ref result 3))
         (reviewee-id (vector-ref result 4))
         (reviewer-id (vector-ref result 5))
         (completed (= 1 (vector-ref result 6)))
         (hash (vector-ref result 7))
         (flagged (= 1(vector-ref result 8)))
         (rec (record class-id assignment-id step-id review-id reviewee-id reviewer-id completed hash flagged)))
    rec))

(provide count-assigned-reviews)
(define (count-assigned-reviews class assignment uid step review)
  (let* ((q (merge "SELECT COUNT(*)"
                   "FROM" table
                   "WHERE" class-id "=? AND"
                           assignment-id "=? AND"
                           reviewer-id "=? AND"
                           step-id "=? AND"
                           review-id "=?"))
         (result (run query-row q class assignment uid step review)))
    (vector-ref result 0)))

(provide select-feedback)
(define (select-feedback class assignment uid)
  (let* ((conn (make-sql-conn))
         (query (merge "SELECT" record-fields
                       "FROM" table
                       "WHERE" class-id "=? AND"
                               assignment-id "=? AND"
                               reviewee-id "=? AND"
                               completed "=true"
                       "ORDER BY" time-stamp "ASC"))
         (prep (prepare conn query))
         (result (query-rows conn prep class assignment uid)))
    (release conn)
    (map vector->record result)))

                       
(provide select-by-hash)
(define (select-by-hash the-hash)
  (let* ((conn (make-sql-conn))
         (query (merge "SELECT" record-fields
                       "FROM" table
                       "WHERE" hash "=? LIMIT 1"))
         (prep (prepare conn query))
         (result (query-row conn prep the-hash)))
    (release conn)
    (vector->record result)))
                   
(provide mark-complete)
(define (mark-complete the-hash)
  (let* ((conn (make-sql-conn))
         (query (merge "UPDATE" table
                       "SET" completed "=1"
                       "WHERE" hash "=?"))
         (prep (prepare conn query)))
    (query-exec conn prep the-hash)
    (release conn)))

(provide mark-incomplete)
(define (mark-incomplete the-hash)
  (let* ((q (merge "UPDATE" table
                   "SET" completed "=0"
                   "WHERE" hash "=?")))
    (run query-exec q the-hash)))

(provide set-flagged)
(define (set-flagged the-hash flag)
  (let* ((set-to (if flag 1 0))
         (q (merge "UPDATE" table
                   "SET" flagged "=?"
                   "WHERE" hash "=?")))
    (run query-exec q set-to the-hash)))

(provide select-reviews)
(define (select-reviews reviewee)
  (let* ((conn (make-sql-conn))
         (query (merge "SELECT" hash "FROM" table "WHERE" reviewee-id "=?"))
         (prep (prepare conn query))
         (result (query-rows conn prep reviewee)))
    (release conn)
    (flatten (map vector->list result))))

(provide select-assigned-reviews)
(define (select-assigned-reviews assignment class step uid)
  (let* ((conn (make-sql-conn))
         (query (merge "SELECT" hash
                       "FROM" table
                       "WHERE" class-id "=? AND"
                               assignment-id "=? AND"
                               step-id "=? AND"
                               reviewer-id "=?"))
         (prep (prepare conn query))
         (result (query-rows conn prep class assignment step uid)))
    (release conn)
    (flatten (map vector->list result))))
    

(provide completed?)
(define (completed? assignment class step reviewer id)
  (let* ((conn (make-sql-conn))
         (query (merge "SELECT COUNT(" completed ")"
                       "FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=? AND"
                               step-id "=? AND"
                               reviewer-id "=? AND"
                               review-id "=? AND"
                               completed "=true"))
         (prep (prepare conn query))
         (result (vector-ref (query-row conn prep assignment class step reviewer id) 0)))
    (release conn)
    (> result 0)))

(provide count-completed)
(define (count-completed assignment class step reviewer id)
  (let* ((conn (make-sql-conn))
         (query (merge "SELECT COUNT(*)"
                       "FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=? AND"
                               step-id "=? AND"
                               reviewer-id "=? AND"
                               completed "=1 AND"
                               review-id "=?"))
         (prep (prepare conn query))
         (result (vector-ref (query-row conn prep assignment class step reviewer id) 0)))
    (release conn)
    result))

(provide count)
(define (count assignment class step reviewee)
  (let* ((conn (make-sql-conn))
         (query (merge "SELECT COUNT(*)"
                       "FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=? AND"
                               step-id "=? AND"
                               reviewee-id "=?"))
         (prep (prepare conn query))
         (result (vector-ref (query-row conn prep assignment class step reviewee) 0)))
    (release conn)
    result))

(provide delete-assignment)
(define (delete-assignment class assignment)
  (let ((query (merge "DELETE FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=?")))
    (run query-exec query assignment class)))

(provide count-completed-reviews)
(define (count-completed-reviews assignment class step review)
  (let* ((q (merge "SELECT COUNT(*)"
                   "FROM" table
                   "WHERE" assignment-id "=? AND"
                           class-id "=? AND"
                           step-id "=? AND"
                           review-id "=? AND"
                           completed "=?"
                   "LIMIT 1"))
         (result (run query-row q assignment class step review 1)))
    (vector-ref result 0)))

(provide count-all-assigned-reviews)
(define (count-all-assigned-reviews assignment class step review)
  (let* ((q (merge "SELECT COUNT(*)"
                   "FROM" table
                   "WHERE" assignment-id "=? AND"
                           class-id "=? AND"
                           step-id "=? AND"
                           review-id "=?"
                   "LIMIT 1"))
         (result (run query-row q assignment class step review)))
    (vector-ref result 0)))

(define (random-hash)
  (for/fold ([s ""])
      ([x (in-range 32)])
    (string-append s
                   (number->string (truncate (random 15)) 16))))
