#lang typed/racket

(require "typed-db.rkt"
         "../../ct-session.rkt"
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

(provide time-stamp time-stamp-type time-stamp-type-1)
(define time-stamp "time_stamp")
(define time-stamp-type "TIMESTAMP DEFAULT 0")
(define time-stamp-type-0 "TIMESTAMP")
(define time-stamp-type-1 "TIMESTAMP DEFAULT 0")

(provide feedback-viewed-time-stamp
         feedback-viewed-time-stamp-type
         feedback-viewed-time-stamp-type-2)
(define feedback-viewed-time-stamp "feedback_viewed_time_stamp")
(define feedback-viewed-time-stamp-type "TIMESTAMP NULL")
(define feedback-viewed-time-stamp-type-2 "TIMESTAMP NULL")

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

(define valid-columns `(,assignment-id ,class-id ,step-id ,reviewee-id ,reviewer-id ,time-stamp ,completed ,instructor-solution ,flagged ,feedback-viewed-time-stamp))

; ct-session -> (U 'class_id 'step_id 'user_id 'time_stamp 'times_reviewed)
(provide get-sort-by)
(: get-sort-by (ct-session -> Symbol))
(define get-sort-by (common:get-sort-by valid-columns (string->symbol reviewer-id)))

(provide sort-by?)
(: sort-by? (Symbol -> Boolean))
(define sort-by? (common:sort-by? valid-columns))


;; Initializes the review table.
(provide init)
(: init (-> Void))
(define (init)
  (let* ((drop (merge "DROP TABLE IF EXISTS" table))
         (create (merge "CREATE TABLE" table "(" assignment-id assignment-id-type "," ; 0
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
                                                 feedback-viewed-time-stamp feedback-viewed-time-stamp-type ","; 11
                                                 "PRIMARY KEY (" hash "))")))
    (query-exec drop)
    (query-exec create)))




(: ok-reviewee (String String String String -> Boolean))
(define (ok-reviewee assignment class step reviewee)
  (or (string=? reviewee "HOLD") (submission:exists? assignment class step reviewee)))


(provide create)
(: create (String String String String String String -> Void))
(define (create assignment class step reviewee reviewer id)
  ;; TODO(joe): should this be an error?
  (when (not (ok-reviewee assignment class step reviewee)) 'no-such-submission)
                                                 ;0 1 2 3 4     5     6 7 8     9   10
  (let ((query (merge "INSERT INTO" table "VALUES(?,?,?,?,?,NOW(),false,?,?,false,false,NULL)")))
                ; 0           1     2    3           4        7         8
    (query-exec query assignment class step reviewee reviewer (random-hash) id)
    ;; TODO: This is not concurrently safe.
    (when (not (string=? reviewee "HOLD"))
      (submission:increment-reviewed assignment class step reviewee))
    
    (void)))

(: random-hash (-> String))
(define (random-hash)
  (for/fold ([s ""])
      ([x (in-range 32)])
    (string-append s
                   (number->string (truncate (random 15)) 16))))

(provide assign-student-reviews)
(: assign-student-reviews (String String String String String Exact-Nonnegative-Integer -> Void))
(define (assign-student-reviews assignment class step uid review-id amount)
  (cond [(<= amount 0) (void)]
        [else (assign-student-review assignment class step uid review-id)
              (assign-student-reviews assignment class step uid review-id (- amount 1))]))

(provide assign-student-review)
(: assign-student-review (String String String String String -> Void))
(define (assign-student-review assignment class step uid review-id)
  ;; TODO(joe): Probably a performance hit to run this query in this way. Would be faster to just get all of them at once.
  (let* ((not-users (map Record-reviewee-id (map select-by-hash (select-assigned-reviews assignment class step uid)))) 
         (reviewee (submission:select-least-reviewed assignment class step (cons uid not-users))))
    (cond [(eq? reviewee 'no-reviews) #f]
          [else (create assignment class step reviewee uid review-id)])))


(provide (struct-out Record))
(struct: Record ([class-id : String] 
                 [assignment-id : String]
                 [step-id : String]
                 [review-id : String]
                 [reviewee-id : String] 
                 [reviewer-id : String] 
                 [completed : Boolean]
                 [hash : String] 
                 [flagged : Boolean] 
                 [time-stamp : TimeStamp]
                 [feedback-viewed-time-stamp : (U Null TimeStamp)]) #:transparent)

(define record-fields
  (string-join (list class-id assignment-id step-id review-id reviewee-id reviewer-id completed hash flagged time-stamp feedback-viewed-time-stamp) ", "))
(define-type Vector-Record (Vector String String String String String String Integer String Integer TimeStamp (U TimeStamp Null)))

(: vector->record (Vector-Record -> Record))
(define (vector->record result)
  (let* ((class-id (vector-ref result 0))
         (assignment-id (vector-ref result 1))
         (step-id (vector-ref result 2))
         (review-id (vector-ref result 3))
         (reviewee-id (vector-ref result 4))
         (reviewer-id (vector-ref result 5))
         (completed (= 1 (vector-ref result 6)))
         (hash (vector-ref result 7))
         (flagged (= 1(vector-ref result 8)))
         (time-stamp (vector-ref result 9))
         (feedback-viewed-time-stamp (vector-ref result 10))
         (rec (Record class-id assignment-id step-id review-id reviewee-id reviewer-id completed hash flagged time-stamp feedback-viewed-time-stamp)))
    rec))

(provide select-by-hash)
(: select-by-hash (String -> Record))
(define (select-by-hash the-hash)
  (let* ((query (merge "SELECT" record-fields
                       "FROM" table
                       "WHERE" hash "=? LIMIT 1"))
         (result (query-row query the-hash)))
    (vector->record (cast result Vector-Record))))

(provide select-assigned-reviews)
(: select-assigned-reviews (String String String String -> (Listof String)))
(define (select-assigned-reviews assignment class step uid)
  (let* ((query (merge "SELECT" hash
                       "FROM" table
                       "WHERE" class-id "=? AND"
                               assignment-id "=? AND"
                               step-id "=? AND"
                               reviewer-id "=?"))
         (results (query-rows query class assignment step uid)) ; (Listof (Vectorof QueryResult))
         (lists (map (cast vector->list (-> (Vectorof QueryResult) (Listof QueryResult))) results))
         (hashes (apply append lists)))
    (cast hashes (Listof String))))

(provide assign-instructor-solution)
(: assign-instructor-solution (String String String String String String -> Void))
(define (assign-instructor-solution assignment class step reviewee reviewer review-id)
  (create-instructor-review assignment class step reviewee reviewer review-id))

(: create-instructor-review (String String String String String String -> Void))
(define (create-instructor-review assignment class step reviewee reviewer id)
  (let ((query (merge "INSERT INTO" table "VALUES(?,?,?,?,?,NOW(),false,?,?,true,false,NULL)")))
    (query-exec query assignment class step reviewee reviewer (random-hash) id)))

(provide count-assigned-reviews)
(: count-assigned-reviews (String String String String String -> Exact-Nonnegative-Integer))
(define (count-assigned-reviews class assignment uid step review)
  (let* ((q (merge "SELECT COUNT(*)"
                   "FROM" table
                   "WHERE" class-id "=? AND"
                           assignment-id "=? AND"
                           reviewer-id "=? AND"
                           step-id "=? AND"
                           review-id "=?"))
         (result (query-value q class assignment uid step review)))
    (cast result Exact-Nonnegative-Integer)))

(provide select-feedback)
(: select-feedback (String String String -> (Listof Record)))
(define (select-feedback class assignment uid)
  (let* ((query (merge "SELECT" record-fields
                       "FROM" table
                       "WHERE" class-id "=? AND"
                               assignment-id "=? AND"
                               reviewee-id "=? AND"
                               completed "=true"
                       "ORDER BY" time-stamp "ASC"))
         (result (query-rows query class assignment uid)))
    (map vector->record (cast result (Listof Vector-Record)))))

(provide mark-feedback-viewed)
(: mark-feedback-viewed (String -> Void))
(define (mark-feedback-viewed the-hash)
  (let ((query (merge "UPDATE" table
                      "SET" feedback-viewed-time-stamp "=NOW()"
                      "WHERE" hash "=? AND"
                              feedback-viewed-time-stamp "IS NULL")))
    (query-exec query the-hash)))
  
              
(provide mark-complete)
(: mark-complete (String -> Void))
(define (mark-complete the-hash)
  (let  ((query (merge "UPDATE" table
                       "SET" completed "=1"
                       "WHERE" hash "=?")))
    (query-exec query the-hash)))

(provide mark-incomplete)
(: mark-incomplete (String -> Void))
(define (mark-incomplete the-hash)
  (let  ((q (merge "UPDATE" table
                   "SET" completed "=0"
                   "WHERE" hash "=?")))
    (query-exec q the-hash)))
     
(provide set-flagged)
(: set-flagged (String Boolean -> Void))
(define (set-flagged the-hash flag)
  (let* ((set-to (if flag 1 0))
         (q (merge "UPDATE" table
                   "SET" flagged "=?"
                   "WHERE" hash "=?")))
    (query-exec q set-to the-hash)))

(provide select-reviews)
(: select-reviews (String -> (Listof String)))
(define (select-reviews reviewee)
  (let* ((query (merge "SELECT" hash "FROM" table "WHERE" reviewee-id "=?"))
         (result (query-rows query reviewee))
         (flat (apply append (map (cast vector->list (-> (Vectorof QueryResult) (Listof QueryResult))) result))))
    (cast flat (Listof String))))

(provide completed?)
(: completed? (String String String String String -> Boolean))
(define (completed? assignment class step reviewer id)
  (let* ((query (merge "SELECT COUNT(" completed ")"
                       "FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=? AND"
                               step-id "=? AND"
                               reviewer-id "=? AND"
                               review-id "=? AND"
                               completed "=true"))
         (result (query-value query assignment class step reviewer id)))
    (> (cast result Exact-Nonnegative-Integer) 0)))
    
(provide count-completed)
(: count-completed (String String String String String -> Exact-Nonnegative-Integer))
(define (count-completed assignment class step reviewer id)
  (let* ((query (merge "SELECT COUNT(*)"
                       "FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=? AND"
                               step-id "=? AND"
                               reviewer-id "=? AND"
                               completed "=1 AND"
                               review-id "=?"))
         (result (query-value query assignment class step reviewer id)))
    (cast result Exact-Nonnegative-Integer)))

(provide count)
(: count (String String String String -> Exact-Nonnegative-Integer))
(define (count assignment class step reviewee)
  (let* ((query (merge "SELECT COUNT(*)"
                       "FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=? AND"
                               step-id "=? AND"
                               reviewee-id "=?"))
         (result (query-value query assignment class step reviewee)))
    (cast result Exact-Nonnegative-Integer)))

(provide delete-assignment)
(: delete-assignment (String String -> Void))
(define (delete-assignment class assignment)
  (let ((query (merge "DELETE FROM" table
                       "WHERE" assignment-id "=? AND"
                               class-id "=?")))
    (query-exec query assignment class)))

(provide count-completed-reviews)
(: count-completed-reviews (String String String String -> Exact-Nonnegative-Integer))
(define (count-completed-reviews assignment class step review)
  (let* ((q (merge "SELECT COUNT(*)"
                   "FROM" table
                   "WHERE" assignment-id "=? AND"
                           class-id "=? AND"
                           step-id "=? AND"
                           review-id "=? AND"
                           completed "=?"
                   "LIMIT 1"))
         (result (query-value q assignment class step review 1)))
    (cast result Exact-Nonnegative-Integer)))

(provide count-all-assigned-reviews)
(: count-all-assigned-reviews (String String String String -> Exact-Nonnegative-Integer))
(define (count-all-assigned-reviews assignment class step review)
  (let* ((q (merge "SELECT COUNT(*)"
                   "FROM" table
                   "WHERE" assignment-id "=? AND"
                           class-id "=? AND"
                           step-id "=? AND"
                           review-id "=?"
                   "LIMIT 1"))
         (result (query-value q assignment class step review)))
    (cast result Exact-Nonnegative-Integer)))

(provide select-all)
(: select-all (String String String String Symbol (U 'asc 'desc) -> (Listof Record)))
(define (select-all assignment class step review sort-by order)
  (let* ((sort-field reviewer-id)
         (direction (order->string order))
         (sort-field (if (sort-by? sort-by) (symbol->string sort-by) reviewer-id))
         (q (merge "SELECT" record-fields
                   "FROM" table
                   "WHERE" assignment-id "=? AND"
                           class-id "=? AND"
                           step-id "=? AND"
                           review-id "=?"
                   "ORDER BY" sort-field direction))
         (results (query-rows q assignment class step review)))
    (map vector->record (cast results (Listof Vector-Record)))))
