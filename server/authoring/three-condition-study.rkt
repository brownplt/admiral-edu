#lang racket

(require db
         (planet esilkensen/yaml:3:1)
         json
         "assignment.rkt"
         "assignment-structs.rkt"
         "util.rkt"
         "../database/mysql.rkt"
         "../config.rkt"
         (prefix-in machine: "progress-machine.rkt")
         "../database/mysql/common.rkt")

(provide get-pending-reviews)

;; 3 different groups

;; assignment-id -> uid -> group
;; TODO: Added to file storage API
(define (lookup-group assignment-id uid)
  (let* ((yaml-string (file->string (string-append assignment-id ".yaml")))
         (yaml (string->yaml yaml-string))
         (does-reviews (hash-ref yaml "does-reviews"))
         (gets-reviewed (hash-ref yaml "gets-reviewed"))
         (no-reviews (hash-ref yaml "no-reviews")))
    (cond [(member uid does-reviews) 'does-reviews]
          [(member uid gets-reviewed) 'gets-reviewed]
          [(member uid no-reviews) 'no-reviews]
          [else 'no-such-student])))

(define (three-next-action assignment-id steps uid)
  (let ((group (lookup-group assignment-id uid)))
    (cond 
      [(null? steps) #t]
      [else 
       (let ((check-result (three-check-step assignment-id (car steps) uid group))
             (rest (cdr steps)))
         (cond
           [(eq? #t check-result) (next-action assignment-id rest uid)]
           [else check-result]))])))

(define (three-check-step assignment-id step uid group)
  (let* ((step-id (Step-id step))
         (has-submitted (> (submission:count assignment-id class-name step-id uid) 0)))
    (cond 
      [(not has-submitted) (MustSubmitNext step (Step-instructions step))]
      [else (cond [(eq? group 'no-reviews) #t]
                  ;;TODO: Select from get-review students only
                  [(eq? group 'does-reviews) (check-reviews assignment-id step (Step-reviews step) uid)]
                  ;;TODO: Notify users that they are in get-reviews
                  [(eq? group 'gets-reviewed) #t]
                  [else (error (format "Unknown group type ~a" group))])])))

(provide three-do-submit-step)
(define (three-do-submit-step assignment-id step-id uid data steps)
  ;(upload-submission class user assignment step data)
  (upload-submission class-name uid assignment-id step-id data)
  (let ((group (lookup-group assignment-id uid)))
    ;; TODO: look to see if there are any pending reviewers
    (if (eq? group 'gets-reviewed) (maybe-assign-reviewers assignment-id step-id uid) (printf "Skipping maybe-assign review. uid: ~a, group: ~a\n" uid group)))
  ;; Assign reviews to the student if applicable
  (let ((next (three-next-action assignment-id steps uid)))
    (printf "Next Action for ~a is ~a\n" uid next)
    (cond
      [(MustReviewNext? next) (three-assign-reviews assignment-id (MustReviewNext-step next) uid)])
    (Success "Assignment submitted.")))

(define (maybe-assign-reviewers assignment-id step-id uid)
  (let* ((step (step-id->step assignment-id step-id))
         (reviews (Step-reviews step)))
    (map (maybe-assign-reviewers-helper assignment-id step-id uid) reviews)))

(define (maybe-assign-reviewers-helper assignment-id step-id uid)
  (lambda (review)
    (cond [(student-submission? review) (let* ((amount (student-submission-amount review))
                                               (pending-reviews (get-pending-reviews assignment-id step-id amount)))
                                          (map (assign-reviewer assignment-id step-id uid) (map (lambda (v) (vector-ref v 0)) pending-reviews)))]
          [else (error "Ooops should not be here.")])))

(define (assign-reviewer assignment-id step-id uid)
  (lambda (hash)
    (let* ((q (merge "UPDATE" review:table
                     "SET" review:reviewee-id "=?"
                     "WHERE" review:hash "=?"))
           (prep (prepare (sql-conn) q))
           (result (query-exec (sql-conn) prep uid hash)))
      result)))                                                        

(define (get-pending-reviews assignment-id step-id amount)
  (let* ((query (merge "SELECT " review:hash ", count(*) as C"
                       "FROM" review:table
                       "WHERE" review:assignment-id "=? AND"
                               review:class-id "=? AND"
                               review:step-id "=? AND"
                               review:reviewee-id "='HOLD'"
                       "GROUP BY" review:reviewer-id
                       "ORDER BY" "C DESC,"
                       review:time-stamp "ASC"
                       "LIMIT ?"))
         (prep (prepare (sql-conn) query))
         (result (query-rows (sql-conn) prep assignment-id class-name step-id amount)))
    (printf "results of query: ~a\n" result)
    result))

(define (three-assign-reviews assignment-id step uid)
  (let* ((reviews (Step-reviews step))
         (step-id (Step-id step)))
    (map (three-assign-review assignment-id step-id uid) reviews)))
    
;; Student is in the does-reviews group
(define (three-assign-review assignment-id step-id uid)
  (lambda (review)
    (let ((review-id (getId review))
          (amount (student-submission-amount review)))
      (cond [(instructor-solution? review) (review:assign-instructor-solution assignment-id class-name step-id "instructor" uid review-id)]
            [(student-submission? review) (assign-student-reviews assignment-id class-name step-id uid review-id amount)]))))

(define (gets-reviewed-list assignment-id step-id)
  ;;TODO Add to file storage API
  (let* ((yaml-string (file->string (string-append assignment-id ".yaml")))
         (yaml (string->yaml yaml-string))
         (gets-reviewed (hash-ref yaml "gets-reviewed")))
    gets-reviewed))

(define (assign-student-reviews assignment-id class-name step-id uid review-id amount)
  (let* ((students (gets-reviewed-list assignment-id step-id))
         (student-commas (string-join (build-list (length students) (lambda (n) "?")) ","))
         (q (merge "SELECT" submission:user-id
                   "FROM" submission:table
                   "WHERE" submission:user-id "IN (" student-commas ") AND"
                           submission:assignment-id "=? AND"
                           submission:class-id "=? AND"
                           submission:step-id "=? AND"
                           submission:times-reviewed "<=?"
                   "ORDER BY" submission:times-reviewed "ASC," submission:time-stamp "ASC"
                   "LIMIT ?"))
         (prep (prepare (sql-conn) q))
         (query-list (append (list (sql-conn) prep ) students (list assignment-id class-name step-id amount amount)))
         (result (apply query-rows query-list))
         (total-found (length result))
         (hold-amount (- amount total-found)))
    (printf "Found ~a students to review, will hold ~a for later\n" total-found hold-amount)
    (map (assign-student-review assignment-id class-name step-id uid review-id) result)
    (assign-student-hold assignment-id class-name step-id uid review-id hold-amount)))

(define (assign-student-review assignment-id class-name step-id uid review-id)
  (lambda (vec)
    (let ((reviewee (vector-ref vec 0)))
      (printf "Assigning review between ~a and ~a\n" reviewee uid)
      (review:create assignment-id class-name step-id reviewee uid review-id))))

(define (assign-student-hold assignment-id class-name step-id uid review-id n)
  (printf "Assigning hold to ~a\n" uid)
  (if (<= n 0) #t
      (begin (review:create assignment-id class-name step-id "HOLD" uid review-id)
             (assign-student-hold assignment-id class-name step-id uid review-id (- n 1)))))