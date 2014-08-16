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
                  [(eq? group 'gets-reviews) #t])])))

(define (three-do-submit-step assignment-id step-id uid data steps)
  ;(upload-submission class user assignment step data)
  (upload-submission class-name uid assignment-id step-id data)
  (let ((group (lookup-group assignment-id uid)))
    ;; TODO: look to see if there are any pending reviewers
    (if (eq? group 'gets-reviewed) (check-pending-review assignment-id step-id) #f))
  ;; Assign reviews to the student if applicable
  (let ((next (next-action assignment-id steps uid)))
    (cond
      [(MustReviewNext? next) (assign-reviews assignment-id next uid)])
    (Success "Assignment submitted.")))

(define (check-pending-review assignment-id step-id)
  (let* ((query (merge "SELECT" review:hash
                       "FROM" review:table
                       "WHERE" review:assignment-id "=? AND"
                               review:class-id "=? AND"
                               review:step-id "=? AND"
                               review:reviewee-id "='HOLD'"
                       "ORDER BY" review:time-stamp "ASC"
                       "LIMIT 1"))
         (prep (prepare sql-conn query))
         (result (query-rows prepare sql-conn assignment-id class-name step-id)))
    (cond [(null? result) #f]
          [else (vector-ref (car result) 0)])))
    

#|
(provide table)
(provide assignment-id assignment-id-type)
(provide step-id step-id-type)
(provide class-id class-id-type)
(provide reviewee-id reviewee-id-type)
(provide reviewer-id reviewer-id-type)
(provide time-stamp time-stamp-type)
(provide review-id review-id-type)
(provide instructor-solution instructor-solution-type)
(provide completed completed-type)
|#