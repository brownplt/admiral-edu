#lang racket

(require (planet esilkensen/yaml:3:1)
         json
         "assignment-structs.rkt"
         "util.rkt"
         "../base.rkt"
         (prefix-in machine: "progress-machine.rkt")
         "assignment.rkt")

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

;; assignment-id -> (steps uid) -> NextAction
(define (get-next-action assignment-id)
  (let* ((yaml-string (file->string "next-action.yaml"))
         (yaml (string->yaml yaml-string))
         (action-string (hash-ref yaml assignment-id)))
    (cond [(equal? "default-next-action" action-string) default-next-action]
          [(equal? "next-action-three-condition-study" action-string) next-action-three-condition-study])))


;; Given an assignment-id and the list of steps to complete, returns the next-action the user must take
;; or #t if the user has completed the assignment
(define (default-next-action assignment-id steps uid)
    (cond 
      [(null? steps) #t]
      [else 
       (let ((check-result (check-step assignment-id (car steps) uid))
             (rest (cdr steps)))
         (cond
           [(eq? #t check-result) (next-action assignment-id rest uid)]
           [else check-result]))]))

(define (next-action-three-condition-study assignment-id steps uid)
  (let ((group (lookup-group assignment-id uid)))
    (cond 
      [(null? steps) #t]
      [else 
       (let ((check-result (check-step-three-condition-study assignment-id (car steps) uid group))
             (rest (cdr steps)))
         (cond
           [(eq? #t check-result) (next-action assignment-id rest uid)]
           [else check-result]))])))

(define (check-step-three-condition-study assignment-id step uid group)
  (let* ((step-id (Step-id step))
         (has-submitted (> (submission:count assignment-id class-name step-id uid) 0)))
    (cond 
      [(not has-submitted) (MustSubmitNext step (Step-instructions step))]
      [else (cond [(eq? group 'no-reviews) #t]
                  ;;TODO: Select from get-review students only
                  [(eq? group 'does-reviews) (check-reviews assignment-id step (Step-reviews step) uid)]
                  ;;TODO: Notify users that they are in get-reviews
                  [(eq? group 'gets-reviews) #t])])))



