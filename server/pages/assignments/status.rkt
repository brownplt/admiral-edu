#lang racket

(require "../../database/mysql/typed-db.rkt"
         "../../base.rkt"
         "../../authoring/assignment.rkt"
         (prefix-in browse: "../browse.rkt")
         (prefix-in error: "../errors.rkt")
         (prefix-in action: "action.rkt"))

(provide load)
(define (load session url message [post #f])
  (match url
    [(list assignment-id) (display-assignment assignment-id message)]
    [(list assignment-id step-id) (display-step session assignment-id step-id message)]
    [(list assignment-id step-id review-id) (display-review session assignment-id step-id review-id message post)]
    [_ error:four-oh-four-xexpr]))
  
(define (display-assignment assignment-id message)
  (let* ((assignment (assignment-id->assignment assignment-id))
         (steps (cons 'ol (apply append (map (step->statistic assignment-id) (Assignment-steps assignment))))))
    (append (header assignment-id message)
            (list steps))))

(define (header assignment-id message)
  (let* ((assignment (assignment-id->assignment assignment-id))
         (record (assignment:select class-name assignment-id))
         (open (assignment:Record-open record))
         (ready (assignment:Record-ready record))
         (status (if ready (if open "Open" "Closed") "Missing Dependencies")))
  `((h1 ,(action:assignments "Assignments"))
    (h2 ,(action:dashboard assignment-id assignment-id))
    (h3 ,(action:status assignment-id "Status") " : " ,status)
    ,(when message message))))

(define (display-step session assignment-id step-id message)
  (let* ((order (get-order session))
         (sort-by (submission:get-sort-by session))
         (next-order (opposite-order order))
         (count (number->string (submission:count-step assignment-id class-name step-id)))
         (submission-records (submission:select-all assignment-id class-name step-id sort-by order)))
  (append (header assignment-id message)
          `((h4 ,step-id)
            (p "Submissions : " ,count)
            (p "Select a User ID to view their submission."))
          `(,(append `(table
                       (tr (th ,(sort-by-action "user_id" next-order "Student ID"))
                           (th ,(sort-by-action "time_stamp" next-order "Submission Date"))))
                     (map submission-record->xexpr submission-records))))))

; (String Order String -> Xexpr)
(define (sort-by-action field order context)
    `(a ((href ,(string-append "?sort-by=" field "&order=" (symbol->string order)))) ,context))

(define (submission-record->xexpr record)
  (let ((user-id (submission:Record-user record))
        (time-stamp (format-time-stamp (submission:Record-time-stamp record))))
    `(tr (td ,(view-submission-action record user-id))
         (td ,time-stamp))))

(define (view-submission-action record user-id)
  (let ((user (submission:Record-user record))
        (assignment (submission:Record-assignment record))
        (step (submission:Record-step record)))
  `(a ((href ,(string-append "/" class-name "/browse/" user "/" assignment "/" step "/"))) ,user-id)))

(define (format-time-stamp time-stamp)
  (let ((year (number->string (TimeStamp-year time-stamp)))
        (month (month->string (TimeStamp-month time-stamp)))
        (day (number->string (TimeStamp-day time-stamp)))
        (hour (number->string (TimeStamp-hour time-stamp)))
        (minute (ensure-leading-zero (number->string (TimeStamp-minute time-stamp))))
        (second (ensure-leading-zero (number->string (TimeStamp-second time-stamp)))))
    (string-append month " " day " " year " " hour ":" minute ":" second)))

(define (ensure-leading-zero str)
  (let ((len (string-length str)))
  (cond [(= len 1) (string-append "0" str)]
        [else str])))

(define (month->string month)
  (match month
    [1 "January"]
    [2 "February"]
    [3 "March"]
    [4 "April"]
    [5 "May"]
    [6 "June"]
    [7 "July"]
    [8 "August"]
    [9 "September"]
    [10 "October"]
    [11 "November"]
    [12 "December"]
    [_ (error (format "Could not convert month ~a to string." month))]))
    

(define (display-review session assignment-id step-id review-id message post)
  (when post (set! message (check-for-action session)))
  (let* ((assigned (number->string (review:count-completed-reviews assignment-id class-name step-id review-id)))
         (completed (number->string (review:count-all-assigned-reviews assignment-id class-name step-id review-id)))
         (sort-by (review:get-sort-by session))
         (order (get-order session))
         (next-order (opposite-order order))
         (review-records (review:select-all assignment-id class-name step-id review-id sort-by order))
         (reviews (apply append (map review-record->xexpr review-records))))
    (append (header assignment-id message)
            `((h4 ,step-id " > " ,review-id)
              (p "Assigned : " ,assigned)
              (p "Completed : " ,completed)
              (p "Selecting a students ID will bring you to the students view of a review."))
            `(,(append `(table
                         (th ,(sort-by-action review:reviewer-id next-order "Reviewer ID"))
                         (th ,(sort-by-action review:reviewee-id next-order "Reviewee ID"))
                         (th ,(sort-by-action review:completed next-order "Completed"))
                         (th ,(sort-by-action review:flagged next-order "Flagged"))
                         (th "Actions"))
                       reviews)))))


(define test-session
  (ct-session "test-class" "jcollard@umass.edu" (hash 'sort-by "reviewee_id")))

(define (review-record->xexpr record)
  (let ((reviewee (review:record-reviewee-id record))
        (reviewer (review:record-reviewer-id record))
        (time-stamp (review:record-time-stamp record))
        (completed (if (review:record-completed record) "Yes" ""))
        (actions (if (review:record-completed record) 
                     (mark-incomplete-action record "Mark Incomplete") 
                     (mark-complete-action record "Mark Complete")))
        (flagged (review:record-flagged record)))
  `((tr
     (td ,(view-review-action record reviewer))
     (td ,(view-feedback-action record reviewee))
     (td ,completed)
     (td ,(if flagged '(b "FLAGGED") ""))
     (td ,actions)))))

(define (mark-incomplete-action record context)
  (let ((hash (review:record-hash record)))
    `(form ((method "post"))
           (input ((type "hidden") (name "action") (value "mark-incomplete")))
           (input ((type "hidden") (name "review-hash") (value ,hash)))
           (input ((type "submit") (value "Mark Incomplete"))))))

(define (mark-complete-action record context)
  (let ((hash (review:record-hash record)))
    `(form ((method "post"))
           (input ((type "hidden") (name "action") (value "mark-complete")))
           (input ((type "hidden") (name "review-hash") (value ,hash)))
           (input ((type "submit") (value "Mark Complete"))))))


(define (check-for-action session)
  (let ((action (get-binding 'action session)))
    (cond [(Failure? action) `(p (b ,(Failure-message action)))]
          [else (do-action (Success-result action) session)])))

(define (do-action action session)
  (cond [(string=? action "mark-incomplete") (do-mark-incomplete session)]
        [(string=? action "mark-complete") (do-mark-complete session)]
        [else `(p (b "No such action: " ,action))]))

(define (do-mark-incomplete session)
  (let ((hash (get-binding 'review-hash session)))
    (cond [(Failure? hash) `(p (b ,(Failure-message hash)))]
          [else (begin
                  (review:mark-incomplete (Success-result hash))
                  '(p (b "Review marked incomplete.")))])))

(define (do-mark-complete session)
  (let ((hash (get-binding 'review-hash session)))
    (cond [(Failure? hash) `(p (b ,(Failure-message hash)))]
          [else (begin
                  (review:mark-complete (Success-result hash))
                  '(p (b "Review marked complete.")))])))

(define (view-review-action record context)
  (let ((hash (review:record-hash record))
        (reviewer (review:record-reviewer-id record)))
    `(a ((href ,(string-append "/" class-name "/su/" reviewer "/review/" hash "/"))) ,context)))

(define (view-feedback-action record context)
  (let ((hash (review:record-hash record))
        (reviewee (review:record-reviewee-id record)))
    `(a ((href ,(string-append "/" class-name "/su/" reviewee "/feedback/view/" hash "/"))) ,context)))

  ;(struct record (class-id assignment-id step-id review-id reviewee-id reviewer-id completed hash flagged) #:transparent)

(define (step->statistic assignment-id)
  (lambda (step)
    (let* ((step-id (Step-id step))
           (submissions (number->string (submission:count-step assignment-id class-name step-id)))
           (reviews (apply append (map (review->statistic assignment-id step-id) (Step-reviews step)))))
      `((li "Step : " ,(action:step-status assignment-id step-id step-id))
        ,(append `(ul 
                   (li "Submissions: " ,submissions))
                 reviews)))))

(define (review->statistic assignment-id step-id)
  (lambda (review)
    (let* ((review-id (Review-id review))
           (completed (number->string (review:count-completed-reviews assignment-id class-name step-id review-id)))
           (assigned (number->string (review:count-all-assigned-reviews assignment-id class-name step-id review-id))))
      `((li "Reviews : " ,(action:review-status assignment-id step-id review-id review-id))
        (ul 
         (li  "Completed: " ,completed)
         (li "Assigned: " ,assigned))))))
      