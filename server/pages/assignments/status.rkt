#lang racket

(require db
         "../../base.rkt"
         "../../authoring/assignment.rkt"
         (prefix-in error: "../errors.rkt")
         (prefix-in action: "action.rkt"))

(provide load)
(define (load session url message [post #f])
  (match url
    [(list assignment-id) (display-assignment assignment-id message)]
    [(list assignment-id step-id) (display-step assignment-id step-id message)]
    [(list assignment-id step-id review-id) (display-review assignment-id step-id review-id message)]
    [_ error:four-oh-four-xexpr]))
  
(define (display-assignment assignment-id message)
  (let* ((assignment (assignment-id->assignment assignment-id))
         (steps (cons 'ol (apply append (map (step->statistic assignment-id) (Assignment-steps assignment))))))
    (append (header assignment-id message)
            (list steps))))

(define (header assignment-id message)
  (let* ((assignment (assignment-id->assignment assignment-id))
         (record (assignment:select class-name assignment-id))
         (open (assignment:record-open record))
         (ready (assignment:record-ready record))
         (status (if ready (if open "Open" "Closed") "Missing Dependencies")))
  `((h1 ,(action:assignments "Assignments"))
    (h2 ,(action:dashboard assignment-id assignment-id))
    (h3 ,(action:status assignment-id "Status") " : " ,status)
    ,(when message message))))

(define (display-step assignment-id step-id message)
  (let ((count (number->string (submission:count-step assignment-id class-name step-id)))
        (submission-records (submission:select-all assignment-id class-name step-id)))
  (append (header assignment-id message)
          `((h4 ,step-id)
            (p "Submissions : " ,count))
          (map submission-record->xexpr submission-records))))

(define (submission-record->xexpr record)
  (let ((user-id (submission:record-user record))
        (time-stamp (format-time-stamp (submission:record-time-stamp record))))
    `(li ,(format "~a - ~a" user-id time-stamp))))

(define (format-time-stamp time-stamp)
  (let ((year (number->string (sql-timestamp-year time-stamp)))
        (month (month->string (sql-timestamp-month time-stamp)))
        (day (number->string (sql-timestamp-day time-stamp)))
        (hour (number->string (sql-timestamp-hour time-stamp)))
        (minute (ensure-leading-zero (number->string (sql-timestamp-minute time-stamp))))
        (second (ensure-leading-zero (number->string (sql-timestamp-second time-stamp)))))
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
    

(define (display-review assignment-id step-id review-id message)
  #f)


(define (step->statistic assignment-id)
  (lambda (step)
    (let* ((step-id (Step-id step))
           (submissions (number->string (submission:count-step assignment-id class-name step-id)))
           (reviews (map (review->statistic assignment-id step-id) (Step-reviews step))))
      `((li "Step : " ,step-id)
        ,(append `(ul 
                   (li "Submissions: " ,submissions))
                 reviews)))))

(define (review->statistic assignment-id step-id)
  (lambda (review)
    (let* ((review-id (Review-id review))
           (completed (number->string (review:count-completed-reviews assignment-id class-name step-id review-id)))
           (assigned (number->string (review:count-all-assigned-reviews assignment-id class-name step-id review-id))))
      `(li ,(string-append "Reviews : " review-id)
        (ul 
         (li  "Completed: " ,completed)
         (li "Assigned: " ,assigned))))))
      

  
  