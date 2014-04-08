#lang racket
(require web-server/servlet
         web-server/servlet-dispatch
         web-server/web-server
         web-server/dispatch
         web-server/http/request-structs
         "db-proto.rkt"
         "assignment-proto.rkt")
 
 
(define-values (start mk-url)
  (dispatch-rules
   [("") index]
   [("assignment") assignment]
   [("assignment" "submit") assignment/submit]
   [("assignment" "submit") #:method "post" post->assignment/submit]
   [("assignment" "status") assignment/status]
   [("assignment" "status") #:method "post" post->assignment/status]))
 
(define (index req)
  (response/xexpr
   `(html
     (body
      (h1 "Welcome")
      (h2 "Command Categories")
      (h3 (a ((href "assignment")) "Assignments"))
      (ul
       (li (a ((href "assignment/status")) "assignment/status"))
       (li (a ((href "assignment/submit")) "assignment/submit")))))))
 
(define (assignment req)
  (response/xexpr
   `(html
     (body
      (h1 "Assignments")
      (h2 "Commands")
      (ul
       (li (a ((href "assignment/status")) "assignment/status"))
       (li (a ((href "assignment/submit")) "assignment/submit")))))))

(define (assignment/status req)
  (response/xexpr
   `(html
     (body
      (h1 "Assignment Status")
      (form ((method "post") (action "status"))
            (p "student-id:" (input ((name "student-id") (type "text"))))
            (p "assignment-id:" (input ((name "assignment-id") (type "text"))))
            (p (input ((name "submit") (type "submit")))))))))

(define (exists-bindings? binds . vals)
  (let ((exists? (lambda (x) exists-bindings? x binds)))
    (foldr (lambda (x y) (and x y)) #t (map exists? vals))))

(define (post->assignment/status req)
  (let ((binds (request-bindings req)))
    (response/xexpr
     (cond
       [(exists-bindings? binds 'student-id 'assignment-id)
        (let* ((student-id (extract-binding/single 'student-id binds))
               (assignment-id (extract-binding/single 'assignment-id binds))
               (started (has-started? student-id assignment-id)))
          (if (and (is-assignment? assignment-id) started)
              ;; If this is an assignment, get the request generator and 
              ;; ask it what we should do next
              (let* ((gen (cadr (get-assignment assignment-id)))
                     (status (get-status student-id assignment-id))
                     (req (gen status)))
                `(html
                  (body
                   (h1 "Assignment Status")
                   (p ,(string-append "student-id: " student-id))
                   (p ,(string-append "assignment-id: " assignment-id))
                   (p ,(string-append "current-step: " status))
                   (p ,(string-append "request-target: " (serv-request-target req)))
                   (p ,(string-append "request-resource-type: " (serv-request-resource-type req))))))
              `(html
                (body
                 (h1 "Assignment Status")
                 (p "The assignment does not exist or you are not currently working on this assigment.")))))]
       [else
        `(html
          (body
           (h1 "Error")))]))))

(define (assignment/submit req)
  (response/xexpr
   `(html
     (body
      (h1 "Assignment Submission")
      (form ((method "post" (action "submit")))
            (p "student-id:" (input ((name "student-id") (type "text"))))
            (p "assignment-id:" (input ((name "assignment-id") (type "text"))))
            (p "submission-step:" (input ((name "submission-step") (type "text"))))
            (p "resource:" (input ((name "resource") (type "text"))))
            (p (input ((name "submit") (type "submit")))))))))

(define (post->assignment/submit req)
  (let ((binds (request-bindings req)))
    (response/xexpr
     (cond 
       [(exists-bindings? binds 'student-id 'assignment-id 'submission-step 'resource)
        (let ((student-id (extract-binding/single 'student-id binds))
              (assignment-id (extract-binding/single 'assignment-id binds))
              (step-id (extract-binding/single 'submission-step binds))
              (submission-url (extract-binding/single 'resource binds)))
          (if (is-assignment? assignment-id)
              ;; If this is an assignment, get the iterator for this assignment
              ;; and try to process the submission
               (let* ((itr (car (get-assignment assignment-id)))
                      (step (itr student-id step-id submission-url))
                      (output (match step
                                [(fail message) (string-append "Failed: " message)]
                                [(result message next) (string-append message (string-append " next step:" next))]
                                [(finished message) (string-append "Finished: " message)])))
                 `(html
                   (body
                    (h1 "Assignment Submission")
                    (p ,output))))
               `(html
                 (body
                  (h1 "Assignment Submission")
                  (p "Assignment does not exist")))))]
       [else
        `(html
          (body
           (h1 "Error")))]))))

(init-sample)
(serve #:dispatch (dispatch/servlet start)
       #:port 8080)