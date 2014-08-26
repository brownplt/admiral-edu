#lang racket

(require web-server/http/bindings)

(require "../base.rkt")

(provide index)
(define (index session role [message '()])
  `(html
    ,(body
      (title session)
      message
      (user-info session role)
      ;(next-step session)
      ;(completed-reviews session)
      (users session role))))

(provide post->index)
(define (post->index session role binds)
  (cond
    [(exists-binding? 'new-uid binds) (new-user session role binds)]
    [(exists-binding? 'file binds) (upload-file session role binds)]
    [else (index session role)]))
  

(define (new-user session role binds)
  (let* ((new-uid (extract-binding/single 'new-uid binds))
         (new-role (extract-binding/single 'new-role binds))
         (output (with-handlers ([exn:fail? could-not-create-user]) (create-new-user (ct-session-class session) new-uid new-role))))
    (index session role output)))

(define (upload-file session role binds)
  (let ((data (extract-binding/single 'file binds))
        (assignment (extract-binding/single 'assignment binds))
        (step (extract-binding/single 'step binds)))
    (upload-submission (ct-session-class session) (ct-session-uid session) assignment step data)
    (index session role '((p "Your submission has been accepted.")))))

(define (could-not-create-user exn)
  '((p "Unable to create user")))

(define (create-new-user class new-uid new-role)
  (if (equal? "" new-uid) 
      '((p "Cannot create user without a user id"))
      ((lambda ()
         (if (not (user:exists? new-uid)) (user:create new-uid) '())
         (role:associate class new-uid new-role)
         `((p , (string-append "Added " new-uid)))))))
       
(define (body . elements)
  (cons 'body (foldr append '() elements)))

(define (title session)
  `((h1 ,(ct-session-class session))))

(define (user-info session role)
  `((h2 "User Information")
    (p ,(string-append "User ID: " (ct-session-uid session)))
    (p ,(string-append "User Role: " (roles:role-name role)))))

(define (users session role)
  (if (not (roles:role-can-edit role)) '()
      (append
       (add-student-form)
       '((h2 "Instructors"))
       (list-instructors session)
       '((h2 "Teaching Assistants"))
       (list-tas session)
       '((h2 "Students")) 
       (list-students session))))

(define (list-students session)
  (let ((student-records (role:in-class (ct-session-class session) student-role 200 0)))        
    (map show-record student-records)))

(define (list-instructors session)
  (let ((records (role:in-class (ct-session-class session) instructor-role 200 0)))        
    (map show-record records)))

(define (list-tas session)
  (let ((records (role:in-class (ct-session-class session) ta-role 200 0)))        
    (map show-record records)))

(define (add-student-form)
  (let* 
      ;; Creates a single <option> field from a role-record
      ((role-option (lambda (record) 
                        `(option ((value ,(number->string (roles:role-id record)))) ,(roles:role-name record))))
       ;; Creates a <select> field containing one <option> for each role in the database
         (role-select (append '(select ((name "new-role"))) (map role-option (roles:all)))))
    `((h3 "Add User")
      (form ((method "post") (action ""))
            (p "User ID: " (input ((name "new-uid") (type "text"))))
            (p "Role: " ,role-select)
            (p (input ((name "submit") (type "submit"))))))))

(define (finished)
  `((h3 "Assignment Complete.")
    (p "You have finished all of the steps for this assignment.")))

(define (review-form step)
    `((h3 "Review Step")
      (p "You must complete a review before continuing to the next step. When you have finished reviewing, return to this page.")
      (p (a ((href ,(string-append "review/clock/" step))) "Access Review"))))

(define (upload-form step)
    `((h3 "Submission Step")
      (form ((method "post") (action "") (enctype "multipart/form-data"))
            (p "Assignment: clock")
            (p ,(string-append "Step: " step)
            (p "File: " (input ((name "file") (type "file") (id "file"))))
            (input ((type "hidden") (name "step") (value ,step)))
            (input ((type "hidden") (name "assignment") (value "clock")))
            (p (input ((name "submit") (type "submit"))))))))

(define (completed-reviews session)
  (let* ((hashes (review:select-reviews (ct-session-uid session))))
    (append '((h3 "Available Reviews"))
    (map (lambda (hash) `(p (a ((href ,(string-append "view-review/" hash))) "View Review"))) hashes))))
      

(define (show-record record)
  `(p ,(role:user-uid record)))

