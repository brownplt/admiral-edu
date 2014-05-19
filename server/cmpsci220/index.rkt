#lang racket

(require web-server/http/bindings)

(require "../ct-session.rkt"
         "../database/mysql.rkt"
         "config.rkt")

(provide index)
(define (index session role [message '()])
  `(html
    ,(body
      (title session)
      message
      (user-info session role)
      (users session role))))

(provide post->index)
(define (post->index session role binds)
  (let* ((new-uid (extract-binding/single 'new-uid binds))
         (new-role (extract-binding/single 'new-role binds))
         (output (with-handlers ([exn:fail? could-not-create-user]) (create-new-user (ct-session-class session) new-uid new-role))))
    (index session role output)))

(define (could-not-create-user exn)
  (print exn)
  '((p "Unable to create user")))

(define (create-new-user class new-uid new-role)
  (if (equal? "" new-uid) 
      '((p "Cannot create user without a user id"))
      ((lambda ()
         (if (not (exists-user new-uid)) (create-user new-uid) '())
         (associate-role class new-uid new-role)
         `((p , (string-append "Added " new-uid)))))))
       
(define (body . elements)
  (cons 'body (foldr append '() elements)))

(define (title session)
  `((h1 ,(ct-session-class session))))

(define (user-info session role)
  `((h2 "User Information")
    (p ,(string-append "User ID: " (ct-session-uid session)))
    (p ,(string-append "User Role: " (role-record-role role)))))

(define (users session role)
  (if (not (role-record-can-edit role)) '()
      (append        
       add-student-form
       '((h2 "Instructors"))
       (list-instructors session)
       '((h2 "Teaching Assistants"))
       (list-tas session)
       '((h2 "Students")) 
       (list-students session))))

(define (list-students session)
  (let ((student-records (select-users-in-class (ct-session-class session) student-role 200 0)))        
    (map show-record student-records)))

(define (list-instructors session)
  (let ((records (select-users-in-class (ct-session-class session) instructor-role 200 0)))        
    (map show-record records)))

(define (list-tas session)
  (let ((records (select-users-in-class (ct-session-class session) ta-role 200 0)))        
    (map show-record records)))

(define add-student-form-prime
  `((h3 "Add User")
    (form ((method "post" (action "submit")))
          (p "User ID: " (input ((name "new-uid") (type "text"))))
          (p "Role: " (select ((name "new-role")) 
                              (option ((value "0")) "Instructor")
                              (option ((value "1")) "Teaching Assistant")
                              (option ((value "2")) "Student")))
          (p (input ((name "submit") (type "submit")))))))

(define add-student-form
  (let* 
      ;; Creates a single <option> field from a role-record
      ((role-option (lambda (record) 
                        `(option ((value ,(number->string (role-record-id record)))) ,(role-record-role record))))
       ;; Creates a <select> field containing one <option> for each role in the database
         (role-select (append '(select ((name "new-role"))) (map role-option (all-roles)))))
    `((h3 "Add User")
      (form ((method "post" (action "submit")))
            (p "User ID: " (input ((name "new-uid") (type "text"))))
            (p "Role: " ,role-select)
            (p (input ((name "submit") (type "submit"))))))))


(define (show-record record)
  `(p ,(user-record-uid record)))

