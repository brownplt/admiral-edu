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
      (render-menu session role))))
      ;(users session role))))
  

(define (new-user session role binds)
  (let* ((new-uid (extract-binding/single 'new-uid binds))
         (new-role (extract-binding/single 'new-role binds))
         (output (with-handlers ([exn:fail? could-not-create-user]) (create-new-user (ct-session-class session) new-uid new-role))))
    (index session role output)))

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
    (p ,(string-append "User Role: " (roles:Record-name role)))))

(define (render-menu session role)
  (let ((start-url (hash-ref (ct-session-table session) 'start-url)))
    (if (not (roles:Record-can-edit role)) (student-view start-url) (instructor-view start-url))))

(define (instructor-view start-url)
  (append
   `((p (a ((href ,(string-append start-url "assignments/"))) "Assignments"))
     (p(a ((href ,(string-append start-url "roster/"))) "Roster")))))

(define (student-view start-url)
  (append
   `((p (a ((href ,(string-append start-url "assignments/"))) "Assignments")))))
    
(define (show-record record)
  `(p ,(role:Record-uid record)))

