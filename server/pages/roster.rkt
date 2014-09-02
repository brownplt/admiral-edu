#lang racket

(require web-server/templates
         web-server/http/bindings)

(require "../base.rkt"
         "../util/roster.rkt"
         (prefix-in error: "errors.rkt")
         (prefix-in authorized: "../util/authorized.rkt"))
         
(define RENDER-ROSTER "render-roster")
(define NEW-STUDENT "new-student")
(define UPLOAD-ROSTER "upload-roster")
(define EDIT-USER "edit")
(define CREATE-STUDENT "create-student")
(define PROCESS-ROSTER "process-roster")
(define ILLEGAL-ACTION "illegal-action")
(define CHANGE-ROLE "change-role")
(define DROP-USER "drop")

(provide load)
(define (load session role rest [message '()])
  (authorized:can-edit session do-load session rest message))

(provide post)
(define (post post-data bindings)
  (lambda (session role rest [message '()])
    (authorized:can-edit session post->do-load post-data bindings session rest message)))

(define (post->do-load post-data bindings session rest message)
  (let* ((action (if (exists-binding? 'action bindings) (extract-binding/single 'action bindings) ILLEGAL-ACTION))
         (message (do-post action post-data bindings)))
    (do-load session rest message)))

(define (do-post action post-data bindings)
  (cond [(equal? action CREATE-STUDENT) (post->create-student bindings)]
        [(equal? action PROCESS-ROSTER) (post->process-roster bindings)]
        [(equal? action ILLEGAL-ACTION) "<p>The action you took could not be processed.</p>"]
        [else ""]))

(define (post->create-student bindings)
  (cond [(not (exists-binding? 'uid bindings)) "<p>Missing User ID.</p>"]
        [else (let* ((uid (extract-binding/single 'uid bindings))
                     (result (register-uid uid)))
                (cond [(Failure? result) (Failure-message result)]
                      [(Success? result) "<p>User added.</p>"]))]))
                
(define (post->process-roster bindings)
  (cond [(not (exists-binding? 'file bindings)) "<p>No roster file found.</p>"]
        [else (let* ((data (extract-binding/single 'file bindings))
                     (results (register-roster (bytes->string/utf-8 data)))
                     (output (map render-result results)))
                (string-join output "\n"))]))

(define (render-result result)
  (cond [(Success? result) (string-append "<p>" (Success-uid result) " added.</p>")]
        [(Failure? result) (string-append "<p style='font-weight:bold; color:red'>Could not add " (Failure-uid result) ": " (Failure-message result))]))

(define (do-load session rest message)
  (let* ((action (if (null? rest) RENDER-ROSTER (car rest)))
         [extra-message (if (null? message) "" message)]
         [body (select-body action rest)]
         [header (string-append class-name " - Roster")])
    (include-template "html/basic.html")))

(define (select-body action rest)
  (cond [(equal? action RENDER-ROSTER) (render-roster)]
        [(equal? action NEW-STUDENT) (new-student)]
        [(equal? action UPLOAD-ROSTER) (upload-roster)]
        [(equal? action EDIT-USER) (edit-user rest)]
        [else (render-roster)]))

(define (edit-user rest)
  (let* ((uid (cadr rest))
         (action (if (null? (cddr rest)) "" (caddr rest))))
    (cond [(equal? action CHANGE-ROLE) (do-change-role uid (cadddr rest))]
          [(equal? action DROP-USER) (do-drop-user uid)]
          [else
           (string-append "<h2>Editing User</h2>\n"
                          "<p>User ID: " uid "</p>\n"
                          "<p><a href='" CHANGE-ROLE "/student-role/'>Set Role: Student</a></p>"
                          "<p><a href='" CHANGE-ROLE "/ta-role/'>Set Role: Teaching Assistant</a></p>"
                          "<p><a href='" CHANGE-ROLE "/instructor-role/'>Set Role: Instructor</a></p>"
                          "<p><a href='" DROP-USER "/'>Drop User from Course</a></p>")])))

(define (do-change-role uid role)
  (let ((result (change-role uid (string->symbol role))))
    (cond [(Success? result) "<p>User role changed.</p><p><a href='../../../../'>Back to Roster</a></p>"]
          [(Failure? result) (string-append "<p>" (Failure-message result) "</p><p><a href='../../../../'>Back to Roster</a></p>")])))

(define (do-drop-user uid)
  (let ((result (drop-uid uid)))
    (cond [(Success? result) "<p>User dropped.</p><p><a href='../../../'>Back to Roster</a></p>"]
          [(Failure? result) (string-append "<p>" (Failure-message result) "</p><p><a href='../../../'>Back to Roster</a></p>")])))
         

(define (new-student)
  (string-append "<h2>New User</h2>\n"
                 "<p>Enter the User ID you would like to add to the roster.</p>"
                 "<form action='../' method='post'>\n"
                 "<input type='hidden' name='action' value='" CREATE-STUDENT "'>\n"
                 "<p>User ID: <input name='uid' type='text'></p>\n"
                 "<p><input type='submit' value='Submit'></p>\n"
                 "</form>\n"))

(define (upload-roster)
  (string-append "<h2>Upload Roster</h2>\n"
                 "<p>Select a file that has one user id per line. Each user id will be added as a student. Their role may be changed later.</p>"
                 "<form action='../' method='post' enctype='multipart/form-data'>\n"
                 "<input type='hidden' name='action' value='" PROCESS-ROSTER "'>\n"
                 "<p><input type='file' name='file'></p>"
                 "<p><input type='submit'></p>\n"
                 "</form>\n"))

(define (render-roster)
  (let ((records (role:all class-name)))
    (string-append "<p><a href='" UPLOAD-ROSTER "/'>Upload Roster</a></p>\n"
                   "<p><a href='" NEW-STUDENT "/'>New User</a></p>\n"
                   (render-instructors records)
                   (render-tas records)
                   (render-students records))))

(define (is-role? id)
  (lambda (record)
    (let* ((role (role:record-role record))
           (role-id (roles:role-id role)))
      (= role-id id))))

(define is-instructor? (is-role? instructor-role))
(define is-ta? (is-role? ta-role))
(define is-student? (is-role? student-role))

(define (render-role title pred?)
  (lambda (records)
    (let* ((records (filter pred? records))
           (output (map render-record records)))
      (string-append title "\n"
                     (string-join output "\n")))))

(define render-instructors (render-role "<h2>Instructors</h2>" is-instructor?))
(define render-tas (render-role "<h2>Teaching Assistants</h2>" is-ta?))
(define render-students (render-role "<h2>Students</h2>" is-student?))

(define (render-record record)
  (let* ((uid (role:record-uid record)))
    (string-append "<p>" uid " - <a href='" EDIT-USER "/" uid "/'>Edit User</a></p>")))