#lang racket

(require "../database/mysql.rkt"
         "../database/mysql/common.rkt"
         "assignment.rkt"
         "three-condition-study.rkt"
         (planet esilkensen/yaml:3:1)
         "../config.rkt"
         db)

(define (make-student id)
    (user:create id)
    (role:associate class-name id student-role))

(define ACE "ace")
(define AMY "amy")
(define ART "art")
(define ALF "alf")
  
(define JOE "joe")
(define JAN "jan")
(define JIM "jim")
(define JON "jon")

(define SAL "sal")
(define SAM "sam")
(define STU "stu")
(define SUK "suk")
(define SAN "san")


(define (init-tests)
  (init-db)
  (class:create class-name)

  
  (roles:create instructor-role "Instructor" 1)
  (roles:create ta-role "Teaching Assistant" 1)
  (roles:create student-role "Student" 0)

  (map make-student (list ACE AMY ART ALF JOE JAN JIM JON SAL SAM STU SUK SAN))
  (create-assignment three-test-assignment)
  (save-assignment-description class-name "test-assignment" (file->string "test-assignment-description.yaml")))

(define three-test-assignment (yaml->assignment (string->yaml (file->string "test-assignment-description.yaml"))))

(define (run-tests)
  (initialize))

; groups:
; gets-review: ACE AMY ART ALF
; does-review: JOE JAN JIM JON
; no-review: SAL SAM STU SUK SAN

(define useless-tar-file
  (file->string "empty.tar"))

(define (test-submit-order-submit user)
  (three-do-submit-step (Assignment-id three-test-assignment) "tests" user useless-tar-file (Assignment-steps three-test-assignment)))

(define (test-submit-order)
  (map test-submit-order-submit (list ACE AMY ART ALF JOE JAN JIM JON SAL SAM STU SUK SAN))  
  (make-hash
    (list
      (cons ACE (list JOE JAN JIM))
      (cons AMY (list JOE JAN JON))
      (cons ART (list JOE JIM JON))
      (cons ALF (list JAN JIM JON)))))

(define (get-reviews assignment-id step-id)
  (let* ((q (merge "SELECT" review:reviewer-id "," review:reviewee-id
                   "FROM" review:table
                   "WHERE" review:assignment-id "=? AND"
                           review:step-id "=?"))
         (prep (prepare sql-conn q))
         (result (query-rows sql-conn prep assignment-id step-id)))
    result))
