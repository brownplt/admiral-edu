#lang racket

(require db
         "common.rkt"
         (prefix-in user: "user.rkt")
         (prefix-in class: "class.rkt")
         (prefix-in roles: "roles.rkt"))

;; The role table defines a relation between users and classes
(provide table class-id class-id-type user-id user-id-type role-id)
(define table "role")
(define class-id "class_id")
(define class-id-type class:id-type)

(define user-id "user_uid")
(define user-id-type user:uid-type)

(define role-id "role_id")
(define role-id-type roles:id-type)

;; Initializes the role table.
(provide init)
(define (init)
  (let* ((drop-query (merge "DROP TABLE IF EXISTS" table))
         (drop (prepare (sql-conn) drop-query))
         (create-query (merge "CREATE TABLE" table "(" 
                                         class-id class-id-type ","
                                         user-id user-id-type ","
                                         role-id role-id-type ","
                                         "PRIMARY KEY (" class-id "," user-id "))"))
         (create (prepare (sql-conn) create-query)))
    (query-exec (sql-conn) drop)
    (query-exec (sql-conn) create)))

;; Retrieve a roles:role for a class/user
(provide select)
(define (select class uid)
  (let* ((query (merge "SELECT" roles:table "." roles:id ","
                                roles:table "." roles:name ","
                                roles:table "." roles:can-edit 
                                
                       "FROM" roles:table "," table
                       
                       "WHERE" roles:table "." roles:id "=" table "." role-id "AND"
                               table "." class-id "=? AND"
                               table "." user-id "=?" 
                       "LIMIT 1"))
         (prep (prepare (sql-conn) query))
         (result (try-with-default #f query-row (sql-conn) prep class uid)))
    (if (not result) result
        (let ((id (vector-ref result 0))
              (name (vector-ref result 1))
              (can-edit (= 1 (vector-ref result 2))))
          (roles:role id name can-edit)))))

;; Returns #t if the class, user combination exists and #f otherwise
(provide exists?)
(define (exists? class user)
  (let* ((query (merge "SELECT COUNT(*) FROM" table "WHERE" class-id "=? AND" user-id "=? LIMIT 1"))
         (prep (prepare (sql-conn) query))
         (result (vector-ref (query-row (sql-conn) prep class user) 0)))
    (= 1 result)))
    
;; Associates a class and user id with the specified role id. Returns #t if successful and #f otherwise.
(provide associate)
(define (associate class uid role-id)
  ;; TODO: Validate that the class, uid, and role exist
  (let ((create (prepare (sql-conn) (merge "INSERT INTO" table "values(?,?,?)"))))
    (if (eq? #f (try-with-default #f query-exec (sql-conn) create class uid role-id)) #f #t)))

;; Retrieve all students for a class
(provide in-class)
(define (in-class class s-role limit page)
  (let* ((lower (* limit page))
         (upper (+ lower limit))
         (query (merge "SELECT" user-id "FROM" table "WHERE" class-id "=? AND" role-id "=? LIMIT ?, ?"))
         (prepped (prepare (sql-conn) query))
         (to-record (lambda (result) (user (vector-ref result 0) class 0))))
    (map to-record (query-rows (sql-conn) query class s-role lower upper))))

(provide user user-uid user-class user-role)
(struct user (uid class role) #:transparent)

;; (define (select-students-in-class class limit page) (internal-select-students-in-class class limit page))