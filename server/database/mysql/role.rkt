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
  (let* ((conn (make-sql-conn))
         (drop-query (merge "DROP TABLE IF EXISTS" table))
         (drop (prepare conn drop-query))
         (create-query (merge "CREATE TABLE" table "(" 
                                         class-id class-id-type ","
                                         user-id user-id-type ","
                                         role-id role-id-type ","
                                         "PRIMARY KEY (" class-id "," user-id "))"))
         (create (prepare conn create-query)))
    (query-exec conn drop)
    (query-exec conn create)
    (release conn)))

(provide delete)
(define (delete class-name uid)
  (let* ((query (merge "DELETE FROM" table
                       "WHERE" class-id "=? AND"
                               user-id "=?"))
         (result (run query-exec query class-name uid)))
    result))

(provide (struct-out record))
(struct record (class uid role) #:transparent)

(define record-cols (string-join (list class-id user-id role-id) ","))

(define (vector->record vec)
  (let* ((class (vector-ref vec 0))
         (uid (vector-ref vec 1))
         (role (roles:get-role (vector-ref vec 2)))
         (result (record class uid role)))
    result))

(provide all)
(define (all class)
  (let* ((query (merge "SELECT" record-cols
                       "FROM" table
                       "WHERE" class-id "=?"))
         (results (run query-rows query class))
         (records (map vector->record results)))
    records))

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
         (conn (make-sql-conn))
         (prep (prepare conn query))
         (result (query-row conn prep class uid)))
    (release conn)
    (if (not result) result
        (let ((id (vector-ref result 0))
              (name (vector-ref result 1))
              (can-edit (= 1 (vector-ref result 2))))
          (roles:Record id name can-edit)))))

;; Returns #t if the class, user combination exists and #f otherwise
(provide exists?)
(define (exists? class user)
  (let* ((conn (make-sql-conn))
         (query (merge "SELECT COUNT(*) FROM" table "WHERE" class-id "=? AND" user-id "=? LIMIT 1"))
         (prep (prepare conn query))
         (result (vector-ref (query-row conn prep class user) 0)))
    (release conn)
    (= 1 result)))
    
;; Associates a class and user id with the specified role id. Returns #t if successful and #f otherwise.
(provide associate)
(define (associate class uid role-id)
  ;; TODO: Validate that the class, uid, and role exist
  (let* ((conn (make-sql-conn))
         (create (prepare conn (merge "INSERT INTO" table "values(?,?,?)")))
         (result (if (eq? #f (query-exec conn create class uid role-id)) #f #t)))
    (release conn)
    result))

(provide set-role)
(define (set-role class uid  new-role)
  (let* ((query (merge "UPDATE" table
                       "SET" role-id "=?"
                       "WHERE" class-id "=? AND"
                               user-id "=?"))
         (result (run query-exec query new-role class uid)))
    result))

;; Retrieve all students for a class
(provide in-class)
(define (in-class class s-role limit page)
  (let* ((conn (make-sql-conn))
         (lower (* limit page))
         (upper (+ lower limit))
         (query (merge "SELECT" user-id "FROM" table "WHERE" class-id "=? AND" role-id "=? LIMIT ?, ?"))
         (prepped (prepare conn query))
         (to-record (lambda (result) (user (vector-ref result 0) class 0)))
         (result (map to-record (query-rows conn query class s-role lower upper))))
    (release conn)
    result))

(provide user user-uid user-class user-role)
(struct user (uid class role) #:transparent)

;; (define (select-students-in-class class limit page) (internal-select-students-in-class class limit page))