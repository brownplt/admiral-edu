#lang typed/racket

(require "typed-db.rkt"
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

(provide init)
(: init (-> Void))
(define (init)
  (let ((drop (merge "DROP TABLE IF EXISTS" table))
        (create (merge "CREATE TABLE" table "(" 
                                         class-id class-id-type ","
                                         user-id user-id-type ","
                                         role-id role-id-type ","
                                         "PRIMARY KEY (" class-id "," user-id "))")))
    (query-exec drop)
    (query-exec create)))

(provide delete)
(: delete (String String -> Void))
(provide delete)
(define (delete class-name uid)
  (let ((query (merge "DELETE FROM" table
                       "WHERE" class-id "=? AND"
                               user-id "=?")))
    (query-exec query class-name uid)))

(provide (struct-out Record))
(struct: Record ([class : String] [uid : String] [role : roles:Record]) #:transparent)

(define record-cols (string-join (list class-id user-id role-id) ","))
(define-type Record-Vector (Vector String String Integer))

(: vector->record (Record-Vector -> Record))
(define (vector->record vec)
  (let* ((class (vector-ref vec 0))
         (uid (vector-ref vec 1))
         (role (roles:get-role (vector-ref vec 2)))
         (result (Record class uid role)))
    result))

(provide all)
(: all (String -> (Listof Record)))
(define (all class)
  (let* ((query (merge "SELECT" record-cols
                       "FROM" table
                       "WHERE" class-id "=?"))
         (results (cast (query-rows query class) (Listof Record-Vector)))
         (records (map vector->record results)))
    records))

(provide select)
(: select (String String -> roles:Record))
(define (select class uid)
  (let* ((query (merge "SELECT" roles:table "." roles:id ","
                                roles:table "." roles:name ","
                                roles:table "." roles:can-edit 
                                
                       "FROM" roles:table "," table
                       
                       "WHERE" roles:table "." roles:id "=" table "." role-id "AND"
                               table "." class-id "=? AND"
                               table "." user-id "=?" 
                       "LIMIT 1"))
         (result (roles:vector->Record (cast (query-row query class uid) (Vector Integer String Integer)))))
    result))

(provide exists?)
(: exists? (String String -> Boolean))
(define (exists? class user)
  (let* ((query (merge "SELECT COUNT(*) FROM" table "WHERE" class-id "=? AND" user-id "=? LIMIT 1"))
         (result (cast (query-value query class user) Integer)))
    (= 1 result)))
    
(provide associate)
(: associate (String String Integer -> Void))
(provide associate)
(define (associate class uid role-id)
  (let ((create (merge "INSERT INTO" table "values(?,?,?)")))
    (query-exec create class uid role-id)))

(provide set-role)
(: set-role (String String Integer -> Void))
(define (set-role class uid  new-role)
  (let ((query (merge "UPDATE" table
                      "SET" role-id "=?"
                      "WHERE" class-id "=? AND"
                              user-id "=?")))
    (query-exec query new-role class uid)))