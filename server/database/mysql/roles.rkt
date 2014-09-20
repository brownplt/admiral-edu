#lang typed/racket

(require "typed-db.rkt")

;; The roles table defines roles and their associated permissions
(provide table id id-type name name-type can-edit can-edit-type)

(define table "roles")
(define id "id")
(define id-type "TINYINT")

(define name "role")
(define name-type "VARCHAR(255)")

(define can-edit "can_edit")
(define can-edit-type "BOOLEAN")

(provide init)
(: init (-> Void))
(define (init)
  (let ((drop (merge "DROP TABLE IF EXISTS" table))
        (create (merge "CREATE TABLE" table "("
                       id id-type ","
                       name name-type ","
                       can-edit can-edit-type ","
                       "PRIMARY KEY (" id "))")))
    (query-exec drop)
    (query-exec create)))

(provide create)
(: create (Integer String (U 0 1) -> Void))
(define (create id role can-edit)
  (let ((query (merge "INSERT INTO" table "VALUES(?,?,?)")))
    (query-exec query id role can-edit)))

(provide (struct-out Record))
(struct: Record ([id : Integer] [name : String] [can-edit : Boolean]) #:transparent)

(define record-fields (string-join (list id name can-edit) ","))
(define-type Record-Vector (Vector Integer String Integer))

(provide vector->Record)
(: vector->Record (Record-Vector -> Record))
(define (vector->Record vec)
  (let ((id (vector-ref vec 0))
        (name (vector-ref vec 1))
        (can-edit (= (vector-ref vec 2) 1)))
    (Record id name can-edit)))

(provide get-role)
(: get-role (Integer -> Record))
(define (get-role s-id)
  (let* ((query (merge "SELECT" record-fields "FROM" table "WHERE" id "=? LIMIT 1"))
         (result (cast (query-row query s-id) Record-Vector)))
    (vector->Record result)))

(provide all)
(: all (-> (Listof Record)))
(define (all)
  (let* ((query (merge "SELECT" record-fields
                       "FROM" table))
         (results (cast (query-rows query) (Listof Record-Vector))))
    (map vector->Record results)))