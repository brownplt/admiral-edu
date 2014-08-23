#lang racket

(require db
         "common.rkt")

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
(define (init)
  (let ((drop (prepare (sql-conn) (merge "DROP TABLE IF EXISTS" table)))
        (create (prepare (sql-conn) (merge "CREATE TABLE" table "("
                                         id id-type ","
                                         name name-type ","
                                         can-edit can-edit-type ","
                                         "PRIMARY KEY (" id "))"))))
    (query-exec (sql-conn) drop)
    (query-exec (sql-conn) create)))

(provide create)
(define (create id role can-edit)
  (let* ((query (merge "INSERT INTO" table "VALUES(?,?,?)"))
        (prep (prepare (sql-conn) query)))
    (query-exec (sql-conn) prep id role can-edit)))

(provide role role-id role-name role-can-edit)
(struct role (id name can-edit) #:transparent)

(provide get-role)
(define (get-role s-id)
  (let* ((query (merge "SELECT" name "," can-edit "FROM" table "WHERE" id "=? LIMIT 1"))
         (prep (prepare (sql-conn) query))
         (result (query-row (sql-conn) prep s-id))
         (name (vector-ref result 0))
         (can-edit (not (= 0 (vector-ref result 1)))))
    (role s-id name can-edit)))

(provide all)
(define (all)
  (let* ((query (merge "SELECT" id "," name "," can-edit "FROM" table))
         (prep (prepare (sql-conn) query))
         (result (query-rows (sql-conn) prep))
         (to-record (lambda (vec) (role (vector-ref vec 0) (vector-ref vec 1) (vector-ref vec 2)))))
    (map to-record result)))