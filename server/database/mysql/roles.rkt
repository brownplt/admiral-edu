#lang racket

(require db
         "common.rkt")

;; The roles table defines roles and their associated permissions
(provide table id id-type role role-type)

(define table "roles")
(define id "id")
(define id-type "TINYINT")

(define role "role")
(define role-type "VARCHAR(255)")

(define can-edit "can_edit")
(define can-edit-type "BOOLEAN")

(provide init)
(define (init sql-conn)
  (let ((drop (prepare sql-conn (merge "DROP TABLE IF EXISTS" table)))
        (create (prepare sql-conn (merge "CREATE TABLE" table "("
                                         id id-type ","
                                         role role-type ","
                                         can-edit can-edit-type ","
                                         "PRIMARY KEY (" id "))"))))
    (query-exec sql-conn drop)
    (query-exec sql-conn create)))

(provide create-role)
(define (create-role sql-conn id role can-edit)
  (let* ((query (merge "INSERT INTO" table "VALUES(?,?,?)"))
        (prep (prepare sql-conn query)))
    (query-exec sql-conn prep id role can-edit)))

(provide role-record role-record-id role-record-role role-record-can-edit)
(struct role-record (id role can-edit) #:transparent)

(provide get-role-record)
(define (get-role-record sql-conn s-id)
  (let* ((query (merge "SELECT" role "," can-edit "FROM" table "WHERE" id "=? LIMIT 1"))
         (prep (prepare sql-conn query))
         (result (query-row sql-conn prep s-id))
         (role (vector-ref result 0))
         (can-edit (not (= 0 (vector-ref result 1)))))
    (role-record s-id role can-edit)))

(provide all-roles)
(define (all-roles sql-conn)
  (let* ((query (merge "SELECT" id "," role "," can-edit "FROM" table))
         (prep (prepare sql-conn query))
         (result (query-rows sql-conn prep))
         (to-record (lambda (vec) (role-record (vector-ref vec 0) (vector-ref vec 1) (vector-ref vec 2)))))
    (map to-record result)))