#lang racket

(require db
         "common.rkt")

;; The roles table defines roles and their associated permissions
(provide roles-table roles-table-id roles-table-id-type roles-table-role roles-table-role-type)

(define roles-table "roles")
(define roles-table-id "id")
(define roles-table-id-type "TINYINT")

(define roles-table-role "role")
(define roles-table-role-type "VARCHAR(255)")

(define roles-table-can-edit "can_edit")
(define roles-table-can-edit-type "BOOLEAN")

(provide init)
(define (init sql-conn)
  (let ((drop (prepare sql-conn (merge "DROP TABLE IF EXISTS" roles-table)))
        (create (prepare sql-conn (merge "CREATE TABLE" roles-table "("
                                         roles-table-id roles-table-id-type ","
                                         roles-table-role roles-table-role-type ","
                                         roles-table-can-edit roles-table-can-edit-type ","
                                         "PRIMARY KEY (" roles-table-id "))"))))
    (query-exec sql-conn drop)
    (query-exec sql-conn create)))

(provide create-role)
(define (create-role sql-conn id role can-edit)
  (let* ((query (merge "INSERT INTO" roles-table "VALUES(?,?,?)"))
        (prep (prepare sql-conn query)))
    (query-exec sql-conn prep id role can-edit)))

(provide role-record role-record-id role-record-role role-record-can-edit)
(struct role-record (id role can-edit) #:transparent)

(provide get-role-record)
(define (get-role-record sql-conn id)
  (let* ((query (merge "SELECT" roles-table-role "," roles-table-can-edit "FROM" roles-table "WHERE" roles-table-id "=? LIMIT 1"))
         (prep (prepare sql-conn query))
         (result (query-row sql-conn prep id))
         (role (vector-ref result 0))
         (can-edit (not (= 0 (vector-ref result 1)))))
    (role-record id role can-edit)))

(provide all-roles)
(define (all-roles sql-conn)
  (let* ((query (merge "SELECT" roles-table-id "," roles-table-role "," roles-table-can-edit "FROM" roles-table))
         (prep (prepare sql-conn query))
         (result (query-rows sql-conn prep))
         (to-record (lambda (vec) (role-record (vector-ref vec 0) (vector-ref vec 1) (vector-ref vec 2)))))
    (map to-record result)))