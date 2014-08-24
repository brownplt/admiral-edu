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
  (let* ((conn (make-sql-conn))
         (drop (prepare conn (merge "DROP TABLE IF EXISTS" table)))
        (create (prepare conn (merge "CREATE TABLE" table "("
                                         id id-type ","
                                         name name-type ","
                                         can-edit can-edit-type ","
                                         "PRIMARY KEY (" id "))"))))
    (query-exec conn drop)
    (query-exec conn create)
    (release conn)))

(provide create)
(define (create id role can-edit)
  (let* ((conn (make-sql-conn))
         (query (merge "INSERT INTO" table "VALUES(?,?,?)"))
        (prep (prepare conn query)))
    (query-exec conn prep id role can-edit)
    (release conn)))

(provide role role-id role-name role-can-edit)
(struct role (id name can-edit) #:transparent)

(provide get-role)
(define (get-role s-id)
  (let* ((conn (make-sql-conn))
         (query (merge "SELECT" name "," can-edit "FROM" table "WHERE" id "=? LIMIT 1"))
         (prep (prepare conn query))
         (result (query-row conn prep s-id))
         (name (vector-ref result 0))
         (can-edit (not (= 0 (vector-ref result 1)))))
    (release conn)
    (role s-id name can-edit)))

(provide all)
(define (all)
  (let* ((conn (make-sql-conn))
         (query (merge "SELECT" id "," name "," can-edit "FROM" table))
         (prep (prepare conn query))
         (result (query-rows conn prep))
         (to-record (lambda (vec) (role (vector-ref vec 0) (vector-ref vec 1) (vector-ref vec 2)))))
    (release conn)
    (map to-record result)))