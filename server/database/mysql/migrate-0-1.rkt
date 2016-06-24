#lang racket

(require db
         "common.rkt"
         "../../util/basic-types.rkt"
         (prefix-in system: "system.rkt")
         (prefix-in review: "review.rkt")
         (prefix-in submission: "submission.rkt"))

;; -> (Result String)
(provide migrate)
(define (migrate)
  (match (check-migrated)
    [(Failure _) (do-migrate)]
    [(Success message) (Failure message)]))

; Returns Success if the system has migrated to atleast version 1
; otherwise returns Failure
; -> (Result String)
(provide check-migrated)
(define (check-migrated)
  (cond [(and (system-table-exists?) (>= (system:select-version) 1)) (Success (format "Database is on version ~a." (system:select-version)))]
        [else (Failure "The system has not migrated to version 1.")]))

(define (do-migrate)
  (let* ((conn (make-sql-conn))
         (handler (lambda (exn) (Failure (format "An exception was raised: ~a." (raise exn)))))
         (result (with-handlers ([exn? handler]) (make-changes conn))))
    (disconnect conn)
    result))

(define (make-changes conn)
  (when (not (system-table-exists?)) (create-system-table))
  (do-submission-table)
  (do-review-table)
  (do-update-version-number)
  (Success (void)))

(define (create-system-table)
  (let* ((drop (merge "DROP TABLE IF EXISTS" system:table))
         (create (merge "CREATE TABLE" system:table "(" 
                        system:version system:version-type ","
                        system:id system:id-type ","
                        "PRIMARY KEY (" system:id "))"))
         (initial-version (merge "INSERT INTO" system:table " VALUES(0,1)")))
    (run query-exec drop)
    (run query-exec create)
    (run query-exec initial-version)))

(define (do-update-version-number)
  (let ((q (merge "UPDATE" system:table
                  "SET" system:version "=1")))
    (run query-exec q)))
    
(define (do-submission-table)
  (let ((q (merge "ALTER TABLE" submission:table
                  "MODIFY COLUMN" submission:time-stamp submission:time-stamp-type-1)))
    (run query-exec q)))

(define (do-review-table)
  (let ((q (merge "ALTER TABLE" review:table
                  "MODIFY COLUMN" review:time-stamp review:time-stamp-type-1)))
    (run query-exec q)))

(provide system-table-exists?)
(define (system-table-exists?)
  (let* ((q (merge "SELECT COUNT(*)"
                   "FROM INFORMATION_SCHEMA.TABLES"
                   "WHERE TABLE_NAME=?"))
         (result (run query-value q system:table)))
    (= 1 result)))