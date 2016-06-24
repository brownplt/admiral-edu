#lang racket

(require db
         "common.rkt"
         "../../util/basic-types.rkt"
         (prefix-in system: "system.rkt")
         (prefix-in submission: "submission.rkt"))

;; This migration adds a published and last-modified columns to the submission table

;; -> (Result String)
(provide migrate)
(define (migrate)
  (match (check-migrated)
    [(Failure _) (do-migrate)]
    [(Success message) (Failure message)]))

; Returns Success if the system has migrated to atleast version 3
; otherwise returns Failure
; -> (Result String)
(provide check-migrated)
(define (check-migrated)
  (let ((version (get-current-version)))
  (cond [(>= version 3) (Success (format "Database is on version ~a." (system:select-version)))]
        [else (Failure "The system has not migrated to version 3.")])))

(define (get-current-version)
  (cond [(not (system-table-exists?)) 0]
        [else (system:select-version)]))

(define (do-migrate)
  (printf "Migrating to version 3.\n")
  (let* ((conn (make-sql-conn))
         (handler (lambda (exn) (Failure (format "An exception was raised: ~a." (raise exn)))))
         (result (with-handlers ([exn? handler]) (make-changes conn))))
    (disconnect conn)
    (printf "Done.\n")
    result))

(define (make-changes conn)
  (do-submission-table)
  (do-update-version-number)
  (Success (void)))


  
(provide system-table-exists?)
(define (system-table-exists?)
  (let* ((q (merge "SELECT COUNT(*)"
                   "FROM INFORMATION_SCHEMA.TABLES"
                   "WHERE TABLE_NAME=?"))
         (result (run query-value q system:table)))
    (= 1 result)))

(define (do-submission-table)
  (let ((create-published (merge "ALTER TABLE" submission:table
                                 "ADD published BOOL"))
        (create-last-updated (merge "ALTER TABLE" submission:table
                                    "ADD last_modified TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP"))
        (update-initial-values (merge "UPDATE" submission:table
                                      "SET published=true, last_modified=time_stamp")))
    (run query-exec create-published)
    (run query-exec create-last-updated)
    (run query-exec update-initial-values)))

(define (do-update-version-number)
  (let ((q (merge "UPDATE" system:table
                  "SET" system:version "=3")))
    (run query-exec q)))

