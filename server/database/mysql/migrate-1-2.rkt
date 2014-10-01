#lang racket

(require db
         "common.rkt"
         "../../util/basic-types.rkt"
         (prefix-in system: "system.rkt")
         (prefix-in review: "review.rkt")
         (prefix-in submission: "submission.rkt"))

;; This migration adds a feedback-viewed-time-stamp to the review table and updates the system number to 2

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
  (let ((version (get-current-version)))
  (cond [(>= version 2) (Success (format "Database is on version ~a." (system:select-version)))]
        [else (Failure "The system has not migrated to version 2.")])))

(define (get-current-version)
  (cond [(not (system-table-exists?)) 0]
        [else (system:select-version)]))

(define (do-migrate)
  (let* ((conn (connect))
         (handler (lambda (exn) (Failure (format "An exception was raised: ~a." (raise exn)))))
         (result (with-handlers ([exn? handler]) (make-changes conn))))
    (disconnect conn)
    result))

(define (make-changes conn)
  (do-review-table)
  (do-update-version-number)
  (Success (void)))


  
(provide system-table-exists?)
(define (system-table-exists?)
  (let* ((q (merge "SELECT COUNT(*)"
                   "FROM INFORMATION_SCHEMA.TABLES"
                   "WHERE TABLE_NAME=?"))
         (result (run query-value q system:table)))
    (= 1 result)))

(define (do-review-table)
  (let ((q (merge "ALTER TABLE" review:table
                  "ADD" review:feedback-viewed-time-stamp review:feedback-viewed-time-stamp-type-2)))
    (run query-exec q)))

(define (do-update-version-number)
  (let ((q (merge "UPDATE" system:table
                  "SET" system:version "=2")))
    (run query-exec q)))

