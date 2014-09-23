#lang typed/racket

(require "../../base.rkt"
         "../typed-xml.rkt")

(define base-url (string-append "/" class-name "/assignments/"))

(: action-item (String XExpr -> XExpr))
(define (action-item url context)
  `(a ((href ,url)) ,context))


;; Links to assignments page
(provide assignments)
(: assignments (String -> XExpr))
(define (assignments context)
  (action-item base-url context))

(provide OPEN)
(define OPEN "open")

(provide open)
(: open (String XExpr -> XExpr))
(define (open assignment-id context)
  (action-item (string-append base-url OPEN "/" assignment-id "/") context))

(provide CLOSE)
(define CLOSE "close")

(provide close)
(: close (String XExpr -> XExpr))
(define (close assignment-id context)
  (action-item (string-append base-url CLOSE "/" assignment-id "/") context))

(provide DELETE)
(define DELETE "delete")

(provide delete)
(: delete (String XExpr -> XExpr))
(define (delete assignment-id context)
  (action-item (string-append base-url DELETE "/" assignment-id "/") context))

(provide LIST)
(define LIST "")

(provide DASHBOARD)
(define DASHBOARD "dashboard")

(provide dashboard)
(: dashboard (String XExpr -> XExpr))
(define (dashboard assignment-id context)
  (action-item (string-append base-url DASHBOARD "/" assignment-id "/") assignment-id))

(provide dependencies)
(: dependencies (String XExpr -> XExpr))
(define (dependencies assignment-id context)
  (action-item (string-append "/" class-name "/dependencies/" assignment-id "/") context))

(provide edit)
(: edit (String XExpr -> XExpr))
(define (edit assignment-id context)
  (action-item (string-append "/" class-name "/author/edit/" assignment-id "/") context))

(provide export)
(: export (String XExpr -> XExpr))
(define (export assignment-id context)
  (action-item (string-append "/" class-name "/export/" assignment-id "/" assignment-id ".zip") context))


(provide STATUS)
(define STATUS "status")

(provide status)
(: status (String XExpr -> XExpr))
(define (status assignment-id context)
  (action-item (string-append base-url "status/" assignment-id "/") context))

(provide step-status)
(: step-status (String String XExpr -> XExpr))
(define (step-status assignment-id step-id context)
  (action-item (string-append base-url "status/" assignment-id "/" step-id "/") context))

(provide review-status)
(: review-status (String String String XExpr -> XExpr))
(define (review-status assignment-id step-id review-id context)
  (action-item (string-append base-url "status/" assignment-id "/" step-id "/" review-id "/") context))