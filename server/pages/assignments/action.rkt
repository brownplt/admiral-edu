#lang racket

(require xml)
(require "../../base.rkt")

(define base-url (string-append "/" class-name "/assignments/"))

(provide assignments)
(define (assignments context)
  (action-item base-url context))

(provide OPEN)
(define OPEN "open")

(provide open)
(define (open assignment-id context)
  (action-item (string-append base-url OPEN "/" assignment-id "/") context))

(provide CLOSE)
(define CLOSE "close")

(provide close)
(define (close assignment-id context)
  (action-item (string-append base-url CLOSE "/" assignment-id "/") context))

(provide DELETE)
(define DELETE "delete")

(provide delete)
(define (delete assignment-id context)
  (action-item (string-append base-url DELETE "/" assignment-id "/") context))

(provide LIST)
(define LIST "")

(provide DASHBOARD)
(define DASHBOARD "dashboard")

(provide dashboard)
(define (dashboard assignment-id context)
  (action-item (string-append base-url DASHBOARD "/" assignment-id "/") assignment-id))

(provide dependencies)
(define (dependencies assignment-id context)
  (action-item (string-append "/" class-name "/dependencies/" assignment-id "/") context))

(provide edit)
(define (edit assignment-id context)
  (action-item (string-append "/" class-name "/author/edit/" assignment-id "/") context))

(provide export)
(define (export assignment-id context)
  (action-item (string-append "/" class-name "/export/" assignment-id "/" assignment-id ".zip") context))

(define (action-item url context)
  `(a ((href ,url)) ,context))