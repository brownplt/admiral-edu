#lang racket

(provide lines)
(define (lines data)
  (string-split data "\n"))