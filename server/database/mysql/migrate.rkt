#lang racket

(require "../../util/basic-types.rkt"
         (prefix-in system: "system.rkt")
         (prefix-in v1: "migrate-0-1.rkt"))


;; ( -> Result void?)
;; Success if at the current 
(provide check-migrated)
(define (check-migrated)
  (when (Failure? (v1:check-migrated)) (v1:migrate))
  (let ((version (system:select-version)))
    (cond
      [(not (= version system:current-version)) (Failure (format "Expected system to be at version ~a but was at version ~a." version))]
      [else (Success (void))])))