#lang typed/racket

(require "../../util/basic-types.rkt"
         (prefix-in system: "system.rkt"))

(require/typed (prefix-in v1: "migrate-0-1.rkt")
               [v1:check-migrated (-> (Result String))]
               [v1:migrate (-> (Result String))])

(require/typed (prefix-in v2: "migrate-1-2.rkt")
               [v2:check-migrated (-> (Result String))]
               [v2:migrate (-> (Result Void))])

;; ( -> Result void?)
;; Success if at the current 
(provide check-migrated)
(: check-migrated (-> (Result Void)))
(define (check-migrated)
  (when (Failure? (v1:check-migrated)) (v1:migrate))
  (when (Failure? (v2:check-migrated)) (v2:migrate))
  (let ((version (system:select-version)))
    (cond
      [(not (= version system:current-version)) (Failure (format "Expected system to be at version ~a but was at version ~a." version))]
      [else (Success (void))])))