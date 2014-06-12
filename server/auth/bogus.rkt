#lang racket

(require web-server/http/bindings)
  
(provide headers->uid)  
(define (headers->uid headers)
  (if (exists-binding? 'oidc_claim_email headers)
      (extract-binding/single 'oidc_claim_email headers)
      'invalid-session))

(provide req->uid)
(define (req->uid req)
  (headers->uid (request-headers req)))
