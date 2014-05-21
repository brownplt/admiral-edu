#lang racket

(require "../ct-session.rkt"
         web-server/http/bindings)
  
(provide headers->session)  
(define (headers->session headers)
  (if (exists-binding? 'oidc_claim_email headers)
      (ct-session "cmpsci220" (extract-binding/single 'oidc_claim_email headers))
      'invalid-session))

(provide req->session)
(define (req->session req)
  (headers->session (request-headers req)))
