#lang racket

(require "ct-session.rkt"
         web-server/http/bindings)
  
(provide headers->session)  
(define (headers->session headers)
  (if (exists-binding? 'uid headers)
      (ct-session "cmpsci220" (extract-binding/single 'OIDC_CLAIM_email headers))
      'invalid-session))

(provide req->session)
(define (req->session req)
  (headers->session (request-headers req)))
