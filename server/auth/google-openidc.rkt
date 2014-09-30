#lang typed/racket

(require/typed web-server/http/bindings
               [extract-binding/single (Symbol (Listof (Pairof Symbol (U String Bytes))) -> (U String Bytes))]
               [exists-binding? (Symbol (Listof (Pairof Symbol (U String Bytes))) -> Boolean)]
               [request-headers (Any -> (Listof (Pairof Symbol (U String Bytes))))])
  
(provide headers->uid)  
(: headers->uid ((Listof (Pairof Symbol (U String Bytes))) -> (U String Bytes 'invalid-session)))
(define (headers->uid headers)
  (if (exists-binding? 'oidc_claim_email headers)
      (extract-binding/single 'oidc_claim_email headers)
      'invalid-session))

(provide req->uid)
(define (req->uid req)
  (headers->uid (request-headers req)))
