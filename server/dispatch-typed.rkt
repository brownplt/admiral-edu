#lang typed/racket

(require/typed web-server/servlet
               ;              (number? bytes? number? (or false bytes?) (listof header?) (listof bytes) -> response?
               [response/full (Number Bytes Number (U False Bytes) (Listof Any) (Listof Bytes) -> Any)]
               ;                      -> response?
               [response/xexpr (XExpr -> Any)]
               [TEXT/HTML-MIME-TYPE Bytes])

(require/typed (prefix-in error: "pages/errors.rkt")
               [error:error-not-registered (ct-session -> XExpr)])

(require "pages/typed-xml.rkt"
         "base.rkt"
         "ct-session.rkt")

(: user-exists? (ct-session -> Boolean))
(define (user-exists? session)
  (let* ((class (ct-session-class session))
         (uid (ct-session-uid session))
         (result (role:exists? class uid)))
    result))

(: render-xexpr (ct-session (ct-session (Listof String) -> (Listof (U XExpr Void))) (Listof String) -> Any))
(define (render-xexpr session page url)
  (let ((valid-user (user-exists? session)))
    (if (not valid-user)
        (response/xexpr (error:error-not-registered session))
        (let* ((xexprs (page session url))
               (strings (map xexpr->string (cast (filter (compose not void?) xexprs) (Listof String))))
               (bytes (string->bytes/utf-8 (string-join strings "\n"))))
        (response/full
         200 #"Okay"
         (current-seconds) TEXT/HTML-MIME-TYPE
         empty
         (list bytes))))))