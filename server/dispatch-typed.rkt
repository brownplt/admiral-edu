#lang typed/racket

(require/typed web-server/servlet
               ;              (number? bytes? number? (or false bytes?) (listof header?) (listof bytes) -> response?
               [response/full (Number Bytes Number (U False Bytes) (Listof Any) (Listof Bytes) -> response)]
               ;                      -> response?
               [response/xexpr (XExpr -> response)]
               [#:struct response ([code : Number]
                                   [message : Bytes]
                                   [seconds : Number]
                                   [mime : (U False Bytes)]
                                   [headers : (Listof Any)]
                                   [output : Any])]
               [TEXT/HTML-MIME-TYPE Bytes])

(require/typed (prefix-in error: "pages/errors.rkt")
               [error:error-not-registered (ct-session -> XExpr)]
               [error:four-oh-four (-> response)])

(require/typed xml
               [xexpr->string (XExpr -> String)])

(require/typed "untyped-dispatch.rkt"
               [get-response (String String -> (Listof Bytes))])

(require "pages/typed-xml.rkt"
         "base.rkt"
         "ct-session.rkt")

(require (prefix-in assignments: "pages/assignments.rkt"))

(: user-exists? (ct-session -> Boolean))
(define (user-exists? session)
  (let* ((class (ct-session-class session))
         (uid (ct-session-uid session))
         (result (role:exists? class uid)))
    result))

(: render-xexpr (String ct-session (ct-session (Listof String) Boolean -> (Listof (U XExpr Void))) (Listof String) Boolean -> response))
(define (render-xexpr title session page url post)
  (let ((valid-user (user-exists? session)))
    (if (not valid-user)
        (response/xexpr (error:error-not-registered session))
        (let* ((xexprs (page session url post))
               (strings (map xexpr->string (cast (filter (compose not void?) xexprs) (Listof XExpr))))
               (body (string-join strings "\n"))
               (response (get-response title body)))
        (response/full
         200 #"Okay"
         (current-seconds) TEXT/HTML-MIME-TYPE
         empty
         response)))))

(provide handlerPrime)
(: handlerPrime (Boolean Any ct-session Any Any (Listof String) -> response))
(define (handlerPrime post post-data session bindings raw-bindings path)
  (match path
    [(cons "assignments" url) (render-xexpr "Assignments" session assignments:load url post)]
    [else (error:four-oh-four)]))