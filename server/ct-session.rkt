#lang racket

;; Captain Teach Session information
(provide ct-session ct-session-class ct-session-uid ct-session?)
(struct ct-session (class uid) #:transparent)
