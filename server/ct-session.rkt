#lang racket

;; Captain Teach Session information
(provide (struct-out ct-session))
(struct ct-session (class uid table) #:transparent)
