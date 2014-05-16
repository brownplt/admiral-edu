#lang racket

(require json net/url) 

;; A Simple Service to faciliate the CT service

;; This was built using the "More: Systems Programming with Racket"
;; tutorail by Matthew Flatt: http://docs.racket-lang.org/more/index.html


;; Starts a service on the specified host. Returns a function
;; to stop the service and free the port.
(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
     (define listener (tcp-listen port-no 5 #t))
     (define (loop)
       (accept-and-handle listener)
       (loop))
     (thread loop))
  (lambda ()
    (custodian-shutdown-all main-cust)))

;; Accepts an incoming request and spawns a new thread to handle it.
;; If no data comes in for ~10 seconds, the thread is killed.
(define (accept-and-handle listener)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread (lambda ()
	      (handle in out)
	      (close-input-port in)
	      (close-output-port out))))
  ; Watcher thread:
  (thread (lambda ()
	    (sleep 10)
	    (custodian-shutdown-all cust))))

;; Handles an incoming POST request where the
;; body is a JSON record.
(define (handle in out)
  (define req
    ;; Match the first line to extract the request:
    (regexp-match #rx"^POST (.+) HTTP/[0-9]+\\.[0-9]+"
                  (read-line in)))
  (when req
    ;; Discard the rest of the header (up to blank line):
    (regexp-match #rx"(\r\n|^)\r\n" in)
    ;; Read the body of the message as json
    (define body (read-json in))
    (let ([response (dispatch (list-ref req 1) body)])
      ;; Send reply:
      (display "HTTP/1.0 200 Okay\r\n" out)
      (display "Server: k\r\nContent-Type: application/json\r\n\r\n" out)
      ;; Responds as JSON
      (display (jsexpr->string response) out))))

;; Looks up the specified path and dispatches to it
(define (dispatch str-path body)
  ; Parse the request as a URL:
  (define url (string->url str-path))
  ; Extract the path part:
  (define path (foldr
		(lambda (s0 s1)
		  (string-append s0 "/" s1))
		""
		(map path/param-path (url-path url))))
  ; Find a handler based on the path's first element:
  (define h (hash-ref dispatch-table path #f))
  (if h
     ; Call a handler:
      (h body)
      ; No handler found:
      (hasheq `error 
	      (hasheq `message "Unknown page."
		      `page str-path))))

(define dispatch-table (make-hash))

;; Classes API

(hash-set! dispatch-table "class/list/"
	   (lambda (body)
	     (hasheq `result
		     (hasheq `message "Not yet implemented"))))

(hash-set! dispatch-table "class/new/"
	   (lambda (body)
	     (hasheq `result
		     (hasheq `message "Not yet implemented"))))

(hash-set! dispatch-table "class/delete/"
	   (lambda (body)
	     (hasheq `result
		     (hasheq `message "Not yet implemented"))))

(hash-set! dispatch-table "class/students/"
	   (lambda (body)
	     (hasheq `result
		     (hasheq `message "Not yet implemented"))))

;; Students API

(hash-set! dispatch-table "student/new/"
	   (lambda (body)
	     (hasheq `result
		     (hasheq `message "Not yet implemented"))))

(hash-set! dispatch-table "student/register/"
	   (lambda (body)
	     (hasheq `result
		     (hasheq `message "Not yet implemented"))))

(hash-set! dispatch-table "student/classes/"
	   (lambda (body)
	     (hasheq `result
		     (hasheq `message "Not yet implemented"))))


;; Assignments API

(hash-set! dispatch-table "assignment/new/"
	   (lambda (body)
	     (hasheq `result
		     (hasheq `message "Not yet implemented"))))

(hash-set! dispatch-table "assignment/list/"
	   (lambda (body)
	     (hasheq `result
		     (hasheq `message "Not yet implemented"))))

(hash-set! dispatch-table "assignment/delete/"
	   (lambda (body)
	     (hasheq `result
		     (hasheq `message "Not yet implemented"))))

(hash-set! dispatch-table "assignment/submit/"
	   (lambda (body)
	     (hasheq `result
		     (hasheq `message "Not yet implemented"))))

(hash-set! dispatch-table "assignment/status/"
	   (lambda (body)
	     (hasheq `result
		     (hasheq `message "Not yet implemented"))))


;; Reviews API

(hash-set! dispatch-table "review/"
	   (lambda (body)
	     (hasheq `result
		     (hasheq `message "Not yet implemented"))))

(hash-set! dispatch-table "review/status/"
	   (lambda (body)
	     (hasheq `result
		     (hasheq `message "Not yet implemented"))))

(hash-set! dispatch-table "review/submit"
	   (lambda (body)
	     (hasheq `result
		     (hasheq `message "Not yet implemented"))))
