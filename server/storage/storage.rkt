#lang racket

(require "../configuration.rkt"
         "cloud-storage-unit.rkt"
         "local-storage-unit.rkt"
         "../database/mysql.rkt"
         (prefix-in local: "local-storage.rkt"))

(provide retrieve-file)
(define retrieve-file 'nil)
(define (set-retrieve-file proc) (set! retrieve-file proc))

(provide write-file)
(define write-file 'nil)
(define (set-write-file proc) (set! write-file proc))

(define delete-path 'nil)
(define (set-delete-path proc) (set! delete-path proc))

(define path-info 'nil)
(define (set-path-info proc) (set! path-info proc))

(provide list-files)
(define list-files 'nil)
(define (set-list-files proc) (set! list-files proc))

(define list-sub-files 'nil)
(define (set-list-sub-files proc) (set! list-sub-files proc))

(provide list-dirs)
(define list-dirs 'nil)
(define (set-list-dirs proc) (set! list-dirs proc))

; Loads cloud-storage bindings
(define (invoke-cloud-storage)
  (invoke-unit cloud-storage@)
  (define-values/invoke-unit/infer cloud-storage@)
  (set-retrieve-file retrieve-file)
  (set-write-file write-file)
  (set-delete-path delete-path)
  (set-path-info path-info)
  (set-list-files list-files)
  (set-list-sub-files list-sub-files)
  (set-list-dirs list-dirs))

; Loads local storage bindings
(define (invoke-local-storage)
  (invoke-unit local-storage@)
  (define-values/invoke-unit/infer local-storage@)
  (set-retrieve-file retrieve-file)
  (set-write-file write-file)
  (set-delete-path delete-path)
  (set-path-info path-info)
  (set-list-files list-files)
  (set-list-sub-files list-sub-files)
  (set-list-dirs list-dirs))

(cond
  [(string=? storage-mode "cloud-storage") (invoke-cloud-storage)]
  [(string=? storage-mode "local") (invoke-local-storage)]
  [else (error (format "Could not load storage mode: ~a" storage-mode))])

;; Ensures the required functions are set
(when (not (procedure? retrieve-file)) (error "retrieve-file not set."))
(when (not (procedure? write-file)) (error "write-file not set."))
(when (not (procedure? delete-path)) (error "delete-path not set."))
(when (not (procedure? path-info)) (error "path-info not set."))
(when (not (procedure? list-files)) (error "list-files not set."))
(when (not (procedure? list-sub-files)) (error "list-sub-files not set."))
(when (not (procedure? list-dirs)) (error "list-dirs not set."))

;; Utility functions
(define (remove-leading-slash p)
  (cond [(string=? "" p) ""]
        [else (begin
                (let* ((fc (string-ref p 0))
                       (pp (if (eq? #\/ fc) (substring p 1) p)))
                  pp))]))

;; Creates a directory tmp/some-hash
(define (get-local-temp-directory)
  (string-append "tmp/" (random-hash) "/"))

;; Creates a random 32 hash
(define (random-hash)
  (for/fold ([s ""])
      ([x (in-range 32)])
    (string-append s
                   (number->string (truncate (random 15)) 16))))

;; Checks if a file-name is acceptable
;; Acceptable filenames include alphanumeric characters, underscore, dash, and period
;; that is, they must meet the regular expression #rx[a-zA-Z0-9_.-]*
;; Returns #t if the file-name is acceptable and #f otherwise.
(provide check-file-name)
(define (check-file-name file-name)
  (let* ((okay-chars #rx"[a-zA-Z0-9_.-]*"))
    (regexp-match-exact? okay-chars file-name)))

;; TODO: Eventually we want to detect the archive type and choose the correct program to extract it.
;; This also needs to be able to handle invalid file types
(provide unarchive)
(define (unarchive path file-name)
  (system (string-append "unzip " file-name " -d " path)))

(define (is-zip? file)
  (let* ((clean (string-trim file))
         (split (string-split clean "."))
         (ext (last split)))
    (equal? "zip" (string-downcase ext))))

(provide is-file?)
(define (is-file? path)
  (eq? 'file (path-info path)))

(provide is-directory?)
(define (is-directory? path)
  (eq? 'directory (path-info path)))

;; Start function definitions


;; class-id -> assignment-id -> step-id -> review-id -> string
;; The path to the default-rubric for the class, assignment, step-id, and review-id
(define (default-rubric-path class assignment step-id review-id)
  (string-append class "/" assignment "/reviews/" step-id "/" review-id "/rubric.json"))

;; class-id -> assignment-id -> step-id -> review-id -> string
;; Given the class, assignment, step-id, review-id, returns a string of the default rubric.
(provide retrieve-default-rubric)
(define (retrieve-default-rubric class assignment step-id review-id)
  (retrieve-file (default-rubric-path class assignment step-id review-id)))

;; class-id -> assignment-id -> step-id -> string -> review-id -> void
;; Given the class, assignment, step, rubric, and review-id, saves the default rubric.
(provide create-default-rubric)
(define (create-default-rubric class assignment step-id rubric review-id)
  (let ((path (default-rubric-path class assignment step-id review-id)))
    (write-file path rubric)))

(define (assignment-description-path class-id assignment-id)
  (string-append class-id "/" assignment-id "/description.yaml"))

;; class-id -> assignment-id -> string -> void
;; Given the class, assignment, and description, writes the description for the assignment.
(provide save-assignment-description)
(define (save-assignment-description class assignment description)
  (let ((path (assignment-description-path class assignment)))
    (write-file path description)))

;; class-id -> assignment-id -> string
;; Given the class and assignment, retrieves the stored assignment description
(provide retrieve-assignment-description)
(define (retrieve-assignment-description class assignment)
    (retrieve-file (assignment-description-path class assignment)))

;; class-id -> assignment-id -> step-id -> review-id -> reviewer-id -> reviewee-id -> string
;; Returns the path to the specified rubric
(define (rubric-path class-id assignment-id step-id review-id reviewer reviewee)
  (string-append class-id "/" assignment-id "/reviews/" step-id "/" review-id "/" reviewer "/" reviewee "/rubric.json"))

;; class-id -> assignment-id -> step-id -> review-id -> reviewer -> reviewee -> string -> void
(provide save-rubric)
(define (save-rubric class assignment step-id review-id reviewer reviewee data)
  (let ((path (rubric-path class assignment step-id review-id reviewer reviewee)))
    (write-file path data)))

(provide retrieve-rubric)
(define (retrieve-rubric class-id assignment-id step-id review-id reviewer-id reviewee-id)
  (let ((path (rubric-path class-id assignment-id step-id review-id reviewer-id reviewee-id)))
    ; If no rubric is found, copy the default-rubric
    (when (not (eq? 'file (path-info path))) 
      (begin
        (let ((default-rubric (retrieve-default-rubric class-id assignment-id step-id review-id)))
          (write-file path default-rubric))))
    
    (retrieve-file (rubric-path class-id assignment-id step-id review-id reviewer-id reviewee-id))))

;; class-id -> assignment-id -> step-id -> review-id -> reviewer-id -> reviewee-id -> file-path -> string
(define (review-comments-path class-id assignment-id step-id review-id reviewer-id reviewee-id file-path)
  (string-append class-id "/" assignment-id "/reviews/" step-id "/" review-id "/" reviewer-id "/" reviewee-id "/" (remove-leading-slash file-path) ".comments.json"))

;; class-id -> assignment-id -> step-id -> review-id -> reviewer-id -> reviewee-id -> file-path -> string -> void
(provide save-review-comments)
(define (save-review-comments class assignment step-id review-id reviewer reviewee file-path data)
  (let ((path (review-comments-path class assignment step-id review-id reviewer reviewee file-path)))
    (write-file path data)))

(provide load-review-comments)
(define (load-review-comments class-id assignment-id step-id review-id reviewer-id reviewee-id file-path)
  (let ((path (review-comments-path class-id assignment-id step-id review-id reviewer-id reviewee-id file-path)))
    (cond [(is-file? path) (retrieve-file path)]
          [else "{\"comments\" : {}}"])))

;; class-id -> 
(define (submission-path class-id assignment-id user-id step-id)
  (string-append class-id "/" assignment-id "/" user-id "/" step-id "/"))

;; class-id -> assignment-id -> user-id -> step-id -> file-name -> string
(provide submission-file-path)
(define (submission-file-path class-id assignment-id user-id step-id file-name)
  (string-append (submission-path class-id assignment-id user-id step-id) file-name))

; Uploads a dependency solution. If necessary, deletes the previous dependency that was uploaded 
(provide upload-dependency-solution)
(define (upload-dependency-solution class-id user-id assignment-id step-id file-name data)
  (let ((path (submission-path class-id assignment-id user-id step-id)))
    ;; Delete previously uploaded files
    (delete-path path)
    
    ;; Write to storage
    (cond [(is-zip? file-name) (do-unarchive-solution class-id user-id assignment-id step-id file-name data)]
          [else (do-single-file-solution class-id user-id assignment-id step-id file-name data)])
    
    ;; If necessary, create database entry
    (when (not (submission:exists? assignment-id class-id step-id user-id)) (submission:create-instructor-solution assignment-id class-id step-id user-id))))


; Uploads a student submission.
(provide upload-submission)
(define (upload-submission class-id user-id assignment-id step-id file-name data)
  
  ;; Write to storage
  (cond [(is-zip? file-name) (do-unarchive-solution class-id user-id assignment-id step-id file-name data)]
        [else (do-single-file-solution class-id user-id assignment-id step-id file-name data)])
  
  ;; Create database entry
  (submission:create assignment-id class-id step-id user-id))

(define (do-unarchive-solution class-id user-id assignment-id step-id file-name data)
  (let* ((temp-dir (get-local-temp-directory))
         (file-path (string-append temp-dir file-name))
         (path (submission-path class-id assignment-id user-id step-id)))
    
    ;; Write the file out
    (local:write-file file-path data)
    
    ;; Unzip it
    (unarchive temp-dir file-path)
    
    ;; Remove the archive file
    (system (string-append "rm \"" file-path "\""))
    
    ;; Copy files to storage
    (let* ((file-names (local:list-sub-files temp-dir))
           (len (string-length temp-dir))
           (submission-file-name (lambda (file-name) (string-append path (substring file-name len))))
           (handle-file (lambda (file-name) 
                          (let* ((s-name (submission-file-name file-name))
                                 (data (file->string file-name)))
                            (write-file s-name data)))))
      (map handle-file file-names))
    
    ;; Remove the temp directory
    (system (string-append "rm \"" temp-dir "\" -rf"))))
      
(define (do-single-file-solution class-id user-id assignment-id step-id file-name data)
  (let ((s-path (submission-file-path class-id assignment-id user-id step-id file-name)))
    (write-file s-path data)))

(define (assignment-path class-id assignment-id)
  (string-append class-id "/" assignment-id "/"))

;; class-id -> assignment-id -> bytes?
;; Zips an assignment directory and returns the bytes
(provide export-assignment)
(define (export-assignment class-id assignment-id)
  (let* ((path (assignment-path class-id assignment-id))
         (temp-dir (get-local-temp-directory))
         (archive-name (string-append assignment-id ".zip"))
         (files (list-sub-files path))
         (store-temp-file (lambda (file-name)
                            (let ((data (retrieve-file file-name)))
                              (local:write-file (string-append temp-dir file-name) data)))))

    ;; Store all files locally
    (map store-temp-file files)
    
    ;; Archive assignment
    (system (string-append "cd " temp-dir "; zip -r " archive-name " "  path))
    
    ;; Convert archive to bytes, delete temporary directory, and return archive's bytes
    (let ((data (file->bytes (string-append temp-dir "/" archive-name))))
      (system (string-append "rm " temp-dir " -rf"))
      data)))

(define (get-review-path review)
  (let* ((class (review:record-class-id review))
         (assignment (review:record-assignment-id review))
         (step (review:record-step-id review))
         (review-id (review:record-review-id review))
         (reviewer (review:record-reviewer-id review))
         (reviewee (review:record-reviewee-id review)))
    (string-append class "/" assignment "/reviews/" step "/" review-id "/" reviewer "/" reviewee "/")))

(define (review-feedback-path review)
  (string-append (get-review-path review) "feedback.txt"))

;; review -> feedback-string -> void
;; Saves the feedback for the specified review
(provide save-review-feedback)
(define (save-review-feedback review feedback)
  (let* ((path (review-feedback-path review)))
    (write-file path feedback)))

;; review -> string
;; Loads the feedback for the specified review
(provide load-review-feedback)
(define (load-review-feedback review)
  (let* ((path (review-feedback-path review)))
    (cond [(is-file? path) (retrieve-file path)]
          [else ""])))


