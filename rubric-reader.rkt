#lang racket

(require json)

(define (rubric->likerts rubric)
  (letrec ((helper (lambda (acc elements)
                     (cond [(empty? elements) acc]
                           [else 
                            (let ((head (first elements)))
                              (cond [(string=? "LikertElement" (hash-ref head 'class)) (helper (cons head acc) (rest elements))]
                                    [else (helper acc (rest elements))]))]))))
    (helper '() (hash-ref rubric 'rubric))))
                   
  
(define (file->rubric file-name)
  (string->jsexpr (file->string file-name)))

(define (string-ends-with? str substr)
  (region-matches str (- (string-length str) (string-length substr)) substr
                  0 (string-length substr)))

(define (region-matches str str-start other other-start length)
  (cond 
    [(or (< other-start 0) (< (- (string-length other) other-start) length))
     #f]
    [(or (< str-start 0) (< (- (string-length str) str-start) length))
     #f]
    [else (for/and ([i length])
            (char=? (string-ref str (+ str-start i))
                    (string-ref other (+ other-start i))))]))

(define (do-sub-dir path)
  (map (lambda (p) (path->complete-path p path)) (directory-list path)))

(define (find-all-rubrics)
  (filter (lambda (str) (string-ends-with? str "rubric.json"))
          (map path->string
               (flatten
                (map do-sub-dir 
                     (flatten 
                      (map do-sub-dir 
                           (map path->complete-path 
                                (filter directory-exists? (directory-list))))))))))
                   
(define (build-likert-info)
  (let ((likerts (flatten (map rubric->likerts (map file->rubric (find-all-rubrics))))))
    (letrec ((helper (lambda (acc ls)
                       (cond [(empty? ls) acc]
                             [else 
                              (let* ((head (first ls))
                                     (id (hash-ref head 'id))
                                     (selected (+ (hash-ref head 'selected) 1))
                                     (rangeSize (hash-ref head 'rangeSize)))
                                (cond [(hash-has-key? acc id) (inc acc id selected (rest ls))]
                                      [else (create acc id selected rangeSize (rest ls))]))])))
             (inc (lambda (acc id selected rest)
                    (let* ((vec (hash-ref acc id))
                           (current (vector-ref vec selected))
                           (vec-prime (vector-set! vec selected (+ current 1))))
                      (helper acc rest))))
             (create (lambda (acc id selected rangeSize rest)
                       (let ((acc-prime (hash-set acc id (make-vector (+ rangeSize 1) 0))))
                         (inc acc-prime id selected rest)))))
      (helper (hash) likerts))))

(build-likert-info)