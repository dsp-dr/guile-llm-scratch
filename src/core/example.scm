;;; example.scm --- Example module implementation
;;; Commentary:
;;; This module demonstrates the literate programming approach
;;; for implementing book concepts in Guile3.

(define-module (core example)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (make-example
            example?
            example-data
            process-example
            transform-data))

;; Define the example record type
(define-record-type <example>
  (make-example data metadata)
  example?
  (data example-data example-set-data!)
  (metadata example-metadata example-set-metadata!))

(define (create-example data)
  "Create a new example with default metadata."
  (make-example data '()))

(define (example-with-metadata example key value)
  "Return a new example with updated metadata."
  (let ((new-example (make-example 
                      (example-data example)
                      (example-metadata example))))
    (example-set-metadata! 
     new-example 
     (acons key value (example-metadata new-example)))
    new-example))

(define (process-example example)
  "Process an example through the transformation pipeline."
  (match example
    (($ <example> data metadata)
     (let* ((validated (validate-data data))
            (transformed (transform-data validated))
            (result (apply-rules transformed metadata)))
       (make-example result 
                    (acons 'processed #t metadata))))
    (_ (error "Invalid example object"))))

(define (validate-data data)
  "Validate input data according to business rules."
  (cond
   ((null? data) 
    (error "Data cannot be empty"))
   ((not (list? data))
    (error "Data must be a list"))
   (else 
    (filter valid-datum? data))))

(define (valid-datum? datum)
  "Check if a single datum is valid."
  (and (not (null? datum))
       (or (number? datum)
           (string? datum)
           (symbol? datum))))

(define (transform-data data)
  "Apply transformation rules to validated data."
  (map (lambda (item)
         (cond
          ((number? item) (* item 2))
          ((string? item) (string-upcase item))
          ((symbol? item) (symbol->string item))
          (else item)))
       data))

(define (apply-rules data metadata)
  "Apply metadata-driven rules to transformed data."
  (let ((rules (assoc-ref metadata 'rules)))
    (if rules
        (fold (lambda (rule result)
                (apply-single-rule rule result))
              data
              rules)
        data)))

(define (apply-single-rule rule data)
  "Apply a single rule to the data."
  (match rule
    (('filter . pred) (filter pred data))
    (('map . func) (map func data))
    (('sort . comp) (sort data comp))
    (_ data)))

(define memoize
  (lambda (f)
    "Return a memoized version of function f."
    (let ((cache (make-hash-table)))
      (lambda args
        (let ((cached-value (hash-ref cache args)))
          (if cached-value
              cached-value
              (let ((result (apply f args)))
                (hash-set! cache args result)
                result)))))))

;; Memoized version of expensive computation
(define process-example-memoized
  (memoize process-example))

(define (safe-process-example example)
  "Process example with error handling using continuations."
  (call-with-current-continuation
   (lambda (return)
     (with-exception-handler
      (lambda (exn)
        (format #t "Error processing: ~a~%" exn)
        (return #f))
      (lambda ()
        (process-example example))
      #:unwind? #t))))

(define (pp-example example)
  "Pretty print an example object."
  (match example
    (($ <example> data metadata)
     (format #t "Example:~%")
     (format #t "  Data: ~s~%" data)
     (format #t "  Metadata:~%")
     (for-each (lambda (pair)
                 (format #t "    ~a: ~s~%" 
                        (car pair) (cdr pair)))
               metadata))
    (_ (format #t "Not an example object: ~s~%" example))))

(define (example->alist example)
  "Convert an example to an association list."
  (match example
    (($ <example> data metadata)
     `((data . ,data)
       (metadata . ,metadata)))
    (_ '())))

(define (alist->example alist)
  "Convert an association list to an example."
  (let ((data (assoc-ref alist 'data))
        (metadata (assoc-ref alist 'metadata)))
    (if (and data metadata)
        (make-example data metadata)
        (error "Invalid alist for example conversion"))))
