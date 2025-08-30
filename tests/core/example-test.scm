;;; example-test.scm --- Tests for example module

(define-module (tests core example-test)
  #:use-module (srfi srfi-64)
  #:use-module (core example))

(test-begin "example-module")

(test-group "Data Structure Tests"
  (test-assert "Create example"
    (example? (make-example '(1 2 3) '())))
  
  (test-equal "Example data accessor"
    '(1 2 3)
    (example-data (make-example '(1 2 3) '())))
  
  (test-equal "Example metadata accessor"
    '((key . value))
    (example-metadata (make-example '() '((key . value))))))

(test-group "Processing Tests"
  (test-equal "Transform numbers"
    '(2 4 6)
    (transform-data '(1 2 3)))
  
  (test-equal "Transform strings"
    '("HELLO" "WORLD")
    (transform-data '("hello" "world")))
  
  (test-equal "Transform symbols"
    '("foo" "bar")
    (transform-data '(foo bar))))

(test-group "Validation Tests"
  (test-error "Empty data validation"
    (validate-data '()))
  
  (test-error "Non-list data validation"
    (validate-data "not a list"))
  
  (test-equal "Valid data passes through"
    '(1 "test" foo)
    (validate-data '(1 "test" foo))))

(test-group "Integration Tests"
  (let ((example (make-example '(1 "hello" foo) '())))
    (test-assert "Process example returns example"
      (example? (process-example example)))
    
    (test-equal "Process example transforms data"
      '(2 "HELLO" "foo")
      (example-data (process-example example)))))

(test-group "Utility Tests"
  (test-equal "Example to alist conversion"
    '((data . (1 2 3)) (metadata . ((key . value))))
    (example->alist (make-example '(1 2 3) '((key . value)))))
  
  (test-assert "Alist to example conversion"
    (example? (alist->example '((data . (1 2 3)) 
                                (metadata . ()))))))

(test-end "example-module")
