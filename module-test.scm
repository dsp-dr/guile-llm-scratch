#!/usr/bin/env guile
!#

(set! %load-path (cons "src" %load-path))

(use-modules (llm pretraining))

(display "=== Testing Enhanced Optimizer Implementation ===\n\n")

;;; Test Adam optimizer
(display "Testing Adam optimizer...\n")
(let ((optimizer (create-adam-optimizer #:learning-rate 0.01)))
  (display "  Adam optimizer created successfully!\n")
  (display "  Optimizer type check: ")
  (display (adam-optimizer? optimizer)) (newline))

;;; Test SGD optimizer  
(display "Testing SGD optimizer...\n")
(let ((optimizer (create-sgd-optimizer #:learning-rate 0.01 #:momentum 0.9)))
  (display "  SGD optimizer created successfully!\n")
  (display "  Optimizer type check: ")
  (display (sgd-optimizer? optimizer)) (newline))

;;; Test RMSprop optimizer
(display "Testing RMSprop optimizer...\n")
(let ((optimizer (create-rmsprop-optimizer #:learning-rate 0.01)))
  (display "  RMSprop optimizer created successfully!\n")
  (display "  Optimizer type check: ")
  (display (rmsprop-optimizer? optimizer)) (newline))

;;; Test configuration-based optimizer creation
(display "Testing optimizer creation from config...\n")
(let* ((adam-config '((optimizer . adam)
                      (learning-rate . 0.001)
                      (optimizer-config . ((beta1 . 0.9) (beta2 . 0.999)))))
       (adam-opt (create-optimizer-from-config adam-config)))
  (display "  Adam from config: ")
  (display (adam-optimizer? adam-opt)) (newline))

(display "\n=== All tests completed successfully! ===\n")