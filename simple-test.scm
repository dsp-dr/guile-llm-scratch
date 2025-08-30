#!/usr/bin/env guile
!#

(add-to-load-path "src")
(load "src/llm/pretrain.scm")

;;; Test data
(define test-parameters '((1.0 2.0 3.0) (4.0 5.0)))
(define test-gradients '((0.1 0.2 0.3) (0.4 0.5)))

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

(let* ((sgd-config '((optimizer . sgd)
                     (learning-rate . 0.01)
                     (optimizer-config . ((momentum . 0.9)))))
       (sgd-opt (create-optimizer-from-config sgd-config)))
  (display "  SGD from config: ")
  (display (sgd-optimizer? sgd-opt)) (newline))

(let* ((rmsprop-config '((optimizer . rmsprop)
                         (learning-rate . 0.01)
                         (optimizer-config . ((alpha . 0.99)))))
       (rmsprop-opt (create-optimizer-from-config rmsprop-config)))
  (display "  RMSprop from config: ")
  (display (rmsprop-optimizer? rmsprop-opt)) (newline))

(display "\n=== All tests completed successfully! ===\n")