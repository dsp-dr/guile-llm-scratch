#!/usr/bin/env guile
!#

;;; test-optimizers.scm --- Test the enhanced optimizer implementations

(add-to-load-path "src")
(use-modules (llm pretraining))

;;; Test data
(define test-parameters '((1.0 2.0 3.0) (4.0 5.0)))
(define test-gradients '((0.1 0.2 0.3) (0.4 0.5)))

;;; Test Adam optimizer
(define (test-adam)
  (format #t "Testing Adam optimizer...~%")
  (let ((optimizer (create-adam-optimizer #:learning-rate 0.01)))
    (format #t "  Initial parameters: ~a~%" test-parameters)
    (let ((updated-params (adam-update! optimizer test-parameters test-gradients)))
      (format #t "  Updated parameters: ~a~%" updated-params)
      #t)))

;;; Test SGD optimizer
(define (test-sgd)
  (format #t "Testing SGD optimizer...~%")
  (let ((optimizer (create-sgd-optimizer #:learning-rate 0.01 #:momentum 0.9)))
    (format #t "  Initial parameters: ~a~%" test-parameters)
    (let ((updated-params (sgd-update! optimizer test-parameters test-gradients)))
      (format #t "  Updated parameters: ~a~%" updated-params)
      #t)))

;;; Test RMSprop optimizer
(define (test-rmsprop)
  (format #t "Testing RMSprop optimizer...~%")
  (let ((optimizer (create-rmsprop-optimizer #:learning-rate 0.01 #:alpha 0.99)))
    (format #t "  Initial parameters: ~a~%" test-parameters)
    (let ((updated-params (rmsprop-update! optimizer test-parameters test-gradients)))
      (format #t "  Updated parameters: ~a~%" updated-params)
      #t)))

;;; Test generic optimizer interface
(define (test-generic-interface)
  (format #t "Testing generic optimizer interface...~%")
  (let ((adam-opt (create-adam-optimizer #:learning-rate 0.01))
        (sgd-opt (create-sgd-optimizer #:learning-rate 0.01))
        (rmsprop-opt (create-rmsprop-optimizer #:learning-rate 0.01)))
    (format #t "  Testing Adam via generic interface...~%")
    (optimizer-update! adam-opt test-parameters test-gradients)
    (format #t "  Testing SGD via generic interface...~%")
    (optimizer-update! sgd-opt test-parameters test-gradients)
    (format #t "  Testing RMSprop via generic interface...~%")
    (optimizer-update! rmsprop-opt test-parameters test-gradients)
    #t))

;;; Test optimizer creation from config
(define (test-config-creation)
  (format #t "Testing optimizer creation from config...~%")
  
  ;; Test Adam config
  (let ((adam-config '((optimizer . adam)
                       (learning-rate . 0.001)
                       (optimizer-config . ((beta1 . 0.9) (beta2 . 0.999))))))
    (let ((adam-opt (create-optimizer-from-config adam-config)))
      (format #t "  Created Adam from config: ~a~%" (adam-optimizer? adam-opt))))
  
  ;; Test SGD config
  (let ((sgd-config '((optimizer . sgd)
                      (learning-rate . 0.01)
                      (optimizer-config . ((momentum . 0.9) (weight-decay . 0.0001))))))
    (let ((sgd-opt (create-optimizer-from-config sgd-config)))
      (format #t "  Created SGD from config: ~a~%" (sgd-optimizer? sgd-opt))))
  
  ;; Test RMSprop config
  (let ((rmsprop-config '((optimizer . rmsprop)
                          (learning-rate . 0.01)
                          (optimizer-config . ((alpha . 0.99) (epsilon . 1e-8))))))
    (let ((rmsprop-opt (create-optimizer-from-config rmsprop-config)))
      (format #t "  Created RMSprop from config: ~a~%" (rmsprop-optimizer? rmsprop-opt))))
  
  #t)

;;; Run all tests
(define (run-tests)
  (format #t "=== Testing Enhanced Optimizer Implementation ===~%~%")
  
  (test-adam)
  (newline)
  
  (test-sgd)
  (newline)
  
  (test-rmsprop)
  (newline)
  
  (test-generic-interface)
  (newline)
  
  (test-config-creation)
  (newline)
  
  (format #t "=== All tests completed successfully! ===~%"))

;;; Run the tests if this file is executed directly
(when (string=? (car (command-line)) "test-optimizers.scm")
  (run-tests))