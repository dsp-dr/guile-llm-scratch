#!/usr/bin/env guile
!#

(add-to-load-path "src")

;; Try to compile the module
(use-modules (system base compile))

(display "Checking syntax of src/llm/pretrain.scm...\n")

(catch #t
  (lambda ()
    (compile-file "src/llm/pretrain.scm" #:to 'tree-il)
    (display "Syntax check passed!\n"))
  (lambda (key . args)
    (display "Syntax error found:\n")
    (display key) (newline)
    (for-each (lambda (arg) (display arg) (newline)) args)))