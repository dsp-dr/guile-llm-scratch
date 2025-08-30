#!/usr/bin/env guile
!#

(use-modules (llm fundamentals))

;; Test tokenize function
(display "Testing tokenize:\n")
(display "Input: 'Hello, world!'\n")
(display "Output: ")
(display (tokenize "Hello, world!"))
(newline)

;; Test BPE
(display "\nTesting BPE tokenize:\n")
(display "Input: 'abc' with 1 merge\n")
(display "Output: ")
(display (bpe-tokenize "abc" 1))
(newline)