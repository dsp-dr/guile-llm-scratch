#!/usr/bin/env guile
!#

(use-modules (llm fundamentals)
             (srfi srfi-69))

(display "Testing BPE tokenization with debug output\n")
(display "=====================================\n")

;; Test input
(define text "abc")
(define tokens (map string (string->list text)))
(display "Initial tokens: ")
(display tokens)
(newline)

;; Test get-token-pairs
(define pairs (get-token-pairs tokens))
(display "Token pairs: ")
(display pairs)
(newline)

;; Test compute-pair-frequencies
(define freq-table (compute-pair-frequencies tokens))
(display "Frequency table:\n")
(hash-table-walk freq-table
                 (lambda (pair count)
                   (display "  ")
                   (display pair)
                   (display " -> ")
                   (display count)
                   (newline)))

;; Test merge-tokens
(display "\nTesting merge-tokens:\n")
(define test-tokens '("a" "b" "c" "a" "b"))
(display "  Input: ")
(display test-tokens)
(newline)
(define merged (merge-tokens test-tokens '("a" . "b") "ab"))
(display "  After merging (a . b) -> ab: ")
(display merged)
(newline)

;; Full BPE test
(display "\nFull BPE tokenization:\n")
(define result (bpe-tokenize text 1))
(display "  Result: ")
(display result)
(newline)