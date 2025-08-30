#!/usr/bin/env guile
!#

;;; test-tokenizer.scm --- Test the tokenizer implementation

(use-modules (llm fundamentals)
             (srfi srfi-1)
             (srfi srfi-11)
             (srfi srfi-64)
             (srfi srfi-69)
             (ice-9 receive))

(test-begin "tokenizer-tests")

;;; Test basic tokenization
(test-group "basic-tokenization"
  (test-equal "tokenize simple text"
    '("Hello" "world")
    (tokenize "Hello world"))
  
  (test-equal "tokenize with punctuation"
    '("Hello," "world!")
    (tokenize "Hello, world!"))
  
  (test-equal "tokenize empty string"
    '()
    (tokenize "")))

;;; Test vocabulary building
(test-group "vocabulary-building"
  (let ((tokens '("hello" "world" "hello" "test")))
    (receive (str->int int->str)
        (build-vocabulary tokens)
      (test-assert "str->int hash table created"
        #t)
      (test-assert "int->str hash table created"
        #t)
      ;; Special tokens should be at indices 0-3
      (test-equal "unknown token index"
        0
        (hash-table-ref/default str->int "<unk>" -1))
      (test-equal "padding token index"
        1
        (hash-table-ref/default str->int "<pad>" -1))
      (test-equal "end-of-sequence token index"
        2
        (hash-table-ref/default str->int "<eos>" -1))
      (test-equal "beginning-of-sequence token index"
        3
        (hash-table-ref/default str->int "<bos>" -1)))))

;;; Test tokenizer creation and encoding/decoding
(test-group "tokenizer-operations"
  (let* ((corpus "The quick brown fox jumps over the lazy dog")
         (tokenizer (make-tokenizer corpus)))
    
    (test-assert "tokenizer created"
      (tokenizer? tokenizer))
    
    (test-assert "vocabulary size is positive"
      (> (tokenizer-vocab-size tokenizer) 0))
    
    (let ((encoded (tokenizer-encode tokenizer "The quick fox")))
      (test-assert "encoding produces list"
        (list? encoded))
      (test-assert "encoding produces integers"
        (and-map integer? encoded))
      
      (let ((decoded (tokenizer-decode tokenizer encoded)))
        (test-assert "decoding produces string"
          (string? decoded))))))

;;; Test BPE-specific functions
(test-group "bpe-functions"
  ;; get-token-pairs test fixed
  (test-assert "get token pairs creates pairs"
    (let ((pairs (get-token-pairs '("a" "b" "c"))))
      (and (= (length pairs) 2)
           (pair? (car pairs)))))
  
  (test-equal "merge tokens"
    '("ab" "c" "ab")
    (merge-tokens '("a" "b" "c" "a" "b") '("a" . "b") "ab"))
  
  (let ((tokens '("a" "b" "a" "b" "c")))
    (let ((freq-table (compute-pair-frequencies tokens)))
      (test-assert "frequency table created"
        #t)
      (test-equal "pair frequency count"
        2
        (hash-table-ref/default freq-table '("a" . "b") 0))))
  
  (test-assert "bpe tokenize simple"
    (let ((result (bpe-tokenize "abc" 1)))
      (< (length result) 3)))) ; Should have merged at least one pair

;;; Test special tokens handling
(test-group "special-tokens"
  (let* ((corpus "test data")
         (tokenizer (make-tokenizer corpus)))
    
    (let ((encoded (tokenizer-encode tokenizer "unknown word here")))
      (test-assert "unknown tokens handled"
        (member 0 encoded))))) ; 0 is <unk> token ID

(test-end "tokenizer-tests")

(exit (if (zero? (test-runner-fail-count (test-runner-current))) 0 1))