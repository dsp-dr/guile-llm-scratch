;;; tokenizer-test.scm --- Tests for BPE tokenizer

(define-module (tests core tokenizer-test)
  #:use-module (srfi srfi-64)
  #:use-module (core tokenizer))

(test-begin "tokenizer")

(test-group "Basic Tokenizer"
  (test-assert "Create tokenizer"
    (tokenizer? (make-tokenizer)))
  
  (test-equal "Initial vocab size"
    260  ; 256 bytes + 4 special tokens
    (get-vocab-size (make-tokenizer))))

(test-group "BPE Training"
  (let* ((texts '("the cat" "the dog" "the cat sat"))
         (tokenizer (train-bpe texts 10)))
    
    (test-assert "Trained tokenizer"
      (tokenizer? tokenizer))
    
    (test-assert "Vocab grew after training"
      (> (get-vocab-size tokenizer) 260))))

(test-group "Encoding and Decoding"
  (let* ((tokenizer (make-tokenizer))
         (text "hello"))
    
    (test-assert "Encode returns list"
      (list? (encode tokenizer text)))
    
    (test-equal "Round-trip encoding"
      text
      (decode tokenizer (encode tokenizer text)))))

(test-group "Persistence"
  (let ((tokenizer (make-tokenizer))
        (temp-file "/tmp/test-tokenizer.scm"))
    
    (save-tokenizer tokenizer temp-file)
    
    (test-assert "Load saved tokenizer"
      (tokenizer? (load-tokenizer temp-file)))
    
    (test-equal "Loaded tokenizer has same vocab size"
      (get-vocab-size tokenizer)
      (get-vocab-size (load-tokenizer temp-file)))))

(test-end "tokenizer")
