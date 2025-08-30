;;; llm-fundamentals.scm --- Understanding Large Language Models
;;; Chapter 1: Implementation

(define-module (llm fundamentals)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-69)
  #:export (tokenize
            detokenize
            build-vocabulary
            encode-text
            decode-ids
            get-vocab-size
            make-tokenizer
            tokenizer?
            tokenizer-encode
            tokenizer-decode
            tokenizer-vocab-size
            get-token-pairs
            merge-tokens
            compute-pair-frequencies
            bpe-tokenize))

;;; Commentary:
;;;
;;; This module implements concepts from Chapter 1
;;; of "Build a Large Language Model (From Scratch)"
;;;
;;; Components implemented:
;;; - Byte Pair Encoding (BPE) tokenization
;;; - Vocabulary building
;;; - Token-to-ID and ID-to-token mapping
;;; - Special tokens handling
;;;

;;; Code:

;;; Tokenizer record type
(define-record-type <tokenizer>
  (make-tokenizer* vocab str->int int->str special-tokens)
  tokenizer?
  (vocab tokenizer-vocab)
  (str->int tokenizer-str->int) 
  (int->str tokenizer-int->str)
  (special-tokens tokenizer-special-tokens))

;;; Special tokens
(define *special-tokens*
  '(("<unk>" . 0)    ; Unknown token
    ("<pad>" . 1)    ; Padding token
    ("<eos>" . 2)    ; End of sequence
    ("<bos>" . 3)))  ; Beginning of sequence

;;; Tokenize text into words and punctuation
(define (tokenize text)
  "Split text into tokens (words and punctuation)."
  ;; Simple tokenization: split on spaces and keep punctuation separate
  (filter (lambda (s) (not (string-null? s)))
          (map string-trim-both
               (string-split text #\space))))

;;; Build vocabulary from tokens
(define (build-vocabulary tokens)
  "Build a vocabulary mapping from a list of tokens.
   Returns two hash tables: str->int and int->str."
  (let* ((unique-tokens (delete-duplicates (sort tokens string<?)))
         (str->int (make-hash-table))
         (int->str (make-hash-table))
         (start-idx (length *special-tokens*)))
    ;; Add special tokens first
    (for-each (lambda (pair)
                (hash-table-set! str->int (car pair) (cdr pair))
                (hash-table-set! int->str (cdr pair) (car pair)))
              *special-tokens*)
    ;; Add regular tokens
    (let loop ((tokens unique-tokens)
               (idx start-idx))
      (if (null? tokens)
          (values str->int int->str)
          (begin
            (hash-table-set! str->int (car tokens) idx)
            (hash-table-set! int->str idx (car tokens))
            (loop (cdr tokens) (+ idx 1)))))))

;;; Get vocabulary size
(define (get-vocab-size tokenizer)
  "Return the size of the vocabulary."
  (hash-table-size (tokenizer-str->int tokenizer)))

;;; Encode text to token IDs
(define (encode-text text tokenizer)
  "Encode text into a list of token IDs using the tokenizer."
  (let ((tokens (tokenize text))
        (str->int (tokenizer-str->int tokenizer))
        (unk-id (cdr (assoc "<unk>" *special-tokens*))))
    (map (lambda (token)
           (hash-table-ref/default str->int token unk-id))
         tokens)))

;;; Decode token IDs to text
(define (decode-ids ids tokenizer)
  "Decode a list of token IDs back into text."
  (let ((int->str (tokenizer-int->str tokenizer)))
    (string-join
     (map (lambda (id)
            (hash-table-ref/default int->str id "<unk>"))
          ids)
     " ")))

;;; Detokenize - join tokens back into text
(define (detokenize tokens)
  "Join tokens back into readable text."
  (let ((text (string-join tokens " ")))
    ;; Remove spaces before punctuation
    (regexp-substitute/global #f
                              "\\s+([,.:;?!\"()\\'])"
                              text
                              'pre 1 'post)))

;;; BPE-specific functions

(define (get-token-pairs tokens)
  "Get all adjacent token pairs from a list."
  (if (< (length tokens) 2)
      '()
      (let loop ((tokens tokens)
                 (pairs '()))
        (if (or (null? tokens) (null? (cdr tokens)))
            (reverse pairs)
            (loop (cdr tokens)
                  (cons (cons (car tokens) (cadr tokens)) pairs))))))

(define (merge-tokens tokens pair new-token)
  "Merge all occurrences of pair in tokens with new-token."
  (let loop ((remaining tokens)
             (result '()))
    (cond
     ((null? remaining) (reverse result))
     ((and (not (null? (cdr remaining)))
           (equal? (car remaining) (car pair))
           (equal? (cadr remaining) (cdr pair)))
      (loop (cddr remaining)
            (cons new-token result)))
     (else
      (loop (cdr remaining)
            (cons (car remaining) result))))))

(define (compute-pair-frequencies tokens)
  "Compute frequency of each token pair."
  (let ((freq-table (make-hash-table equal?)))
    (for-each
     (lambda (pair)
       (hash-table-set! freq-table pair
                        (+ 1 (hash-table-ref/default freq-table pair 0))))
     (get-token-pairs tokens))
    freq-table))

(define (most-frequent-pair freq-table)
  "Find the most frequent token pair."
  (let ((max-pair #f)
        (max-count 0))
    (hash-table-walk
     freq-table
     (lambda (pair count)
       (if (> count max-count)
           (begin
             (set! max-pair pair)
             (set! max-count count)))))
    (values max-pair max-count)))

;;; Main BPE tokenization
(define (bpe-tokenize text num-merges)
  "Apply BPE tokenization with specified number of merges."
  (let ((tokens (map string (string->list text)))
        (merges '()))
    (let loop ((tokens tokens)
               (n num-merges))
      (if (or (<= n 0) (< (length tokens) 2))
          tokens
          (let* ((freq-table (compute-pair-frequencies tokens)))
            (receive (pair count)
                (most-frequent-pair freq-table)
              (if (and pair (> count 0))
                  (let ((new-token (string-append (car pair) (cdr pair))))
                    (set! merges (cons (cons pair new-token) merges))
                    (loop (merge-tokens tokens pair new-token)
                          (- n 1)))
                  tokens)))))))

;;; Create a tokenizer
(define (make-tokenizer corpus)
  "Create a tokenizer from a text corpus."
  (let ((tokens (tokenize corpus)))
    (receive (str->int int->str)
        (build-vocabulary tokens)
      (make-tokenizer* tokens str->int int->str *special-tokens*))))

;;; Tokenizer interface functions
(define (tokenizer-encode tokenizer text)
  "Encode text using the tokenizer."
  (encode-text text tokenizer))

(define (tokenizer-decode tokenizer ids)
  "Decode token IDs using the tokenizer."
  (decode-ids ids tokenizer))

(define (tokenizer-vocab-size tokenizer)
  "Get the vocabulary size of the tokenizer."
  (get-vocab-size tokenizer))

;;; {module['name']}.scm ends here
