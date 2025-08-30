;;; text.scm --- Text Preprocessing for LLM Training
;;; Chapter 2: Implementation

(define-module (llm text)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (preprocess-text
            load-corpus
            split-sentences
            clean-corpus
            text-statistics
            chunk-text
            sliding-window-chunks
            word-statistics
            character-frequency
            validate-text
            normalize-whitespace
            remove-special-chars
            corpus-vocabulary
            text-entropy
            sentence-lengths))

;;; Commentary:
;;;
;;; This module implements text preprocessing functions for LLM training.
;;; Based on "Build a Large Language Model (From Scratch)" Chapter 2.
;;;
;;; Functions:
;;; - preprocess-text: clean and normalize text
;;; - load-corpus: load text from files
;;; - split-sentences: split text into sentences
;;; - clean-corpus: clean and filter text chunks
;;; - text-statistics: compute corpus statistics
;;; - chunk-text: split text into fixed-size chunks
;;; - sliding-window-chunks: create overlapping text chunks
;;; - word-statistics: detailed word-level statistics
;;; - character-frequency: analyze character distribution
;;; - validate-text: check text validity
;;; - normalize-whitespace: standardize whitespace
;;; - remove-special-chars: remove non-alphanumeric characters
;;; - corpus-vocabulary: extract unique vocabulary
;;; - text-entropy: calculate text entropy
;;; - sentence-lengths: analyze sentence length distribution
;;;

;;; Code:

(define (preprocess-text text)
  "Clean and normalize text for training"
  (if (not (string? text))
      ""
      (let* ((cleaned (string-trim text))
             (normalized (regexp-substitute/global #f "\\s+" cleaned 'pre " " 'post))
             (punctuation-fixed (regexp-substitute/global #f "([.!?])([A-Za-z])" normalized 'pre 1 " " 2 'post)))
        (string-downcase punctuation-fixed))))

(define (load-corpus file-path)
  "Load text corpus from file"
  (if (not (file-exists? file-path))
      '()
      (let* ((content (call-with-input-file file-path get-string-all))
             (lines (string-split content #\newline))
             (non-empty (filter (lambda (line) 
                                  (not (string-null? (string-trim line)))) 
                               lines)))
        non-empty)))

(define (split-sentences text)
  "Split text into sentences"
  (if (not (string? text))
      '()
      (let* ((with-spaces (regexp-substitute/global #f "([.!?])([A-Z])" text 'pre 1 " " 2 'post))
             (sentences (string-split with-spaces #\. #\! #\?))
             (cleaned (map string-trim sentences))
             (non-empty (filter (lambda (s) (not (string-null? s))) cleaned)))
        (map (lambda (s) 
               (if (or (string-suffix? "." s)
                       (string-suffix? "!" s)
                       (string-suffix? "?" s))
                   s
                   (string-append s ".")))
             non-empty))))

(define (clean-corpus text-list)
  "Clean a list of text chunks"
  (if (not (list? text-list))
      '()
      (let* ((min-length 10)
             (max-length 1000)
             (length-filtered (filter (lambda (text)
                                       (and (string? text)
                                            (>= (string-length text) min-length)
                                            (<= (string-length text) max-length)))
                                     text-list))
             (trimmed (map string-trim length-filtered))
             (deduped (delete-duplicates trimmed string=?)))
        deduped)))

(define (text-statistics corpus)
  "Compute corpus statistics"
  (if (not (list? corpus))
      '((texts . 0) (words . 0) (characters . 0) (avg-length . 0))
      (let* ((text-count (length corpus))
             (all-text (string-join corpus " "))
             (word-count (length (string-split all-text #\space)))
             (char-count (string-length all-text))
             (avg-length (if (> text-count 0) 
                           (exact->inexact (/ char-count text-count))
                           0)))
        `((texts . ,text-count)
          (words . ,word-count) 
          (characters . ,char-count)
          (avg-length . ,avg-length)))))

(define (chunk-text text chunk-size)
  "Split text into fixed-size chunks (by characters)"
  (if (or (not (string? text)) (<= chunk-size 0))
      '()
      (let loop ((remaining text) (chunks '()))
        (if (string-null? remaining)
            (reverse chunks)
            (let* ((len (min chunk-size (string-length remaining)))
                   (chunk (substring remaining 0 len))
                   (rest (substring remaining len)))
              (loop rest (cons chunk chunks)))))))

(define (sliding-window-chunks text window-size step-size)
  "Create overlapping text chunks with sliding window"
  (if (or (not (string? text)) 
          (<= window-size 0) 
          (<= step-size 0)
          (< (string-length text) window-size))
      '()
      (let loop ((pos 0) (chunks '()))
        (if (> (+ pos window-size) (string-length text))
            (reverse chunks)
            (let ((chunk (substring text pos (min (+ pos window-size) 
                                                  (string-length text)))))
              (loop (+ pos step-size) (cons chunk chunks)))))))

(define (word-statistics corpus)
  "Compute detailed word-level statistics"
  (if (not (list? corpus))
      '((unique-words . 0) (total-words . 0) (avg-word-length . 0) (word-frequencies . ()))
      (let* ((all-text (string-join corpus " "))
             (words (string-split (string-downcase all-text) #\space))
             (word-freq (fold (lambda (word acc)
                               (assoc-set! acc word 
                                         (1+ (or (assoc-ref acc word) 0))))
                             '() words))
             (unique-count (length word-freq))
             (total-count (length words))
             (avg-length (if (> total-count 0)
                           (exact->inexact 
                            (/ (apply + (map string-length words)) total-count))
                           0))
             (sorted-freq (sort word-freq 
                               (lambda (a b) (> (cdr a) (cdr b))))))
        `((unique-words . ,unique-count)
          (total-words . ,total-count)
          (avg-word-length . ,avg-length)
          (word-frequencies . ,(take sorted-freq 
                                     (min 20 (length sorted-freq))))))))

(define (character-frequency text)
  "Analyze character distribution in text"
  (if (not (string? text))
      '()
      (let ((freq-table (make-hash-table)))
        (string-for-each 
         (lambda (char)
           (hash-set! freq-table char 
                     (1+ (or (hash-ref freq-table char) 0))))
         text)
        (sort (hash-map->list cons freq-table)
              (lambda (a b) (> (cdr a) (cdr b)))))))

(define (validate-text text)
  "Check if text is valid for processing"
  (and (string? text)
       (> (string-length text) 0)
       (not (string-every char-whitespace? text))
       (< (string-length text) 1000000))) ; Max 1MB

(define (normalize-whitespace text)
  "Standardize all whitespace to single spaces"
  (if (not (string? text))
      ""
      (regexp-substitute/global #f "\\s+" 
                               (string-trim text) 
                               'pre " " 'post)))

(define (remove-special-chars text)
  "Remove non-alphanumeric characters except spaces and basic punctuation"
  (if (not (string? text))
      ""
      (regexp-substitute/global #f "[^a-zA-Z0-9\\s.!?,;:'-]" text 'pre "" 'post)))

(define (corpus-vocabulary corpus)
  "Extract unique vocabulary from corpus"
  (if (not (list? corpus))
      '()
      (let* ((all-text (string-join corpus " "))
             (normalized (string-downcase all-text))
             (words (string-split normalized #\space))
             (cleaned-words (map (lambda (w)
                                  (regexp-substitute/global #f "[^a-z]" w 'pre "" 'post))
                               words))
             (non-empty (filter (lambda (w) (not (string-null? w))) cleaned-words))
             (vocabulary (delete-duplicates non-empty string=?)))
        (sort vocabulary string<?))))

(define (text-entropy text)
  "Calculate Shannon entropy of text (character-level)"
  (if (not (string? text))
      0
      (let* ((freq-table (make-hash-table))
             (total-chars (string-length text)))
        (string-for-each 
         (lambda (char)
           (hash-set! freq-table char 
                     (1+ (or (hash-ref freq-table char) 0))))
         text)
        (if (= total-chars 0)
            0
            (let ((entropy 0))
              (hash-for-each
               (lambda (char count)
                 (let ((probability (/ count total-chars)))
                   (set! entropy (- entropy (* probability (log probability))))))
               freq-table)
              (/ entropy (log 2)))))))  ; Convert to bits

(define (sentence-lengths corpus)
  "Analyze sentence length distribution in corpus"
  (if (not (list? corpus))
      '((min . 0) (max . 0) (mean . 0) (median . 0))
      (let* ((sentences (append-map split-sentences corpus))
             (lengths (map string-length sentences))
             (sorted-lengths (sort lengths <))
             (count (length sorted-lengths)))
        (if (= count 0)
            '((min . 0) (max . 0) (mean . 0) (median . 0))
            (let* ((min-len (car sorted-lengths))
                   (max-len (car (last-pair sorted-lengths)))
                   (mean-len (exact->inexact (/ (apply + sorted-lengths) count)))
                   (median-len (if (even? count)
                                 (exact->inexact 
                                  (/ (+ (list-ref sorted-lengths (- (/ count 2) 1))
                                        (list-ref sorted-lengths (/ count 2)))
                                     2))
                                 (list-ref sorted-lengths (/ (- count 1) 2)))))
              `((min . ,min-len)
                (max . ,max-len)
                (mean . ,mean-len)
                (median . ,median-len)
                (distribution . ,(take sorted-lengths (min 10 count)))))))))

;;; text.scm ends here
