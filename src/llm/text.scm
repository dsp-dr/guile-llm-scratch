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
            text-statistics))

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

;;; text.scm ends here
