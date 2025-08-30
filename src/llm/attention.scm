;;; attention.scm --- Attention Mechanisms
;;; Chapter 3: Implementation

(define-module (llm attention)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (softmax
            scaled-dot-product-attention
            multi-head-attention
            create-causal-mask))

;;; Commentary:
;;;
;;; This module implements concepts from Chapter 3
;;; of "Build a Large Language Model (From Scratch)"
;;;
;;; Components:
;;; - softmax function with numerical stability
;;; - scaled dot-product attention
;;; - multi-head attention
;;; - causal masking
;;;

;;; Code:

(define (softmax x)
  "Apply softmax function with numerical stability using max subtraction trick"
  (let* ((max-val (apply max x))
         (shifted (map (lambda (val) (- val max-val)) x))
         (exp-vals (map exp shifted))
         (sum-exp (apply + exp-vals)))
    (map (lambda (exp-val) (/ exp-val sum-exp)) exp-vals)))

(define (matrix-transpose matrix)
  "Transpose a matrix represented as list of lists"
  (if (null? (car matrix))
      '()
      (cons (map car matrix)
            (matrix-transpose (map cdr matrix)))))

(define (matrix-multiply A B)
  "Multiply two matrices A and B"
  (let ((B-T (matrix-transpose B)))
    (map (lambda (row-a)
           (map (lambda (col-b)
                  (apply + (map * row-a col-b)))
                B-T))
         A)))

(define (matrix-scalar-multiply matrix scalar)
  "Multiply matrix by scalar"
  (map (lambda (row)
         (map (lambda (val) (* val scalar)) row))
       matrix))

(define (matrix-add A B)
  "Add two matrices element-wise"
  (map (lambda (row-a row-b)
         (map + row-a row-b))
       A B))

(define (matrix-subtract A B)
  "Subtract matrix B from matrix A element-wise"
  (map (lambda (row-a row-b)
         (map - row-a row-b))
       A B))

(define (apply-mask matrix mask)
  "Apply mask to matrix, setting masked positions to -inf"
  (if (not mask)
      matrix
      (map (lambda (row mask-row)
             (map (lambda (val mask-val)
                    (if (= mask-val 0)
                        -inf.0
                        val))
                  row mask-row))
           matrix mask)))

(define (matrix-softmax matrix)
  "Apply softmax to each row of a matrix"
  (map softmax matrix))

(define (scaled-dot-product-attention query key value mask)
  "Core attention mechanism: Attention(Q,K,V) = softmax(QK^T/√d_k)V"
  (let* ((d-k (length (car key)))
         (scale-factor (/ 1 (sqrt d-k)))
         (key-T (matrix-transpose key))
         (scores (matrix-multiply query key-T))
         (scaled-scores (matrix-scalar-multiply scores scale-factor))
         (masked-scores (apply-mask scaled-scores mask))
         (attention-weights (matrix-softmax masked-scores)))
    (matrix-multiply attention-weights value)))

(define (create-causal-mask seq-length)
  "Create upper triangular mask to prevent looking at future tokens"
  (let loop ((i 0) (result '()))
    (if (>= i seq-length)
        (reverse result)
        (let ((row (let loop-inner ((j 0) (row-result '()))
                     (if (>= j seq-length)
                         (reverse row-result)
                         (loop-inner (+ j 1)
                                   (cons (if (<= j i) 1 0) row-result))))))
          (loop (+ i 1) (cons row result))))))

(define (split-heads input num-heads)
  "Split input into multiple attention heads"
  (let* ((seq-len (length input))
         (d-model (length (car input)))
         (d-head (/ d-model num-heads)))
    (map (lambda (head-idx)
           (map (lambda (token)
                  (let ((start (* head-idx d-head))
                        (end (* (+ head-idx 1) d-head)))
                    (list-head (list-tail token start) d-head)))
                input))
         (iota num-heads))))

(define (concatenate-heads heads)
  "Concatenate results from multiple attention heads"
  (let ((seq-len (length (car heads))))
    (map (lambda (pos)
           (apply append
                  (map (lambda (head)
                         (list-ref head pos))
                       heads)))
         (iota seq-len))))

(define (multi-head-attention input num-heads d-model)
  "Run attention with multiple heads in parallel"
  (let* ((seq-length (length input))
         (mask (create-causal-mask seq-length))
         (heads (split-heads input num-heads))
         (attention-outputs
          (map (lambda (head)
                 (scaled-dot-product-attention head head head mask))
               heads)))
    (concatenate-heads attention-outputs)))

;;; attention.scm ends here
