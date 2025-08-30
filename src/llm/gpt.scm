;;; gpt.scm --- GPT Transformer Implementation
;;; Chapter 4: Implementation

(define-module (llm gpt)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:use-module (llm attention)
  #:export (make-gpt-config
            gpt-config?
            gpt-config-vocab-size
            gpt-config-max-position
            gpt-config-n-embd
            gpt-config-n-layer
            gpt-config-n-head
            gpt-config-dropout
            make-gpt-model
            gpt-model?
            layer-normalization
            create-layer-norm
            feed-forward-network
            transformer-block
            create-position-encoding
            gpt-forward
            generate-text))

;;; Commentary:
;;;
;;; This module implements a GPT transformer model from scratch
;;; based on "Build a Large Language Model (From Scratch)"
;;;
;;; Components:
;;; - Layer normalization
;;; - Position encoding
;;; - Feed-forward networks
;;; - Transformer blocks
;;; - Complete GPT model
;;;

;;; Code:

;;; Configuration record
(define-record-type <gpt-config>
  (make-gpt-config vocab-size max-position n-embd n-layer n-head dropout)
  gpt-config?
  (vocab-size gpt-config-vocab-size)
  (max-position gpt-config-max-position)
  (n-embd gpt-config-n-embd)
  (n-layer gpt-config-n-layer)
  (n-head gpt-config-n-head)
  (dropout gpt-config-dropout))

;;; Model record
(define-record-type <gpt-model>
  (make-gpt-model* config token-embedding position-embedding layers ln-final)
  gpt-model?
  (config gpt-model-config)
  (token-embedding gpt-model-token-embedding)
  (position-embedding gpt-model-position-embedding)
  (layers gpt-model-layers)
  (ln-final gpt-model-ln-final))

;;; Layer normalization parameters
(define-record-type <layer-norm>
  (make-layer-norm* gamma beta eps)
  layer-norm?
  (gamma layer-norm-gamma)
  (beta layer-norm-beta)
  (eps layer-norm-eps))

;;; Helper: compute mean of a list
(define (mean lst)
  "Compute the arithmetic mean of a list of numbers"
  (/ (apply + lst) (length lst)))

;;; Helper: compute variance of a list
(define (variance lst)
  "Compute the variance of a list of numbers"
  (let ((m (mean lst)))
    (mean (map (lambda (x) (expt (- x m) 2)) lst))))

;;; Layer normalization
(define (layer-normalization input gamma beta #:optional (eps 1e-5))
  "Apply layer normalization: LayerNorm(x) = γ * (x - μ) / √(σ² + ε) + β"
  (map (lambda (row)
         (let* ((m (mean row))
                (v (variance row))
                (std (sqrt (+ v eps))))
           (map (lambda (x g b)
                  (+ (* g (/ (- x m) std)) b))
                row gamma beta)))
       input))

;;; Create layer normalization with learnable parameters
(define (create-layer-norm dim #:optional (eps 1e-5))
  "Create a layer normalization module with learnable gamma and beta"
  (let ((gamma (make-list dim 1.0))
        (beta (make-list dim 0.0)))
    (make-layer-norm* gamma beta eps)))

;;; Apply layer norm using record
(define (apply-layer-norm ln-module input)
  "Apply layer normalization using a layer-norm record"
  (layer-normalization input 
                       (layer-norm-gamma ln-module)
                       (layer-norm-beta ln-module)
                       #:eps (layer-norm-eps ln-module)))

;;; ReLU activation
(define (relu x)
  "ReLU activation function: max(0, x)"
  (max 0 x))

;;; GELU activation (Gaussian Error Linear Unit)
(define (gelu x)
  "GELU activation: x * Φ(x) approximation"
  (* x 0.5 (+ 1 (tanh (* (sqrt (/ 2 3.14159))
                        (+ x (* 0.044715 (expt x 3))))))))

;;; Apply activation function element-wise
(define (apply-activation matrix activation-fn)
  "Apply activation function to each element of matrix"
  (map (lambda (row)
         (map activation-fn row))
       matrix))

;;; Linear transformation
(define (linear-transform input weight bias)
  "Apply linear transformation: y = xW + b"
  (let ((output (matrix-multiply input weight)))
    (if bias
        (map (lambda (row) (map + row bias)) output)
        output)))

;;; Feed-forward network
(define (feed-forward-network input dim-model dim-ff)
  "Two-layer feed-forward network with GELU activation"
  (let* ((d-model dim-model)
         (d-ff dim-ff)
         (weight1 (make-random-matrix d-model d-ff))
         (bias1 (make-list d-ff 0.0))
         (weight2 (make-random-matrix d-ff d-model))
         (bias2 (make-list d-model 0.0))
         (hidden (linear-transform input weight1 bias1))
         (activated (apply-activation hidden gelu)))
    (linear-transform activated weight2 bias2)))

;;; Dropout (simplified - returns input during inference)
(define (dropout input rate #:optional (training #f))
  "Apply dropout regularization (simplified version)"
  (if (not training)
      input
      (map (lambda (row)
             (map (lambda (x)
                    (if (< (random:uniform) rate)
                        0
                        (/ x (- 1 rate))))
                  row))
           input)))

;;; Residual connection
(define (residual-add input output)
  "Add residual connection: input + output"
  (matrix-add input output))

;;; Transformer block
(define (transformer-block input config)
  "Complete transformer block with attention and feed-forward"
  (let* ((d-model (gpt-config-n-embd config))
         (n-heads (gpt-config-n-head config))
         (d-ff (* 4 d-model))
         (ln1 (create-layer-norm d-model))
         (ln2 (create-layer-norm d-model))
         (norm1 (apply-layer-norm ln1 input))
         (attn-out (multi-head-attention norm1 n-heads d-model))
         (attn-residual (residual-add input attn-out))
         (norm2 (apply-layer-norm ln2 attn-residual))
         (ff-out (feed-forward-network norm2 d-model d-ff))
         (output (residual-add attn-residual ff-out)))
    output))

;;; Position encoding
(define (create-position-encoding max-position d-model)
  "Create sinusoidal position encodings"
  (let ((encodings (make-vector max-position)))
    (do ((pos 0 (+ pos 1)))
        ((>= pos max-position))
      (let ((row (make-vector d-model)))
        (do ((i 0 (+ i 1)))
            ((>= i d-model))
          (let* ((angle-rate (/ 1 (expt 10000 (/ (* 2 i) d-model))))
                 (angle (* pos angle-rate)))
            (vector-set! row i
                        (if (even? i)
                            (sin angle)
                            (cos angle)))))
        (vector-set! encodings pos (vector->list row))))
    (vector->list encodings)))

;;; Token embedding
(define (create-token-embedding vocab-size d-model)
  "Create token embedding matrix"
  (make-random-matrix vocab-size d-model))

;;; Random matrix initialization
(define (make-random-matrix rows cols)
  "Create a random matrix with Xavier initialization"
  (let ((scale (sqrt (/ 2.0 (+ rows cols)))))
    (map (lambda (_)
           (map (lambda (_)
                  (* scale (- (* 2 (random:uniform)) 1)))
                (iota cols)))
         (iota rows))))

;;; Embedding lookup
(define (embed-tokens token-ids embedding-matrix)
  "Look up embeddings for token IDs"
  (map (lambda (id)
         (list-ref embedding-matrix id))
       token-ids))

;;; Create GPT model
(define (make-gpt-model config)
  "Create a complete GPT model with given configuration"
  (let* ((vocab-size (gpt-config-vocab-size config))
         (max-pos (gpt-config-max-position config))
         (d-model (gpt-config-n-embd config))
         (n-layers (gpt-config-n-layer config))
         (token-emb (create-token-embedding vocab-size d-model))
         (pos-emb (create-position-encoding max-pos d-model))
         (layers (map (lambda (_) 
                       (lambda (x) (transformer-block x config)))
                     (iota n-layers)))
         (ln-final (create-layer-norm d-model)))
    (make-gpt-model* config token-emb pos-emb layers ln-final)))

;;; Forward pass through GPT
(define (gpt-forward model token-ids)
  "Forward pass through the GPT model"
  (let* ((config (gpt-model-config model))
         (token-emb (gpt-model-token-embedding model))
         (pos-emb (gpt-model-position-embedding model))
         (layers (gpt-model-layers model))
         (ln-final (gpt-model-ln-final model))
         (seq-len (length token-ids))
         (token-embeddings (embed-tokens token-ids token-emb))
         (positions (iota seq-len))
         (pos-embeddings (map (lambda (pos)
                               (list-ref pos-emb pos))
                             positions))
         (embeddings (matrix-add token-embeddings pos-embeddings)))
    (let loop ((x embeddings)
               (layers-remaining layers))
      (if (null? layers-remaining)
          (apply-layer-norm ln-final x)
          (loop ((car layers-remaining) x)
                (cdr layers-remaining))))))

;;; Softmax for output probabilities
(define (output-projection hidden vocab-size)
  "Project hidden states to vocabulary logits"
  (let ((weight (make-random-matrix (length (car hidden)) vocab-size)))
    (matrix-multiply hidden (matrix-transpose weight))))

;;; Sample from probability distribution
(define (sample-from-probs probs #:optional (temperature 1.0))
  "Sample token ID from probability distribution"
  (let* ((scaled (map (lambda (p) (/ p temperature)) probs))
         (soft-probs (softmax scaled))
         (cumsum (let loop ((ps soft-probs)
                           (acc 0)
                           (result '()))
                  (if (null? ps)
                      (reverse result)
                      (let ((new-acc (+ acc (car ps))))
                        (loop (cdr ps) new-acc (cons new-acc result))))))
         (rand (random:uniform))
         (idx (let loop ((cs cumsum) (i 0))
                (if (or (null? cs) (>= (car cs) rand))
                    i
                    (loop (cdr cs) (+ i 1))))))
    idx))

;;; Text generation
(define (generate-text model initial-tokens max-length 
                      #:optional (temperature 1.0))
  "Generate text using the GPT model"
  (let ((config (gpt-model-config model))
        (vocab-size (gpt-config-vocab-size config)))
    (let loop ((tokens initial-tokens)
               (remaining (- max-length (length initial-tokens))))
      (if (<= remaining 0)
          tokens
          (let* ((hidden (gpt-forward model tokens))
                 (last-hidden (list (list-ref hidden (- (length hidden) 1))))
                 (logits (output-projection last-hidden vocab-size))
                 (probs (car (matrix-softmax logits)))
                 (next-token (sample-from-probs probs #:temperature temperature)))
            (loop (append tokens (list next-token))
                  (- remaining 1)))))))

;;; Matrix operations (re-exported from attention module)
(define matrix-multiply 
  (@@ (llm attention) matrix-multiply))

(define matrix-add
  (@@ (llm attention) matrix-add))

(define matrix-transpose
  (@@ (llm attention) matrix-transpose))

(define matrix-softmax
  (@@ (llm attention) matrix-softmax))

;;; Random number generation
(define random:uniform
  (let ((rng (seed->random-state (current-time))))
    (lambda ()
      (random:uniform rng))))

;;; gpt.scm ends here