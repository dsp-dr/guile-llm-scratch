;;; attention-test.scm --- Tests for attention mechanisms

(define-module (tests core attention-test)
  #:use-module (srfi srfi-64)
  #:use-module (core tensor)
  #:use-module (core attention))

(test-begin "attention")

(test-group "Softmax"
  (let ((input (tensor-from-list '((1 2 3) (4 5 6)))))
    (test-assert "Softmax returns tensor"
      (tensor? (softmax input)))
    
    (test-assert "Softmax sums to 1"
      (let* ((output (softmax input))
             (row-sum (tensor-sum (tensor-slice output '(0 0) '(1 3)))))
        (< (abs (- row-sum 1.0)) 0.01)))))

(test-group "Positional Encoding"
  (test-equal "Positional encoding shape"
    '(10 16)
    (tensor-shape (positional-encoding 10 16)))
  
  (test-assert "Positional encoding values bounded"
    (let* ((pe (positional-encoding 10 16))
           (data (tensor-data pe))
           (all-bounded #t))
      (do ((i 0 (+ i 1)))
          ((or (>= i (f32vector-length data))
               (not all-bounded)))
        (let ((val (f32vector-ref data i)))
          (when (or (> val 1.0) (< val -1.0))
            (set! all-bounded #f))))
      all-bounded)))

(test-group "Masks"
  (test-assert "Causal mask is lower triangular"
    (let ((mask (causal-mask 4)))
      (and (= 1.0 (tensor-ref mask 1 0))
           (= 1.0 (tensor-ref mask 1 1))
           (= 0.0 (tensor-ref mask 1 2)))))
  
  (test-assert "Sliding window mask"
    (let ((mask (sliding-window-mask 5 2)))
      (= 1.0 (tensor-ref mask 2 2)))))

(test-group "Multi-Head Attention"
  (test-assert "Create MHA layer"
    (multi-head-attention? (make-multi-head-attention 8 512)))
  
  (let ((mha (make-multi-head-attention 4 16)))
    (test-equal "MHA properties"
      '(4 16 4 4)
      (list (mha-n-heads mha)
            (mha-d-model mha)
            (mha-d-k mha)
            (mha-d-v mha)))))

(test-end "attention")
