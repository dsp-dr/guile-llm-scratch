;;; transformer-test.scm --- Tests for transformer architecture

(define-module (tests core transformer-test)
  #:use-module (srfi srfi-64)
  #:use-module (core tensor)
  #:use-module (core attention)
  #:use-module (core transformer))

(test-begin "transformer")

(test-group "Feed-Forward Network"
  (let ((ff (make-feed-forward 16 64)))
    (test-assert "Create feed-forward"
      (feed-forward? ff))
    
    (test-equal "Feed-forward dimensions"
      '(16 64)
      (list (ff-d-model ff) (ff-d-ff ff)))))

(test-group "Transformer Block"
  (let ((block (make-transformer-block 16 4 64)))
    (test-assert "Create transformer block"
      (transformer-block? block))
    
    (let ((input (tensor-random '(1 10 16))))
      (test-assert "Forward through block"
        (tensor? (forward-block block input))))))

(test-group "Encoder"
  (let ((encoder (make-encoder 2 16 4 64)))
    (test-assert "Create encoder"
      (encoder? encoder))
    
    (test-equal "Encoder layers"
      2
      (encoder-n-layers encoder))))

(test-group "Complete Transformer"
  (let ((config '((d-model . 32)
                  (n-heads . 4)
                  (n-layers . 2)
                  (d-ff . 128)
                  (vocab-size . 100)
                  (max-seq-len . 16))))
    (test-assert "Create transformer from config"
      (transformer? (make-transformer config)))
    
    (let ((model (make-transformer config))
          (input (tensor-from-list '((1 2 3 4 5)))))
      (test-assert "Forward pass"
        (tensor? (forward-transformer model input))))))

(test-end "transformer")
