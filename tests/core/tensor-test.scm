;;; tensor-test.scm --- Tests for tensor operations

(define-module (tests core tensor-test)
  #:use-module (srfi srfi-64)
  #:use-module (core tensor))

(test-begin "tensor-operations")

(test-group "Creation and Properties"
  (test-assert "Create tensor from list"
    (tensor? (tensor-from-list '((1 2 3) (4 5 6)))))
  
  (test-equal "Tensor shape"
    '(2 3)
    (tensor-shape (tensor-from-list '((1 2 3) (4 5 6)))))
  
  (test-equal "Tensor size"
    6
    (tensor-size (tensor-from-list '((1 2 3) (4 5 6)))))
  
  (test-equal "Tensor ndim"
    2
    (tensor-ndim (tensor-from-list '((1 2 3) (4 5 6))))))

(test-group "Element Access"
  (let ((t (tensor-from-list '((1 2 3) (4 5 6)))))
    (test-equal "tensor-ref"
      5.0
      (tensor-ref t 1 1))
    
    (tensor-set! t 10.0 0 1)
    (test-equal "tensor-set!"
      10.0
      (tensor-ref t 0 1))))

(test-group "Tensor Creation Functions"
  (test-assert "tensor-zeros"
    (let ((t (tensor-zeros '(2 3))))
      (and (tensor? t)
           (= 0.0 (tensor-ref t 0 0)))))
  
  (test-assert "tensor-ones"
    (let ((t (tensor-ones '(2 3))))
      (and (tensor? t)
           (= 1.0 (tensor-ref t 0 0))))))

(test-group "Reshaping"
  (test-equal "tensor-reshape"
    '(3 2)
    (tensor-shape (tensor-reshape (tensor-from-list '((1 2 3) (4 5 6)))
                                  '(3 2))))
  
  (test-equal "tensor-transpose 2D"
    '((1.0 3.0) (2.0 4.0))
    (tensor->list (tensor-transpose (tensor-from-list '((1 2) (3 4)))))))

(test-group "Element-wise Operations"
  (let ((t1 (tensor-from-list '((1 2) (3 4))))
        (t2 (tensor-from-list '((5 6) (7 8)))))
    
    (test-equal "tensor-add"
      '((6.0 8.0) (10.0 12.0))
      (tensor->list (tensor-add t1 t2)))
    
    (test-equal "tensor-multiply"
      '((5.0 12.0) (21.0 32.0))
      (tensor->list (tensor-multiply t1 t2)))))

(test-group "Matrix Operations"
  (let ((v1 (tensor-from-list '(1 2 3)))
        (v2 (tensor-from-list '(4 5 6))))
    (test-equal "tensor-dot"
      32.0
      (tensor-dot v1 v2)))
  
  (let ((m1 (tensor-from-list '((1 2) (3 4))))
        (m2 (tensor-from-list '((5 6) (7 8)))))
    (test-equal "tensor-matmul"
      '((19.0 22.0) (43.0 50.0))
      (tensor->list (tensor-matmul m1 m2)))))

(test-group "Reduction Operations"
  (let ((t (tensor-from-list '((1 2 3) (4 5 6)))))
    (test-equal "tensor-sum"
      21.0
      (tensor-sum t))
    
    (test-equal "tensor-mean"
      3.5
      (tensor-mean t))))

(test-end "tensor-operations")
