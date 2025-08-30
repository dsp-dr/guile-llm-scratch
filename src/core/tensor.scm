;;; tensor.scm --- Tensor operations for neural networks
;;; Commentary:
;;; Pure Scheme implementation of tensor operations optimized for LLMs.
;;; Uses SRFI-4 for efficient numeric arrays and SRFI-25 for multi-dimensional arrays.

(define-module (core tensor)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-4)   ; Homogeneous numeric vectors
  #:use-module (srfi srfi-8)   ; receive
  #:use-module (srfi srfi-9)   ; Records
  #:use-module (srfi srfi-26)  ; cut
  #:use-module (srfi srfi-42)  ; Eager comprehensions
  #:use-module (srfi srfi-43)  ; Vector library
  #:export (make-tensor
            tensor?
            tensor-shape
            tensor-data
            tensor-ndim
            tensor-size
            tensor-ref
            tensor-set!
            tensor-zeros
            tensor-ones
            tensor-random
            tensor-from-list
            tensor->list
            tensor-reshape
            tensor-transpose
            tensor-add
            tensor-subtract
            tensor-multiply
            tensor-divide
            tensor-dot
            tensor-matmul
            tensor-sum
            tensor-mean
            tensor-std
            tensor-broadcast
            tensor-slice
            tensor-concat
            tensor-split))

;; Core tensor structure using SRFI-4 f32vectors for efficiency
(define-record-type <tensor>
  (%make-tensor data shape strides)
  tensor?
  (data tensor-data)        ; f32vector containing flattened data
  (shape tensor-shape)      ; List of dimension sizes
  (strides tensor-strides)) ; List of strides for each dimension

(define (compute-strides shape)
  "Compute strides for row-major (C-style) ordering."
  (let ((ndim (length shape)))
    (if (zero? ndim)
        '()
        (let loop ((dims (cdr shape))
                   (strides (list 1)))
          (if (null? dims)
              (reverse strides)
              (loop (cdr dims)
                    (cons (* (car strides) (car dims))
                          strides)))))))

(define (compute-size shape)
  "Compute total number of elements from shape."
  (fold * 1 shape))

(define (make-tensor data shape)
  "Create a tensor from data and shape."
  (let* ((size (compute-size shape))
         (f32-data (cond
                    ((f32vector? data) data)
                    ((vector? data) (list->f32vector (vector->list data)))
                    ((list? data) (list->f32vector data))
                    (else (error "Invalid data type for tensor"))))
         (strides (compute-strides shape)))
    (unless (= (f32vector-length f32-data) size)
      (error "Data size doesn't match shape" size (f32vector-length f32-data)))
    (%make-tensor f32-data shape strides)))

(define (tensor-ndim tensor)
  "Return number of dimensions."
  (length (tensor-shape tensor)))

(define (tensor-size tensor)
  "Return total number of elements."
  (compute-size (tensor-shape tensor)))

(define (flat-index shape strides indices)
  "Convert multi-dimensional indices to flat index."
  (let loop ((indices indices)
             (strides strides)
             (flat-idx 0))
    (if (null? indices)
        flat-idx
        (loop (cdr indices)
              (cdr strides)
              (+ flat-idx (* (car indices) (car strides)))))))

(define (tensor-ref tensor . indices)
  "Get element at given indices."
  (let ((shape (tensor-shape tensor))
        (data (tensor-data tensor))
        (strides (tensor-strides tensor)))
    (unless (= (length indices) (length shape))
      (error "Wrong number of indices" indices shape))
    (f32vector-ref data (flat-index shape strides indices))))

(define (tensor-set! tensor value . indices)
  "Set element at given indices."
  (let ((shape (tensor-shape tensor))
        (data (tensor-data tensor))
        (strides (tensor-strides tensor)))
    (unless (= (length indices) (length shape))
      (error "Wrong number of indices" indices shape))
    (f32vector-set! data (flat-index shape strides indices) value)))

(define (tensor-zeros shape)
  "Create tensor filled with zeros."
  (make-tensor (make-f32vector (compute-size shape) 0.0) shape))

(define (tensor-ones shape)
  "Create tensor filled with ones."
  (make-tensor (make-f32vector (compute-size shape) 1.0) shape))

(define (tensor-random shape #:optional (low 0.0) (high 1.0))
  "Create tensor with random values between low and high."
  (let* ((size (compute-size shape))
         (range (- high low))
         (data (make-f32vector size)))
    (do ((i 0 (+ i 1)))
        ((>= i size))
      (f32vector-set! data i (+ low (* range (random:uniform)))))
    (make-tensor data shape)))

(define (tensor-from-list nested-list)
  "Create tensor from nested list structure."
  (define (get-shape lst)
    (if (not (list? lst))
        '()
        (cons (length lst)
              (if (null? lst)
                  '()
                  (get-shape (car lst))))))
  
  (define (flatten lst)
    (cond
     ((null? lst) '())
     ((not (pair? lst)) (list lst))
     (else (append (flatten (car lst))
                   (flatten (cdr lst))))))
  
  (let ((shape (get-shape nested-list))
        (flat-data (flatten nested-list)))
    (make-tensor flat-data shape)))

(define (tensor->list tensor)
  "Convert tensor to nested list structure."
  (define (unflatten data shape strides)
    (if (null? shape)
        (f32vector-ref data 0)
        (let ((dim-size (car shape)))
          (list-ec (: i dim-size)
                   (unflatten (f32vector-copy data 
                                             (* i (car strides))
                                             (+ (* i (car strides)) 
                                                (if (null? (cdr strides))
                                                    1
                                                    (car strides))))
                             (cdr shape)
                             (cdr strides))))))
  
  (unflatten (tensor-data tensor)
             (tensor-shape tensor)
             (tensor-strides tensor)))

(define (tensor-reshape tensor new-shape)
  "Reshape tensor to new dimensions (must have same total size)."
  (let ((old-size (tensor-size tensor))
        (new-size (compute-size new-shape)))
    (unless (= old-size new-size)
      (error "Cannot reshape: size mismatch" old-size new-size))
    (make-tensor (f32vector-copy (tensor-data tensor)) new-shape)))

(define (tensor-transpose tensor #:optional (axes #f))
  "Transpose tensor dimensions."
  (match (tensor-shape tensor)
    ((rows cols)  ; 2D case
     (let* ((result (tensor-zeros (list cols rows)))
            (result-data (tensor-data result))
            (source-data (tensor-data tensor)))
       (do-ec (: i rows)
              (: j cols)
              (f32vector-set! result-data
                             (+ (* j rows) i)
                             (f32vector-ref source-data
                                          (+ (* i cols) j))))
       result))
    (_ (error "transpose only implemented for 2D tensors currently"))))

(define (tensor-binary-op op tensor1 tensor2)
  "Apply binary operation element-wise with broadcasting."
  (let ((shape1 (tensor-shape tensor1))
        (shape2 (tensor-shape tensor2)))
    (unless (equal? shape1 shape2)
      (error "Shape mismatch for binary operation" shape1 shape2))
    (let* ((result-data (make-f32vector (tensor-size tensor1)))
           (data1 (tensor-data tensor1))
           (data2 (tensor-data tensor2)))
      (do ((i 0 (+ i 1)))
          ((>= i (f32vector-length result-data)))
        (f32vector-set! result-data i
                       (op (f32vector-ref data1 i)
                           (f32vector-ref data2 i))))
      (make-tensor result-data shape1))))

(define (tensor-add tensor1 tensor2)
  "Element-wise addition."
  (tensor-binary-op + tensor1 tensor2))

(define (tensor-subtract tensor1 tensor2)
  "Element-wise subtraction."
  (tensor-binary-op - tensor1 tensor2))

(define (tensor-multiply tensor1 tensor2)
  "Element-wise multiplication."
  (tensor-binary-op * tensor1 tensor2))

(define (tensor-divide tensor1 tensor2)
  "Element-wise division."
  (tensor-binary-op / tensor1 tensor2))

(define (tensor-dot vec1 vec2)
  "Compute dot product of two 1D tensors."
  (unless (and (= 1 (tensor-ndim vec1))
               (= 1 (tensor-ndim vec2))
               (equal? (tensor-shape vec1) (tensor-shape vec2)))
    (error "dot product requires two 1D tensors of same size"))
  (let ((data1 (tensor-data vec1))
        (data2 (tensor-data vec2))
        (size (car (tensor-shape vec1))))
    (do ((i 0 (+ i 1))
         (sum 0.0 (+ sum (* (f32vector-ref data1 i)
                           (f32vector-ref data2 i)))))
        ((>= i size) sum))))

(define (tensor-matmul tensor1 tensor2)
  "Matrix multiplication for 2D tensors."
  (match (list (tensor-shape tensor1) (tensor-shape tensor2))
    (((m k1) (k2 n))
     (unless (= k1 k2)
       (error "Matrix dimensions incompatible for multiplication" k1 k2))
     (let* ((result (tensor-zeros (list m n)))
            (result-data (tensor-data result))
            (data1 (tensor-data tensor1))
            (data2 (tensor-data tensor2)))
       (do-ec (: i m)
              (: j n)
              (let ((sum 0.0))
                (do ((k 0 (+ k 1)))
                    ((>= k k1))
                  (set! sum (+ sum (* (f32vector-ref data1 (+ (* i k1) k))
                                     (f32vector-ref data2 (+ (* k n) j))))))
                (f32vector-set! result-data (+ (* i n) j) sum)))
       result))
    (_ (error "matmul requires 2D tensors"))))

(define (tensor-sum tensor #:optional (axis #f))
  "Sum tensor elements along axis (or all if axis is #f)."
  (if axis
      (error "Axis-specific sum not yet implemented")
      (let ((data (tensor-data tensor))
            (size (tensor-size tensor)))
        (do ((i 0 (+ i 1))
             (sum 0.0 (+ sum (f32vector-ref data i))))
            ((>= i size) sum)))))

(define (tensor-mean tensor #:optional (axis #f))
  "Compute mean of tensor elements."
  (/ (tensor-sum tensor axis) (tensor-size tensor)))

(define (tensor-std tensor #:optional (axis #f))
  "Compute standard deviation of tensor elements."
  (let* ((mean (tensor-mean tensor axis))
         (data (tensor-data tensor))
         (size (tensor-size tensor))
         (variance (/ (do ((i 0 (+ i 1))
                          (sum 0.0 (+ sum (expt (- (f32vector-ref data i) mean) 2))))
                         ((>= i size) sum))
                     size)))
    (sqrt variance)))

(define (tensor-broadcast tensor target-shape)
  "Broadcast tensor to target shape."
  (let ((source-shape (tensor-shape tensor)))
    (cond
     ((equal? source-shape target-shape) tensor)
     ((and (= 1 (length source-shape))
           (= 2 (length target-shape))
           (= (car source-shape) (cadr target-shape)))
      ;; Broadcasting vector to matrix (common case)
      (let* ((rows (car target-shape))
             (cols (cadr target-shape))
             (result (tensor-zeros target-shape))
             (result-data (tensor-data result))
             (source-data (tensor-data tensor)))
        (do-ec (: i rows)
               (: j cols)
               (f32vector-set! result-data
                              (+ (* i cols) j)
                              (f32vector-ref source-data j)))
        result))
     (else (error "Broadcasting not supported for these shapes" 
                  source-shape target-shape)))))

(define (tensor-slice tensor start-indices end-indices)
  "Extract a slice of the tensor."
  (let* ((shape (tensor-shape tensor))
         (ndim (length shape))
         (slice-shape (map - end-indices start-indices))
         (result (tensor-zeros slice-shape)))
    ;; Simple implementation for 2D case
    (when (= ndim 2)
      (let ((start-row (car start-indices))
            (start-col (cadr start-indices))
            (end-row (car end-indices))
            (end-col (cadr end-indices))
            (orig-cols (cadr shape)))
        (do-ec (: i (- end-row start-row))
               (: j (- end-col start-col))
               (tensor-set! result
                           (tensor-ref tensor
                                      (+ start-row i)
                                      (+ start-col j))
                           i j))))
    result))

(define (tensor-concat tensors #:optional (axis 0))
  "Concatenate tensors along specified axis."
  (error "Concatenation not yet implemented"))

(define (tensor-split tensor n #:optional (axis 0))
  "Split tensor into n parts along axis."
  (error "Splitting not yet implemented"))
