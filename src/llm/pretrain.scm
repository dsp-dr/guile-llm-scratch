;;; pretraining.scm --- Pretraining on Unlabeled Data
;;; Chapter 5: Implementation

(define-module (llm pretraining)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:export (create-data-loader
            train-epoch
            validate
            save-checkpoint
            load-checkpoint
            compute-loss
            update-parameters
            pretrain-model
            
            make-batch
            batch?
            batch-input-ids
            batch-target-ids
            batch-mask
            
            make-training-state
            training-state?
            state-epoch
            state-step
            state-loss
            state-metrics
            
            create-adam-optimizer
            create-sgd-optimizer
            create-rmsprop-optimizer
            create-adagrad-optimizer
            create-optimizer-from-config
            adam-optimizer?
            sgd-optimizer?
            rmsprop-optimizer?
            adagrad-optimizer?
            optimizer-update!
            
            create-lr-scheduler
            step-lr-scheduler
            exponential-lr-scheduler
            cosine-annealing-lr-scheduler
            warmup-lr-scheduler
            scheduler-step!
            get-current-lr
            
            clip-gradients-by-norm
            clip-gradients-by-value))

;;; Commentary:
;;;
;;; This module implements concepts from Chapter 5
;;; of "Build a Large Language Model (From Scratch)"
;;;
;;; Components implemented:
;;; - data-loader with batch generation
;;; - training-loop with optimization
;;; - loss-functions (cross-entropy)
;;; - checkpoint management
;;; - multiple optimizers (Adam, SGD, RMSprop, AdaGrad)
;;; - gradient clipping (by norm and by value)
;;; - learning rate scheduling (step, exponential, cosine, warmup)
;;; - parameter updates
;;;

;;; Code:

;;; Data Structures

(define-record-type <batch>
  (make-batch input-ids target-ids mask)
  batch?
  (input-ids batch-input-ids)
  (target-ids batch-target-ids)
  (mask batch-mask))

(define-record-type <training-state>
  (make-training-state epoch step loss metrics)
  training-state?
  (epoch state-epoch set-state-epoch!)
  (step state-step set-state-step!)
  (loss state-loss set-state-loss!)
  (metrics state-metrics set-state-metrics!))

(define-record-type <data-loader>
  (%make-data-loader data batch-size seq-length stride shuffle? current-pos)
  data-loader?
  (data loader-data)
  (batch-size loader-batch-size)
  (seq-length loader-seq-length)
  (stride loader-stride)
  (shuffle? loader-shuffle?)
  (current-pos loader-current-pos set-loader-current-pos!))

;;; Training Configuration

(define default-config
  '((batch-size . 32)
    (learning-rate . 0.0001)
    (optimizer . adam)
    (optimizer-config . ((beta1 . 0.9) (beta2 . 0.999) (epsilon . 1e-8)))
    (num-epochs . 10)
    (warmup-steps . 1000)
    (gradient-clip . 1.0)
    (checkpoint-interval . 1000)
    (seq-length . 256)
    (stride . 256)))

;;; Data Loading Functions

(define* (create-data-loader data #:key 
                            (batch-size 32)
                            (seq-length 256)
                            (stride 256)
                            (shuffle? #f))
  "Create a data loader for generating batches from tokenized data"
  (let ((processed-data (if shuffle? 
                            (shuffle-list data)
                            data)))
    (%make-data-loader processed-data
                       batch-size
                       seq-length
                       stride
                       shuffle?
                       0)))

(define (shuffle-list lst)
  "Randomly shuffle a list"
  (let ((vec (list->vector lst)))
    (do ((i (- (vector-length vec) 1) (- i 1)))
        ((< i 1) (vector->list vec))
      (let* ((j (random (+ i 1)))
             (tmp (vector-ref vec i)))
        (vector-set! vec i (vector-ref vec j))
        (vector-set! vec j tmp)))))

(define (create-batch-from-sequences sequences pad-token)
  "Create a batch from sequences with padding"
  (let* ((batch-size (length sequences))
         (max-len (apply max (map length sequences)))
         (input-ids (make-list batch-size '()))
         (target-ids (make-list batch-size '()))
         (masks (make-list batch-size '())))
    
    (let loop ((seqs sequences)
               (inputs '())
               (targets '())
               (masks '()))
      (if (null? seqs)
          (make-batch (reverse inputs)
                      (reverse targets)
                      (reverse masks))
          (let* ((seq (car seqs))
                 (padded-seq (pad-sequence seq max-len pad-token))
                 (input (take padded-seq (- max-len 1)))
                 (target (drop padded-seq 1))
                 (mask (map (lambda (x) (if (= x pad-token) 0 1)) padded-seq)))
            (loop (cdr seqs)
                  (cons input inputs)
                  (cons target targets)
                  (cons mask masks)))))))

(define (pad-sequence seq target-length pad-token)
  "Pad a sequence to target length with pad token"
  (let ((current-length (length seq)))
    (if (>= current-length target-length)
        (take seq target-length)
        (append seq (make-list (- target-length current-length) pad-token)))))

(define (get-next-batch loader)
  "Get the next batch from the data loader"
  (let* ((data (loader-data loader))
         (batch-size (loader-batch-size loader))
         (seq-length (loader-seq-length loader))
         (stride (loader-stride loader))
         (current-pos (loader-current-pos loader))
         (data-length (length data)))
    
    (if (>= current-pos data-length)
        #f  ; End of data
        (let* ((batch-sequences
                (let loop ((i 0) (sequences '()))
                  (if (or (>= i batch-size)
                          (>= (+ current-pos (* i stride) seq-length) data-length))
                      (reverse sequences)
                      (let* ((start (+ current-pos (* i stride)))
                             (end (min (+ start seq-length) data-length))
                             (seq (list-head (list-tail data start) (- end start))))
                        (loop (+ i 1) (cons seq sequences))))))
               (batch (create-batch-from-sequences batch-sequences 0))) ; Using 0 as pad token
          
          (set-loader-current-pos! loader (+ current-pos (* batch-size stride)))
          batch))))

(define* (split-data data #:key (train-ratio 0.9) (val-ratio 0.05))
  "Split data into train, validation, and test sets"
  (let* ((n (length data))
         (train-size (inexact->exact (floor (* n train-ratio))))
         (val-size (inexact->exact (floor (* n val-ratio))))
         (train-data (take data train-size))
         (val-data (take (drop data train-size) val-size))
         (test-data (drop data (+ train-size val-size))))
    (values train-data val-data test-data)))

;;; Loss Functions

(define* (compute-cross-entropy-loss logits targets #:optional (mask #f))
  "Compute cross-entropy loss for language modeling"
  (let* ((batch-size (length logits))
         (seq-length (length (car logits)))
         (vocab-size (length (car (car logits))))
         (total-loss 0.0)
         (count 0))
    
    (do ((b 0 (+ b 1)))
        ((>= b batch-size))
      (do ((t 0 (+ t 1)))
          ((>= t seq-length))
        (when (or (not mask) 
                  (> (list-ref (list-ref mask b) t) 0))
          (let* ((logit-vec (list-ref (list-ref logits b) t))
                 (target (list-ref (list-ref targets b) t))
                 (max-logit (apply max logit-vec))
                 (exp-logits (map (lambda (x) (exp (- x max-logit))) logit-vec))
                 (sum-exp (apply + exp-logits))
                 (log-prob (- (list-ref logit-vec target) (log sum-exp))))
            (set! total-loss (- total-loss log-prob))
            (set! count (+ count 1))))))
    
    (if (> count 0)
        (/ total-loss count)
        0.0)))

(define (compute-loss batch model-forward)
  "Compute loss for a batch using the model"
  (let* ((input-ids (batch-input-ids batch))
         (target-ids (batch-target-ids batch))
         (mask (batch-mask batch))
         (logits (model-forward input-ids)))
    (compute-cross-entropy-loss logits target-ids mask)))

;;; Gradient Computation (Simplified)

(define (compute-gradients loss parameters)
  "Compute gradients for parameters (simplified stub)"
  ;; In a real implementation, this would use automatic differentiation
  ;; For now, return random small gradients for testing
  (map (lambda (param)
         (map (lambda (x) (* 0.001 (- (random:uniform) 0.5))) param))
       parameters))

(define (clip-gradients gradients max-norm)
  "Clip gradients by global norm (deprecated, use clip-gradients-by-norm)"
  (clip-gradients-by-norm gradients max-norm))

(define (clip-gradients-by-norm gradients max-norm)
  "Clip gradients by global norm to prevent gradient explosion"
  (let* ((flat-grads (apply append gradients))
         (grad-norm (sqrt (apply + (map (lambda (x) (* x x)) flat-grads)))))
    (if (> grad-norm max-norm)
        (let ((scale (/ max-norm grad-norm)))
          (map (lambda (grad-group)
                 (map (lambda (g) (* g scale)) grad-group))
               gradients))
        gradients)))

(define (clip-gradients-by-value gradients min-value max-value)
  "Clip gradient values to be within [min-value, max-value]"
  (map (lambda (grad-group)
         (map (lambda (g)
                (cond ((< g min-value) min-value)
                      ((> g max-value) max-value)
                      (else g)))
              grad-group))
       gradients))

;;; Optimizer Implementation

(define-record-type <adam-optimizer>
  (make-adam-optimizer learning-rate beta1 beta2 epsilon m v t)
  adam-optimizer?
  (learning-rate adam-lr)
  (beta1 adam-beta1)
  (beta2 adam-beta2)
  (epsilon adam-epsilon)
  (m adam-m set-adam-m!)
  (v adam-v set-adam-v!)
  (t adam-t set-adam-t!))

(define* (create-adam-optimizer #:key 
                              (learning-rate 0.001)
                              (beta1 0.9)
                              (beta2 0.999)
                              (epsilon 1e-8))
  "Create an Adam optimizer"
  (make-adam-optimizer learning-rate beta1 beta2 epsilon '() '() 0))

(define (adam-update! optimizer parameters gradients)
  "Update parameters using Adam optimizer"
  (let ((lr (adam-lr optimizer))
        (beta1 (adam-beta1 optimizer))
        (beta2 (adam-beta2 optimizer))
        (epsilon (adam-epsilon optimizer))
        (t (+ (adam-t optimizer) 1)))
    
    (set-adam-t! optimizer t)
    
    ;; Initialize momentum vectors if needed
    (when (null? (adam-m optimizer))
      (set-adam-m! optimizer (map (lambda (p) (make-list (length p) 0.0)) parameters))
      (set-adam-v! optimizer (map (lambda (p) (make-list (length p) 0.0)) parameters)))
    
    (let ((m (adam-m optimizer))
          (v (adam-v optimizer))
          (lr-t (* lr (/ (sqrt (- 1 (expt beta2 t)))
                        (- 1 (expt beta1 t))))))
      
      ;; Update each parameter
      (map (lambda (param grad m-vec v-vec)
             (map (lambda (p g m-val v-val)
                    (let* ((new-m (+ (* beta1 m-val) (* (- 1 beta1) g)))
                           (new-v (+ (* beta2 v-val) (* (- 1 beta2) (* g g))))
                           (update (/ (* lr-t new-m) (+ (sqrt new-v) epsilon))))
                      (- p update)))
                  param grad m-vec v-vec))
           parameters gradients m v))))

;;; SGD Optimizer Implementation

(define-record-type <sgd-optimizer>
  (make-sgd-optimizer learning-rate momentum weight-decay nesterov v)
  sgd-optimizer?
  (learning-rate sgd-lr)
  (momentum sgd-momentum)
  (weight-decay sgd-weight-decay)
  (nesterov sgd-nesterov)
  (v sgd-v set-sgd-v!))

(define* (create-sgd-optimizer #:key 
                              (learning-rate 0.01)
                              (momentum 0.0)
                              (weight-decay 0.0)
                              (nesterov #f))
  "Create an SGD optimizer with optional momentum and Nesterov acceleration"
  (make-sgd-optimizer learning-rate momentum weight-decay nesterov '()))

(define (sgd-update! optimizer parameters gradients)
  "Update parameters using SGD optimizer"
  (let ((lr (sgd-lr optimizer))
        (momentum (sgd-momentum optimizer))
        (weight-decay (sgd-weight-decay optimizer))
        (nesterov (sgd-nesterov optimizer)))
    
    ;; Initialize velocity if needed
    (when (null? (sgd-v optimizer))
      (set-sgd-v! optimizer (map (lambda (p) (make-list (length p) 0.0)) parameters)))
    
    (let ((v (sgd-v optimizer)))
      ;; Update each parameter
      (map (lambda (param grad v-vec)
             (map (lambda (p g v-val)
                    (let* (;; Apply weight decay if specified
                           (effective-grad (if (> weight-decay 0)
                                             (+ g (* weight-decay p))
                                             g))
                           ;; Update velocity
                           (new-v (+ (* momentum v-val) effective-grad))
                           ;; Compute parameter update
                           (update (if nesterov
                                     (* lr (+ effective-grad (* momentum new-v)))
                                     (* lr new-v))))
                      (- p update)))
                  param grad v-vec))
           parameters gradients v))))

;;; RMSprop Optimizer Implementation

(define-record-type <rmsprop-optimizer>
  (make-rmsprop-optimizer learning-rate alpha epsilon weight-decay momentum centered v s)
  rmsprop-optimizer?
  (learning-rate rmsprop-lr)
  (alpha rmsprop-alpha)
  (epsilon rmsprop-epsilon)
  (weight-decay rmsprop-weight-decay)
  (momentum rmsprop-momentum)
  (centered rmsprop-centered)
  (v rmsprop-v set-rmsprop-v!)
  (s rmsprop-s set-rmsprop-s!))

(define* (create-rmsprop-optimizer #:key 
                                  (learning-rate 0.01)
                                  (alpha 0.99)
                                  (epsilon 1e-8)
                                  (weight-decay 0.0)
                                  (momentum 0.0)
                                  (centered #f))
  "Create an RMSprop optimizer"
  (make-rmsprop-optimizer learning-rate alpha epsilon weight-decay momentum centered '() '()))

(define (rmsprop-update! optimizer parameters gradients)
  "Update parameters using RMSprop optimizer"
  (let ((lr (rmsprop-lr optimizer))
        (alpha (rmsprop-alpha optimizer))
        (epsilon (rmsprop-epsilon optimizer))
        (weight-decay (rmsprop-weight-decay optimizer))
        (momentum (rmsprop-momentum optimizer))
        (centered (rmsprop-centered optimizer)))
    
    ;; Initialize running averages if needed
    (when (null? (rmsprop-v optimizer))
      (set-rmsprop-v! optimizer (map (lambda (p) (make-list (length p) 0.0)) parameters))
      (set-rmsprop-s! optimizer (map (lambda (p) (make-list (length p) 0.0)) parameters)))
    
    (let ((v (rmsprop-v optimizer))
          (s (rmsprop-s optimizer)))
      ;; Update each parameter
      (map (lambda (param grad v-vec s-vec)
             (map (lambda (p g v-val s-val)
                    (let* (;; Apply weight decay if specified
                           (effective-grad (if (> weight-decay 0)
                                             (+ g (* weight-decay p))
                                             g))
                           ;; Update running average of squared gradients
                           (new-s (+ (* alpha s-val) (* (- 1 alpha) (* effective-grad effective-grad))))
                           ;; Compute denominator
                           (denom (+ (sqrt new-s) epsilon))
                           ;; Update velocity if using momentum
                           (new-v (if (> momentum 0)
                                    (+ (* momentum v-val) (/ effective-grad denom))
                                    (/ effective-grad denom)))
                           ;; Compute parameter update
                           (update (* lr (if (> momentum 0) new-v (/ effective-grad denom)))))
                      (- p update)))
                  param grad v-vec s-vec))
           parameters gradients v s))))

;;; AdaGrad Optimizer Implementation

(define-record-type <adagrad-optimizer>
  (make-adagrad-optimizer learning-rate initial-accumulator-value epsilon weight-decay accum)
  adagrad-optimizer?
  (learning-rate adagrad-lr)
  (initial-accumulator-value adagrad-initial-accum)
  (epsilon adagrad-epsilon)
  (weight-decay adagrad-weight-decay)
  (accum adagrad-accum set-adagrad-accum!))

(define* (create-adagrad-optimizer #:key 
                                   (learning-rate 0.01)
                                   (initial-accumulator-value 0.0)
                                   (epsilon 1e-10)
                                   (weight-decay 0.0))
  "Create an AdaGrad optimizer"
  (make-adagrad-optimizer learning-rate initial-accumulator-value epsilon weight-decay '()))

(define (adagrad-update! optimizer parameters gradients)
  "Update parameters using AdaGrad optimizer"
  (let ((lr (adagrad-lr optimizer))
        (epsilon (adagrad-epsilon optimizer))
        (weight-decay (adagrad-weight-decay optimizer))
        (initial-accum (adagrad-initial-accum optimizer)))
    
    ;; Initialize accumulator if needed
    (when (null? (adagrad-accum optimizer))
      (set-adagrad-accum! optimizer 
        (map (lambda (p) (make-list (length p) initial-accum)) parameters)))
    
    (let ((accum (adagrad-accum optimizer)))
      ;; Update each parameter
      (map (lambda (param grad accum-vec)
             (map (lambda (p g a)
                    (let* (;; Apply weight decay if specified
                           (effective-grad (if (> weight-decay 0)
                                             (+ g (* weight-decay p))
                                             g))
                           ;; Update accumulator (sum of squared gradients)
                           (new-accum (+ a (* effective-grad effective-grad)))
                           ;; Compute adaptive learning rate
                           (adaptive-lr (/ lr (+ (sqrt new-accum) epsilon)))
                           ;; Compute parameter update
                           (update (* adaptive-lr effective-grad)))
                      (- p update)))
                  param grad accum-vec))
           parameters gradients accum))))

;;; Generic Optimizer Interface

(define (create-optimizer-from-config config)
  "Create optimizer based on configuration"
  (let ((optimizer-type (assoc-ref config 'optimizer))
        (learning-rate (assoc-ref config 'learning-rate))
        (opt-config (assoc-ref config 'optimizer-config)))
    (case optimizer-type
      ((adam)
       (create-adam-optimizer 
         #:learning-rate learning-rate
         #:beta1 (or (assoc-ref opt-config 'beta1) 0.9)
         #:beta2 (or (assoc-ref opt-config 'beta2) 0.999)
         #:epsilon (or (assoc-ref opt-config 'epsilon) 1e-8)))
      ((sgd)
       (create-sgd-optimizer
         #:learning-rate learning-rate
         #:momentum (or (assoc-ref opt-config 'momentum) 0.0)
         #:weight-decay (or (assoc-ref opt-config 'weight-decay) 0.0)
         #:nesterov (or (assoc-ref opt-config 'nesterov) #f)))
      ((rmsprop)
       (create-rmsprop-optimizer
         #:learning-rate learning-rate
         #:alpha (or (assoc-ref opt-config 'alpha) 0.99)
         #:epsilon (or (assoc-ref opt-config 'epsilon) 1e-8)
         #:weight-decay (or (assoc-ref opt-config 'weight-decay) 0.0)
         #:momentum (or (assoc-ref opt-config 'momentum) 0.0)
         #:centered (or (assoc-ref opt-config 'centered) #f)))
      ((adagrad)
       (create-adagrad-optimizer
         #:learning-rate learning-rate
         #:initial-accumulator-value (or (assoc-ref opt-config 'initial-accumulator-value) 0.0)
         #:epsilon (or (assoc-ref opt-config 'epsilon) 1e-10)
         #:weight-decay (or (assoc-ref opt-config 'weight-decay) 0.0)))
      (else (error "Unknown optimizer type in configuration:" optimizer-type)))))

(define (optimizer-update! optimizer parameters gradients)
  "Generic function to update parameters using any optimizer"
  (cond 
    ((adam-optimizer? optimizer) (adam-update! optimizer parameters gradients))
    ((sgd-optimizer? optimizer) (sgd-update! optimizer parameters gradients))
    ((rmsprop-optimizer? optimizer) (rmsprop-update! optimizer parameters gradients))
    ((adagrad-optimizer? optimizer) (adagrad-update! optimizer parameters gradients))
    (else (error "Unknown optimizer type"))))

(define (update-parameters parameters gradients optimizer)
  "Update parameters using the optimizer (backward compatibility)"
  (optimizer-update! optimizer parameters gradients))

;;; Training Loop

(define (train-epoch model train-loader optimizer device)
  "Train for one epoch"
  (let ((total-loss 0.0)
        (num-batches 0))
    
    (let loop ((batch (get-next-batch train-loader)))
      (when batch
        (let* ((loss (compute-loss batch model))
               (gradients (compute-gradients loss (model 'get-parameters))))
          
          ;; Clip gradients
          (let ((clipped-grads (clip-gradients gradients 1.0)))
            ;; Update parameters
            (let ((new-params (update-parameters (model 'get-parameters) 
                                                clipped-grads 
                                                optimizer)))
              (model 'set-parameters new-params)))
          
          (set! total-loss (+ total-loss loss))
          (set! num-batches (+ num-batches 1))
          (loop (get-next-batch train-loader)))))
    
    (if (> num-batches 0)
        (/ total-loss num-batches)
        0.0)))

(define (validate model val-loader)
  "Validate the model on validation data"
  (let ((total-loss 0.0)
        (num-batches 0))
    
    (let loop ((batch (get-next-batch val-loader)))
      (when batch
        (let ((loss (compute-loss batch model)))
          (set! total-loss (+ total-loss loss))
          (set! num-batches (+ num-batches 1))
          (loop (get-next-batch val-loader)))))
    
    (if (> num-batches 0)
        (/ total-loss num-batches)
        0.0)))

;;; Checkpoint Management

(define (serialize-optimizer-state optimizer)
  "Serialize optimizer state based on optimizer type"
  (cond
    ((adam-optimizer? optimizer)
     `(adam ,(adam-lr optimizer) ,(adam-beta1 optimizer) ,(adam-beta2 optimizer) 
            ,(adam-epsilon optimizer) ,(adam-m optimizer) ,(adam-v optimizer) ,(adam-t optimizer)))
    ((sgd-optimizer? optimizer)
     `(sgd ,(sgd-lr optimizer) ,(sgd-momentum optimizer) ,(sgd-weight-decay optimizer) 
           ,(sgd-nesterov optimizer) ,(sgd-v optimizer)))
    ((rmsprop-optimizer? optimizer)
     `(rmsprop ,(rmsprop-lr optimizer) ,(rmsprop-alpha optimizer) ,(rmsprop-epsilon optimizer)
               ,(rmsprop-weight-decay optimizer) ,(rmsprop-momentum optimizer) 
               ,(rmsprop-centered optimizer) ,(rmsprop-v optimizer) ,(rmsprop-s optimizer)))
    ((adagrad-optimizer? optimizer)
     `(adagrad ,(adagrad-lr optimizer) ,(adagrad-initial-accum optimizer) 
               ,(adagrad-epsilon optimizer) ,(adagrad-weight-decay optimizer) 
               ,(adagrad-accum optimizer)))
    (else (error "Unknown optimizer type for serialization"))))

(define (save-checkpoint model optimizer training-state filename)
  "Save model checkpoint to file"
  (call-with-output-file filename
    (lambda (port)
      (write `((model-params . ,(model 'get-parameters))
               (optimizer-state . ,(serialize-optimizer-state optimizer))
               (training-state . ,(list (state-epoch training-state)
                                      (state-step training-state)
                                      (state-loss training-state)
                                      (state-metrics training-state))))
             port))))

(define (deserialize-optimizer-state opt-state)
  "Deserialize optimizer state and create appropriate optimizer"
  (let ((opt-type (car opt-state)))
    (case opt-type
      ((adam)
       (let ((optimizer (make-adam-optimizer (list-ref opt-state 1) 
                                           (list-ref opt-state 2)
                                           (list-ref opt-state 3)
                                           (list-ref opt-state 4)
                                           (list-ref opt-state 5)
                                           (list-ref opt-state 6)
                                           (list-ref opt-state 7))))
         optimizer))
      ((sgd)
       (let ((optimizer (make-sgd-optimizer (list-ref opt-state 1)
                                          (list-ref opt-state 2)
                                          (list-ref opt-state 3)
                                          (list-ref opt-state 4)
                                          (list-ref opt-state 5))))
         optimizer))
      ((rmsprop)
       (let ((optimizer (make-rmsprop-optimizer (list-ref opt-state 1)
                                              (list-ref opt-state 2)
                                              (list-ref opt-state 3)
                                              (list-ref opt-state 4)
                                              (list-ref opt-state 5)
                                              (list-ref opt-state 6)
                                              (list-ref opt-state 7)
                                              (list-ref opt-state 8))))
         optimizer))
      ((adagrad)
       (let ((optimizer (make-adagrad-optimizer (list-ref opt-state 1)
                                              (list-ref opt-state 2)
                                              (list-ref opt-state 3)
                                              (list-ref opt-state 4)
                                              (list-ref opt-state 5))))
         optimizer))
      (else (error "Unknown optimizer type in checkpoint")))))

(define (load-checkpoint filename model)
  "Load model checkpoint from file and return optimizer and training state"
  (call-with-input-file filename
    (lambda (port)
      (let ((checkpoint (read port)))
        (model 'set-parameters (assoc-ref checkpoint 'model-params))
        (let ((optimizer (deserialize-optimizer-state (assoc-ref checkpoint 'optimizer-state)))
              (train-state-data (assoc-ref checkpoint 'training-state)))
          (values optimizer
                  (make-training-state (list-ref train-state-data 0)
                                      (list-ref train-state-data 1)
                                      (list-ref train-state-data 2)
                                      (list-ref train-state-data 3))))))))

;;; Main Pretraining Function

(define* (pretrain-model model data config #:key 
                        (checkpoint-dir "./checkpoints")
                        (device 'cpu))
  "Main pretraining function"
  (let* ((batch-size (assoc-ref config 'batch-size))
         (num-epochs (assoc-ref config 'num-epochs))
         (learning-rate (assoc-ref config 'learning-rate))
         (checkpoint-interval (assoc-ref config 'checkpoint-interval))
         (seq-length (assoc-ref config 'seq-length))
         (stride (assoc-ref config 'stride)))
    
    ;; Split data
    (let-values (((train-data val-data test-data) 
                  (split-data data #:train-ratio 0.9 #:val-ratio 0.05)))
      
      ;; Create data loaders
      (let ((train-loader (create-data-loader train-data 
                                             #:batch-size batch-size
                                             #:seq-length seq-length
                                             #:stride stride
                                             #:shuffle? #t))
            (val-loader (create-data-loader val-data
                                           #:batch-size batch-size
                                           #:seq-length seq-length
                                           #:stride stride
                                           #:shuffle? #f))
            (optimizer (create-optimizer-from-config config))
            (training-state (make-training-state 0 0 0.0 '())))
        
        ;; Training loop
        (do ((epoch 0 (+ epoch 1)))
            ((>= epoch num-epochs))
          
          (format #t "Epoch ~a/~a~%" (+ epoch 1) num-epochs)
          
          ;; Train
          (let ((train-loss (train-epoch model train-loader optimizer device)))
            (format #t "  Train Loss: ~,4f~%" train-loss)
            
            ;; Validate
            (let ((val-loss (validate model val-loader)))
              (format #t "  Val Loss: ~,4f~%" val-loss)
              
              ;; Update training state
              (set-state-epoch! training-state (+ epoch 1))
              (set-state-loss! training-state val-loss)
              
              ;; Save checkpoint if needed
              (when (zero? (modulo (state-step training-state) checkpoint-interval))
                (let ((checkpoint-file (format #f "~a/checkpoint_epoch_~a.scm" 
                                              checkpoint-dir epoch)))
                  (save-checkpoint model optimizer training-state checkpoint-file)
                  (format #t "  Saved checkpoint: ~a~%" checkpoint-file))))))
        
        training-state))))

;;; Helper function for assoc-ref
(define (assoc-ref alist key)
  (let ((entry (assoc key alist)))
    (if entry
        (cdr entry)
        #f)))

;;; pretraining.scm ends here
