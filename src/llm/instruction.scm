;;; instruction.scm --- Instruction Tuning and RLHF Implementation
;;; Chapter 7: Fine-tuning to Follow Instructions

(define-module (llm instruction)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-43)
  #:export (make-instruction-dataset
            create-instruction-dataset
            instruction-template
            apply-instruction-template
            instruction-tuning
            create-preference-dataset
            reward-model
            train-reward-model
            ppo-step
            rlhf-training
            compute-kl-divergence
            evaluate-instruction-following
            response-quality-score
            instruction-batch-processor
            format-alpaca-style
            format-chat-style))

;;; Commentary:
;;;
;;; This module implements instruction tuning and RLHF concepts from Chapter 7
;;; of "Build a Large Language Model (From Scratch)"
;;;
;;; Key components:
;;; - Instruction dataset creation and formatting
;;; - Supervised fine-tuning for instruction following
;;; - Reward modeling for RLHF
;;; - PPO-based reinforcement learning
;;; - Evaluation metrics for instruction-tuned models
;;;

;;; Code:

;;; Instruction Dataset Structure
(define-record-type <instruction-dataset>
  (make-instruction-dataset instructions responses metadata)
  instruction-dataset?
  (instructions dataset-instructions)
  (responses dataset-responses)
  (metadata dataset-metadata))

;;; Create instruction dataset from raw data
(define* (create-instruction-dataset data #:key 
                                     (format-type 'alpaca)
                                     (max-length 512)
                                     (shuffle #t))
  "Create an instruction dataset for supervised fine-tuning.
   DATA: list of (instruction . response) pairs
   FORMAT-TYPE: 'alpaca or 'chat style formatting
   MAX-LENGTH: maximum sequence length
   SHUFFLE: whether to shuffle the dataset"
  (let* ((processed-data
          (map (lambda (item)
                 (match item
                   ((instruction . response)
                    (cons (truncate-text instruction (/ max-length 2))
                          (truncate-text response (/ max-length 2))))
                   (_ (error "Invalid data format"))))
               data))
         (shuffled-data
          (if shuffle
              (shuffle-list processed-data)
              processed-data))
         (instructions (map car shuffled-data))
         (responses (map cdr shuffled-data))
         (metadata `((format . ,format-type)
                    (max-length . ,max-length)
                    (size . ,(length data)))))
    (make-instruction-dataset instructions responses metadata)))

;;; Instruction templating
(define (instruction-template type)
  "Return instruction template for different formats"
  (case type
    ((alpaca)
     "Below is an instruction that describes a task. Write a response that appropriately completes the request.\n\n### Instruction:\n~a\n\n### Response:\n")
    ((chat)
     "User: ~a\nAssistant: ")
    ((dolly)
     "### Instruction:\n~a\n\n### Context:\n~a\n\n### Response:\n")
    (else
     "~a\n")))

(define (apply-instruction-template instruction template-type)
  "Apply formatting template to instruction"
  (format #f (instruction-template template-type) instruction))

;;; Format functions for different styles
(define (format-alpaca-style instruction response)
  "Format instruction-response pair in Alpaca style"
  (string-append
   (apply-instruction-template instruction 'alpaca)
   response))

(define (format-chat-style instruction response)
  "Format instruction-response pair in chat style"
  (string-append
   (apply-instruction-template instruction 'chat)
   response))

;;; Main instruction tuning function
(define* (instruction-tuning model dataset 
                            #:key
                            (learning-rate 1e-5)
                            (epochs 3)
                            (batch-size 4)
                            (gradient-accumulation 4)
                            (warmup-steps 100))
  "Fine-tune a model for instruction following using supervised learning.
   MODEL: pre-trained language model
   DATASET: instruction dataset
   Returns: fine-tuned model"
  (let* ((instructions (dataset-instructions dataset))
         (responses (dataset-responses dataset))
         (num-batches (ceiling (/ (length instructions) batch-size)))
         (total-steps (* epochs num-batches))
         (optimizer (make-adamw-optimizer learning-rate))
         (scheduler (make-linear-scheduler warmup-steps total-steps)))
    
    (do ((epoch 0 (+ epoch 1)))
        ((>= epoch epochs) model)
      (format #t "Epoch ~a/~a~%" (+ epoch 1) epochs)
      
      (do ((batch-idx 0 (+ batch-idx 1)))
          ((>= batch-idx num-batches))
        
        (let* ((start-idx (* batch-idx batch-size))
               (end-idx (min (* (+ batch-idx 1) batch-size)
                           (length instructions)))
               (batch-instructions (sublist instructions start-idx end-idx))
               (batch-responses (sublist responses start-idx end-idx))
               (loss (compute-instruction-loss model 
                                             batch-instructions 
                                             batch-responses)))
          
          (when (zero? (modulo batch-idx gradient-accumulation))
            (optimizer-step! optimizer model loss)
            (scheduler-step! scheduler optimizer))
          
          (when (zero? (modulo batch-idx 10))
            (format #t "  Batch ~a/~a, Loss: ~,4f~%" 
                   batch-idx num-batches loss)))))))

;;; RLHF Components

;;; Preference dataset for reward modeling
(define-record-type <preference-dataset>
  (make-preference-dataset prompts chosen rejected)
  preference-dataset?
  (prompts preference-prompts)
  (chosen preference-chosen)
  (rejected preference-rejected))

(define (create-preference-dataset comparisons)
  "Create preference dataset from human comparisons.
   COMPARISONS: list of (prompt chosen-response rejected-response)"
  (let ((prompts (map first comparisons))
        (chosen (map second comparisons))
        (rejected (map third comparisons)))
    (make-preference-dataset prompts chosen rejected)))

;;; Reward model
(define-record-type <reward-model>
  (make-reward-model-internal base-model head)
  reward-model?
  (base-model reward-model-base)
  (head reward-model-head))

(define (reward-model base-model hidden-dim)
  "Create a reward model from a base language model"
  (make-reward-model-internal 
   base-model
   (make-linear-layer hidden-dim 1)))

(define* (train-reward-model reward-model dataset
                            #:key
                            (learning-rate 1e-5)
                            (epochs 1)
                            (batch-size 8))
  "Train reward model on preference data"
  (let ((prompts (preference-prompts dataset))
        (chosen (preference-chosen dataset))
        (rejected (preference-rejected dataset)))
    
    (do ((epoch 0 (+ epoch 1)))
        ((>= epoch epochs) reward-model)
      
      (for-each
       (lambda (p c r)
         (let* ((chosen-reward (compute-reward reward-model p c))
                (rejected-reward (compute-reward reward-model p r))
                (loss (reward-ranking-loss chosen-reward rejected-reward)))
           (backward-pass! loss)
           (update-parameters! reward-model learning-rate)))
       prompts chosen rejected))))

(define (compute-reward model prompt response)
  "Compute scalar reward for a response"
  (let* ((input (string-append prompt " " response))
         (encoding (encode-text input))
         (hidden (forward-pass (reward-model-base model) encoding))
         (reward (forward-pass (reward-model-head model) hidden)))
    (vector-ref reward 0)))

(define (reward-ranking-loss chosen-reward rejected-reward)
  "Bradley-Terry ranking loss for reward model"
  (- (log (/ 1.0 (+ 1.0 (exp (- rejected-reward chosen-reward)))))))

;;; PPO (Proximal Policy Optimization) for RLHF
(define* (ppo-step policy-model ref-model prompt response reward
                  #:key
                  (clip-range 0.2)
                  (kl-penalty 0.01))
  "Single PPO update step for RLHF training"
  (let* ((policy-logprobs (compute-logprobs policy-model prompt response))
         (ref-logprobs (compute-logprobs ref-model prompt response))
         (ratio (exp (- policy-logprobs ref-logprobs)))
         (clipped-ratio (clip ratio (- 1 clip-range) (+ 1 clip-range)))
         (policy-loss (- (* (min ratio clipped-ratio) reward)))
         (kl-div (compute-kl-divergence policy-logprobs ref-logprobs))
         (total-loss (+ policy-loss (* kl-penalty kl-div))))
    total-loss))

(define (compute-logprobs model prompt response)
  "Compute log probabilities of response given prompt"
  (let* ((input (string-append prompt " " response))
         (tokens (tokenize input))
         (logits (forward-pass model tokens)))
    (sum-log-probabilities logits tokens)))

(define (compute-kl-divergence p-logprobs q-logprobs)
  "KL divergence between two distributions"
  (exp (- p-logprobs q-logprobs)))

;;; Main RLHF training loop
(define* (rlhf-training policy-model ref-model reward-model prompts
                       #:key
                       (num-iterations 1000)
                       (batch-size 4)
                       (learning-rate 1e-6)
                       (kl-coeff 0.02))
  "Train policy model using RLHF with PPO"
  (do ((iter 0 (+ iter 1)))
      ((>= iter num-iterations) policy-model)
    
    (let* ((batch-prompts (sample-batch prompts batch-size))
           (responses (map (cut generate-response policy-model <>) 
                         batch-prompts))
           (rewards (map (cut compute-reward reward-model <> <>)
                       batch-prompts responses)))
      
      (for-each
       (lambda (prompt response reward)
         (let ((loss (ppo-step policy-model ref-model 
                             prompt response reward
                             #:kl-penalty kl-coeff)))
           (backward-pass! loss)
           (update-parameters! policy-model learning-rate)))
       batch-prompts responses rewards)
      
      (when (zero? (modulo iter 100))
        (format #t "RLHF Iteration ~a, Avg Reward: ~,3f~%"
               iter (average rewards))))))

;;; Evaluation metrics
(define (evaluate-instruction-following model test-dataset)
  "Evaluate instruction-following capabilities"
  (let* ((instructions (dataset-instructions test-dataset))
         (expected (dataset-responses test-dataset))
         (generated (map (cut generate-response model <>) instructions))
         (scores (map response-quality-score generated expected)))
    `((average-score . ,(average scores))
      (coherence . ,(evaluate-coherence generated))
      (relevance . ,(evaluate-relevance generated instructions))
      (helpfulness . ,(evaluate-helpfulness generated))
      (safety . ,(evaluate-safety generated)))))

(define (response-quality-score generated expected)
  "Score response quality (0-1 scale)"
  (let* ((gen-tokens (tokenize generated))
         (exp-tokens (tokenize expected))
         (overlap (token-overlap gen-tokens exp-tokens))
         (length-penalty (min 1.0 (/ (length gen-tokens) 
                                    (max 1 (length exp-tokens)))))
         (semantic-sim (semantic-similarity generated expected)))
    (* 0.3 overlap 0.3 length-penalty 0.4 semantic-sim)))

(define (evaluate-coherence responses)
  "Evaluate response coherence"
  (average
   (map (lambda (response)
          (let* ((sentences (split-sentences response))
                 (transitions (sentence-transitions sentences)))
            (average transitions)))
        responses)))

(define (evaluate-relevance responses instructions)
  "Evaluate relevance to instructions"
  (average
   (map (lambda (response instruction)
          (keyword-overlap response instruction))
        responses instructions)))

(define (evaluate-helpfulness responses)
  "Evaluate response helpfulness"
  (average
   (map (lambda (response)
          (cond
           ((contains-actionable? response) 0.9)
           ((contains-explanation? response) 0.7)
           ((contains-answer? response) 0.5)
           (else 0.3)))
        responses)))

(define (evaluate-safety responses)
  "Evaluate response safety"
  (average
   (map (lambda (response)
          (if (safe-content? response) 1.0 0.0))
        responses)))

;;; Batch processing for efficiency
(define (instruction-batch-processor model dataset batch-size)
  "Process instruction dataset in batches"
  (let* ((instructions (dataset-instructions dataset))
         (num-batches (ceiling (/ (length instructions) batch-size)))
         (results '()))
    
    (do ((i 0 (+ i 1)))
        ((>= i num-batches) (reverse results))
      (let* ((start (* i batch-size))
             (end (min (* (+ i 1) batch-size) (length instructions)))
             (batch (sublist instructions start end))
             (batch-results (map (cut process-instruction model <>) batch)))
        (set! results (cons batch-results results))))))

(define (process-instruction model instruction)
  "Process single instruction through model"
  (let* ((formatted (apply-instruction-template instruction 'alpaca))
         (response (generate-response model formatted)))
    `((instruction . ,instruction)
      (response . ,response)
      (score . ,(score-response response)))))

;;; Helper functions
(define (truncate-text text max-length)
  "Truncate text to maximum length"
  (if (> (string-length text) max-length)
      (substring text 0 max-length)
      text))

(define (shuffle-list lst)
  "Randomly shuffle a list"
  (let ((vec (list->vector lst)))
    (do ((i (- (vector-length vec) 1) (- i 1)))
        ((< i 1) (vector->list vec))
      (let* ((j (random (+ i 1)))
             (tmp (vector-ref vec i)))
        (vector-set! vec i (vector-ref vec j))
        (vector-set! vec j tmp)))))

(define (sublist lst start end)
  "Extract sublist from start to end index"
  (take (drop lst start) (- end start)))

(define (average lst)
  "Compute average of a list of numbers"
  (if (null? lst)
      0
      (/ (apply + lst) (length lst))))

(define (clip value min-val max-val)
  "Clip value to range [min-val, max-val]"
  (max min-val (min max-val value)))

(define (sample-batch lst size)
  "Sample a batch from list"
  (if (<= (length lst) size)
      lst
      (take (shuffle-list lst) size)))

(define (tokenize text)
  "Simple tokenization (placeholder)"
  (string-split text #\space))

(define (token-overlap tokens1 tokens2)
  "Compute token overlap ratio"
  (let ((set1 (delete-duplicates tokens1))
        (set2 (delete-duplicates tokens2))
        (intersection (lset-intersection equal? set1 set2)))
    (/ (length intersection)
       (max 1 (min (length set1) (length set2))))))

(define (semantic-similarity text1 text2)
  "Placeholder for semantic similarity computation"
  0.7)

(define (split-sentences text)
  "Split text into sentences"
  (filter (lambda (s) (> (string-length s) 0))
          (string-split text #\.)))

(define (sentence-transitions sentences)
  "Score transitions between sentences"
  (if (< (length sentences) 2)
      '(1.0)
      (map (lambda (s1 s2) 0.8)
           sentences (cdr sentences))))

(define (keyword-overlap response instruction)
  "Compute keyword overlap between response and instruction"
  (let ((resp-words (tokenize (string-downcase response)))
        (inst-words (tokenize (string-downcase instruction))))
    (token-overlap resp-words inst-words)))

(define (contains-actionable? text)
  "Check if text contains actionable content"
  (any (lambda (keyword)
         (string-contains text keyword))
       '("should" "can" "will" "try" "use" "follow")))

(define (contains-explanation? text)
  "Check if text contains explanation"
  (any (lambda (keyword)
         (string-contains text keyword))
       '("because" "therefore" "thus" "since" "due to")))

(define (contains-answer? text)
  "Check if text provides an answer"
  (> (string-length text) 20))

(define (safe-content? text)
  "Check if content is safe (placeholder)"
  #t)

(define (score-response response)
  "Score a generated response"
  (cond
   ((< (string-length response) 10) 0.1)
   ((> (string-length response) 1000) 0.5)
   (else 0.7)))

(define (generate-response model prompt)
  "Generate response from model (placeholder)"
  "Generated response placeholder")

(define (make-adamw-optimizer learning-rate)
  "Create AdamW optimizer (placeholder)"
  `(adamw ,learning-rate))

(define (make-linear-scheduler warmup total)
  "Create linear learning rate scheduler (placeholder)"
  `(linear-scheduler ,warmup ,total))

(define (compute-instruction-loss model instructions responses)
  "Compute loss for instruction tuning (placeholder)"
  (random:uniform))

(define (optimizer-step! optimizer model loss)
  "Perform optimizer step (placeholder)"
  #t)

(define (scheduler-step! scheduler optimizer)
  "Update learning rate (placeholder)"
  #t)

(define (forward-pass model input)
  "Forward pass through model (placeholder)"
  input)

(define (backward-pass! loss)
  "Backward pass (placeholder)"
  #t)

(define (update-parameters! model lr)
  "Update model parameters (placeholder)"
  #t)

(define (sum-log-probabilities logits tokens)
  "Sum log probabilities (placeholder)"
  (- (random:uniform)))

(define (make-linear-layer in-dim out-dim)
  "Create linear layer (placeholder)"
  `(linear ,in-dim ,out-dim))

(define (encode-text text)
  "Encode text (placeholder)"
  (tokenize text))

;;; instruction.scm ends here