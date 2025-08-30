# Agent 3: Training Specialist - Autonomous Brief

You are Agent 3, the Training Specialist, working autonomously on implementing Chapter 5 of "Build a Large Language Model (From Scratch)".

## Your Mission
Implement the complete pretraining pipeline including data loading, training loop, and optimization in Guile Scheme.

## Your Resources
- **Book**: `book_chapters/training.pdf` (70 pages)
- **Target Module**: `src/llm/pretrain.scm`
- **GitHub Issues**: #7, #8, #9

## Autonomous Work Plan

### Phase 1: Understand Training (First 45 minutes)
1. Read `book_chapters/training.pdf` thoroughly
2. Understand pretraining objectives
3. Study optimization techniques
4. Plan data pipeline architecture

### Phase 2: Data Pipeline (Hours 1-2)
```scheme
;; In src/llm/pretrain.scm
;; Implement:
;; - data loader for text files
;; - batch creation
;; - sequence padding
;; - data shuffling
;; - train/validation split
```

### Phase 3: Training Components (Hours 2-3)
```scheme
;; Implement:
;; - loss functions (cross-entropy)
;; - gradient computation
;; - optimizer (Adam or SGD)
;; - learning rate scheduling
;; - gradient clipping
```

### Phase 4: Training Loop (Hours 3-4)
```scheme
;; Implement:
;; - forward pass integration
;; - backward pass
;; - parameter updates
;; - validation loop
;; - metrics tracking
;; - checkpoint saving
```

## Specific Tasks to Complete

1. **Issue #7: Data loader implementation**
   - Create batch generator
   - Implement sliding window for sequences
   - Add data augmentation options
   - Build efficient data pipeline

2. **Issue #8: Training loop and optimization**
   - Implement main training loop
   - Add gradient accumulation
   - Create optimizer state management
   - Build learning rate scheduler

3. **Issue #9: Checkpoint and persistence**
   - Save model parameters
   - Save optimizer state
   - Save training metrics
   - Implement resume capability

## Implementation Strategy

### Data Structures
```scheme
;; Training batch structure
(define-record-type <batch>
  (make-batch input-ids target-ids mask)
  batch?
  (input-ids batch-input-ids)
  (target-ids batch-target-ids)
  (mask batch-mask))

;; Training state
(define-record-type <training-state>
  (make-training-state epoch step loss metrics)
  training-state?
  (epoch state-epoch)
  (step state-step)
  (loss state-loss)
  (metrics state-metrics))
```

### Core Functions
```scheme
#:export (create-data-loader
          train-epoch
          validate
          save-checkpoint
          load-checkpoint
          compute-loss
          update-parameters
          pretrain-model)
```

## Dependencies & Stubs

If Agent 1 & 2's work isn't ready, use stubs:
```scheme
;; Temporary implementations
(define (tokenize text) (string->list text))
(define (forward-pass model batch) (make-list 100 0.1))
(define (backward-pass loss) '())
```

## Training Configuration
```scheme
(define default-config
  '((batch-size . 32)
    (learning-rate . 0.0001)
    (num-epochs . 10)
    (warmup-steps . 1000)
    (gradient-clip . 1.0)
    (checkpoint-interval . 1000)))
```

## Autonomous Operation Instructions

1. Start with data loading utilities
2. Implement loss functions
3. Build training loop incrementally
4. Test with dummy data first
5. Add checkpointing early
6. Monitor memory usage
7. Commit after each component

## Optimization Priorities

1. **Correctness first** - Ensure math is right
2. **Memory efficiency** - Batch processing
3. **Speed optimization** - Later iteration
4. **Numerical stability** - Prevent overflow/underflow

## Success Criteria
- [ ] Data loader creates proper batches
- [ ] Loss computation works
- [ ] Training loop runs without errors
- [ ] Checkpoints save/load correctly
- [ ] Metrics are tracked
- [ ] Memory usage is reasonable

Begin by reading the PDF to understand training dynamics, then implement the data loader.