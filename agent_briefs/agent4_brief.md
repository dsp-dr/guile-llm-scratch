# Agent 4: Application Specialist - Autonomous Brief

You are Agent 4, the Application Specialist, working autonomously on implementing Chapters 6-7 of "Build a Large Language Model (From Scratch)".

## Your Mission
Implement fine-tuning, instruction following, and application layer features in Guile Scheme.

## Your Resources
- **Book**: `book_chapters/fine_tuning.pdf` (150 pages)
- **Target Modules**: `src/llm/finetune.scm`, `src/llm/instruction.scm`
- **GitHub Issues**: #10, #11, #12

## Autonomous Work Plan

### Phase 1: Study Applications (First hour)
1. Read `book_chapters/fine_tuning.pdf` thoroughly
2. Understand fine-tuning strategies (Chapter 6)
3. Understand instruction tuning (Chapter 7)
4. Plan implementation approach

### Phase 2: Fine-tuning Pipeline (Hours 1-3)
```scheme
;; In src/llm/finetune.scm
;; Implement:
;; - task-specific heads
;; - classification layers
;; - transfer learning setup
;; - parameter freezing
;; - few-shot learning
```

### Phase 3: Instruction Following (Hours 3-4)
```scheme
;; In src/llm/instruction.scm
;; Implement:
;; - prompt templates
;; - instruction parsing
;; - response generation
;; - chain-of-thought prompting
;; - RLHF basics (if covered)
```

### Phase 4: Evaluation & Utilities (Hours 4-5)
```scheme
;; Implement:
;; - perplexity calculation
;; - BLEU score
;; - accuracy metrics
;; - generation utilities
;; - inference optimization
```

## Specific Tasks to Complete

1. **Issue #10: Fine-tuning pipeline**
   - Read Chapter 6 on fine-tuning
   - Implement classification head
   - Create fine-tuning data loader
   - Build transfer learning utilities
   - Add gradient freezing options

2. **Issue #11: Instruction tuning**
   - Read Chapter 7 on instructions
   - Create prompt engineering tools
   - Implement instruction templates
   - Build response generation
   - Add context management

3. **Issue #12: Evaluation metrics**
   - Implement standard metrics
   - Create benchmarking tools
   - Build inference utilities
   - Add generation strategies

## Implementation Components

### Fine-tuning Architecture
```scheme
;; Task-specific heads
(define-record-type <classification-head>
  (make-classification-head input-dim num-classes dropout)
  classification-head?
  (input-dim head-input-dim)
  (num-classes head-num-classes)
  (dropout head-dropout))

;; Fine-tuning configuration
(define-record-type <finetune-config>
  (make-finetune-config task learning-rate freeze-layers epochs)
  finetune-config?
  (task config-task)
  (learning-rate config-lr)
  (freeze-layers config-freeze)
  (epochs config-epochs))
```

### Instruction Processing
```scheme
;; Prompt templates
(define instruction-templates
  '((qa . "Question: ~a\nAnswer:")
    (summarize . "Summarize the following text:\n~a\nSummary:")
    (translate . "Translate to ~a:\n~a\nTranslation:")
    (complete . "Complete the following:\n~a")))

;; Response generation
(define (generate-response model prompt config)
  ;; Implement various generation strategies
  ;; - greedy decoding
  ;; - beam search
  ;; - sampling with temperature
  ;; - top-k sampling
  ;; - top-p (nucleus) sampling
  ...)
```

### Exports
```scheme
#:export (finetune-model
          create-classification-head
          freeze-parameters
          unfreeze-parameters
          generate-text
          apply-instruction-template
          evaluate-model
          calculate-perplexity
          calculate-bleu-score)
```

## Dependencies & Integration

You'll need outputs from all previous agents:
```scheme
;; From Agent 1
(use-modules (llm fundamentals)
             (llm text))

;; From Agent 2
(use-modules (llm attention)
             (llm gpt))

;; From Agent 3
(use-modules (llm pretrain))
```

If not available, create minimal stubs to continue.

## Application Examples to Create

1. **Text Classification**
   - Sentiment analysis
   - Topic classification
   - Spam detection

2. **Text Generation**
   - Story continuation
   - Code completion
   - Dialogue generation

3. **Question Answering**
   - Factual QA
   - Reading comprehension
   - Open-ended QA

## Autonomous Operation Instructions

1. Start with fine-tuning infrastructure
2. Implement one task type completely
3. Test with small examples
4. Build instruction handling
5. Add evaluation metrics
6. Create demo applications
7. Document usage examples

## Priority Order

1. **Core fine-tuning** - Enable task adaptation
2. **Basic generation** - Greedy decoding first
3. **Instruction templates** - Common patterns
4. **Evaluation metrics** - Measure performance
5. **Advanced generation** - Beam search, sampling
6. **Demo applications** - Show capabilities

## Success Criteria
- [ ] Fine-tuning pipeline works
- [ ] Can adapt to new tasks
- [ ] Instruction following implemented
- [ ] Text generation functional
- [ ] Metrics calculate correctly
- [ ] Demo applications run

Begin by reading the PDF to understand fine-tuning approaches, then implement the classification head.