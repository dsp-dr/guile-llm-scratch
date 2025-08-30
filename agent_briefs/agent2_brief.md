# Agent 2: Architecture Specialist - Autonomous Brief

You are Agent 2, the Architecture Specialist, working autonomously on implementing Chapters 3-4 of "Build a Large Language Model (From Scratch)".

## Your Mission
Build the complete attention mechanism and GPT architecture in Guile Scheme based on your assigned book section.

## Your Resources
- **Book**: `book_chapters/attention_architecture.pdf` (100 pages)
- **Target Modules**: `src/llm/attention.scm`, `src/llm/gpt.scm`
- **GitHub Issues**: #4, #5, #6

## Autonomous Work Plan

### Phase 1: Study Architecture (First 45 minutes)
1. Read `book_chapters/attention_architecture.pdf` thoroughly
2. Understand self-attention mechanism (Chapter 3)
3. Understand GPT architecture (Chapter 4)
4. Sketch implementation approach

### Phase 2: Attention Mechanism (Hours 1-2)
```scheme
;; In src/llm/attention.scm
;; Implement:
;; - scaled dot-product attention
;; - attention scores computation
;; - softmax function
;; - attention weights
;; - causal masking
```

### Phase 3: Multi-Head Attention (Hours 2-3)
```scheme
;; In src/llm/attention.scm (continued)
;; Implement:
;; - multi-head attention layer
;; - query, key, value projections
;; - head splitting and merging
;; - output projection
```

### Phase 4: GPT Architecture (Hours 3-5)
```scheme
;; In src/llm/gpt.scm
;; Implement:
;; - transformer block
;; - layer normalization
;; - feed-forward network
;; - residual connections
;; - full GPT model
;; - positional encoding
```

## Specific Tasks to Complete

1. **Issue #4: Self-attention mechanism**
   - Read Chapter 3 on attention
   - Implement scaled dot-product attention
   - Add attention masking for causality
   - Create attention visualization helpers

2. **Issue #5: Multi-head attention**
   - Implement parallel attention heads
   - Create linear projections (W_q, W_k, W_v)
   - Build head concatenation logic
   - Add dropout for regularization

3. **Issue #6: GPT structure**
   - Read Chapter 4 on transformer architecture
   - Build transformer blocks
   - Stack multiple layers
   - Add positional embeddings
   - Create model initialization

## Implementation Guidelines

- Use matrix operations efficiently
- Implement numerical stability (e.g., in softmax)
- Make layers composable
- Support variable sequence lengths
- Include shape assertions for debugging

## Dependencies & Integration

You may need stub implementations from Agent 1:
```scheme
;; Temporary stubs if Agent 1's work isn't ready
(define (get-vocab-size) 50000)  ; placeholder
(define (encode-text text) '())   ; placeholder
```

Your exports for Agent 3:
```scheme
#:export (self-attention
          multi-head-attention
          transformer-block
          gpt-model
          forward-pass
          get-model-parameters
          initialize-model)
```

## Mathematical Functions Needed

Implement these utilities first:
```scheme
;; Matrix operations
(define (matrix-multiply A B) ...)
(define (softmax scores) ...)
(define (layer-norm x) ...)
(define (gelu x) ...)  ; or relu

;; Shape utilities
(define (reshape tensor shape) ...)
(define (transpose matrix) ...)
```

## Autonomous Operation Instructions

1. Start with mathematical utilities
2. Build attention from bottom up
3. Test with small examples
4. Gradually increase complexity
5. Commit after each major component
6. Document tensor shapes clearly
7. Update issues with progress

## Success Criteria
- [ ] Self-attention working
- [ ] Multi-head attention implemented
- [ ] Transformer block complete
- [ ] Full GPT model assembled
- [ ] Forward pass functional
- [ ] Shape tests passing

Begin by reading the PDF to understand attention mechanisms, then implement the mathematical utilities.