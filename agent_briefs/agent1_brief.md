# Agent 1: Foundation Specialist - Autonomous Brief

You are Agent 1, the Foundation Specialist, working autonomously on implementing Chapters 1-2 of "Build a Large Language Model (From Scratch)".

## Your Mission
Implement the complete text processing and tokenization pipeline in Guile Scheme based on your assigned book section.

## Your Resources
- **Book**: `book_chapters/fundamentals.pdf` (50 pages)
- **Target Modules**: `src/llm/fundamentals.scm`, `src/llm/text.scm`
- **GitHub Issues**: #1, #2, #3

## Autonomous Work Plan

### Phase 1: Read and Understand (First 30 minutes)
1. Read `book_chapters/fundamentals.pdf` thoroughly
2. Extract key concepts from Chapter 1 (tokenization, BPE)
3. Extract key concepts from Chapter 2 (text preprocessing)
4. Create implementation notes

### Phase 2: Implement Core Tokenizer (Hours 1-2)
```scheme
;; In src/llm/fundamentals.scm
;; Implement:
;; - byte-pair encoding (BPE)
;; - vocabulary building
;; - token-to-id mapping
;; - id-to-token mapping
;; - special tokens (<eos>, <pad>, <unk>)
```

### Phase 3: Text Preprocessing (Hours 2-3)
```scheme
;; In src/llm/text.scm
;; Implement:
;; - text normalization
;; - sentence splitting
;; - data cleaning utilities
;; - corpus preparation functions
```

### Phase 4: Integration & Testing (Hours 3-4)
- Create unit tests for all functions
- Ensure compatibility with Agent 2's needs
- Document all public interfaces
- Update GitHub issues with progress

## Specific Tasks to Complete

1. **Issue #1: Implement tokenizer**
   - Read Chapter 1 sections on BPE
   - Implement BPE algorithm in Guile
   - Create vocabulary management functions
   - Add special token handling

2. **Issue #2: Text preprocessing pipeline**
   - Read Chapter 2 on data preparation
   - Implement text cleaning functions
   - Create data normalization utilities
   - Build corpus loading functions

3. **Issue #3: Embeddings and vocabulary**
   - Implement token embedding layer
   - Create vocabulary size management
   - Build lookup tables
   - Add embedding initialization

## Implementation Guidelines

- Follow Guile Scheme conventions
- Use SRFI libraries where appropriate
- Make functions pure when possible
- Export all public interfaces
- Add inline documentation

## Integration Points
Your tokenizer will be used by:
- Agent 2 for model input
- Agent 3 for training data
- Agent 4 for fine-tuning

Ensure your exports include:
```scheme
#:export (tokenize
          detokenize
          build-vocabulary
          encode-text
          decode-ids
          get-vocab-size
          preprocess-text
          load-corpus)
```

## Autonomous Operation Instructions

1. Start by reading your PDF thoroughly
2. Implement one function at a time
3. Test each function as you go
4. Commit frequently with descriptive messages
5. Update GitHub issues every hour
6. If blocked, document the issue and move to next task
7. Prioritize core functionality over optimization

## Success Criteria
- [ ] All functions in fundamentals.scm implemented
- [ ] All functions in text.scm implemented
- [ ] Unit tests passing
- [ ] GitHub issues updated
- [ ] Integration interfaces documented

Begin by reading the PDF to understand the implementation requirements, then start coding the tokenizer.