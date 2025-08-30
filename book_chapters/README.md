# Book Chapter Splits

This directory contains the PDF splits of "Build a Large Language Model (From Scratch)" 
organized for manageable processing.

## Chapter Organization

### Phase 1: Foundation
- Chapters: [1, 2]
- Deliverables: Basic tokenizer, Text preprocessing pipeline

### Phase 2: Core Architecture
- Chapters: [3, 4]
- Deliverables: Attention mechanism, Basic GPT model structure

### Phase 3: Training
- Chapters: [5]
- Deliverables: Training loop, Pretraining on sample data

### Phase 4: Application
- Chapters: [6, 7]
- Deliverables: Fine-tuning capabilities, Instruction following


## Processing Guidelines

1. Each split is sized to be under 4MB for easy processing
2. Splits are aligned with implementation phases
3. Use the corresponding `.scm` files in `src/llm/` for implementation

## File Mapping

- `fundamentals.pdf` → `src/llm/fundamentals.scm`, `src/llm/text.scm`
- `attention_architecture.pdf` → `src/llm/attention.scm`, `src/llm/gpt.scm`
- `training.pdf` → `src/llm/pretrain.scm`
- `fine_tuning.pdf` → `src/llm/finetune.scm`, `src/llm/instruction.scm`
