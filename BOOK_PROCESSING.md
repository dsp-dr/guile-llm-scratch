# Book Processing System

## Overview
This document describes the PDF processing system for "Build a Large Language Model (From Scratch)" and its integration with the Guile implementation.

## Tools Created

### 1. `tools/pdf_analyzer.py`
- Analyzes PDF structure and extracts table of contents
- Maps book page numbers to PDF page numbers
- Outputs structure to JSON for further processing

### 2. `tools/pdf_splitter.py`
- Splits PDFs by chapters or fixed page sizes
- Maintains metadata about splits
- Can extract specific page ranges or sections

### 3. `tools/extract_toc.py`
- Extracts and estimates book structure
- Creates implementation plan mapping chapters to code modules
- Generates `book_structure.json` and `implementation_plan.json`

### 4. `tools/book_processor.py`
- Main orchestration tool
- Creates optimized splits aligned with implementation phases
- Generates Guile scaffold structure

## Book Splits

The book has been divided into 4 manageable PDFs in `book_chapters/`:

| Split | Size | Pages | Content | Chapters |
|-------|------|-------|---------|----------|
| `fundamentals.pdf` | 9.7 MB | 50 | LLM fundamentals and text processing | 1-2 |
| `attention_architecture.pdf` | 4.5 MB | 100 | Attention mechanisms and GPT architecture | 3-4 |
| `training.pdf` | 4.5 MB | 70 | Pretraining on unlabeled data | 5 |
| `fine_tuning.pdf` | 5.6 MB | 150 | Fine-tuning and instruction following | 6-7 |

## Implementation Mapping

### Phase 1: Foundation (Chapters 1-2)
- **PDF**: `fundamentals.pdf`
- **Modules**: 
  - `src/llm/fundamentals.scm` - tokenization, embeddings, model architecture
  - `src/llm/text.scm` - preprocessing, tokenizer, vocabulary
- **Deliverables**: Basic tokenizer, Text preprocessing pipeline

### Phase 2: Core Architecture (Chapters 3-4)
- **PDF**: `attention_architecture.pdf`
- **Modules**:
  - `src/llm/attention.scm` - self-attention, multi-head, positional encoding
  - `src/llm/gpt.scm` - transformer blocks, model layers, forward pass
- **Deliverables**: Attention mechanism, Basic GPT model structure

### Phase 3: Training (Chapter 5)
- **PDF**: `training.pdf`
- **Module**: `src/llm/pretrain.scm` - data loader, training loop, loss functions
- **Deliverables**: Training loop, Pretraining on sample data

### Phase 4: Application (Chapters 6-7)
- **PDF**: `fine_tuning.pdf`
- **Modules**:
  - `src/llm/finetune.scm` - classification head, task-specific training
  - `src/llm/instruction.scm` - prompt engineering, RLHF, evaluation
- **Deliverables**: Fine-tuning capabilities, Instruction following

## Usage

### Process a specific chapter split:
```bash
# Analyze a split
python3 tools/pdf_analyzer.py book_chapters/fundamentals.pdf

# Extract specific pages
python3 tools/pdf_splitter.py book_chapters/fundamentals.pdf --size 10
```

### Regenerate splits:
```bash
python3 tools/book_processor.py tmp/Build_a_Large_Language_Model_\(From_Scrat.pdf --split
```

### Update scaffold:
```bash
python3 tools/book_processor.py tmp/Build_a_Large_Language_Model_\(From_Scrat.pdf --scaffold
```

## Implementation Workflow

1. **Read**: Open the relevant PDF split for the phase you're implementing
2. **Code**: Implement in the corresponding `.scm` module
3. **Test**: Create tests in `tests/` as you progress
4. **Iterate**: Move to next phase after completing deliverables

## File Structure
```
guile-llm-scratch/
├── book_chapters/         # Split PDFs (manageable sizes)
│   ├── fundamentals.pdf
│   ├── attention_architecture.pdf
│   ├── training.pdf
│   └── fine_tuning.pdf
├── src/llm/              # Guile implementation modules
│   ├── fundamentals.scm
│   ├── text.scm
│   ├── attention.scm
│   ├── gpt.scm
│   ├── pretrain.scm
│   ├── finetune.scm
│   └── instruction.scm
├── tools/                # PDF processing tools
│   ├── pdf_analyzer.py
│   ├── pdf_splitter.py
│   ├── extract_toc.py
│   └── book_processor.py
└── tmp/                  # Original PDF
    └── Build_a_Large_Language_Model_(From_Scrat.pdf
```

## Notes

- Each split is optimized to be processable by AI tools (under 10MB)
- The structure allows incremental implementation following the book
- Modules are scaffolded with TODOs matching book chapters
- The system maintains traceability between book content and code implementation