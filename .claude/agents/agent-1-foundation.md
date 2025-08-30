# Agent 1: Foundation Layer

## Identity
**Name**: Foundation Agent  
**Session**: `llm-foundation`  
**Focus**: Core tokenization and text processing  

## Current Assignment
- **Primary Module**: `src/llm/fundamentals.scm` (216 lines ✅ Complete)
- **Secondary Module**: `src/llm/text.scm` (99 lines ✅ Complete)
- **Current Task**: Optimize and enhance text preprocessing functions
- **Resource Guide**: `agent_resources/text_guide.txt` (2KB)

## Completed Implementations
### fundamentals.scm (216 lines)
- BPE tokenizer from scratch
- Vocabulary building and management
- Token encoding/decoding functions
- Merge operations and statistics

### text.scm (99 lines)  
- Text preprocessing pipeline
- Corpus loading and cleaning
- Text statistics and normalization
- Integration with tokenizer

## Current Focus Areas
1. **Text Preprocessing Optimization**
   - Enhance cleaning functions
   - Add more text normalization options
   - Improve corpus loading efficiency

2. **Integration Support**
   - Ensure clean interfaces with other modules
   - Add comprehensive error handling
   - Document all exported functions

## Key Functions Implemented
```scheme
;; fundamentals.scm
(bpe-tokenize text num-merges)
(build-vocabulary corpus)
(encode-text tokenizer text)
(decode-tokens tokenizer tokens)

;; text.scm  
(preprocess-text text)
(load-corpus file-path)
(text-statistics corpus)
(normalize-text text)
```

## Dependencies
- Uses: Core Guile modules, SRFI libraries
- Provides to: All other agents (tokenization services)

## Resource Location
- **Guide**: `agent_resources/text_guide.txt`
- **Book Content**: `book_chapters_tiny/fundamentals/` (if needed)
- **Tests**: Available for validation

## Recovery Commands
```bash
tmux attach -t llm-foundation
# If session missing:
tmux new-session -d -s llm-foundation -c /home/dsp-dr/ghq/github.com/dsp-dr/guile-llm-scratch
tmux send-keys -t llm-foundation "claude --dangerously-skip-permissions" C-m
```