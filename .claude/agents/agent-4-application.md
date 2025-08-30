# Agent 4: Application Layer

## Identity
**Name**: Application Agent  
**Session**: `llm-application`  
**Focus**: Fine-tuning and instruction tuning applications  

## Current Assignment
- **Primary Module**: `src/llm/finetune.scm` (205 lines ✅ Complete Base)
- **Secondary Module**: `src/llm/instruction.scm` (25 lines 🔨 In Progress)  
- **Current Task**: Complete instruction tuning and RLHF implementation
- **Resource Guide**: `agent_resources/finetune_guide.txt` (2KB)

## Completed Implementations
### finetune.scm (205 lines)
- **Classification Head**: Linear layer with softmax output
- **Fine-tuning Pipeline**: Task-specific adaptation framework
- **Loss Computation**: Cross-entropy and custom loss functions
- **Evaluation Metrics**: Accuracy, precision, recall calculations
- **Data Processing**: Task-specific data preparation

## Current Focus Areas
### instruction.scm (Priority)
1. **Instruction Tuning Framework**
   - Instruction-response pair processing
   - Prompt template management
   - Response quality evaluation

2. **RLHF Basics** 
   - Reward model integration
   - Policy gradient foundation
   - Human feedback simulation
   - Preference ranking

3. **Integration Features**
   - Connection with fine-tuning pipeline
   - Multi-task learning support
   - Evaluation on instruction following

## Key Functions Implemented
```scheme
;; finetune.scm (Complete)
(create-classification-head input-dim num-classes #:optional (dropout 0.1))
(fine-tune-model model training-data config)
(compute-classification-loss predictions targets)
(evaluate-model model test-data)

;; instruction.scm (In Progress)  
(create-instruction-dataset instructions responses)
(instruction-tuning model dataset config)
(evaluate-instruction-following model test-instructions)
```

## Dependencies
- **Uses**: Agent 2's models, Agent 3's training infrastructure
- **Provides to**: End-user applications, model serving
- **Integrates with**: All other components for complete pipeline

## Critical Next Steps
1. **Complete instruction.scm**
   - Instruction processing pipeline
   - Response generation evaluation  
   - Basic RLHF components
   - Integration testing

2. **Advanced Applications**
   - Multi-task fine-tuning
   - Domain adaptation utilities
   - Model serving preparation

## Resource Location
- **Guide**: `agent_resources/finetune_guide.txt`
- **Book Content**: `book_chapters_tiny/fine_tuning/` (149 pages)
- **Reference**: Complete finetune.scm for patterns

## Recovery Commands
```bash
tmux attach -t llm-application
# If session missing:
tmux new-session -d -s llm-application -c /home/dsp-dr/ghq/github.com/dsp-dr/guile-llm-scratch
tmux send-keys -t llm-application "claude --dangerously-skip-permissions" C-m
```

## Success Metrics
- [x] Classification head implementation
- [x] Fine-tuning pipeline framework
- [x] Loss functions and evaluation
- [ ] Complete instruction tuning module
- [ ] RLHF basic implementation  
- [ ] Integration with other agents' work
- [ ] End-to-end application testing

## Integration Priority
As the application layer, this agent is responsible for bringing together:
- Agent 1's tokenization
- Agent 2's models  
- Agent 3's training
- Into working applications that users can actually use