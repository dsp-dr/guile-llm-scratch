# Agent Assignment Briefs

## Agent 1: Foundation Specialist

### Mission
Implement the foundational text processing and tokenization components based on Chapters 1-2 of "Build a Large Language Model (From Scratch)".

### Resources
- **Book Reference**: `book_chapters/fundamentals.pdf` (50 pages)
- **Target Modules**: 
  - `src/llm/fundamentals.scm`
  - `src/llm/text.scm`
- **GitHub Issues**: #1, #2, #3
- **Tmux Session**: `llm-foundation`

### Primary Tasks
1. **Tokenizer Implementation** (Issue #1)
   - Byte-pair encoding (BPE)
   - Vocabulary building
   - Special token handling
   
2. **Text Preprocessing** (Issue #2)
   - Data cleaning utilities
   - Text normalization
   - Corpus preparation
   
3. **Embeddings & Vocabulary** (Issue #3)
   - Token-to-ID mapping
   - Embedding layer implementation
   - Vocabulary management

### Success Criteria
- [ ] All unit tests passing
- [ ] Compatible interfaces for Agent 2
- [ ] Documentation complete
- [ ] Example usage provided

### Command to Start
```bash
tmux attach -t llm-foundation
# In pane 0, run: claude
# Initial prompt: "I am Agent 1 working on Foundation. Read book_chapters/fundamentals.pdf and implement the tokenizer based on Chapter 1 in src/llm/fundamentals.scm. Reference GitHub issue #1 for requirements."
```

---

## Agent 2: Architecture Specialist

### Mission
Build the attention mechanisms and GPT model architecture based on Chapters 3-4.

### Resources
- **Book Reference**: `book_chapters/attention_architecture.pdf` (100 pages)
- **Target Modules**:
  - `src/llm/attention.scm`
  - `src/llm/gpt.scm`
- **GitHub Issues**: #4, #5, #6
- **Tmux Session**: `llm-architecture`

### Primary Tasks
1. **Self-Attention** (Issue #4)
   - Scaled dot-product attention
   - Attention weight computation
   - Masking strategies
   
2. **Multi-Head Attention** (Issue #5)
   - Parallel attention heads
   - Linear projections
   - Output concatenation
   
3. **GPT Architecture** (Issue #6)
   - Transformer blocks
   - Layer normalization
   - Feed-forward networks

### Dependencies
- Requires: Token embeddings from Agent 1
- Provides: Model architecture for Agent 3

### Command to Start
```bash
tmux attach -t llm-architecture
# In pane 0, run: claude
# Initial prompt: "I am Agent 2 working on Architecture. Read book_chapters/attention_architecture.pdf and implement the self-attention mechanism from Chapter 3 in src/llm/attention.scm. Reference GitHub issue #4."
```

---

## Agent 3: Training Specialist

### Mission
Implement the pretraining pipeline and optimization based on Chapter 5.

### Resources
- **Book Reference**: `book_chapters/training.pdf` (70 pages)
- **Target Module**: `src/llm/pretrain.scm`
- **GitHub Issues**: #7, #8, #9
- **Tmux Session**: `llm-training`

### Primary Tasks
1. **Data Loader** (Issue #7)
   - Batch preparation
   - Data shuffling
   - Sequence padding
   
2. **Training Loop** (Issue #8)
   - Forward pass
   - Loss computation
   - Backpropagation
   
3. **Checkpointing** (Issue #9)
   - Model serialization
   - Training state persistence
   - Resume capability

### Dependencies
- Requires: Model from Agent 2, Tokenizer from Agent 1
- Provides: Trained model for Agent 4

### Command to Start
```bash
tmux attach -t llm-training
# In pane 0, run: claude
# Initial prompt: "I am Agent 3 working on Training. Read book_chapters/training.pdf and implement the data loader from Chapter 5 in src/llm/pretrain.scm. Reference GitHub issue #7."
```

---

## Agent 4: Application Specialist

### Mission
Implement fine-tuning and instruction-following capabilities based on Chapters 6-7.

### Resources
- **Book Reference**: `book_chapters/fine_tuning.pdf` (150 pages)
- **Target Modules**:
  - `src/llm/finetune.scm`
  - `src/llm/instruction.scm`
- **GitHub Issues**: #10, #11, #12
- **Tmux Session**: `llm-application`

### Primary Tasks
1. **Fine-tuning Pipeline** (Issue #10)
   - Task-specific heads
   - Transfer learning
   - Gradient freezing
   
2. **Instruction Tuning** (Issue #11)
   - Prompt templates
   - Response generation
   - RLHF basics
   
3. **Evaluation Metrics** (Issue #12)
   - Perplexity calculation
   - BLEU scores
   - Custom metrics

### Dependencies
- Requires: All previous agent outputs
- Provides: Complete application layer

### Command to Start
```bash
tmux attach -t llm-application
# In pane 0, run: claude
# Initial prompt: "I am Agent 4 working on Application. Read book_chapters/fine_tuning.pdf and implement the fine-tuning pipeline from Chapter 6 in src/llm/finetune.scm. Reference GitHub issue #10."
```

---

## Coordinator: Integration Manager

### Mission
Oversee all agents, manage integration points, and ensure project coherence.

### Resources
- **Monitoring Dashboard**: `llm-coordinator`
- **All GitHub Issues**: #1-#12
- **Integration Points**: All module interfaces

### Responsibilities
1. **Progress Tracking**
   - Monitor issue completion
   - Track blockers
   - Manage dependencies
   
2. **Integration Management**
   - Review merge requests
   - Resolve conflicts
   - Test integration points
   
3. **Quality Assurance**
   - Code review
   - Test coverage
   - Documentation review

### Command to Monitor
```bash
tmux attach -t llm-coordinator
# View all agent progress across 4 panes
```

### Daily Workflow
1. **Morning**: Check overnight progress
2. **Midday**: Resolve blockers
3. **Evening**: Integration testing

---

## Communication Protocol

### Issue Updates
Each agent should update their issues with:
- Progress percentage
- Blockers encountered
- Dependencies needed
- Estimated completion

### Commit Messages
Use conventional commits:
```
feat(module): implement function_name based on section X.Y
fix(module): correct issue in function_name
docs(module): add documentation for feature
test(module): add tests for function_name
```

### Integration Checkpoints
- **Day 2**: Agent 1 → Agent 2 (tokenizer interface)
- **Day 3**: Agent 1 → Agent 3 (data preprocessing)
- **Day 4**: Agent 2 → Agent 3 (model architecture)
- **Day 5**: All → Agent 4 (complete pipeline)

---

## Quick Reference

### Start All Agents
```bash
bash start_all_agents.sh
```

### Monitor Progress
```bash
# Check all issues
gh issue list --milestone "Milestone 1: Foundation Implementation"

# Check specific agent
gh issue list --label agent-1-foundation

# View coordinator dashboard
tmux attach -t llm-coordinator
```

### Integration Commands
```bash
# Merge agent work
git checkout main
git merge agent-1-foundation
git merge agent-2-architecture

# Run tests
guile -L src tests/run-all-tests.scm
```