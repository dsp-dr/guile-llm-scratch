#!/bin/bash
# Setup tmux sessions for multi-agent LLM implementation

PROJECT_DIR="$HOME/ghq/github.com/dsp-dr/guile-llm-scratch"

echo "Setting up tmux sessions for multi-agent LLM implementation..."

# Kill existing sessions if they exist
tmux kill-session -t llm-foundation 2>/dev/null
tmux kill-session -t llm-architecture 2>/dev/null
tmux kill-session -t llm-training 2>/dev/null
tmux kill-session -t llm-application 2>/dev/null
tmux kill-session -t llm-coordinator 2>/dev/null

# Create coordinator session with 4-pane layout for monitoring
tmux new-session -d -s llm-coordinator -n main -c "$PROJECT_DIR"
tmux split-window -h -t llm-coordinator:main
tmux split-window -v -t llm-coordinator:main.0
tmux split-window -v -t llm-coordinator:main.2

# Set up coordinator panes
tmux send-keys -t llm-coordinator:main.0 'echo "=== COORDINATOR DASHBOARD ===" && git status' C-m
tmux send-keys -t llm-coordinator:main.1 'echo "=== AGENT 1: FOUNDATION ===" && gh issue list --label agent-1-foundation' C-m
tmux send-keys -t llm-coordinator:main.2 'echo "=== AGENT 2: ARCHITECTURE ===" && gh issue list --label agent-2-architecture' C-m
tmux send-keys -t llm-coordinator:main.3 'echo "=== AGENTS 3&4: TRAINING/APP ===" && gh issue list --label agent-3-training,agent-4-application' C-m

# Create Agent 1: Foundation (Chapters 1-2)
tmux new-session -d -s llm-foundation -n work -c "$PROJECT_DIR"
tmux send-keys -t llm-foundation:work 'echo "=== AGENT 1: FOUNDATION SPECIALIST ===" && echo "Book: book_chapters/fundamentals.pdf" && echo "Modules: src/llm/fundamentals.scm, src/llm/text.scm" && echo "Issues: #1, #2, #3" && echo ""' C-m
tmux split-window -h -t llm-foundation:work -c "$PROJECT_DIR"
tmux send-keys -t llm-foundation:work.1 'ls -la book_chapters/fundamentals.pdf src/llm/fundamentals.scm src/llm/text.scm' C-m

# Create Agent 2: Architecture (Chapters 3-4)
tmux new-session -d -s llm-architecture -n work -c "$PROJECT_DIR"
tmux send-keys -t llm-architecture:work 'echo "=== AGENT 2: ARCHITECTURE SPECIALIST ===" && echo "Book: book_chapters/attention_architecture.pdf" && echo "Modules: src/llm/attention.scm, src/llm/gpt.scm" && echo "Issues: #4, #5, #6" && echo ""' C-m
tmux split-window -h -t llm-architecture:work -c "$PROJECT_DIR"
tmux send-keys -t llm-architecture:work.1 'ls -la book_chapters/attention_architecture.pdf src/llm/attention.scm src/llm/gpt.scm' C-m

# Create Agent 3: Training (Chapter 5)
tmux new-session -d -s llm-training -n work -c "$PROJECT_DIR"
tmux send-keys -t llm-training:work 'echo "=== AGENT 3: TRAINING SPECIALIST ===" && echo "Book: book_chapters/training.pdf" && echo "Module: src/llm/pretrain.scm" && echo "Issues: #7, #8, #9" && echo ""' C-m
tmux split-window -h -t llm-training:work -c "$PROJECT_DIR"
tmux send-keys -t llm-training:work.1 'ls -la book_chapters/training.pdf src/llm/pretrain.scm' C-m

# Create Agent 4: Application (Chapters 6-7)
tmux new-session -d -s llm-application -n work -c "$PROJECT_DIR"
tmux send-keys -t llm-application:work 'echo "=== AGENT 4: APPLICATION SPECIALIST ===" && echo "Book: book_chapters/fine_tuning.pdf" && echo "Modules: src/llm/finetune.scm, src/llm/instruction.scm" && echo "Issues: #10, #11, #12" && echo ""' C-m
tmux split-window -h -t llm-application:work -c "$PROJECT_DIR"
tmux send-keys -t llm-application:work.1 'ls -la book_chapters/fine_tuning.pdf src/llm/finetune.scm src/llm/instruction.scm' C-m

echo ""
echo "✅ Tmux sessions created successfully!"
echo ""
echo "Available sessions:"
echo "  - llm-coordinator   : Central coordination and monitoring"
echo "  - llm-foundation    : Agent 1 - Chapters 1-2 (Foundation)"
echo "  - llm-architecture  : Agent 2 - Chapters 3-4 (Architecture)"
echo "  - llm-training      : Agent 3 - Chapter 5 (Training)"
echo "  - llm-application   : Agent 4 - Chapters 6-7 (Application)"
echo ""
echo "To attach to a session: tmux attach -t <session-name>"
echo "To start an agent: tmux send-keys -t <session>:work.0 'claude' C-m"
echo ""
echo "Quick start all agents:"
echo "  ./start_all_agents.sh"