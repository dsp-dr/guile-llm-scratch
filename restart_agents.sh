#!/bin/bash
# Restart agents with better initialization

echo "Restarting all agents with proper initialization..."

# Kill existing sessions
tmux kill-session -t llm-foundation 2>/dev/null
tmux kill-session -t llm-architecture 2>/dev/null  
tmux kill-session -t llm-training 2>/dev/null
tmux kill-session -t llm-application 2>/dev/null

sleep 2

PROJECT_DIR="$(pwd)"

# Function to create and initialize an agent
init_agent() {
    local session=$1
    local agent_num=$2
    local brief_file="agent_briefs/agent${agent_num}_brief.md"
    
    echo "Initializing $session..."
    
    # Create session
    tmux new-session -d -s $session -n work -c "$PROJECT_DIR"
    sleep 1
    
    # Start Claude with bypass permissions
    tmux send-keys -t $session:work "claude --dangerously-skip-permissions" C-m
    sleep 3
    
    # Read the brief
    tmux send-keys -t $session:work "cat $brief_file" C-m
    sleep 2
    
    # Send specific work instruction based on agent
    case $agent_num in
        1)
            tmux send-keys -t $session:work "I need to implement the tokenizer. Let me start by reading book_chapters/fundamentals.pdf to understand BPE from Chapter 1, then implement it in src/llm/fundamentals.scm" C-m
            ;;
        2)
            tmux send-keys -t $session:work "I need to implement attention mechanisms. Let me read book_chapters/attention_architecture.pdf to understand self-attention from Chapter 3, then implement it in src/llm/attention.scm" C-m
            ;;
        3)
            tmux send-keys -t $session:work "I need to implement the training pipeline. Let me read book_chapters/training.pdf to understand pretraining from Chapter 5, then implement the data loader in src/llm/pretrain.scm" C-m
            ;;
        4)
            tmux send-keys -t $session:work "I need to implement fine-tuning. Let me read book_chapters/fine_tuning.pdf to understand fine-tuning from Chapter 6, then implement it in src/llm/finetune.scm" C-m
            ;;
    esac
    
    echo "  $session initialized and working"
}

# Initialize all agents
init_agent "llm-foundation" 1
init_agent "llm-architecture" 2
init_agent "llm-training" 3
init_agent "llm-application" 4

echo ""
echo "All agents restarted and initialized!"
echo ""
echo "Check status with:"
echo "  tmux attach -t llm-foundation"
echo "  tmux attach -t llm-architecture"
echo "  tmux attach -t llm-training"
echo "  tmux attach -t llm-application"