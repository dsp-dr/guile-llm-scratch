#!/bin/bash
# Force agents to start working by sending multiple enters and commands

echo "Forcing agents to start processing..."

# Function to force start an agent
force_start() {
    local session=$1
    local agent_name=$2
    local task=$3
    
    echo "Force starting $agent_name..."
    
    # Send multiple enters to clear any pending input
    tmux send-keys -t $session:work C-m
    sleep 1
    tmux send-keys -t $session:work C-m
    sleep 1
    
    # Send the actual command
    tmux send-keys -t $session:work "$task" C-m
    sleep 2
    
    # Send another enter to make sure it processes
    tmux send-keys -t $session:work C-m
    sleep 1
    
    echo "  $agent_name should be working now"
}

# Force start each agent with their primary task
force_start "llm-foundation" "Agent 1" \
    "Read book_chapters/fundamentals.pdf and implement BPE tokenizer in src/llm/fundamentals.scm"

force_start "llm-architecture" "Agent 2" \
    "Read book_chapters/attention_architecture.pdf and implement self-attention in src/llm/attention.scm"

force_start "llm-training" "Agent 3" \
    "Read book_chapters/training.pdf and implement data loader in src/llm/pretrain.scm"

force_start "llm-application" "Agent 4" \
    "Read book_chapters/fine_tuning.pdf and implement fine-tuning pipeline in src/llm/finetune.scm"

echo ""
echo "All agents force-started!"
echo ""
echo "Wait 30 seconds then check with:"
echo "  for s in llm-foundation llm-architecture llm-training llm-application; do"
echo '    echo "=== $s ==="; tmux capture-pane -t $s:work -p | tail -20; echo ""'
echo "  done"