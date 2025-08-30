#!/bin/bash
# Emergency restart of agents with lightweight resources

echo "==============================================="
echo "   EMERGENCY AGENT RESTART - LIGHTWEIGHT MODE"
echo "==============================================="
echo ""

# Kill all existing sessions
echo "Killing all existing agents..."
tmux kill-session -t llm-foundation 2>/dev/null
tmux kill-session -t llm-architecture 2>/dev/null
tmux kill-session -t llm-training 2>/dev/null
tmux kill-session -t llm-application 2>/dev/null

sleep 2

PROJECT_DIR="$(pwd)"

# Function to create and start an agent with specific instructions
create_lightweight_agent() {
    local session=$1
    local agent_name=$2
    local guide_file=$3
    local module_file=$4
    local direct_command=$5
    
    echo "Creating $agent_name with lightweight resources..."
    
    # Create session
    tmux new-session -d -s $session -n work -c "$PROJECT_DIR"
    sleep 1
    
    # Start Claude
    tmux send-keys -t $session:work "claude --dangerously-skip-permissions" C-m
    sleep 3
    
    # Send direct implementation command
    tmux send-keys -t $session:work "$direct_command" C-m
    sleep 2
    
    echo "  ✓ $agent_name started with direct task"
}

# Agent 1: Foundation - Focus on text.scm
create_lightweight_agent "llm-foundation" "Agent 1 (Foundation)" \
    "agent_resources/text_guide.txt" \
    "src/llm/text.scm" \
    "Read agent_resources/text_guide.txt and implement all the text preprocessing functions in src/llm/text.scm. Start with preprocess-text function."

# Agent 2: Architecture - Focus on attention.scm  
create_lightweight_agent "llm-architecture" "Agent 2 (Architecture)" \
    "agent_resources/attention_guide.txt" \
    "src/llm/attention.scm" \
    "Read agent_resources/attention_guide.txt and implement the self-attention mechanism in src/llm/attention.scm. Start with the softmax function."

# Agent 3: Training - Optimize existing code
create_lightweight_agent "llm-training" "Agent 3 (Training)" \
    "" \
    "src/llm/pretrain.scm" \
    "Your pretrain.scm is excellent! Add more optimizer options (SGD, RMSprop) and improve the existing functions. Check the current implementation and enhance it."

# Agent 4: Application - Focus on finetune.scm
create_lightweight_agent "llm-application" "Agent 4 (Application)" \
    "agent_resources/finetune_guide.txt" \
    "src/llm/finetune.scm" \
    "Read agent_resources/finetune_guide.txt and implement the fine-tuning pipeline in src/llm/finetune.scm. Start with create-classification-head function."

echo ""
echo "Starting continuous work daemon (lightweight mode)..."

# Lightweight continuation daemon
nohup bash -c '
AGENT_TASKS=(
    "llm-foundation:Continue implementing text preprocessing functions in src/llm/text.scm. Use agent_resources/text_guide.txt as reference."
    "llm-architecture:Continue implementing attention mechanism in src/llm/attention.scm. Use agent_resources/attention_guide.txt as reference."
    "llm-training:Enhance your existing pretrain.scm with additional optimizers and features."
    "llm-application:Continue implementing fine-tuning in src/llm/finetune.scm. Use agent_resources/finetune_guide.txt as reference."
)

while true; do
    for task in "${AGENT_TASKS[@]}"; do
        IFS=":" read -r session command <<< "$task"
        if tmux has-session -t "$session" 2>/dev/null; then
            tmux send-keys -t "$session:work" "$command" C-m
            sleep 2
        fi
    done
    
    # Auto-commit every cycle
    if [ -n "$(git status --porcelain)" ]; then
        git add -A 2>/dev/null
        git commit -m "feat: lightweight agent progress $(date +%H:%M)" 2>/dev/null
    fi
    
    sleep 180  # Every 3 minutes
done
' > lightweight_daemon.log 2>&1 &

DAEMON_PID=$!

# Create status file
cat > lightweight_status.txt << EOF
LIGHTWEIGHT AGENT MODE ACTIVE
==============================
Started: $(date)
Daemon PID: $DAEMON_PID
Resources: agent_resources/ directory with implementation guides

Agent Tasks:
- llm-foundation: Text preprocessing (agent_resources/text_guide.txt)
- llm-architecture: Attention mechanism (agent_resources/attention_guide.txt)  
- llm-training: Enhance existing pretrain.scm
- llm-application: Fine-tuning pipeline (agent_resources/finetune_guide.txt)

To stop:
  kill $DAEMON_PID
  tmux kill-server

Logs:
  tail -f lightweight_daemon.log
EOF

echo "==============================================="
echo "✅ LIGHTWEIGHT MODE ACTIVATED"
echo "==============================================="
echo ""
echo "Agents now use small text guides instead of large PDFs:"
echo "  • agent_resources/text_guide.txt (2KB)"
echo "  • agent_resources/attention_guide.txt (2KB)"
echo "  • agent_resources/finetune_guide.txt (2KB)"
echo ""
echo "Daemon PID: $DAEMON_PID"
echo "Check progress: tail -f lightweight_daemon.log"
echo ""
echo "This should resolve the PDF size issues!"
echo "==============================================="