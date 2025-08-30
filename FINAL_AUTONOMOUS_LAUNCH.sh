#!/bin/bash
# FINAL LAUNCH SCRIPT - Ensures all agents are working for BOS->SEA flight

echo "================================================"
echo "   FINAL AUTONOMOUS LAUNCH FOR FLIGHT"
echo "================================================"
echo ""

# Kill everything and start fresh
echo "Cleaning up old sessions..."
tmux kill-server 2>/dev/null
sleep 2

PROJECT_DIR="$(pwd)"

# Function to fully initialize an agent
init_and_start_agent() {
    local session=$1
    local agent_num=$2
    local module=$3
    local task=$4
    
    echo "Starting Agent $agent_num ($session)..."
    
    # Create session
    tmux new-session -d -s $session -n work -c "$PROJECT_DIR"
    sleep 1
    
    # Start Claude with skip permissions
    tmux send-keys -t $session:work "claude --dangerously-skip-permissions" C-m
    sleep 3
    
    # Send the work command directly
    tmux send-keys -t $session:work "$task" C-m
    sleep 2
    
    # Send enter to process
    tmux send-keys -t $session:work C-m
    sleep 1
    
    echo "  ✓ Agent $agent_num started"
}

# Start all agents with direct implementation commands
init_and_start_agent "llm-foundation" 1 "fundamentals" \
    "Implement a BPE tokenizer in src/llm/fundamentals.scm with functions: tokenize, build-vocabulary, encode-text, decode-ids. Also implement text preprocessing in src/llm/text.scm"

init_and_start_agent "llm-architecture" 2 "attention" \
    "Implement self-attention and multi-head attention in src/llm/attention.scm. Include scaled dot-product attention, softmax, and masking. Then implement transformer blocks in src/llm/gpt.scm"

init_and_start_agent "llm-training" 3 "pretrain" \
    "Implement a data loader and training loop in src/llm/pretrain.scm. Include batch creation, loss computation, and parameter updates"

init_and_start_agent "llm-application" 4 "finetune" \
    "Implement fine-tuning pipeline in src/llm/finetune.scm and instruction following in src/llm/instruction.scm. Include task-specific heads and prompt templates"

echo ""
echo "Starting continuous work daemon..."
# Start the continuation script
nohup bash -c 'while true; do
    for s in llm-foundation llm-architecture llm-training llm-application; do
        if tmux has-session -t $s 2>/dev/null; then
            tmux send-keys -t $s:work "Continue implementing the next function in your module. Check the module file and add the next unimplemented function." C-m
            sleep 2
        fi
    done
    sleep 120
done' > continuous_work.log 2>&1 &
CONTINUE_PID=$!

echo "Continuation daemon PID: $CONTINUE_PID"

# Start recovery daemon
nohup bash -c 'while true; do
    for s in llm-foundation llm-architecture llm-training llm-application; do
        if ! tmux has-session -t $s 2>/dev/null; then
            tmux new-session -d -s $s -n work -c "$(pwd)"
            sleep 1
            tmux send-keys -t $s:work "claude --dangerously-skip-permissions" C-m
            sleep 3
            tmux send-keys -t $s:work "Continue implementing your assigned module. Check git status to see what was done." C-m
        fi
    done
    
    # Auto-commit every 10 minutes
    if [ -n "$(git status --porcelain)" ]; then
        git add -A 2>/dev/null
        git commit -m "feat: autonomous progress $(date +%H:%M)" 2>/dev/null
    fi
    
    sleep 300
done' > recovery.log 2>&1 &
RECOVERY_PID=$!

echo "Recovery daemon PID: $RECOVERY_PID"

# Create status file
cat > flight_status.txt << EOF
AUTONOMOUS FLIGHT MODE ACTIVE
==============================
Started: $(date)
Continuation PID: $CONTINUE_PID
Recovery PID: $RECOVERY_PID

Agents:
- llm-foundation: BPE tokenizer & text processing
- llm-architecture: Attention & GPT model
- llm-training: Data loader & training loop
- llm-application: Fine-tuning & instructions

To stop after landing:
  kill $CONTINUE_PID $RECOVERY_PID
  tmux kill-server

Logs:
  tail -f continuous_work.log
  tail -f recovery.log
EOF

echo ""
echo "================================================"
echo "✅ SYSTEM FULLY AUTONOMOUS"
echo "================================================"
echo ""
echo "4 agents are now working on:"
echo "  • Agent 1: BPE tokenizer (src/llm/fundamentals.scm)"
echo "  • Agent 2: Attention mechanisms (src/llm/attention.scm)"
echo "  • Agent 3: Training pipeline (src/llm/pretrain.scm)"
echo "  • Agent 4: Fine-tuning (src/llm/finetune.scm)"
echo ""
echo "They will:"
echo "  • Continue working every 2 minutes"
echo "  • Restart if crashed (every 5 minutes)"
echo "  • Auto-commit code (every 10 minutes)"
echo ""
echo "Have a great flight! ✈️"
echo "Check progress when you land with: git log --oneline"
echo "================================================"