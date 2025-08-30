#!/bin/bash
# Improved daemon for agent nudging - sends to default window

AGENT_TASKS=(
    "llm-foundation:Continue implementing text preprocessing in src/llm/text.scm. Add functions for corpus statistics, text chunking, and validation."
    "llm-architecture:CRITICAL: Complete GPT transformer in src/llm/gpt.scm. Implement transformer-block, layer-norm, position-encoding functions."
    "llm-training:Enhance src/llm/pretrain.scm with AdaGrad optimizer, learning rate schedules, and gradient clipping."
    "llm-application:CRITICAL: Build instruction tuning in src/llm/instruction.scm. Add RLHF basics and instruction-following evaluation."
)

echo "Starting improved daemon at $(date)"

while true; do
    for task in "${AGENT_TASKS[@]}"; do
        IFS=":" read -r session command <<< "$task"
        if tmux has-session -t "$session" 2>/dev/null; then
            # Send to default window (no :work specification)
            tmux send-keys -t "$session" "$command" C-m
            echo "$(date): Sent to $session"
            sleep 2
        fi
    done
    
    # Auto-commit every cycle
    if [ -n "$(git status --porcelain 2>/dev/null)" ]; then
        git add -A 2>/dev/null
        git commit -m "feat: lightweight agent progress $(date +%H:%M)" 2>/dev/null
        echo "$(date): Auto-committed changes"
    fi
    
    echo "$(date): Cycle complete, sleeping 180s"
    sleep 180  # Every 3 minutes
done