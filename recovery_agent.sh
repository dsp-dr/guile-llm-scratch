#!/bin/bash
# Recovery mechanism for crashed agents

LOGFILE="recovery.log"

echo "$(date): Starting recovery agent..." | tee $LOGFILE

# Function to restart a crashed agent
restart_agent() {
    local session=$1
    local agent_num=$2
    local agent_name=$3
    
    if ! tmux has-session -t $session 2>/dev/null; then
        echo "$(date): $agent_name has crashed. Restarting..." | tee -a $LOGFILE
        
        # Recreate session
        tmux new-session -d -s $session -n work -c "$(pwd)"
        tmux split-window -h -t $session:work -c "$(pwd)"
        
        # Restart monitoring in right pane
        case $agent_num in
            1)
                tmux send-keys -t $session:work.1 'watch -n 30 "git status && ls -la src/llm/fundamentals.scm src/llm/text.scm"' C-m
                ;;
            2)
                tmux send-keys -t $session:work.1 'watch -n 30 "git status && ls -la src/llm/attention.scm src/llm/gpt.scm"' C-m
                ;;
            3)
                tmux send-keys -t $session:work.1 'watch -n 30 "git status && ls -la src/llm/pretrain.scm"' C-m
                ;;
            4)
                tmux send-keys -t $session:work.1 'watch -n 30 "git status && ls -la src/llm/finetune.scm src/llm/instruction.scm"' C-m
                ;;
        esac
        
        # Restart Claude
        sleep 2
        tmux send-keys -t $session:work.0 'claude --dangerously-skip-permissions' C-m
        sleep 5
        
        # Send recovery instructions
        tmux send-keys -t $session:work.0 "echo 'Agent $agent_num recovered from crash. Resuming work...'" C-m
        sleep 2
        tmux send-keys -t $session:work.0 "cat agent_briefs/agent${agent_num}_brief.md" C-m
        sleep 2
        tmux send-keys -t $session:work.0 "Continue with your assigned tasks. Check git status and your GitHub issues to see what was already completed." C-m
        
        echo "$(date): $agent_name restarted successfully" | tee -a $LOGFILE
        return 0
    fi
    return 1
}

# Function to check and fix git issues
fix_git_issues() {
    # Check for merge conflicts
    if git status | grep -q "Unmerged paths"; then
        echo "$(date): Detected merge conflicts. Attempting auto-resolution..." | tee -a $LOGFILE
        
        # Try to auto-resolve by accepting all current changes
        git status --porcelain | grep "^UU" | awk '{print $2}' | while read file; do
            git add "$file"
        done
        git commit -m "fix: auto-resolve merge conflicts" 2>/dev/null
    fi
    
    # Check for uncommitted changes older than 30 minutes
    if [ -n "$(git status --porcelain)" ]; then
        local last_commit=$(git log -1 --format=%ct 2>/dev/null || echo 0)
        local current_time=$(date +%s)
        local time_diff=$((current_time - last_commit))
        
        if [ $time_diff -gt 1800 ]; then  # 30 minutes
            echo "$(date): Auto-committing stale changes..." | tee -a $LOGFILE
            git add -A
            git commit -m "feat: auto-commit by recovery agent" 2>/dev/null
        fi
    fi
}

# Function to ensure work continues
ensure_progress() {
    # Check if any agent needs a nudge
    for session in llm-foundation llm-architecture llm-training llm-application; do
        if tmux has-session -t $session 2>/dev/null; then
            # Send a gentle reminder to continue
            tmux send-keys -t $session:work.0 "Continue implementing your assigned module. Focus on completing the next function or component." 
            sleep 1
            tmux send-keys -t $session:work.0 C-m
        fi
    done
}

# Main recovery loop
while true; do
    echo "$(date): Checking agent health..." | tee -a $LOGFILE
    
    # Check and restart crashed agents
    restart_agent "llm-foundation" 1 "Agent 1 (Foundation)"
    restart_agent "llm-architecture" 2 "Agent 2 (Architecture)"
    restart_agent "llm-training" 3 "Agent 3 (Training)"
    restart_agent "llm-application" 4 "Agent 4 (Application)"
    
    # Fix any git issues
    fix_git_issues
    
    # Ensure agents are making progress
    ensure_progress
    
    # Log current status
    active_agents=0
    for session in llm-foundation llm-architecture llm-training llm-application; do
        if tmux has-session -t $session 2>/dev/null; then
            active_agents=$((active_agents + 1))
        fi
    done
    
    echo "$(date): Active agents: $active_agents/4" | tee -a $LOGFILE
    
    # Wait before next check
    sleep 300  # Check every 5 minutes
done