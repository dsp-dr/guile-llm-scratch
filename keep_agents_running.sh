#!/bin/bash
# Autonomous agent continuation script - keeps all agents working
# Runs while you're AFK from BOS to SEA (~6 hours flight)

LOGFILE="agent_continuation.log"
INTERVAL=120  # Check every 2 minutes
MAX_ITERATIONS=180  # Run for ~6 hours

echo "$(date): Starting autonomous agent continuation system" | tee -a $LOGFILE
echo "Will run for approximately 6 hours with checks every 2 minutes" | tee -a $LOGFILE

# Function to send continuation command to a tmux session
continue_agent() {
    local session=$1
    local agent_name=$2
    
    if tmux has-session -t $session 2>/dev/null; then
        echo "$(date): Continuing $agent_name..." | tee -a $LOGFILE
        
        # Send continuation message
        tmux send-keys -t $session:work.0 "Continue working on the next top priority task. Check your GitHub issues and implement the next component from your assigned book section." 
        sleep 2
        tmux send-keys -t $session:work.0 C-m
        
        # Log the action
        echo "$(date): Sent continuation command to $agent_name" | tee -a $LOGFILE
    else
        echo "$(date): WARNING - Session $session not found" | tee -a $LOGFILE
    fi
}

# Function to check agent progress
check_progress() {
    echo "$(date): Checking agent progress..." | tee -a $LOGFILE
    
    # Check git commits
    local commits=$(git log --oneline --since="2 hours ago" 2>/dev/null | wc -l)
    echo "  Recent commits: $commits" | tee -a $LOGFILE
    
    # Check modified files
    local modified=$(git status --porcelain 2>/dev/null | wc -l)
    echo "  Modified files: $modified" | tee -a $LOGFILE
    
    # Check issue status
    for label in agent-1-foundation agent-2-architecture agent-3-training agent-4-application; do
        local open_issues=$(gh issue list --label $label --state open 2>/dev/null | wc -l)
        echo "  $label open issues: $open_issues" | tee -a $LOGFILE
    done
}

# Main loop
iteration=0
while [ $iteration -lt $MAX_ITERATIONS ]; do
    echo "" | tee -a $LOGFILE
    echo "$(date): ===== Iteration $((iteration + 1))/$MAX_ITERATIONS =====" | tee -a $LOGFILE
    
    # Continue each agent
    continue_agent "llm-foundation" "Agent 1 (Foundation)"
    sleep 5
    continue_agent "llm-architecture" "Agent 2 (Architecture)"
    sleep 5
    continue_agent "llm-training" "Agent 3 (Training)"
    sleep 5
    continue_agent "llm-application" "Agent 4 (Application)"
    
    # Every 10 iterations, check progress
    if [ $((iteration % 10)) -eq 0 ]; then
        check_progress
    fi
    
    # Commit any pending changes every 5 iterations
    if [ $((iteration % 5)) -eq 0 ]; then
        echo "$(date): Auto-committing any pending changes..." | tee -a $LOGFILE
        git add -A 2>/dev/null
        git commit -m "feat: autonomous progress - iteration $iteration" 2>/dev/null
    fi
    
    iteration=$((iteration + 1))
    
    # Wait before next iteration
    echo "$(date): Waiting $INTERVAL seconds before next iteration..." | tee -a $LOGFILE
    sleep $INTERVAL
done

echo "$(date): Autonomous continuation complete after $iteration iterations" | tee -a $LOGFILE
echo "Safe travels! The agents have been working autonomously." | tee -a $LOGFILE