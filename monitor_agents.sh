#!/bin/bash
# Real-time monitoring dashboard for autonomous agents

LOGDIR="logs"
mkdir -p $LOGDIR

# Function to check agent health
check_agent_health() {
    local session=$1
    if tmux has-session -t $session 2>/dev/null; then
        echo "✓ Running"
    else
        echo "✗ Stopped"
    fi
}

# Function to get recent commits
get_recent_commits() {
    git log --oneline --since="6 hours ago" 2>/dev/null | head -5
}

# Function to get modified files
get_modified_files() {
    git status --porcelain 2>/dev/null | head -10
}

# Function to check issue progress
check_issues() {
    echo "Open Issues by Agent:"
    for i in {1..4}; do
        local count=$(gh issue list --label "agent-$i-*" --state open 2>/dev/null | wc -l)
        echo "  Agent $i: $count open issues"
    done
}

# Main monitoring loop
while true; do
    clear
    echo "========================================="
    echo "   AUTONOMOUS AGENT MONITORING DASHBOARD"
    echo "   $(date)"
    echo "========================================="
    echo ""
    
    echo "AGENT STATUS:"
    echo "  Agent 1 (Foundation):   $(check_agent_health llm-foundation)"
    echo "  Agent 2 (Architecture): $(check_agent_health llm-architecture)"
    echo "  Agent 3 (Training):     $(check_agent_health llm-training)"
    echo "  Agent 4 (Application):  $(check_agent_health llm-application)"
    echo ""
    
    echo "RECENT COMMITS (last 6 hours):"
    commits=$(get_recent_commits)
    if [ -z "$commits" ]; then
        echo "  No recent commits"
    else
        echo "$commits" | sed 's/^/  /'
    fi
    echo ""
    
    echo "MODIFIED FILES:"
    modified=$(get_modified_files)
    if [ -z "$modified" ]; then
        echo "  No modified files"
    else
        echo "$modified" | sed 's/^/  /'
    fi
    echo ""
    
    check_issues
    echo ""
    
    echo "LOG FILES:"
    echo "  agent_continuation.log: $(tail -1 agent_continuation.log 2>/dev/null | cut -c1-50)..."
    echo "  agent_launch.log: $(tail -1 agent_launch.log 2>/dev/null | cut -c1-50)..."
    echo ""
    
    echo "MEMORY USAGE:"
    ps aux | grep "claude\|guile" | grep -v grep | awk '{printf "  %-20s %s\n", $11, $4"%"}' | head -5
    echo ""
    
    echo "========================================="
    echo "Press Ctrl+C to exit | Refreshing in 30s"
    
    sleep 30
done