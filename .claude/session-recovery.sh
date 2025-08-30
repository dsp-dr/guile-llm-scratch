#!/usr/bin/env bash
# Quick session recovery for coordinator

set -e

echo "🔄 Coordinator Session Recovery"
echo "================================"

# 1. Verify project location
if [[ ! -f "README.org" ]] || [[ ! -d "src/llm" ]]; then
    echo "❌ Not in project directory. Navigating..."
    cd /home/dsp-dr/ghq/github.com/dsp-dr/guile-llm-scratch
fi

echo "📍 Project location: $(pwd)"

# 2. Check agent sessions
echo "🤖 Checking agent sessions..."
missing_sessions=()
required_sessions=("llm-foundation" "llm-architecture" "llm-training" "llm-application")

for session in "${required_sessions[@]}"; do
    if ! tmux has-session -t "$session" 2>/dev/null; then
        missing_sessions+=("$session")
        echo "⚠️  Missing: $session"
    else
        echo "✅ Active: $session"
    fi
done

# 3. Check daemon status  
daemon_pid=62349
if ps -p $daemon_pid > /dev/null 2>&1; then
    echo "✅ Daemon running: PID $daemon_pid"
else
    echo "❌ Daemon not found: PID $daemon_pid"
    echo "   Use: ./keep_agents_running.sh & to restart"
fi

# 4. Check recent progress
echo "📊 Recent progress:"
recent_commits=$(git log --oneline --since="1 hour ago" | wc -l)
total_lines=$(find src/llm -name "*.scm" -exec wc -l {} \; | awk '{total += $1} END {print total}')
echo "   Commits (last hour): $recent_commits"
echo "   Total lines: $total_lines"

# 5. Recovery suggestions
if [[ ${#missing_sessions[@]} -gt 0 ]]; then
    echo ""
    echo "🔧 Recovery Commands:"
    echo "   ./setup_tmux_agents.sh    # Restart all missing sessions"
    echo "   Or manually:"
    for session in "${missing_sessions[@]}"; do
        echo "   tmux new-session -d -s $session -c $(pwd)"
        echo "   tmux send-keys -t $session 'claude --dangerously-skip-permissions' C-m"
    done
fi

echo ""
echo "📖 Full context available in:"
echo "   .claude/coordinator-resurrection.org"
echo "   .claude/agents/*.md"
echo ""
echo "🚀 Ready to continue coordination!"