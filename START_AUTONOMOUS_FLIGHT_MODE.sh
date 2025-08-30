#!/bin/bash
# MASTER AUTONOMOUS LAUNCH SCRIPT
# Run this before your BOS->SEA flight for fully autonomous operation

echo "================================================"
echo "   AUTONOMOUS FLIGHT MODE - BOS → SEA"
echo "   Estimated flight time: 6 hours"
echo "   4 Agents will work autonomously"
echo "================================================"
echo ""

# Check prerequisites
echo "Checking prerequisites..."

if ! command -v tmux &> /dev/null; then
    echo "ERROR: tmux not found. Please install tmux."
    exit 1
fi

if ! command -v claude &> /dev/null; then
    echo "ERROR: claude not found. Please install Claude Code."
    exit 1
fi

if ! command -v gh &> /dev/null; then
    echo "ERROR: gh (GitHub CLI) not found. Please install gh."
    exit 1
fi

echo "✓ All prerequisites met"
echo ""

# Make all scripts executable
chmod +x *.sh
chmod +x tools/*.py

echo "Starting autonomous system..."
echo ""

# 1. Launch all agents with --dangerously-skip-permissions
echo "Step 1: Launching agents with skip-permissions..."
bash launch_autonomous_agents.sh
sleep 10

# 2. Start the continuation daemon
echo ""
echo "Step 2: Starting continuation daemon..."
nohup bash keep_agents_running.sh > continuation_daemon.log 2>&1 &
CONTINUATION_PID=$!
echo "Continuation daemon started with PID: $CONTINUATION_PID"

# 3. Start the recovery agent
echo ""
echo "Step 3: Starting recovery agent..."
nohup bash recovery_agent.sh > recovery_daemon.log 2>&1 &
RECOVERY_PID=$!
echo "Recovery agent started with PID: $RECOVERY_PID"

# 4. Start monitoring dashboard (optional, in new terminal)
echo ""
echo "Step 4: Monitoring dashboard ready..."
echo "To monitor progress, run in a new terminal:"
echo "  bash monitor_agents.sh"

# 5. Create status file
echo ""
echo "Creating status file..."
cat > autonomous_status.txt << EOF
AUTONOMOUS FLIGHT MODE ACTIVATED
================================
Start Time: $(date)
Continuation Daemon PID: $CONTINUATION_PID
Recovery Agent PID: $RECOVERY_PID

Agents Running:
- Agent 1 (Foundation): llm-foundation
- Agent 2 (Architecture): llm-architecture
- Agent 3 (Training): llm-training
- Agent 4 (Application): llm-application

To check status:
  tmux list-sessions
  tail -f agent_continuation.log
  tail -f recovery.log
  bash monitor_agents.sh

To stop autonomous mode:
  kill $CONTINUATION_PID $RECOVERY_PID
  tmux kill-server  # kills all agents

Safe travels!
EOF

cat autonomous_status.txt

# 6. Final summary
echo ""
echo "================================================"
echo "✅ AUTONOMOUS MODE FULLY ACTIVATED"
echo "================================================"
echo ""
echo "The system will now run autonomously for ~6 hours"
echo "Agents will:"
echo "  • Read their assigned PDF sections"
echo "  • Implement their modules"
echo "  • Commit code regularly"
echo "  • Update GitHub issues"
echo "  • Continue working if they pause"
echo "  • Restart if they crash"
echo ""
echo "When you land in SEA, you should have:"
echo "  • 7 implemented modules"
echo "  • Dozens of commits"
echo "  • Progress on all 12 GitHub issues"
echo "  • A working LLM implementation in Guile"
echo ""
echo "Logs to check when you land:"
echo "  • agent_continuation.log"
echo "  • recovery.log"
echo "  • git log --oneline"
echo "  • gh issue list"
echo ""
echo "Have a great flight! The agents are working hard for you. ✈️"
echo ""
echo "Pro tip: Run 'bash monitor_agents.sh' in another terminal"
echo "         to watch real-time progress before you board."
echo "================================================"