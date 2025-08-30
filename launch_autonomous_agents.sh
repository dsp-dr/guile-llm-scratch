#!/bin/bash
# Launch all agents with dangerous-skip-permissions for autonomous operation

PROJECT_DIR="$HOME/ghq/github.com/dsp-dr/guile-llm-scratch"
LOGFILE="agent_launch.log"

echo "$(date): Launching autonomous agents with skip-permissions..." | tee $LOGFILE

# Kill and recreate sessions to ensure clean start
tmux kill-session -t llm-foundation 2>/dev/null
tmux kill-session -t llm-architecture 2>/dev/null
tmux kill-session -t llm-training 2>/dev/null
tmux kill-session -t llm-application 2>/dev/null

sleep 2

# Create fresh sessions
echo "Creating fresh tmux sessions..." | tee -a $LOGFILE

# Agent 1: Foundation
tmux new-session -d -s llm-foundation -n work -c "$PROJECT_DIR"
tmux split-window -h -t llm-foundation:work -c "$PROJECT_DIR"
tmux send-keys -t llm-foundation:work.1 'watch -n 30 "git status && echo && ls -la src/llm/fundamentals.scm src/llm/text.scm 2>/dev/null"' C-m

# Agent 2: Architecture  
tmux new-session -d -s llm-architecture -n work -c "$PROJECT_DIR"
tmux split-window -h -t llm-architecture:work -c "$PROJECT_DIR"
tmux send-keys -t llm-architecture:work.1 'watch -n 30 "git status && echo && ls -la src/llm/attention.scm src/llm/gpt.scm 2>/dev/null"' C-m

# Agent 3: Training
tmux new-session -d -s llm-training -n work -c "$PROJECT_DIR"
tmux split-window -h -t llm-training:work -c "$PROJECT_DIR"
tmux send-keys -t llm-training:work.1 'watch -n 30 "git status && echo && ls -la src/llm/pretrain.scm 2>/dev/null"' C-m

# Agent 4: Application
tmux new-session -d -s llm-application -n work -c "$PROJECT_DIR"
tmux split-window -h -t llm-application:work -c "$PROJECT_DIR"
tmux send-keys -t llm-application:work.1 'watch -n 30 "git status && echo && ls -la src/llm/finetune.scm src/llm/instruction.scm 2>/dev/null"' C-m

echo "Sessions created. Launching Claude agents..." | tee -a $LOGFILE

# Start each agent with dangerous-skip-permissions
echo "Starting Agent 1 (Foundation)..." | tee -a $LOGFILE
tmux send-keys -t llm-foundation:work.0 'claude --dangerously-skip-permissions' C-m
sleep 3

echo "Starting Agent 2 (Architecture)..." | tee -a $LOGFILE
tmux send-keys -t llm-architecture:work.0 'claude --dangerously-skip-permissions' C-m
sleep 3

echo "Starting Agent 3 (Training)..." | tee -a $LOGFILE
tmux send-keys -t llm-training:work.0 'claude --dangerously-skip-permissions' C-m
sleep 3

echo "Starting Agent 4 (Application)..." | tee -a $LOGFILE
tmux send-keys -t llm-application:work.0 'claude --dangerously-skip-permissions' C-m
sleep 3

echo "All agents launched. Waiting for initialization..." | tee -a $LOGFILE
sleep 10

echo "Sending initial instructions to agents..." | tee -a $LOGFILE

# Send initial instructions to each agent
tmux send-keys -t llm-foundation:work.0 "cat agent_briefs/agent1_brief.md" C-m
sleep 2

tmux send-keys -t llm-architecture:work.0 "cat agent_briefs/agent2_brief.md" C-m
sleep 2

tmux send-keys -t llm-training:work.0 "cat agent_briefs/agent3_brief.md" C-m
sleep 2

tmux send-keys -t llm-application:work.0 "cat agent_briefs/agent4_brief.md" C-m
sleep 2

echo "$(date): All agents launched and initialized!" | tee -a $LOGFILE
echo "" | tee -a $LOGFILE
echo "Agents are now running autonomously with --dangerously-skip-permissions" | tee -a $LOGFILE
echo "They will work on their assigned tasks from the book splits" | tee -a $LOGFILE
echo "" | tee -a $LOGFILE
echo "To keep them working continuously, run:" | tee -a $LOGFILE
echo "  nohup bash keep_agents_running.sh &" | tee -a $LOGFILE
echo "" | tee -a $LOGFILE
echo "To monitor progress:" | tee -a $LOGFILE
echo "  tmux attach -t llm-coordinator" | tee -a $LOGFILE
echo "  tail -f agent_continuation.log" | tee -a $LOGFILE