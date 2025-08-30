#!/bin/bash
# Start Claude Code in all agent tmux sessions

echo "Starting Claude Code agents..."

# Start Agent 1: Foundation
tmux send-keys -t llm-foundation:work.0 'claude' C-m
echo "✓ Agent 1 (Foundation) started"

# Start Agent 2: Architecture  
tmux send-keys -t llm-architecture:work.0 'claude' C-m
echo "✓ Agent 2 (Architecture) started"

# Start Agent 3: Training
tmux send-keys -t llm-training:work.0 'claude' C-m
echo "✓ Agent 3 (Training) started"

# Start Agent 4: Application
tmux send-keys -t llm-application:work.0 'claude' C-m
echo "✓ Agent 4 (Application) started"

echo ""
echo "All agents started! Monitor progress with:"
echo "  tmux attach -t llm-coordinator"