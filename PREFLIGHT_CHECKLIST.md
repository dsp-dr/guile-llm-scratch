# Pre-Flight Checklist ✈️

## Before Starting Autonomous Mode

### System Requirements
- [ ] tmux installed and working
- [ ] Claude Code installed (`claude --version`)
- [ ] GitHub CLI configured (`gh auth status`)
- [ ] Git repository clean (`git status`)
- [ ] Python 3 with PyPDF2 (`python3 -c "import PyPDF2"`)

### Files Verification
- [ ] Book splits exist in `book_chapters/`:
  - [ ] `fundamentals.pdf` (50 pages)
  - [ ] `attention_architecture.pdf` (100 pages)
  - [ ] `training.pdf` (70 pages)
  - [ ] `fine_tuning.pdf` (150 pages)
- [ ] Agent briefs exist in `agent_briefs/`:
  - [ ] `agent1_brief.md`
  - [ ] `agent2_brief.md`
  - [ ] `agent3_brief.md`
  - [ ] `agent4_brief.md`
- [ ] Module scaffolds exist in `src/llm/`:
  - [ ] `fundamentals.scm`
  - [ ] `text.scm`
  - [ ] `attention.scm`
  - [ ] `gpt.scm`
  - [ ] `pretrain.scm`
  - [ ] `finetune.scm`
  - [ ] `instruction.scm`

### GitHub Setup
- [ ] 12 issues created (#1-#12)
- [ ] 5 milestones configured
- [ ] Labels created for agents

## Launch Sequence

### 1. Clean Start (Optional)
```bash
# Kill any existing sessions
tmux kill-server

# Clear old logs
rm -f *.log

# Ensure clean git state
git add -A && git commit -m "chore: pre-flight checkpoint"
```

### 2. Launch Autonomous Mode
```bash
# ONE COMMAND TO RULE THEM ALL
bash START_AUTONOMOUS_FLIGHT_MODE.sh
```

### 3. Verify Launch (30 seconds after start)
```bash
# Check all agents are running
tmux list-sessions | grep llm

# Check continuation daemon
ps aux | grep keep_agents_running

# Check recovery daemon  
ps aux | grep recovery_agent

# Monitor initial activity
tail -f agent_continuation.log
```

### 4. Optional: Watch Real-Time Progress
```bash
# In a new terminal
bash monitor_agents.sh
```

## During Flight

The system will:
- ✅ Keep agents working every 2 minutes
- ✅ Restart crashed agents automatically
- ✅ Commit code periodically
- ✅ Fix git conflicts if they occur
- ✅ Update GitHub issues
- ✅ Log all activities

## After Landing (SEA)

### Check Results
```bash
# View what was accomplished
git log --oneline --since="7 hours ago"

# Check implementation progress
ls -la src/llm/*.scm

# Review issues
gh issue list

# Check logs
tail -100 agent_continuation.log
tail -100 recovery.log
```

### Stop Autonomous Mode
```bash
# Read the status file for PIDs
cat autonomous_status.txt

# Kill daemons (use PIDs from status file)
kill [CONTINUATION_PID] [RECOVERY_PID]

# Or nuclear option
tmux kill-server
pkill -f keep_agents_running
pkill -f recovery_agent
```

## Troubleshooting

### If agents aren't starting:
```bash
# Check Claude is installed
which claude

# Try manual start in tmux
tmux attach -t llm-foundation
claude --dangerously-skip-permissions
```

### If no progress is being made:
```bash
# Check agent sessions
tmux attach -t llm-foundation  # Check each agent

# Manually nudge an agent
tmux send-keys -t llm-foundation:work.0 "Continue with the next task" C-m
```

### If git has conflicts:
```bash
# The recovery agent should handle this, but manually:
git status
git add -A
git commit -m "fix: resolve conflicts"
```

## Expected Outcomes

After 6 hours, you should have:

1. **Code Implementation**
   - 7 modules with actual Guile code
   - Core functions implemented
   - Basic structure complete

2. **Documentation**
   - Inline documentation in code
   - Function signatures defined
   - Module exports specified

3. **Git History**
   - 20-50 commits
   - Descriptive commit messages
   - Regular progress checkpoints

4. **GitHub Issues**
   - Progress on all 12 issues
   - Comments with implementation notes
   - Some issues potentially closed

## Flight Path: BOS → SEA

- **Departure**: Boston Logan (BOS)
- **Arrival**: Seattle-Tacoma (SEA)
- **Flight Time**: ~6 hours
- **Coding Time**: ~6 hours of autonomous work
- **Expected Progress**: 30-50% of full implementation

## Emergency Contacts

If something goes wrong, check:
1. `autonomous_status.txt` - System state and PIDs
2. `agent_continuation.log` - What agents have been doing
3. `recovery.log` - Any crashes and restarts
4. `git log` - Actual work completed
5. `tmux list-sessions` - Active agents

---

**Safe travels! Your AI agents are ready to work while you fly! 🚀**