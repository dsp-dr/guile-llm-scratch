# Coordinator Agent Profile

## Role
Master coordinator for autonomous LLM implementation during BOS→SEA flight.

## Current Mission
- **Project**: Pure Guile Scheme LLM implementation from scratch
- **Status**: 1,367+ lines, 5/8 modules complete 
- **Timeline**: ~3 hours into 6-hour autonomous session
- **Target**: 2000+ lines, complete LLM by landing

## Key Responsibilities
1. **Monitor Progress**: Track all 4 agents via tmux sessions
2. **System Health**: Ensure daemon PID 62349 keeps running
3. **Integration**: Help connect modules when agents complete them
4. **Problem Solving**: Debug issues, restart failed agents
5. **Documentation**: Update progress, commit milestones

## Agent Network Status
- **Agent 1 (Foundation)**: `llm-foundation` - Working on text.scm
- **Agent 2 (Architecture)**: `llm-architecture` - Working on attention.scm  
- **Agent 3 (Training)**: `llm-training` - Enhancing pretrain.scm
- **Agent 4 (Application)**: `llm-application` - Working on finetune.scm

## Critical Context
- **Implementation Approach**: Lightweight 2KB text guides instead of large PDFs
- **Auto-commits**: Every 3 minutes via daemon
- **Book Content**: Available locally, git-ignored
- **GitHub**: All code pushed, no copyrighted content

## Recovery Instructions
If coordinator dies:
```bash
cd /home/dsp-dr/ghq/github.com/dsp-dr/guile-llm-scratch
claude -c
# Read .claude/coordinator-resurrection.org for full context
```

## Emergency Contacts
- **GitHub**: https://github.com/dsp-dr/guile-llm-scratch
- **Keybase**: jwalsh notified with progress updates
- **Local**: All agents running in tmux sessions

## Key Files to Monitor
- `src/llm/*.scm` - Main implementation
- `lightweight_daemon.log` - Auto-commit history
- `agent_resources/` - Agent guidance materials
- Git commits every 3 minutes

## Success Metrics
- [ ] GPT transformer complete (Agent 2)
- [ ] Instruction tuning complete (Agent 4)  
- [ ] Module integration in main.scm
- [ ] 2000+ lines total
- [ ] Working end-to-end pipeline