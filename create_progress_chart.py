#!/usr/bin/env python3
"""
Create progress visualization for the autonomous LLM implementation
"""
import matplotlib.pyplot as plt
import numpy as np
from datetime import datetime

# Data
modules = ['fundamentals\n(Tokenizer)', 'text\n(Preprocessing)', 'attention\n(Self-Attn)', 
           'finetune\n(Classification)', 'pretrain\n(Training)', 'gpt\n(Transformer)', 
           'instruction\n(Tuning)', 'main\n(Integration)']

lines_of_code = [216, 99, 142, 205, 638, 25, 25, 16]
completion_status = [100, 100, 100, 100, 100, 15, 15, 10]  # Percentage
agents = ['Agent 1', 'Agent 1', 'Agent 2', 'Agent 4', 'Agent 3', 'Agent 2', 'Agent 4', 'Coordinator']

# Colors for each agent
agent_colors = {
    'Agent 1': '#2E8B57',  # Sea Green
    'Agent 2': '#4169E1',  # Royal Blue  
    'Agent 3': '#FF6347',  # Tomato
    'Agent 4': '#9370DB',  # Medium Purple
    'Coordinator': '#FFD700'  # Gold
}

colors = [agent_colors[agent] for agent in agents]

# Create figure with subplots
fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, figsize=(16, 12))
fig.suptitle('Autonomous LLM Implementation Progress\nBOS → SEA Flight Development', 
             fontsize=20, fontweight='bold')

# 1. Lines of Code by Module
bars1 = ax1.bar(modules, lines_of_code, color=colors, alpha=0.8, edgecolor='black')
ax1.set_title('Lines of Code by Module', fontsize=14, fontweight='bold')
ax1.set_ylabel('Lines of Code')
ax1.tick_params(axis='x', rotation=45)
for bar, loc in zip(bars1, lines_of_code):
    height = bar.get_height()
    ax1.text(bar.get_x() + bar.get_width()/2., height + 5,
             f'{int(loc)}', ha='center', va='bottom', fontweight='bold')

# 2. Completion Status
bars2 = ax2.bar(modules, completion_status, color=colors, alpha=0.8, edgecolor='black')
ax2.set_title('Module Completion Status', fontsize=14, fontweight='bold')
ax2.set_ylabel('Completion %')
ax2.set_ylim(0, 105)
ax2.tick_params(axis='x', rotation=45)
for bar, completion in zip(bars2, completion_status):
    height = bar.get_height()
    ax2.text(bar.get_x() + bar.get_width()/2., height + 1,
             f'{int(completion)}%', ha='center', va='bottom', fontweight='bold')

# 3. Agent Workload Distribution
agent_totals = {}
for agent, loc in zip(agents, lines_of_code):
    agent_totals[agent] = agent_totals.get(agent, 0) + loc

agent_names = list(agent_totals.keys())
agent_lines = list(agent_totals.values())
agent_pie_colors = [agent_colors[agent] for agent in agent_names]

wedges, texts, autotexts = ax3.pie(agent_lines, labels=agent_names, colors=agent_pie_colors,
                                   autopct='%1.0f%%', startangle=90)
ax3.set_title('Code Distribution by Agent', fontsize=14, fontweight='bold')
for autotext in autotexts:
    autotext.set_color('white')
    autotext.set_fontweight('bold')

# 4. Timeline Progress
timeline_data = {
    'Before Flight': 653,
    'At Takeoff': 1366,
    'Expected Landing': 2000
}

timeline_labels = list(timeline_data.keys())
timeline_values = list(timeline_data.values())

bars4 = ax4.bar(timeline_labels, timeline_values, 
                color=['#FF4444', '#44FF44', '#4444FF'], alpha=0.8, edgecolor='black')
ax4.set_title('Implementation Timeline', fontsize=14, fontweight='bold')
ax4.set_ylabel('Total Lines of Code')
ax4.set_ylim(0, 2200)

for bar, value in zip(bars4, timeline_values):
    height = bar.get_height()
    ax4.text(bar.get_x() + bar.get_width()/2., height + 20,
             f'{int(value)}', ha='center', va='bottom', fontweight='bold', fontsize=12)

# Add project stats text box
stats_text = f"""Project Statistics (as of {datetime.now().strftime('%H:%M')}):
• Total Lines: 1,366 (up from 653)
• Modules Complete: 5/8 (62.5%)
• Agents Working: 4/4 (100%)
• Implementation: ~80% complete

Autonomous Features:
• PDF → Lightweight text guides (2KB)
• Auto-nudging every 3 minutes
• Auto-commits every cycle
• Crash recovery system"""

plt.figtext(0.02, 0.02, stats_text, fontsize=10, 
            bbox=dict(boxstyle="round,pad=0.5", facecolor="lightgray", alpha=0.8))

plt.tight_layout()
plt.subplots_adjust(top=0.9, bottom=0.15)
plt.savefig('llm_progress_visualization.png', dpi=300, bbox_inches='tight')
print("Progress visualization saved as: llm_progress_visualization.png")

# Create architecture diagram
fig2, ax = plt.subplots(figsize=(14, 10))
ax.set_xlim(0, 10)
ax.set_ylim(0, 8)
ax.axis('off')

# Draw architecture blocks
blocks = [
    {'name': 'Text Input', 'pos': (1, 7), 'color': '#E8E8E8', 'agent': None},
    {'name': 'Tokenizer\n(fundamentals.scm)', 'pos': (1, 6), 'color': agent_colors['Agent 1'], 'agent': 'Agent 1'},
    {'name': 'Text Processing\n(text.scm)', 'pos': (3, 6), 'color': agent_colors['Agent 1'], 'agent': 'Agent 1'},
    {'name': 'Attention\n(attention.scm)', 'pos': (1, 4.5), 'color': agent_colors['Agent 2'], 'agent': 'Agent 2'},
    {'name': 'GPT Model\n(gpt.scm)', 'pos': (3, 4.5), 'color': agent_colors['Agent 2'], 'agent': 'Agent 2'},
    {'name': 'Pretraining\n(pretrain.scm)', 'pos': (5, 4.5), 'color': agent_colors['Agent 3'], 'agent': 'Agent 3'},
    {'name': 'Fine-tuning\n(finetune.scm)', 'pos': (1, 3), 'color': agent_colors['Agent 4'], 'agent': 'Agent 4'},
    {'name': 'Instructions\n(instruction.scm)', 'pos': (3, 3), 'color': agent_colors['Agent 4'], 'agent': 'Agent 4'},
    {'name': 'Main\n(main.scm)', 'pos': (5, 3), 'color': agent_colors['Coordinator'], 'agent': 'Coord'},
    {'name': 'Trained LLM', 'pos': (5, 1.5), 'color': '#90EE90', 'agent': None},
]

# Draw blocks and labels
for block in blocks:
    x, y = block['pos']
    color = block['color']
    
    # Draw rectangle
    rect = plt.Rectangle((x-0.4, y-0.3), 0.8, 0.6, 
                        facecolor=color, edgecolor='black', linewidth=2, alpha=0.8)
    ax.add_patch(rect)
    
    # Add text
    ax.text(x, y, block['name'], ha='center', va='center', 
            fontsize=10, fontweight='bold', wrap=True)
    
    # Add agent label
    if block['agent']:
        ax.text(x, y-0.45, block['agent'], ha='center', va='center',
                fontsize=8, style='italic', color='white',
                bbox=dict(boxstyle="round,pad=0.1", facecolor='black', alpha=0.7))

# Draw arrows showing data flow
arrows = [
    ((1, 6.7), (1, 6.3)),  # Input to Tokenizer
    ((1, 5.7), (1, 4.8)),  # Tokenizer to Attention
    ((3, 5.7), (3, 4.8)),  # Text Proc to GPT
    ((1.4, 4.5), (2.6, 4.5)),  # Attention to GPT
    ((3.4, 4.5), (4.6, 4.5)),  # GPT to Pretraining
    ((1, 4.2), (1, 3.3)),  # Attention to Fine-tuning
    ((3, 4.2), (3, 3.3)),  # GPT to Instructions
    ((5, 4.2), (5, 3.3)),  # Pretraining to Main
    ((1.4, 3), (4.6, 3)),  # Fine-tuning to Main
    ((3.4, 3), (4.6, 3)),  # Instructions to Main
    ((5, 2.7), (5, 1.8)),  # Main to Output
]

for start, end in arrows:
    ax.annotate('', xy=end, xytext=start,
                arrowprops=dict(arrowstyle='->', lw=2, color='gray'))

plt.title('LLM Architecture - Module Dependencies\nAutonomous Implementation by 4 Agents', 
          fontsize=16, fontweight='bold', pad=20)

# Add legend
legend_elements = [plt.Rectangle((0,0),1,1, facecolor=color, alpha=0.8, edgecolor='black') 
                  for color in agent_colors.values()]
legend_labels = list(agent_colors.keys())
ax.legend(legend_elements, legend_labels, loc='upper right', 
          bbox_to_anchor=(0.98, 0.98), title='Agents')

plt.savefig('llm_architecture_diagram.png', dpi=300, bbox_inches='tight')
print("Architecture diagram saved as: llm_architecture_diagram.png")

print("\nVisualization files created:")
print("1. llm_progress_visualization.png - Progress charts and statistics")
print("2. llm_architecture_diagram.png - Module dependencies and agent assignments")