#!/usr/bin/env python3
"""
Create ultra-compact text summaries and implementation guides for agents
"""
import PyPDF2
from pathlib import Path

def extract_text_from_pages(pdf_path, page_range):
    """Extract text from specific page range"""
    with open(pdf_path, 'rb') as file:
        reader = PyPDF2.PdfReader(file)
        text_content = []
        
        for page_num in range(page_range[0], min(page_range[1], len(reader.pages))):
            page = reader.pages[page_num]
            text = page.extract_text()
            if text.strip():
                text_content.append(f"--- Page {page_num} ---\n{text}\n")
        
        return '\n'.join(text_content)

def create_implementation_guide(module_name, key_concepts, functions_needed):
    """Create implementation guide for a module"""
    guide = f"""# {module_name} Implementation Guide

## Key Concepts
{key_concepts}

## Functions to Implement
{functions_needed}

## Guile Implementation Template
```scheme
(define-module (llm {module_name.lower()})
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export ({' '.join([f.split('(')[0] for f in functions_needed.split('\n') if f.strip().startswith('-')])}))

; TODO: Implement the functions listed above
```

## Next Steps
1. Read the key concepts above
2. Implement each function one by one
3. Test with simple examples
4. Export all public functions
"""
    return guide

if __name__ == "__main__":
    # Create resources directory
    resources_dir = Path("agent_resources")
    resources_dir.mkdir(exist_ok=True)
    
    # Agent 1: Foundation - already complete, but let's help with text.scm
    text_guide = create_implementation_guide(
        "text",
        """Text preprocessing for LLM training:
- Clean and normalize text data
- Handle special characters and encoding
- Prepare text corpora for tokenization
- Split text into sentences and paragraphs""",
        """- preprocess-text: Clean text data
- normalize-text: Handle encoding and special chars  
- split-sentences: Break text into sentences
- load-corpus: Load text files
- clean-corpus: Remove unwanted content"""
    )
    
    with open(resources_dir / "text_module_guide.txt", 'w') as f:
        f.write(text_guide)
    
    # Agent 2: Architecture 
    attention_guide = create_implementation_guide(
        "attention",
        """Self-attention mechanism fundamentals:
- Scaled dot-product attention: Attention(Q,K,V) = softmax(QK^T/√d)V
- Multi-head attention: Run attention in parallel with different projections
- Causal masking: Prevent looking at future tokens
- Query, Key, Value matrices from same input""",
        """- scaled-dot-product-attention: Core attention mechanism
- multi-head-attention: Parallel attention heads
- create-attention-mask: Causal masking for GPT
- softmax: Softmax function for attention weights
- linear-projection: Matrix multiplication for Q,K,V"""
    )
    
    with open(resources_dir / "attention_module_guide.txt", 'w') as f:
        f.write(attention_guide)
    
    # Extract key pages from attention PDF
    try:
        attention_text = extract_text_from_pages("book_chapters/attention_architecture.pdf", (0, 10))
        with open(resources_dir / "attention_key_pages.txt", 'w') as f:
            f.write(attention_text[:5000])  # First 5000 chars only
    except Exception as e:
        print(f"Could not extract attention text: {e}")
    
    # Agent 4: Application
    finetune_guide = create_implementation_guide(
        "finetune",
        """Fine-tuning for task adaptation:
- Add task-specific head on top of pretrained model
- Freeze/unfreeze layers selectively  
- Use smaller learning rate than pretraining
- Classification head: linear layer + softmax""",
        """- create-classification-head: Add classification layer
- finetune-model: Main fine-tuning loop
- freeze-parameters: Freeze model layers
- unfreeze-parameters: Unfreeze for training
- task-specific-loss: Loss for classification"""
    )
    
    with open(resources_dir / "finetune_module_guide.txt", 'w') as f:
        f.write(finetune_guide)
    
    # Create simple implementation commands file
    commands = """# Quick Implementation Commands for Agents

## Agent 1 (Foundation) - Text Module
Read agent_resources/text_module_guide.txt
Implement in src/llm/text.scm

## Agent 2 (Architecture) - Attention 
Read agent_resources/attention_module_guide.txt
Read agent_resources/attention_key_pages.txt
Implement in src/llm/attention.scm

## Agent 3 (Training) - Already Complete!
Your pretrain.scm is excellent. You could:
- Add more optimizers (SGD, RMSprop)
- Add learning rate scheduling
- Improve data loading efficiency

## Agent 4 (Application) - Fine-tuning
Read agent_resources/finetune_module_guide.txt  
Implement in src/llm/finetune.scm

## GPT Model Structure (for Agent 2)
```scheme
(define-record-type <gpt-model>
  (make-gpt-model layers vocab-size embedding-dim)
  gpt-model?
  (layers gpt-layers)
  (vocab-size gpt-vocab-size)
  (embedding-dim gpt-embedding-dim))

(define (transformer-block input)
  ; 1. Multi-head attention
  ; 2. Add & Norm
  ; 3. Feed-forward  
  ; 4. Add & Norm
  )
```
"""
    
    with open(resources_dir / "quick_commands.txt", 'w') as f:
        f.write(commands)
    
    print(f"Created implementation guides in: {resources_dir}")
    print("Files created:")
    for file in resources_dir.glob("*.txt"):
        print(f"  - {file.name} ({file.stat().st_size} bytes)")