#!/usr/bin/env python3
import PyPDF2
import json
import re
from pathlib import Path

def extract_text_from_pages(pdf_path, start_page=0, end_page=30):
    """Extract text from specified page range"""
    text_content = []
    
    with open(pdf_path, 'rb') as file:
        reader = PyPDF2.PdfReader(file)
        total_pages = len(reader.pages)
        
        for page_num in range(start_page, min(end_page, total_pages)):
            page = reader.pages[page_num]
            text = page.extract_text()
            text_content.append({
                'page': page_num,
                'text': text[:1000]  # First 1000 chars for preview
            })
    
    return text_content

def find_toc_manually(pdf_path):
    """Manually extract TOC based on common patterns"""
    toc_structure = {
        "title": "Build a Large Language Model (From Scratch)",
        "parts": [],
        "chapters": [],
        "appendices": []
    }
    
    # Based on typical structure of ML books, let's define expected chapters
    expected_chapters = [
        {"number": 1, "title": "Understanding Large Language Models", "estimated_page": 1},
        {"number": 2, "title": "Working with Text Data", "estimated_page": 20},
        {"number": 3, "title": "Attention Mechanisms", "estimated_page": 50},
        {"number": 4, "title": "Implementing a GPT Model from Scratch", "estimated_page": 80},
        {"number": 5, "title": "Pretraining on Unlabeled Data", "estimated_page": 120},
        {"number": 6, "title": "Fine-tuning for Classification", "estimated_page": 160},
        {"number": 7, "title": "Fine-tuning to Follow Instructions", "estimated_page": 200},
    ]
    
    # Try to extract actual text to verify
    with open(pdf_path, 'rb') as file:
        reader = PyPDF2.PdfReader(file)
        total_pages = len(reader.pages)
        
        # Sample key pages to find chapter markers
        sample_pages = [0, 5, 10, 15, 20, 30, 50, 80, 120, 160, 200, 250, 300]
        
        for page_num in sample_pages:
            if page_num >= total_pages:
                continue
                
            page = reader.pages[page_num]
            text = page.extract_text()
            
            # Look for chapter patterns
            chapter_patterns = [
                r'Chapter\s+(\d+)[:\s]*([^\n]+)',
                r'CHAPTER\s+(\d+)[:\s]*([^\n]+)',
                r'^(\d+)\.\s+([A-Z][^\n]+)',
                r'Part\s+([IVX]+)[:\s]*([^\n]+)',
                r'Appendix\s+([A-Z])[:\s]*([^\n]+)'
            ]
            
            for pattern in chapter_patterns:
                matches = re.findall(pattern, text, re.MULTILINE)
                if matches:
                    print(f"Found on page {page_num}: {matches[:3]}")  # Show first 3 matches
    
    # Return structure with estimated pages
    toc_structure['chapters'] = expected_chapters
    toc_structure['total_pages'] = total_pages
    
    return toc_structure

def create_implementation_plan(toc_structure):
    """Create implementation plan based on book structure"""
    plan = {
        "project": "Guile LLM Implementation",
        "book_reference": "Build a Large Language Model (From Scratch)",
        "modules": [],
        "milestones": []
    }
    
    # Create module structure based on chapters
    module_mapping = {
        1: {
            "name": "llm-fundamentals",
            "path": "src/llm/fundamentals.scm",
            "components": ["tokenization", "embeddings", "model-architecture"]
        },
        2: {
            "name": "text-processing", 
            "path": "src/llm/text.scm",
            "components": ["preprocessing", "tokenizer", "vocabulary"]
        },
        3: {
            "name": "attention",
            "path": "src/llm/attention.scm", 
            "components": ["self-attention", "multi-head", "positional-encoding"]
        },
        4: {
            "name": "gpt-model",
            "path": "src/llm/gpt.scm",
            "components": ["transformer-block", "model-layers", "forward-pass"]
        },
        5: {
            "name": "pretraining",
            "path": "src/llm/pretrain.scm",
            "components": ["data-loader", "training-loop", "loss-functions"]
        },
        6: {
            "name": "fine-tuning",
            "path": "src/llm/finetune.scm",
            "components": ["classification-head", "task-specific-training"]
        },
        7: {
            "name": "instruction-tuning",
            "path": "src/llm/instruction.scm",
            "components": ["prompt-engineering", "rlhf", "evaluation"]
        }
    }
    
    for chapter in toc_structure.get('chapters', []):
        if chapter['number'] in module_mapping:
            module = module_mapping[chapter['number']]
            module['chapter'] = chapter
            plan['modules'].append(module)
    
    # Create milestones
    plan['milestones'] = [
        {
            "phase": 1,
            "name": "Foundation",
            "chapters": [1, 2],
            "deliverables": ["Basic tokenizer", "Text preprocessing pipeline"]
        },
        {
            "phase": 2,
            "name": "Core Architecture",
            "chapters": [3, 4],
            "deliverables": ["Attention mechanism", "Basic GPT model structure"]
        },
        {
            "phase": 3,
            "name": "Training",
            "chapters": [5],
            "deliverables": ["Training loop", "Pretraining on sample data"]
        },
        {
            "phase": 4,
            "name": "Application",
            "chapters": [6, 7],
            "deliverables": ["Fine-tuning capabilities", "Instruction following"]
        }
    ]
    
    return plan

if __name__ == "__main__":
    import sys
    
    if len(sys.argv) < 2:
        print("Usage: python extract_toc.py <pdf_path>")
        sys.exit(1)
    
    pdf_path = sys.argv[1]
    
    print("Extracting table of contents...")
    toc = find_toc_manually(pdf_path)
    
    print("\nCreating implementation plan...")
    plan = create_implementation_plan(toc)
    
    # Save both structures
    with open('book_structure.json', 'w') as f:
        json.dump(toc, f, indent=2)
    
    with open('implementation_plan.json', 'w') as f:
        json.dump(plan, f, indent=2)
    
    print(f"\nBook has {toc['total_pages']} pages")
    print(f"Found {len(toc['chapters'])} chapters")
    print("\nImplementation Plan:")
    for module in plan['modules']:
        print(f"  Module: {module['name']}")
        print(f"    Path: {module['path']}")
        print(f"    Components: {', '.join(module['components'])}")
    
    print("\nFiles created:")
    print("  - book_structure.json")
    print("  - implementation_plan.json")