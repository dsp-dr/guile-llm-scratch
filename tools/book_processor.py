#!/usr/bin/env python3
"""
Book Processor - Main tool for splitting and processing the LLM book
"""
import PyPDF2
import json
import os
from pathlib import Path
from typing import Dict, List, Tuple

class BookProcessor:
    def __init__(self, pdf_path: str):
        self.pdf_path = Path(pdf_path)
        self.load_structures()
        
    def load_structures(self):
        """Load book structure and implementation plan"""
        with open('book_structure.json', 'r') as f:
            self.book_structure = json.load(f)
        
        with open('implementation_plan.json', 'r') as f:
            self.implementation_plan = json.load(f)
    
    def split_book_smart(self, output_dir: str = "book_chapters"):
        """Split book into manageable chunks aligned with implementation phases"""
        output_path = Path(output_dir)
        output_path.mkdir(exist_ok=True)
        
        # Define optimal splits based on content and size
        splits = [
            {
                "name": "fundamentals",
                "chapters": [1, 2],
                "pages": (0, 50),  # Estimated
                "max_size_mb": 2,
                "description": "LLM fundamentals and text processing"
            },
            {
                "name": "attention_architecture", 
                "chapters": [3, 4],
                "pages": (50, 150),
                "max_size_mb": 3,
                "description": "Attention mechanisms and GPT architecture"
            },
            {
                "name": "training",
                "chapters": [5],
                "pages": (150, 220),
                "max_size_mb": 2.5,
                "description": "Pretraining on unlabeled data"
            },
            {
                "name": "fine_tuning",
                "chapters": [6, 7],
                "pages": (220, 370),
                "max_size_mb": 3.5,
                "description": "Fine-tuning and instruction following"
            }
        ]
        
        with open(self.pdf_path, 'rb') as pdf_file:
            reader = PyPDF2.PdfReader(pdf_file)
            
            for split in splits:
                writer = PyPDF2.PdfWriter()
                start_page, end_page = split['pages']
                
                # Extract pages for this split
                for page_num in range(start_page, min(end_page, len(reader.pages))):
                    writer.add_page(reader.pages[page_num])
                
                # Save split PDF
                split_filename = f"{split['name']}.pdf"
                split_path = output_path / split_filename
                
                with open(split_path, 'wb') as output_file:
                    writer.write(output_file)
                
                # Check file size
                file_size_mb = split_path.stat().st_size / (1024 * 1024)
                
                page_count = end_page - start_page
                split['output'] = {
                    'filename': split_filename,
                    'path': str(split_path),
                    'size_mb': round(file_size_mb, 2),
                    'page_count': page_count
                }
                
                print(f"Created: {split_filename} ({file_size_mb:.1f} MB, {page_count} pages)")
                print(f"  Content: {split['description']}")
                print(f"  Chapters: {split['chapters']}")
        
        # Save split metadata
        metadata_path = output_path / "splits_metadata.json"
        with open(metadata_path, 'w') as f:
            json.dump(splits, f, indent=2)
        
        return splits
    
    def create_guile_scaffold(self):
        """Create Guile implementation scaffold based on book structure"""
        
        # Create directory structure
        dirs = [
            "src/llm",
            "src/llm/layers",
            "src/llm/utils",
            "tests",
            "examples",
            "data",
            "models",
            "book_chapters"
        ]
        
        for dir_path in dirs:
            Path(dir_path).mkdir(parents=True, exist_ok=True)
        
        # Create main module files based on implementation plan
        for module in self.implementation_plan['modules']:
            module_path = Path(module['path'])
            
            if not module_path.exists():
                content = self._generate_module_template(module)
                module_path.parent.mkdir(parents=True, exist_ok=True)
                with open(module_path, 'w') as f:
                    f.write(content)
                print(f"Created: {module_path}")
        
        # Create README for book chapters
        readme_content = self._generate_chapter_readme()
        with open('book_chapters/README.md', 'w') as f:
            f.write(readme_content)
        
        # Create main entry point
        self._create_main_entry()
        
        print("\nScaffold created successfully!")
    
    def _generate_module_template(self, module: Dict) -> str:
        """Generate Guile module template"""
        chapter = module.get('chapter', {})
        template = f""";;; {module['name']}.scm --- {chapter.get('title', module['name'])}
;;; Chapter {chapter.get('number', 'N/A')}: Implementation

(define-module (llm {module['name'].replace('-', ' ')})
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export ())

;;; Commentary:
;;;
;;; This module implements concepts from Chapter {chapter.get('number', 'N/A')}
;;; of "Build a Large Language Model (From Scratch)"
;;;
;;; Components to implement:
"""
        
        for component in module['components']:
            template += f";;; - {component}\n"
        
        template += """;;;

;;; Code:

;; TODO: Implement based on book chapter

;;; {module['name']}.scm ends here
"""
        return template
    
    def _generate_chapter_readme(self) -> str:
        """Generate README for book chapters directory"""
        content = """# Book Chapter Splits

This directory contains the PDF splits of "Build a Large Language Model (From Scratch)" 
organized for manageable processing.

## Chapter Organization

"""
        
        for phase in self.implementation_plan['milestones']:
            content += f"### Phase {phase['phase']}: {phase['name']}\n"
            content += f"- Chapters: {phase['chapters']}\n"
            content += f"- Deliverables: {', '.join(phase['deliverables'])}\n\n"
        
        content += """
## Processing Guidelines

1. Each split is sized to be under 4MB for easy processing
2. Splits are aligned with implementation phases
3. Use the corresponding `.scm` files in `src/llm/` for implementation

## File Mapping

- `fundamentals.pdf` → `src/llm/fundamentals.scm`, `src/llm/text.scm`
- `attention_architecture.pdf` → `src/llm/attention.scm`, `src/llm/gpt.scm`
- `training.pdf` → `src/llm/pretrain.scm`
- `fine_tuning.pdf` → `src/llm/finetune.scm`, `src/llm/instruction.scm`
"""
        return content
    
    def _create_main_entry(self):
        """Create main entry point for the project"""
        main_content = """;;; main.scm --- Main entry point for Guile LLM implementation

(define-module (llm main)
  #:use-module (llm fundamentals)
  #:use-module (llm text)
  #:use-module (llm attention)
  #:use-module (llm gpt)
  #:export (main))

(define (main args)
  "Main entry point for the LLM implementation"
  (display "Guile LLM Implementation\\n")
  (display "Based on 'Build a Large Language Model (From Scratch)'\\n")
  (display "\\nModules loaded. Ready for implementation.\\n"))

;;; main.scm ends here
"""
        with open('src/llm/main.scm', 'w') as f:
            f.write(main_content)
        print("Created: src/llm/main.scm")

def main():
    import sys
    
    if len(sys.argv) < 2:
        print("Usage: python book_processor.py <pdf_path> [--split|--scaffold|--all]")
        sys.exit(1)
    
    pdf_path = sys.argv[1]
    processor = BookProcessor(pdf_path)
    
    action = sys.argv[2] if len(sys.argv) > 2 else '--all'
    
    if action in ['--split', '--all']:
        print("Splitting book into manageable chunks...")
        splits = processor.split_book_smart()
        print(f"\nCreated {len(splits)} split files in book_chapters/")
    
    if action in ['--scaffold', '--all']:
        print("\nCreating Guile implementation scaffold...")
        processor.create_guile_scaffold()
    
    print("\nReady to start implementation!")
    print("Next steps:")
    print("1. Read the split PDFs in book_chapters/")
    print("2. Implement corresponding modules in src/llm/")
    print("3. Test incrementally as you progress through phases")

if __name__ == "__main__":
    main()