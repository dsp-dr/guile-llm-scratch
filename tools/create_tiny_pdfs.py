#!/usr/bin/env python3
"""
Create tiny PDFs and PNG extracts for agents to process
"""
import PyPDF2
import os
from pathlib import Path

def split_pdf_ultra_small(input_pdf, output_dir, max_pages=3):
    """Split PDF into ultra-small chunks of max_pages each"""
    output_path = Path(output_dir)
    output_path.mkdir(exist_ok=True)
    
    splits = []
    
    with open(input_pdf, 'rb') as file:
        reader = PyPDF2.PdfReader(file)
        total_pages = len(reader.pages)
        
        chunk_num = 0
        for start_page in range(0, total_pages, max_pages):
            writer = PyPDF2.PdfWriter()
            end_page = min(start_page + max_pages, total_pages)
            
            # Extract pages
            for page_num in range(start_page, end_page):
                writer.add_page(reader.pages[page_num])
            
            # Save tiny PDF
            chunk_filename = f"tiny_{chunk_num:03d}_p{start_page}-{end_page-1}.pdf"
            chunk_path = output_path / chunk_filename
            
            with open(chunk_path, 'wb') as output_file:
                writer.write(output_file)
            
            file_size = chunk_path.stat().st_size
            splits.append({
                'filename': chunk_filename,
                'pages': f"{start_page}-{end_page-1}",
                'size_kb': file_size // 1024,
                'page_count': end_page - start_page
            })
            
            print(f"Created: {chunk_filename} ({file_size//1024}KB, {end_page-start_page} pages)")
            
            # Stop if we get very small files
            if file_size < 50000:  # Less than 50KB
                chunk_num += 1
                continue
                
            chunk_num += 1
    
    return splits

def extract_key_pages_text(input_pdf, key_pages, output_file):
    """Extract text from key pages only"""
    with open(input_pdf, 'rb') as file:
        reader = PyPDF2.PdfReader(file)
        
        extracted_text = []
        for page_num in key_pages:
            if page_num < len(reader.pages):
                page = reader.pages[page_num]
                text = page.extract_text()
                extracted_text.append(f"=== PAGE {page_num} ===\n{text}\n")
        
        with open(output_file, 'w') as out:
            out.write('\n'.join(extracted_text))
    
    return len(extracted_text)

if __name__ == "__main__":
    # Create tiny splits directory
    tiny_dir = Path("book_chapters_tiny")
    tiny_dir.mkdir(exist_ok=True)
    
    # Define key pages for each book section
    key_pages = {
        'fundamentals.pdf': [0, 1, 2, 10, 20, 30],  # First pages + key sections
        'attention_architecture.pdf': [0, 1, 2, 5, 10, 15, 25, 30],  # Attention basics
        'training.pdf': [0, 1, 2, 5, 10, 15],  # Training overview 
        'fine_tuning.pdf': [0, 1, 2, 5, 10, 15, 20]  # Fine-tuning basics
    }
    
    print("Creating ultra-small PDFs and text extracts...")
    
    for book_pdf in Path("book_chapters").glob("*.pdf"):
        print(f"\nProcessing {book_pdf.name}...")
        
        # Create tiny PDF splits (3 pages each)
        tiny_subdir = tiny_dir / book_pdf.stem
        splits = split_pdf_ultra_small(book_pdf, tiny_subdir, max_pages=3)
        
        # Extract key pages as text
        if book_pdf.name in key_pages:
            text_file = tiny_subdir / "key_pages.txt"
            pages_extracted = extract_key_pages_text(book_pdf, key_pages[book_pdf.name], text_file)
            print(f"  Extracted {pages_extracted} key pages to text")
        
        # Create summary file
        summary_file = tiny_subdir / "splits_summary.txt"
        with open(summary_file, 'w') as f:
            f.write(f"Tiny PDF splits for {book_pdf.name}\n")
            f.write(f"Original size: {book_pdf.stat().st_size//1024}KB\n\n")
            f.write("Available splits:\n")
            for split in splits[:10]:  # First 10 splits only
                f.write(f"- {split['filename']}: pages {split['pages']} ({split['size_kb']}KB)\n")
            f.write(f"\nTotal: {len(splits)} tiny PDFs created\n")
    
    print(f"\nAll tiny PDFs created in: {tiny_dir}")
    print("Each PDF is 3 pages or less and should be under Claude's limits.")