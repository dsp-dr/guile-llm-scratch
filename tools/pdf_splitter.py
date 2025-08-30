#!/usr/bin/env python3
import PyPDF2
import json
from pathlib import Path
from typing import Dict, List, Optional, Tuple

class PDFSplitter:
    def __init__(self, pdf_path: str, structure_json: str = None):
        self.pdf_path = Path(pdf_path)
        self.structure = None
        
        if structure_json:
            with open(structure_json, 'r') as f:
                self.structure = json.load(f)
        else:
            # Try to load from default location
            default_json = self.pdf_path.stem + '_structure.json'
            if Path(default_json).exists():
                with open(default_json, 'r') as f:
                    self.structure = json.load(f)
    
    def split_by_chapters(self, output_dir: str = "splits") -> List[Dict]:
        """Split PDF into individual chapter PDFs"""
        output_path = Path(output_dir)
        output_path.mkdir(exist_ok=True)
        
        if not self.structure or 'chapters' not in self.structure:
            raise ValueError("No structure data available. Run pdf_analyzer first.")
        
        splits_info = []
        
        with open(self.pdf_path, 'rb') as pdf_file:
            reader = PyPDF2.PdfReader(pdf_file)
            chapters = self.structure['chapters']
            
            for i, chapter in enumerate(chapters):
                writer = PyPDF2.PdfWriter()
                
                # Determine page range
                start_pdf_page = chapter['pdf_page']
                if i + 1 < len(chapters):
                    end_pdf_page = chapters[i + 1]['pdf_page'] - 1
                else:
                    end_pdf_page = len(reader.pages) - 1
                
                # Extract pages
                for page_num in range(start_pdf_page, min(end_pdf_page + 1, len(reader.pages))):
                    writer.add_page(reader.pages[page_num])
                
                # Save chapter PDF
                chapter_filename = f"chapter_{i+1:02d}_{self._sanitize_filename(chapter['label'])}.pdf"
                chapter_path = output_path / chapter_filename
                
                with open(chapter_path, 'wb') as output_file:
                    writer.write(output_file)
                
                split_info = {
                    'chapter': chapter['label'],
                    'title': chapter['title'],
                    'filename': chapter_filename,
                    'path': str(chapter_path),
                    'pdf_pages': f"{start_pdf_page}-{end_pdf_page}",
                    'page_count': end_pdf_page - start_pdf_page + 1,
                    'book_page_start': chapter['book_page']
                }
                
                splits_info.append(split_info)
                print(f"Created: {chapter_filename} ({split_info['page_count']} pages)")
        
        # Save split info
        info_path = output_path / "splits_info.json"
        with open(info_path, 'w') as f:
            json.dump(splits_info, f, indent=2)
        
        return splits_info
    
    def split_by_size(self, max_pages: int = 50, output_dir: str = "splits") -> List[Dict]:
        """Split PDF into chunks of maximum page size"""
        output_path = Path(output_dir)
        output_path.mkdir(exist_ok=True)
        
        splits_info = []
        
        with open(self.pdf_path, 'rb') as pdf_file:
            reader = PyPDF2.PdfReader(pdf_file)
            total_pages = len(reader.pages)
            
            chunk_num = 0
            for start_page in range(0, total_pages, max_pages):
                writer = PyPDF2.PdfWriter()
                end_page = min(start_page + max_pages, total_pages)
                
                # Extract pages
                for page_num in range(start_page, end_page):
                    writer.add_page(reader.pages[page_num])
                
                # Save chunk PDF
                chunk_filename = f"chunk_{chunk_num:03d}_pages_{start_page:04d}-{end_page-1:04d}.pdf"
                chunk_path = output_path / chunk_filename
                
                with open(chunk_path, 'wb') as output_file:
                    writer.write(output_file)
                
                split_info = {
                    'chunk': chunk_num,
                    'filename': chunk_filename,
                    'path': str(chunk_path),
                    'pdf_pages': f"{start_page}-{end_page-1}",
                    'page_count': end_page - start_page
                }
                
                # Try to determine which chapters are in this chunk
                if self.structure and 'chapters' in self.structure:
                    chapters_in_chunk = []
                    for chapter in self.structure['chapters']:
                        if start_page <= chapter['pdf_page'] < end_page:
                            chapters_in_chunk.append(chapter['label'])
                    if chapters_in_chunk:
                        split_info['chapters'] = chapters_in_chunk
                
                splits_info.append(split_info)
                print(f"Created: {chunk_filename} ({split_info['page_count']} pages)")
                chunk_num += 1
        
        # Save split info
        info_path = output_path / "chunks_info.json"
        with open(info_path, 'w') as f:
            json.dump(splits_info, f, indent=2)
        
        return splits_info
    
    def extract_page_range(self, start_page: int, end_page: int, output_file: str) -> str:
        """Extract specific page range to a new PDF"""
        with open(self.pdf_path, 'rb') as pdf_file:
            reader = PyPDF2.PdfReader(pdf_file)
            writer = PyPDF2.PdfWriter()
            
            # Ensure valid page range
            start_page = max(0, start_page)
            end_page = min(end_page, len(reader.pages) - 1)
            
            for page_num in range(start_page, end_page + 1):
                writer.add_page(reader.pages[page_num])
            
            with open(output_file, 'wb') as output:
                writer.write(output)
        
        return output_file
    
    def extract_chapter_sections(self, chapter_index: int, output_dir: str = "sections") -> List[Dict]:
        """Extract individual sections from a chapter"""
        output_path = Path(output_dir)
        output_path.mkdir(exist_ok=True)
        
        if not self.structure or 'chapters' not in self.structure:
            raise ValueError("No structure data available. Run pdf_analyzer first.")
        
        if chapter_index >= len(self.structure['chapters']):
            raise ValueError(f"Chapter index {chapter_index} out of range")
        
        chapter = self.structure['chapters'][chapter_index]
        sections = chapter.get('sections', [])
        
        if not sections:
            print(f"No sections found in {chapter['label']}")
            return []
        
        splits_info = []
        
        with open(self.pdf_path, 'rb') as pdf_file:
            reader = PyPDF2.PdfReader(pdf_file)
            
            for i, section in enumerate(sections):
                writer = PyPDF2.PdfWriter()
                
                # Determine page range
                start_pdf_page = section['pdf_page']
                if i + 1 < len(sections):
                    end_pdf_page = sections[i + 1]['pdf_page'] - 1
                elif chapter_index + 1 < len(self.structure['chapters']):
                    end_pdf_page = self.structure['chapters'][chapter_index + 1]['pdf_page'] - 1
                else:
                    end_pdf_page = len(reader.pages) - 1
                
                # Extract pages
                for page_num in range(start_pdf_page, min(end_pdf_page + 1, len(reader.pages))):
                    writer.add_page(reader.pages[page_num])
                
                # Save section PDF
                section_filename = f"{chapter['label'].lower().replace(' ', '_')}_section_{section['number']}.pdf"
                section_path = output_path / section_filename
                
                with open(section_path, 'wb') as output_file:
                    writer.write(output_file)
                
                split_info = {
                    'chapter': chapter['label'],
                    'section': section['number'],
                    'title': section['title'],
                    'filename': section_filename,
                    'path': str(section_path),
                    'pdf_pages': f"{start_pdf_page}-{end_pdf_page}",
                    'page_count': end_pdf_page - start_pdf_page + 1
                }
                
                splits_info.append(split_info)
                print(f"Created: {section_filename} ({split_info['page_count']} pages)")
        
        return splits_info
    
    def _sanitize_filename(self, name: str) -> str:
        """Sanitize filename by removing invalid characters"""
        import re
        # Remove or replace invalid filename characters
        name = re.sub(r'[<>:"/\\|?*]', '_', name)
        name = name.replace(' ', '_')
        return name.lower()

if __name__ == "__main__":
    import sys
    
    if len(sys.argv) < 2:
        print("Usage: python pdf_splitter.py <pdf_path> [structure_json] [--chapters|--size N]")
        sys.exit(1)
    
    pdf_path = sys.argv[1]
    structure_json = sys.argv[2] if len(sys.argv) > 2 and not sys.argv[2].startswith('--') else None
    
    splitter = PDFSplitter(pdf_path, structure_json)
    
    if '--chapters' in sys.argv:
        print("Splitting by chapters...")
        splits = splitter.split_by_chapters()
        print(f"\nCreated {len(splits)} chapter files")
    elif '--size' in sys.argv:
        idx = sys.argv.index('--size')
        max_pages = int(sys.argv[idx + 1]) if idx + 1 < len(sys.argv) else 50
        print(f"Splitting into chunks of {max_pages} pages...")
        splits = splitter.split_by_size(max_pages)
        print(f"\nCreated {len(splits)} chunk files")
    else:
        print("Please specify --chapters or --size N")