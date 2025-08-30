#!/usr/bin/env python3
import PyPDF2
import json
import re
from pathlib import Path
from typing import Dict, List, Tuple, Optional

class PDFAnalyzer:
    def __init__(self, pdf_path: str):
        self.pdf_path = Path(pdf_path)
        self.reader = None
        self.toc_data = []
        self.page_mapping = {}
        
    def load_pdf(self):
        with open(self.pdf_path, 'rb') as file:
            self.reader = PyPDF2.PdfReader(file)
            return len(self.reader.pages)
    
    def extract_toc_from_pages(self, max_pages: int = 30) -> List[Dict]:
        """Extract TOC by analyzing first pages of the PDF"""
        toc_entries = []
        toc_pattern = re.compile(r'^(Chapter\s+\d+|Part\s+[IVX]+|Appendix\s+[A-Z])[:\s]+(.+?)[\s\.]+(\d+)\s*$', re.MULTILINE)
        section_pattern = re.compile(r'^(\d+\.\d+(?:\.\d+)?)\s+(.+?)[\s\.]+(\d+)\s*$', re.MULTILINE)
        
        with open(self.pdf_path, 'rb') as file:
            reader = PyPDF2.PdfReader(file)
            
            for pdf_page_num in range(min(max_pages, len(reader.pages))):
                page = reader.pages[pdf_page_num]
                text = page.extract_text()
                
                # Look for chapter entries
                for match in toc_pattern.finditer(text):
                    toc_entries.append({
                        'type': 'chapter',
                        'label': match.group(1),
                        'title': match.group(2).strip(),
                        'book_page': int(match.group(3)),
                        'pdf_page': None  # Will be calculated later
                    })
                
                # Look for section entries
                for match in section_pattern.finditer(text):
                    toc_entries.append({
                        'type': 'section',
                        'number': match.group(1),
                        'title': match.group(2).strip(),
                        'book_page': int(match.group(3)),
                        'pdf_page': None
                    })
        
        return toc_entries
    
    def find_page_number_mapping(self, sample_size: int = 10) -> Dict[int, int]:
        """Map book page numbers to PDF page numbers by scanning for page numbers in text"""
        mapping = {}
        
        with open(self.pdf_path, 'rb') as file:
            reader = PyPDF2.PdfReader(file)
            
            # Sample pages throughout the document
            total_pages = len(reader.pages)
            sample_pages = [i * (total_pages // sample_size) for i in range(sample_size)]
            
            for pdf_page_num in sample_pages:
                if pdf_page_num >= total_pages:
                    continue
                    
                page = reader.pages[pdf_page_num]
                text = page.extract_text()
                
                # Look for page numbers at the bottom/top of pages
                page_num_matches = re.findall(r'^\s*(\d+)\s*$|^\s*Page\s+(\d+)\s*$|\s+(\d+)\s*$', text, re.MULTILINE)
                
                for match in page_num_matches:
                    for group in match:
                        if group and group.isdigit():
                            book_page = int(group)
                            if 1 <= book_page <= 1000:  # Reasonable page range
                                mapping[book_page] = pdf_page_num
                                break
        
        return mapping
    
    def estimate_pdf_page(self, book_page: int, mapping: Dict[int, int]) -> int:
        """Estimate PDF page number from book page number using known mappings"""
        if book_page in mapping:
            return mapping[book_page]
        
        # Find closest known pages
        known_pages = sorted(mapping.keys())
        if not known_pages:
            return book_page  # Fallback to direct mapping
        
        # Linear interpolation
        lower = [p for p in known_pages if p <= book_page]
        upper = [p for p in known_pages if p > book_page]
        
        if lower and upper:
            lower_book = max(lower)
            upper_book = min(upper)
            lower_pdf = mapping[lower_book]
            upper_pdf = mapping[upper_book]
            
            # Linear interpolation
            ratio = (book_page - lower_book) / (upper_book - lower_book)
            estimated = lower_pdf + ratio * (upper_pdf - lower_pdf)
            return int(estimated)
        elif lower:
            # Extrapolate from lower bound
            lower_book = max(lower)
            return mapping[lower_book] + (book_page - lower_book)
        else:
            # Extrapolate from upper bound
            upper_book = min(upper)
            return max(0, mapping[upper_book] - (upper_book - book_page))
    
    def analyze_structure(self) -> Dict:
        """Complete analysis of PDF structure"""
        total_pages = self.load_pdf()
        toc_entries = self.extract_toc_from_pages()
        page_mapping = self.find_page_number_mapping()
        
        # Update TOC with estimated PDF pages
        for entry in toc_entries:
            entry['pdf_page'] = self.estimate_pdf_page(entry['book_page'], page_mapping)
        
        return {
            'pdf_path': str(self.pdf_path),
            'total_pdf_pages': total_pages,
            'page_mapping': page_mapping,
            'toc': toc_entries,
            'chapters': self._organize_by_chapters(toc_entries)
        }
    
    def _organize_by_chapters(self, toc_entries: List[Dict]) -> List[Dict]:
        """Organize TOC entries into chapter hierarchy"""
        chapters = []
        current_chapter = None
        
        for entry in toc_entries:
            if entry['type'] == 'chapter':
                if current_chapter:
                    chapters.append(current_chapter)
                current_chapter = {
                    'label': entry['label'],
                    'title': entry['title'],
                    'book_page': entry['book_page'],
                    'pdf_page': entry['pdf_page'],
                    'sections': []
                }
            elif entry['type'] == 'section' and current_chapter:
                current_chapter['sections'].append({
                    'number': entry['number'],
                    'title': entry['title'],
                    'book_page': entry['book_page'],
                    'pdf_page': entry['pdf_page']
                })
        
        if current_chapter:
            chapters.append(current_chapter)
        
        return chapters
    
    def save_analysis(self, output_path: str = None):
        """Save analysis to JSON file"""
        if output_path is None:
            output_path = self.pdf_path.stem + '_structure.json'
        
        analysis = self.analyze_structure()
        
        with open(output_path, 'w') as f:
            json.dump(analysis, f, indent=2)
        
        return output_path

if __name__ == "__main__":
    import sys
    
    if len(sys.argv) < 2:
        print("Usage: python pdf_analyzer.py <pdf_path>")
        sys.exit(1)
    
    analyzer = PDFAnalyzer(sys.argv[1])
    analysis = analyzer.analyze_structure()
    
    print(f"PDF Analysis Complete:")
    print(f"Total PDF Pages: {analysis['total_pdf_pages']}")
    print(f"Found {len(analysis['chapters'])} chapters")
    print(f"\nChapters:")
    for chapter in analysis['chapters']:
        print(f"  {chapter['label']}: {chapter['title']}")
        print(f"    Book Page: {chapter['book_page']}, PDF Page: {chapter['pdf_page']}")
        if chapter['sections']:
            print(f"    {len(chapter['sections'])} sections")
    
    output_file = analyzer.save_analysis()
    print(f"\nAnalysis saved to: {output_file}")