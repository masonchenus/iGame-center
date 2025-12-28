"""
Input Processor for AI Backend

Handles input validation, preprocessing, and sanitization.
"""

import re
import json
import html
from typing import Dict, Any, List, Optional, Union, Tuple
from urllib.parse import unquote
from .config import ProcessorConfig, get_config


class InputProcessor:
    """
    Processes and validates input data for the AI system.
    
    Responsibilities:
    - Input validation and sanitization
    - Text normalization and cleaning
    - Data type detection and conversion
    - Security filtering
    """
    
    def __init__(self, config: Optional[ProcessorConfig] = None):
        """Initialize the input processor with configuration."""
        self.config = config or get_config()
        
        # Security patterns for filtering malicious content
        self._security_patterns = {
            'script_tags': re.compile(r'<script[^>]*>.*?</script>', re.IGNORECASE | re.DOTALL),
            'javascript_protocol': re.compile(r'javascript:', re.IGNORECASE),
            'sql_injection': re.compile(r'(\bunion\b|\bselect\b|\bdrop\b|\binsert\b|\bdelete\b|\bupdate\b)', re.IGNORECASE),
            'command_injection': re.compile(r'[;&|`$(){}\[\]\\]', re.IGNORECASE)
        }
        
        # Content type patterns for detection
        self._content_patterns = {
            'json': re.compile(r'^\s*[\{\[]'),
            'code': re.compile(r'(def |class |function |var |let |const |import |from )', re.IGNORECASE),
            'url': re.compile(r'https?://[^\s\]]+'),
            'email': re.compile(r'\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b')
        }
    
    def process(self, data: Union[str, Dict[str, Any]]) -> Dict[str, Any]:
        """
        Process and validate input data.
        
        Args:
            data: Input data (string or dict)
            
        Returns:
            Dict containing processed input data and metadata
        """
        # Handle string input
        if isinstance(data, str):
            return self._process_string_input(data)
        
        # Handle dict input
        elif isinstance(data, dict):
            return self._process_dict_input(data)
        
        # Handle other types
        else:
            return self._process_other_input(data)
    
    def _process_string_input(self, text: str) -> Dict[str, Any]:
        """Process string input data."""
        start_metadata = {
            "original_length": len(text),
            "input_type": "string",
            "processing_timestamp": self._get_timestamp()
        }
        
        try:
            # Step 1: Basic validation
            if not self._basic_validation(text):
                return self._create_error_response("Invalid input data", start_metadata)
            
            # Step 2: Security filtering
            if self.config.sanitize_input:
                text = self._apply_security_filtering(text)
            
            # Step 3: URL decoding
            text = self._apply_url_decoding(text)
            
            # Step 4: HTML unescaping
            text = self._apply_html_unescaping(text)
            
            # Step 5: Text normalization
            if self.config.normalize_text:
                text = self._normalize_text(text)
            
            # Step 6: Content analysis
            content_analysis = self._analyze_content(text)
            
            # Step 7: Length validation
            if len(text) > self.config.max_input_length:
                text = text[:self.config.max_input_length]
                content_analysis["truncated"] = True
            
            return {
                "content": text,
                "content_analysis": content_analysis,
                "metadata": {**start_metadata, "final_length": len(text)}
            }
            
        except Exception as e:
            return self._create_error_response(f"Input processing error: {str(e)}", start_metadata)
    
    def _process_dict_input(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Process dictionary input data."""
        start_metadata = {
            "original_data": data,
            "input_type": "dict",
            "processing_timestamp": self._get_timestamp()
        }
        
        try:
            # Extract content from common fields
            content = self._extract_content_from_dict(data)
            
            # Process the extracted content
            processed_content = self._process_string_input(content)
            
            # Merge with original data
            result = processed_content.copy()
            result["original_data"] = data
            result["extracted_fields"] = list(data.keys())
            
            return result
            
        except Exception as e:
            return self._create_error_response(f"Dict processing error: {str(e)}", start_metadata)
    
    def _process_other_input(self, data: Any) -> Dict[str, Any]:
        """Process other input types."""
        start_metadata = {
            "original_data": data,
            "input_type": type(data).__name__,
            "processing_timestamp": self._get_timestamp()
        }
        
        try:
            # Convert to string
            content = str(data)
            processed_content = self._process_string_input(content)
            
            result = processed_content.copy()
            result["original_data"] = data
            result["type_converted"] = True
            
            return result
            
        except Exception as e:
            return self._create_error_response(f"Type processing error: {str(e)}", start_metadata)
    
    def _basic_validation(self, text: str) -> bool:
        """Perform basic input validation."""
        # Check for empty input
        if not self.config.allow_empty_input and not text.strip():
            return False
        
        # Check for extremely long input
        if len(text) > self.config.max_input_length * 2:  # Allow some buffer
            return False
        
        # Check for control characters (except newlines and tabs)
        for char in text:
            if ord(char) < 32 and char not in '\n\t\r':
                return False
        
        return True
    
    def _apply_security_filtering(self, text: str) -> str:
        """Apply security filtering to remove malicious content."""
        # Remove script tags
        text = self._security_patterns['script_tags'].sub('', text)
        
        # Remove javascript protocol
        text = self._security_patterns['javascript_protocol'].sub('', text)
        
        # Basic SQL injection protection (escape quotes)
        text = text.replace("'", "''").replace('"', '""')
        
        # Remove potentially dangerous command injection characters in specific contexts
        if self.config.strict_validation:
            text = self._security_patterns['command_injection'].sub('', text)
        
        return text
    
    def _apply_url_decoding(self, text: str) -> str:
        """Apply URL decoding if needed."""
        # Check if text contains URL encoding
        if '%' in text and any(ord(c) > 127 for c in text):
            try:
                return unquote(text)
            except Exception:
                return text  # Return original if decoding fails
        return text
    
    def _apply_html_unescaping(self, text: str) -> str:
        """Apply HTML unescaping."""
        if '&' in text and (';' in text or '&amp;' in text):
            try:
                return html.unescape(text)
            except Exception:
                return text
        return text
    
    def _normalize_text(self, text: str) -> str:
        """Normalize text content."""
        # Remove excessive whitespace
        text = re.sub(r'\s+', ' ', text)
        
        # Normalize line endings
        text = text.replace('\r\n', '\n').replace('\r', '\n')
        
        # Remove leading/trailing whitespace
        text = text.strip()
        
        # Normalize Unicode characters
        text = text.replace('\u200b', '')  # Zero-width space
        text = text.replace('\ufeff', '')  # BOM
        
        return text
    
    def _analyze_content(self, text: str) -> Dict[str, Any]:
        """Analyze content characteristics."""
        analysis = {
            "word_count": len(text.split()),
            "character_count": len(text),
            "line_count": len(text.split('\n')),
            "has_punctuation": bool(re.search(r'[.!?;,:]', text)),
            "has_numbers": bool(re.search(r'\d', text)),
            "has_uppercase": bool(re.search(r'[A-Z]', text)),
            "has_lowercase": bool(re.search(r'[a-z]', text))
        }
        
        # Detect content type
        content_type = "text"
        for pattern_name, pattern in self._content_patterns.items():
            if pattern.search(text):
                content_type = pattern_name
                break
        
        analysis["content_type"] = content_type
        
        # Detect language (simple heuristic)
        if re.search(r'\b(the|and|of|to|in|is|you|that|it|he|was|for|on|are)\b', text.lower()):
            analysis["language"] = "english"
        else:
            analysis["language"] = "unknown"
        
        return analysis
    
    def _extract_content_from_dict(self, data: Dict[str, Any]) -> str:
        """Extract main content from dictionary input."""
        # Common field names for content
        content_fields = ['content', 'text', 'prompt', 'message', 'input', 'data', 'query']
        
        for field in content_fields:
            if field in data and isinstance(data[field], str):
                return data[field]
        
        # If no standard field found, use the first string value
        for value in data.values():
            if isinstance(value, str):
                return value
        
        # Fallback: convert entire dict to string
        return str(data)
    
    def _create_error_response(self, error_message: str, metadata: Dict[str, Any]) -> Dict[str, Any]:
        """Create standardized error response."""
        return {
            "content": "",
            "error": error_message,
            "metadata": {
                **metadata,
                "error": True,
                "processing_successful": False
            }
        }
    
    def _get_timestamp(self) -> float:
        """Get current timestamp."""
        import time
        return time.time()
    
    def validate_json_input(self, json_str: str) -> Tuple[bool, Optional[Dict[str, Any]], Optional[str]]:
        """
        Validate JSON input.
        
        Returns:
            Tuple of (is_valid, parsed_data, error_message)
        """
        try:
            data = json.loads(json_str)
            return True, data, None
        except json.JSONDecodeError as e:
            return False, None, f"Invalid JSON: {str(e)}"
    
    def validate_file_input(self, file_content: str, max_size_mb: Optional[float] = None) -> Dict[str, Any]:
        """Validate file input content."""
        if max_size_mb is None:
            max_size_mb = self.config.max_file_size_mb
        
        size_mb = len(file_content.encode('utf-8')) / (1024 * 1024)
        
        if size_mb > max_size_mb:
            return {
                "valid": False,
                "error": f"File size ({size_mb:.2f}MB) exceeds limit ({max_size_mb}MB)"
            }
        
        return {
            "valid": True,
            "size_mb": size_mb,
            "processed": self._process_string_input(file_content)
        }
    
    def batch_validate(self, input_list: List[Union[str, Dict[str, Any]]]) -> List[Dict[str, Any]]:
        """Validate multiple inputs in batch."""
        results = []
        for i, input_data in enumerate(input_list):
            try:
                result = self.process(input_data)
                result["batch_index"] = i
                result["batch_valid"] = True
                results.append(result)
            except Exception as e:
                results.append({
                    "batch_index": i,
                    "batch_valid": False,
                    "error": str(e),
                    "processing_successful": False
                })
        
        return results
