"""
Output Processor for AI Backend

Handles output formatting, post-processing, and response preparation.
"""

import json
import time
import re
from typing import Dict, Any, List, Optional, Union
from datetime import datetime
from .config import ProcessorConfig, get_config


class OutputProcessor:
    """
    Processes and formats output data from the AI system.
    
    Responsibilities:
    - Output formatting and structuring
    - Response metadata generation
    - Performance tracking
    - Multiple output format support
    """
    
    def __init__(self, config: Optional[ProcessorConfig] = None):
        """Initialize the output processor with configuration."""
        self.config = config or get_config()
        
        # Output format templates
        self._format_templates = {
            'json': self._format_json,
            'markdown': self._format_markdown,
            'plain': self._format_plain,
            'structured': self._format_structured,
            'code': self._format_code
        }
    
    def process(self, data: Dict[str, Any], output_format: str = "default") -> Dict[str, Any]:
        """
        Process and format output data.
        
        Args:
            data: Input data to process
            output_format: Target output format
            
        Returns:
            Dict containing processed and formatted output
        """
        try:
            # Step 1: Apply output formatting
            formatted_data = self._apply_formatting(data, output_format)
            
            # Step 2: Add metadata
            metadata = self._generate_metadata(formatted_data, output_format)
            
            # Step 3: Apply post-processing
            final_output = self._apply_post_processing(formatted_data)
            
            # Step 4: Length validation
            if self.config.max_output_length > 0:
                final_output = self._validate_output_length(final_output)
            
            result = {
                "content": final_output,
                "metadata": metadata,
                "format": output_format
            }
            
            # Add additional metadata if configured
            if self.config.add_metadata:
                result["enhanced_metadata"] = self._generate_enhanced_metadata(result)
            
            return result
            
        except Exception as e:
            return self._create_error_response(str(e), output_format)
    
    def _apply_formatting(self, data: Dict[str, Any], output_format: str) -> str:
        """Apply formatting based on specified format."""
        content = data.get("content", "")
        
        # If no specific format requested, use default
        if output_format == "default":
            return self._format_default(data)
        
        # Use specific formatter if available
        formatter = self._format_templates.get(output_format, self._format_default)
        return formatter(data)
    
    def _format_default(self, data: Dict[str, Any]) -> str:
        """Default formatting."""
        content = data.get("content", "")
        
        # If content is already a string, return as-is
        if isinstance(content, str):
            return content
        
        # If content is a dict, format as JSON
        if isinstance(content, dict):
            try:
                return json.dumps(content, indent=2, ensure_ascii=False)
            except Exception:
                return str(content)
        
        # If content is a list, format as bullet points if appropriate
        if isinstance(content, list):
            if all(isinstance(item, str) for item in content):
                return '\n'.join(f"• {item}" for item in content)
            else:
                return '\n'.join(str(item) for item in content)
        
        # Fallback to string conversion
        return str(content)
    
    def _format_json(self, data: Dict[str, Any]) -> str:
        """Format as JSON."""
        content = data.get("content", {})
        try:
            return json.dumps(content, indent=2, ensure_ascii=False)
        except Exception:
            return json.dumps({"error": "Unable to format as JSON", "raw_content": str(content)})
    
    def _format_markdown(self, data: Dict[str, Any]) -> str:
        """Format as Markdown."""
        content = data.get("content", "")
        
        if isinstance(content, str):
            # Basic markdown formatting
            content = re.sub(r'\*\*(.*?)\*\*', r'**\1**', content)  # Bold
            content = re.sub(r'\*(.*?)\*', r'*\1*', content)        # Italic
            content = re.sub(r'`(.*?)`', r'`\1`', content)          # Code
            content = re.sub(r'^### (.*?)$', r'### \1', content, flags=re.MULTILINE)  # Headers
            content = re.sub(r'^## (.*?)$', r'## \1', content, flags=re.MULTILINE)   # Headers
            content = re.sub(r'^# (.*?)$', r'# \1', content, flags=re.MULTILINE)     # Headers
            return content
        else:
            # Convert other types to markdown
            return f"```json\n{self._format_json(data)}\n```"
    
    def _format_plain(self, data: Dict[str, Any]) -> str:
        """Format as plain text."""
        content = data.get("content", "")
        
        if isinstance(content, str):
            # Remove markdown formatting
            content = re.sub(r'\*\*(.*?)\*\*', r'\1', content)
            content = re.sub(r'\*(.*?)\*', r'\1', content)
            content = re.sub(r'`(.*?)`', r'\1', content)
            content = re.sub(r'^#+\s*', '', content, flags=re.MULTILINE)
            return content
        else:
            return str(content)
    
    def _format_structured(self, data: Dict[str, Any]) -> str:
        """Format as structured data."""
        content = data.get("content", {})
        
        if isinstance(content, dict):
            lines = []
            for key, value in content.items():
                if isinstance(value, dict):
                    lines.append(f"{key}:")
                    for sub_key, sub_value in value.items():
                        lines.append(f"  {sub_key}: {sub_value}")
                elif isinstance(value, list):
                    lines.append(f"{key}:")
                    for item in value:
                        lines.append(f"  - {item}")
                else:
                    lines.append(f"{key}: {value}")
            return '\n'.join(lines)
        else:
            return self._format_default(data)
    
    def _format_code(self, data: Dict[str, Any]) -> str:
        """Format as code."""
        content = data.get("content", "")
        language = data.get("language", "text")
        
        if isinstance(content, str):
            return f"```{language}\n{content}\n```"
        else:
            return f"```json\n{self._format_json(data)}\n```"
    
    def _generate_metadata(self, content: str, output_format: str) -> Dict[str, Any]:
        """Generate output metadata."""
        metadata = {
            "timestamp": time.time(),
            "datetime": datetime.now().isoformat(),
            "format": output_format,
            "content_length": len(content),
            "word_count": len(content.split()),
            "line_count": len(content.split('\n'))
        }
        
        if self.config.include_timing:
            metadata["processing_complete"] = True
        
        return metadata
    
    def _apply_post_processing(self, content: str) -> str:
        """Apply post-processing to the content."""
        # Clean up excessive whitespace
        content = re.sub(r'\n\s*\n\s*\n', '\n\n', content)  # Multiple newlines
        content = re.sub(r'^\s+', '', content, flags=re.MULTILINE)  # Leading spaces
        content = re.sub(r'\s+$', '', content, flags=re.MULTILINE)  # Trailing spaces
        
        # Normalize line endings
        content = content.replace('\r\n', '\n').replace('\r', '\n')
        
        return content
    
    def _validate_output_length(self, content: str) -> str:
        """Validate and truncate output if necessary."""
        if len(content) > self.config.max_output_length:
            if self.config.max_output_length > 3:
                return content[:self.config.max_output_length-3] + "..."
            else:
                return content[:self.config.max_output_length]
        return content
    
    def _generate_enhanced_metadata(self, result: Dict[str, Any]) -> Dict[str, Any]:
        """Generate enhanced metadata for the result."""
        content = result.get("content", "")
        metadata = result.get("metadata", {})
        
        enhanced = {
            "readability_score": self._calculate_readability_score(content),
            "complexity_level": self._assess_complexity_level(content),
            "suggested_actions": self._generate_suggested_actions(content),
            "content_summary": self._generate_content_summary(content)
        }
        
        # Add processing stats if available
        if "stats" in result:
            enhanced["processing_stats"] = result["stats"]
        
        return enhanced
    
    def _calculate_readability_score(self, content: str) -> float:
        """Calculate a simple readability score."""
        if not content.strip():
            return 0.0
        
        words = content.split()
        sentences = re.split(r'[.!?]+', content)
        
        if not sentences:
            return 0.0
        
        avg_words_per_sentence = len(words) / len(sentences)
        avg_chars_per_word = sum(len(word) for word in words) / len(words) if words else 0
        
        # Simple readability formula (Flesch-inspired)
        score = 206.835 - (1.015 * avg_words_per_sentence) - (84.6 * (avg_chars_per_word / 4.7))
        return max(0, min(100, score))
    
    def _assess_complexity_level(self, content: str) -> str:
        """Assess the complexity level of the content."""
        word_count = len(content.split())
        avg_word_length = sum(len(word) for word in content.split()) / word_count if content.split() else 0
        
        if word_count < 50:
            return "simple"
        elif word_count < 200 and avg_word_length < 6:
            return "moderate"
        elif word_count < 500 and avg_word_length < 8:
            return "complex"
        else:
            return "advanced"
    
    def _generate_suggested_actions(self, content: str) -> List[str]:
        """Generate suggested actions based on content."""
        suggestions = []
        
        if "error" in content.lower() or "fail" in content.lower():
            suggestions.append("Review for potential errors")
        
        if len(content.split('\n')) > 20:
            suggestions.append("Consider breaking into smaller sections")
        
        if "?" in content:
            suggestions.append("Review questions for clarity")
        
        if content.count('[') > content.count(']'):
            suggestions.append("Check for unclosed brackets")
        
        if not suggestions:
            suggestions.append("Content appears well-formatted")
        
        return suggestions
    
    def _generate_content_summary(self, content: str) -> str:
        """Generate a brief summary of the content."""
        sentences = re.split(r'[.!?]+', content)
        sentences = [s.strip() for s in sentences if s.strip()]
        
        if not sentences:
            return "Empty content"
        
        # Take first sentence or first few words
        first_sentence = sentences[0]
        if len(first_sentence) > 100:
            return first_sentence[:100] + "..."
        
        return first_sentence
    
    def _create_error_response(self, error_message: str, output_format: str) -> Dict[str, Any]:
        """Create standardized error response."""
        return {
            "content": f"Error: {error_message}",
            "metadata": {
                "timestamp": time.time(),
                "datetime": datetime.now().isoformat(),
                "format": output_format,
                "error": True,
                "processing_successful": False
            },
            "error": error_message
        }
    
    def format_batch_output(self, results: List[Dict[str, Any]], output_format: str = "default") -> Dict[str, Any]:
        """Format multiple outputs in batch."""
        formatted_results = []
        
        for i, result in enumerate(results):
            try:
                formatted = self.process(result, output_format)
                formatted["batch_index"] = i
                formatted_results.append(formatted)
            except Exception as e:
                formatted_results.append({
                    "content": f"Error formatting result {i}: {str(e)}",
                    "metadata": {"batch_index": i, "error": True},
                    "error": str(e)
                })
        
        return {
            "batch_results": formatted_results,
            "total_processed": len(formatted_results),
            "successful": sum(1 for r in formatted_results if "error" not in r),
            "failed": sum(1 for r in formatted_results if "error" in r)
        }
    
    def export_to_format(self, data: Dict[str, Any], export_format: str, filename: Optional[str] = None) -> str:
        """Export data to specified format."""
        content = self.process(data, export_format)["content"]
        
        if filename:
            try:
                with open(filename, 'w', encoding='utf-8') as f:
                    f.write(content)
                return f"Exported to {filename}"
            except Exception as e:
                return f"Export failed: {str(e)}"
        
        return content
