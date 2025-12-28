"""
Data Processor for AI Backend

Core data processing logic that transforms data throughout the AI pipeline.
"""

import time
import re
from typing import Dict, Any, List, Optional, Union, Callable
from concurrent.futures import ThreadPoolExecutor, as_completed
from .config import ProcessorConfig, get_config
from .input_processor import InputProcessor
from .output_processor import OutputProcessor


class DataProcessor:
    """
    Core data processor that manages the complete data transformation pipeline.
    
    Handles:
    - Input validation and preprocessing
    - Data transformation and normalization
    - Output formatting and post-processing
    - Performance monitoring and caching
    """
    
    def __init__(self, config: Optional[ProcessorConfig] = None):
        """Initialize the data processor with configuration."""
        self.config = config or get_config()
        self.input_processor = InputProcessor(self.config)
        self.output_processor = OutputProcessor(self.config)
        self._cache = {}
        self._processing_stats = {
            "total_processed": 0,
            "cache_hits": 0,
            "average_processing_time": 0.0
        }
    
    def process(self, 
                data: Union[str, Dict[str, Any]], 
                processing_type: str = "default",
                custom_transformers: Optional[List[Callable]] = None) -> Dict[str, Any]:
        """
        Process data through the complete pipeline.
        
        Args:
            data: Input data (string or dict)
            processing_type: Type of processing to apply
            custom_transformers: Optional custom transformation functions
            
        Returns:
            Dict containing processed data and metadata
        """
        start_time = time.time()
        
        try:
            # Generate cache key for caching
            cache_key = self._generate_cache_key(data, processing_type)
            
            # Check cache first
            if self.config.enable_caching and cache_key in self._cache:
                self._processing_stats["cache_hits"] += 1
                result = self._cache[cache_key].copy()
                result["from_cache"] = True
                return result
            
            # Step 1: Input Processing
            processed_input = self.input_processor.process(data)
            
            # Step 2: Data Transformation
            transformed_data = self._apply_transformations(
                processed_input, processing_type, custom_transformers
            )
            
            # Step 3: Output Processing
            final_output = self.output_processor.process(transformed_data)
            
            # Prepare result with metadata
            processing_time = time.time() - start_time
            result = {
                "processed_data": final_output,
                "metadata": {
                    "processing_type": processing_type,
                    "processing_time": processing_time,
                    "input_size": len(str(data)),
                    "output_size": len(str(final_output)),
                    "from_cache": False,
                    "timestamp": time.time()
                },
                "stats": self._processing_stats.copy()
            }
            
            # Cache the result
            if self.config.enable_caching:
                self._cache_result(cache_key, result)
            
            # Update processing stats
            self._update_processing_stats(processing_time)
            
            return result
            
        except Exception as e:
            processing_time = time.time() - start_time
            return {
                "error": str(e),
                "metadata": {
                    "processing_type": processing_type,
                    "processing_time": processing_time,
                    "error": True,
                    "timestamp": time.time()
                }
            }
    
    def _apply_transformations(self, 
                              data: Dict[str, Any], 
                              processing_type: str,
                              custom_transformers: Optional[List[Callable]] = None) -> Dict[str, Any]:
        """Apply data transformations based on processing type."""
        
        # Apply built-in transformations based on type
        if processing_type == "text":
            data = self._transform_text(data)
        elif processing_type == "structured":
            data = self._transform_structured(data)
        elif processing_type == "code":
            data = self._transform_code(data)
        elif processing_type == "data":
            data = self._transform_data(data)
        
        # Apply custom transformers
        if custom_transformers:
            for transformer in custom_transformers:
                try:
                    data = transformer(data)
                except Exception as e:
                    # Log error but continue with other transformers
                    if self.config.detailed_logging:
                        print(f"Custom transformer error: {e}")
        
        return data
    
    def _transform_text(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Apply text-specific transformations."""
        if "content" in data:
            content = data["content"]
            
            # Normalize whitespace
            content = re.sub(r'\s+', ' ', content)
            
            # Clean up special characters while preserving meaningful ones
            content = re.sub(r'[^\w\s\.\,\!\?\;\:\-\(\)\[\]\{\}\"\'\/\\]', '', content)
            
            data["content"] = content.strip()
            data["word_count"] = len(content.split())
            data["char_count"] = len(content)
        
        return data
    
    def _transform_structured(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Apply structured data transformations."""
        # Ensure proper JSON structure
        if "content" in data and isinstance(data["content"], str):
            try:
                import json
                # Try to parse as JSON if it looks like structured data
                if data["content"].strip().startswith(('{', '[')):
                    parsed = json.loads(data["content"])
                    data["content"] = parsed
                    data["is_json"] = True
            except json.JSONDecodeError:
                data["is_json"] = False
        
        return data
    
    def _transform_code(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Apply code-specific transformations."""
        if "content" in data:
            content = data["content"]
            
            # Basic code normalization
            lines = content.split('\n')
            # Remove trailing whitespace
            lines = [line.rstrip() for line in lines]
            data["content"] = '\n'.join(lines)
            
            # Basic syntax validation
            if data.get("language"):
                data["syntax_valid"] = self._basic_syntax_check(content, data["language"])
        
        return data
    
    def _transform_data(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Apply data analysis transformations."""
        if "content" in data:
            content = data["content"]
            
            # Basic data analysis
            if isinstance(content, (list, dict)):
                data["size"] = len(content) if isinstance(content, list) else len(content.keys())
                data["type"] = type(content).__name__
            
            # Statistical analysis for numeric data
            if isinstance(content, list) and content and all(isinstance(x, (int, float)) for x in content):
                data["statistics"] = {
                    "mean": sum(content) / len(content),
                    "min": min(content),
                    "max": max(content),
                    "sum": sum(content)
                }
        
        return data
    
    def _basic_syntax_check(self, code: str, language: str) -> bool:
        """Basic syntax validation for different languages."""
        if language.lower() == "python":
            # Basic Python syntax check
            try:
                compile(code, '<string>', 'exec')
                return True
            except SyntaxError:
                return False
        # Add more language checks as needed
        return True  # Default to True for unsupported languages
    
    def _generate_cache_key(self, data: Union[str, Dict[str, Any]], processing_type: str) -> str:
        """Generate a cache key for the data and processing type."""
        import hashlib
        data_str = str(data) if not isinstance(data, str) else data
        key_str = f"{processing_type}:{data_str}"
        return hashlib.md5(key_str.encode()).hexdigest()
    
    def _cache_result(self, cache_key: str, result: Dict[str, Any]) -> None:
        """Cache the processing result."""
        if len(self._cache) >= self.config.cache_size:
            # Remove oldest entry (simple FIFO)
            oldest_key = next(iter(self._cache))
            del self._cache[oldest_key]
        
        self._cache[cache_key] = result
    
    def _update_processing_stats(self, processing_time: float) -> None:
        """Update processing statistics."""
        self._processing_stats["total_processed"] += 1
        
        # Update average processing time
        current_avg = self._processing_stats["average_processing_time"]
        count = self._processing_stats["total_processed"]
        self._processing_stats["average_processing_time"] = (
            (current_avg * (count - 1) + processing_time) / count
        )
    
    def batch_process(self, 
                     data_list: List[Union[str, Dict[str, Any]]], 
                     processing_type: str = "default") -> List[Dict[str, Any]]:
        """
        Process multiple data items in batch.
        
        Args:
            data_list: List of data items to process
            processing_type: Type of processing to apply
            
        Returns:
            List of processed results
        """
        if not self.config.parallel_processing:
            return [self.process(data, processing_type) for data in data_list]
        
        results = []
        with ThreadPoolExecutor(max_workers=self.config.max_workers) as executor:
            future_to_data = {
                executor.submit(self.process, data, processing_type): data 
                for data in data_list
            }
            
            for future in as_completed(future_to_data):
                try:
                    result = future.result()
                    results.append(result)
                except Exception as e:
                    results.append({
                        "error": str(e),
                        "metadata": {"error": True, "timestamp": time.time()}
                    })
        
        return results
    
    def get_stats(self) -> Dict[str, Any]:
        """Get processing statistics."""
        return self._processing_stats.copy()
    
    def clear_cache(self) -> None:
        """Clear the processing cache."""
        self._cache.clear()
    
    def optimize_config(self) -> None:
        """Optimize configuration based on usage patterns."""
        # Simple optimization: adjust batch size based on average processing time
        avg_time = self._processing_stats["average_processing_time"]
        
        if avg_time > 1.0:  # Slow processing
            self.config.batch_size = max(1, self.config.batch_size // 2)
            self.config.max_workers = max(1, self.config.max_workers // 2)
        elif avg_time < 0.1:  # Fast processing
            self.config.batch_size = min(100, self.config.batch_size * 2)
            self.config.max_workers = min(16, self.config.max_workers * 2)
