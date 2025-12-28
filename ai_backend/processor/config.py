"""
Processor Configuration

Configuration settings for the AI data processing pipeline.
"""

import os
from typing import Dict, Any, List
from dataclasses import dataclass
from enum import Enum


class ProcessingMode(Enum):
    """Processing modes for different types of data."""
    FAST = "fast"
    ACCURATE = "accurate"
    BALANCED = "balanced"
    CUSTOM = "custom"


@dataclass
class ProcessorConfig:
    """Configuration class for AI data processing."""
    
    # Processing mode settings
    mode: ProcessingMode = ProcessingMode.BALANCED
    
    # Input processing settings
    max_input_length: int = 10000
    validate_input: bool = True
    sanitize_input: bool = True
    normalize_text: bool = True
    
    # Output processing settings
    max_output_length: int = 5000
    format_output: bool = True
    add_metadata: bool = True
    include_timing: bool = True
    
    # Data transformation settings
    enable_caching: bool = True
    cache_size: int = 1000
    batch_size: int = 32
    
    # Performance settings
    parallel_processing: bool = True
    max_workers: int = 4
    timeout_seconds: int = 30
    
    # Validation settings
    strict_validation: bool = False
    allow_empty_input: bool = True
    max_file_size_mb: int = 10
    
    # Logging settings
    log_processing_time: bool = True
    log_validation_errors: bool = True
    detailed_logging: bool = False
    
    @classmethod
    def from_dict(cls, config_dict: Dict[str, Any]) -> 'ProcessorConfig':
        """Create config from dictionary."""
        # Convert string mode to enum if needed
        if 'mode' in config_dict and isinstance(config_dict['mode'], str):
            config_dict['mode'] = ProcessingMode(config_dict['mode'])
        
        return cls(**config_dict)
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert config to dictionary."""
        result = {}
        for key, value in self.__dict__.items():
            if isinstance(value, Enum):
                result[key] = value.value
            else:
                result[key] = value
        return result
    
    @classmethod
    def get_default_config(cls) -> 'ProcessorConfig':
        """Get default configuration."""
        return cls()
    
    @classmethod
    def get_fast_config(cls) -> 'ProcessorConfig':
        """Get configuration optimized for speed."""
        return cls(
            mode=ProcessingMode.FAST,
            max_input_length=5000,
            max_output_length=2000,
            parallel_processing=True,
            max_workers=8,
            validate_input=True,
            sanitize_input=True,
            format_output=False,
            add_metadata=False
        )
    
    @classmethod
    def get_accurate_config(cls) -> 'ProcessorConfig':
        """Get configuration optimized for accuracy."""
        return cls(
            mode=ProcessingMode.ACCURATE,
            max_input_length=20000,
            max_output_length=10000,
            parallel_processing=False,
            validate_input=True,
            sanitize_input=True,
            normalize_text=True,
            format_output=True,
            add_metadata=True,
            strict_validation=True,
            detailed_logging=True
        )


# Global configuration instance
_config = None


def get_config() -> ProcessorConfig:
    """Get the global processor configuration."""
    global _config
    if _config is None:
        _config = ProcessorConfig.get_default_config()
    return _config


def set_config(config: ProcessorConfig) -> None:
    """Set the global processor configuration."""
    global _config
    _config = config


def reset_config() -> None:
    """Reset configuration to default."""
    global _config
    _config = ProcessorConfig.get_default_config()
