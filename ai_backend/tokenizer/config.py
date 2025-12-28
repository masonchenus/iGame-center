"""
Tokenizer Configuration

Configuration settings for the AI tokenization system.
"""

import os
from typing import Dict, Any, List, Optional
from dataclasses import dataclass, field
from enum import Enum
import json


class TokenizationStrategy(Enum):
    """Tokenization strategies."""
    WORD = "word"
    CHARACTER = "character"
    BPE = "bpe"  # Byte-Pair Encoding
    SUBWORD = "subword"
    SENTENCE = "sentence"
    MORPHEME = "morpheme"


class TokenizationMode(Enum):
    """Tokenization modes."""
    BASIC = "basic"
    ADVANCED = "advanced"
    PRESERVE_PUNCTUATION = "preserve_punctuation"
    PRESERVE_CASE = "preserve_case"
    NORMALIZE_UNICODE = "normalize_unicode"


@dataclass
class TokenizerConfig:
    """Configuration class for AI tokenization."""
    
    # Core tokenization settings
    strategy: TokenizationStrategy = TokenizationStrategy.WORD
    mode: TokenizationMode = TokenizationMode.ADVANCED
    
    # Tokenization parameters
    max_tokens: int = 512
    max_characters: int = 10000
    min_token_length: int = 1
    max_token_length: int = 100
    
    # BPE-specific settings
    vocab_size: int = 50000
    min_frequency: int = 2
    unk_token: str = "[UNK]"
    pad_token: str = "[PAD]"
    bos_token: str = "[BOS]"
    eos_token: str = "[EOS]"
    sep_token: str = "[SEP]"
    cls_token: str = "[CLS]"
    mask_token: str = "[MASK]"
    
    # Text preprocessing settings
    lowercase: bool = True
    remove_punctuation: bool = False
    remove_numbers: bool = False
    remove_stopwords: bool = False
    normalize_unicode: bool = True
    strip_accents: bool = True
    
    # Advanced tokenization settings
    preserve_case: bool = False
    preserve_punctuation: bool = True
    handle_contractions: bool = True
    split_camel_case: bool = False
    split_hyphens: bool = False
    
    # Language-specific settings
    language: str = "english"
    multi_language: bool = False
    detect_language: bool = False
    
    # Performance settings
    cache_tokens: bool = True
    cache_size: int = 10000
    parallel_tokenization: bool = True
    batch_size: int = 32
    
    # Special token handling
    add_special_tokens: bool = True
    special_tokens: List[str] = field(default_factory=list)
    
    # Encoding settings
    encoding: str = "utf-8"
    errors: str = "strict"
    
    # Output settings
    return_offsets: bool = True
    return_token_type_ids: bool = False
    return_attention_mask: bool = False
    return_special_tokens_mask: bool = False
    
    # Validation settings
    validate_input: bool = True
    max_input_length: int = 1000000  # 1MB
    allow_empty_input: bool = True
    
    # Logging and debugging
    log_tokenization_stats: bool = True
    debug_mode: bool = False
    verbose_output: bool = False
    
    # Model integration settings
    model_max_length: int = 512
    padding_side: str = "right"  # "left" or "right"
    truncation_strategy: str = "longest_first"  # "longest_first" or "only_first"
    
    @classmethod
    def from_dict(cls, config_dict: Dict[str, Any]) -> 'TokenizerConfig':
        """Create config from dictionary."""
        # Convert string enums if needed
        if 'strategy' in config_dict and isinstance(config_dict['strategy'], str):
            config_dict['strategy'] = TokenizationStrategy(config_dict['strategy'])
        
        if 'mode' in config_dict and isinstance(config_dict['mode'], str):
            config_dict['mode'] = TokenizationMode(config_dict['mode'])
        
        return cls(**config_dict)
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert config to dictionary."""
        result = {}
        for key, value in self.__dict__.items():
            if isinstance(value, Enum):
                result[key] = value.value
            elif isinstance(value, list):
                result[key] = value.copy()
            else:
                result[key] = value
        return result
    
    @classmethod
    def get_default_config(cls) -> 'TokenizerConfig':
        """Get default configuration."""
        return cls()
    
    @classmethod
    def get_bpe_config(cls, vocab_size: int = 50000) -> 'TokenizerConfig':
        """Get configuration optimized for BPE tokenization."""
        return cls(
            strategy=TokenizationStrategy.BPE,
            vocab_size=vocab_size,
            min_frequency=2,
            lowercase=True,
            normalize_unicode=True,
            add_special_tokens=True,
            return_offsets=True
        )
    
    @classmethod
    def get_word_config(cls) -> 'TokenizerConfig':
        """Get configuration optimized for word tokenization."""
        return cls(
            strategy=TokenizationStrategy.WORD,
            mode=TokenizationMode.PRESERVE_PUNCTUATION,
            lowercase=True,
            preserve_case=False,
            preserve_punctuation=True,
            handle_contractions=True,
            return_offsets=True
        )
    
    @classmethod
    def get_character_config(cls) -> 'TokenizerConfig':
        """Get configuration optimized for character tokenization."""
        return cls(
            strategy=TokenizationStrategy.CHARACTER,
            mode=TokenizationMode.BASIC,
            lowercase=False,
            preserve_case=True,
            preserve_punctuation=True,
            add_special_tokens=True,
            return_offsets=True
        )
    
    @classmethod
    def get_bert_config(cls, max_length: int = 512) -> 'TokenizerConfig':
        """Get BERT-style configuration."""
        return cls(
            strategy=TokenizationStrategy.WORD,
            mode=TokenizationMode.ADVANCED,
            max_tokens=max_length,
            model_max_length=max_length,
            padding_side="right",
            truncation_strategy="longest_first",
            add_special_tokens=True,
            return_token_type_ids=True,
            return_attention_mask=True,
            return_special_tokens_mask=True,
            lowercase=True,
            preserve_punctuation=True
        )
    
    @classmethod
    def get_gpt_config(cls, max_length: int = 512) -> 'TokenizerConfig':
        """Get GPT-style configuration."""
        return cls(
            strategy=TokenizationStrategy.BPE,
            vocab_size=50257,  # GPT-2 vocab size
            max_tokens=max_length,
            model_max_length=max_length,
            padding_side="right",
            add_special_tokens=False,  # GPT doesn't use [CLS] and [SEP]
            return_token_type_ids=False,
            return_attention_mask=True,
            lowercase=False,
            preserve_case=True
        )


# Global configuration instance
_config = None


def get_config() -> TokenizerConfig:
    """Get the global tokenizer configuration."""
    global _config
    if _config is None:
        _config = TokenizerConfig.get_default_config()
    return _config


def set_config(config: TokenizerConfig) -> None:
    """Set the global tokenizer configuration."""
    global _config
    _config = config


def reset_config() -> None:
    """Reset configuration to default."""
    global _config
    _config = TokenizerConfig.get_default_config()


def load_config_from_file(config_path: str) -> TokenizerConfig:
    """Load configuration from JSON file."""
    try:
        with open(config_path, 'r', encoding='utf-8') as f:
            config_dict = json.load(f)
        return TokenizerConfig.from_dict(config_dict)
    except Exception as e:
        raise ValueError(f"Failed to load config from {config_path}: {str(e)}")


def save_config_to_file(config: TokenizerConfig, config_path: str) -> None:
    """Save configuration to JSON file."""
    try:
        with open(config_path, 'w', encoding='utf-8') as f:
            json.dump(config.to_dict(), f, indent=2, ensure_ascii=False)
    except Exception as e:
        raise ValueError(f"Failed to save config to {config_path}: {str(e)}")


def validate_config(config: TokenizerConfig) -> List[str]:
    """Validate configuration and return list of warnings/errors."""
    warnings = []
    
    # Validate token limits
    if config.max_tokens < 1:
        warnings.append("max_tokens should be at least 1")
    
    if config.max_token_length < config.min_token_length:
        warnings.append("max_token_length should be >= min_token_length")
    
    # Validate BPE settings
    if config.strategy == TokenizationStrategy.BPE:
        if config.vocab_size < 100:
            warnings.append("vocab_size should be at least 100 for BPE")
        
        if config.min_frequency < 1:
            warnings.append("min_frequency should be at least 1")
    
    # Validate batch size
    if config.batch_size < 1:
        warnings.append("batch_size should be at least 1")
    
    # Validate encoding
    try:
        "test".encode(config.encoding, errors=config.errors)
    except (LookupError, TypeError):
        warnings.append(f"Invalid encoding: {config.encoding}")
    
    # Validate special tokens
    if config.add_special_tokens and not config.special_tokens:
        # Set default special tokens if not provided
        default_tokens = [config.unk_token, config.pad_token]
        if config.bos_token != "[BOS]":
            default_tokens.append(config.bos_token)
        if config.eos_token != "[EOS]":
            default_tokens.append(config.eos_token)
        config.special_tokens = default_tokens
    
    # Validate input limits
    if config.max_input_length < 1000:
        warnings.append("max_input_length should be at least 1000 for practical use")
    
    return warnings
