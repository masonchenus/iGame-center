"""
Base Tokenizer for AI Backend

Abstract base class and common functionality for all tokenizers.
"""

import re
import json
from typing import Dict, Any, List, Optional, Union, Tuple
from dataclasses import dataclass, field
from abc import ABC, abstractmethod
from .config import TokenizerConfig, get_config


@dataclass
class TokenizationResult:
    """Result of tokenization process."""
    tokens: List[str]
    token_ids: Optional[List[int]] = None
    offsets: Optional[List[Tuple[int, int]]] = None
    token_type_ids: Optional[List[int]] = None
    attention_mask: Optional[List[int]] = None
    special_tokens_mask: Optional[List[int]] = None
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    @property
    def token_count(self) -> int:
        """Get number of tokens."""
        return len(self.tokens)
    
    @property
    def is_empty(self) -> bool:
        """Check if tokenization resulted in empty tokens."""
        return len(self.tokens) == 0
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert result to dictionary."""
        return {
            "tokens": self.tokens,
            "token_ids": self.token_ids,
            "offsets": self.offsets,
            "token_type_ids": self.token_type_ids,
            "attention_mask": self.attention_mask,
            "special_tokens_mask": self.special_tokens_mask,
            "metadata": self.metadata,
            "token_count": self.token_count
        }


class BaseTokenizer(ABC):
    """
    Abstract base class for all tokenizers.
    
    Provides common functionality that all tokenizers should inherit.
    """
    
    def __init__(self, config: Optional[TokenizerConfig] = None):
        """Initialize the base tokenizer."""
        self.config = config or get_config()
        self._cache = {}
        self._cache_hits = 0
        self._cache_misses = 0
        
        # Special tokens
        self.special_tokens = self._initialize_special_tokens()
        
        # Statistics
        self._stats = {
            "total_tokenized": 0,
            "total_characters": 0,
            "average_tokens_per_text": 0.0,
            "cache_hit_rate": 0.0
        }
    
    def _initialize_special_tokens(self) -> Dict[str, str]:
        """Initialize special tokens based on configuration."""
        tokens = {
            "unk": self.config.unk_token,
            "pad": self.config.pad_token,
            "bos": self.config.bos_token,
            "eos": self.config.eos_token,
            "sep": self.config.sep_token,
            "cls": self.config.cls_token,
            "mask": self.config.mask_token
        }
        
        # Add custom special tokens
        if self.config.special_tokens:
            for i, token in enumerate(self.config.special_tokens):
                tokens[f"custom_{i}"] = token
        
        return tokens
    
    def tokenize(self, text: Union[str, List[str]]) -> TokenizationResult:
        """
        Tokenize input text(s).
        
        Args:
            text: Input text(s) to tokenize
            
        Returns:
            TokenizationResult containing tokens and metadata
        """
        if isinstance(text, list):
            return self._batch_tokenize(text)
        else:
            return self._tokenize_single(text)
    
    def _tokenize_single(self, text: str) -> TokenizationResult:
        """Tokenize a single text."""
        # Input validation
        if not self._validate_input(text):
            return TokenizationResult(
                tokens=[],
                metadata={"error": "Invalid input", "input_length": len(text)}
            )
        
        # Check cache
        cache_key = self._get_cache_key(text)
        if cache_key in self._cache:
            self._cache_hits += 1
            return self._cache[cache_key]
        
        self._cache_misses += 1
        
        # Preprocess text
        processed_text = self._preprocess_text(text)
        
        # Perform actual tokenization
        tokens = self._do_tokenize(processed_text)
        
        # Post-process tokens
        processed_tokens = self._post_process_tokens(tokens)
        
        # Create result
        result = self._create_result(processed_tokens, text)
        
        # Cache result
        if self.config.cache_tokens:
            self._cache_result(cache_key, result)
        
        # Update statistics
        self._update_stats(text, result)
        
        return result
    
    def _batch_tokenize(self, texts: List[str]) -> TokenizationResult:
        """Tokenize multiple texts in batch."""
        if not self.config.parallel_tokenization:
            # Sequential processing
            all_tokens = []
            for text in texts:
                result = self._tokenize_single(text)
                all_tokens.extend(result.tokens)
        else:
            # Parallel processing (simplified - could use ThreadPoolExecutor)
            all_tokens = []
            for text in texts:
                result = self._tokenize_single(text)
                all_tokens.extend(result.tokens)
        
        return TokenizationResult(
            tokens=all_tokens,
            metadata={"batch_size": len(texts), "individual_results": len(texts)}
        )
    
    def _validate_input(self, text: str) -> bool:
        """Validate input text."""
        if not self.config.allow_empty_input and not text.strip():
            return False
        
        if len(text) > self.config.max_input_length:
            return False
        
        # Check for valid encoding
        try:
            text.encode(self.config.encoding, errors=self.config.errors)
        except (LookupError, TypeError, UnicodeEncodeError):
            return False
        
        return True
    
    def _preprocess_text(self, text: str) -> str:
        """Preprocess text before tokenization."""
        processed = text
        
        # Normalize unicode
        if self.config.normalize_unicode:
            processed = self._normalize_unicode(processed)
        
        # Strip accents
        if self.config.strip_accents:
            processed = self._strip_accents(processed)
        
        # Convert to lowercase
        if self.config.lowercase:
            processed = processed.lower()
        
        # Remove numbers
        if self.config.remove_numbers:
            processed = re.sub(r'\d+', '', processed)
        
        # Handle contractions
        if self.config.handle_contractions:
            processed = self._handle_contractions(processed)
        
        # Split camelCase
        if self.config.split_camel_case:
            processed = self._split_camel_case(processed)
        
        # Split hyphens
        if self.config.split_hyphens:
            processed = self._split_hyphens(processed)
        
        # Remove punctuation (if enabled)
        if self.config.remove_punctuation:
            processed = re.sub(r'[^\w\s]', '', processed)
        
        return processed
    
    def _normalize_unicode(self, text: str) -> str:
        """Normalize Unicode characters."""
        import unicodedata
        return unicodedata.normalize('NFKC', text)
    
    def _strip_accents(self, text: str) -> str:
        """Strip accent characters."""
        import unicodedata
        return ''.join(char for char in text if unicodedata.category(char) != 'Mn')
    
    def _handle_contractions(self, text: str) -> str:
        """Handle common English contractions."""
        contractions = {
            "don't": "do not",
            "won't": "will not",
            "can't": "cannot",
            "n't": " not",
            "'re": " are",
            "'ve": " have",
            "'ll": " will",
            "'d": " would",
            "'m": " am"
        }
        
        for contraction, expansion in contractions.items():
            text = text.replace(contraction, expansion)
        
        return text
    
    def _split_camel_case(self, text: str) -> str:
        """Split camelCase words."""
        # Add space before capital letters (except first character)
        text = re.sub(r'([a-z])([A-Z])', r'\1 \2', text)
        return text
    
    def _split_hyphens(self, text: str) -> str:
        """Split hyphenated words."""
        return text.replace('-', ' ')
    
    @abstractmethod
    def _do_tokenize(self, text: str) -> List[str]:
        """Perform the actual tokenization. Must be implemented by subclasses."""
        pass
    
    def _post_process_tokens(self, tokens: List[str]) -> List[str]:
        """Post-process tokens after tokenization."""
        processed = tokens
        
        # Filter tokens by length
        processed = [token for token in processed 
                    if self.config.min_token_length <= len(token) <= self.config.max_token_length]
        
        # Remove stopwords if enabled
        if self.config.remove_stopwords:
            processed = self._remove_stopwords(processed)
        
        # Add special tokens if configured
        if self.config.add_special_tokens:
            processed = self._add_special_tokens(processed)
        
        # Truncate if necessary
        if len(processed) > self.config.max_tokens:
            processed = processed[:self.config.max_tokens]
        
        return processed
    
    def _remove_stopwords(self, tokens: List[str]) -> List[str]:
        """Remove common stopwords (simplified implementation)."""
        stopwords = {
            "the", "a", "an", "and", "or", "but", "in", "on", "at", "to", "for", 
            "of", "with", "by", "is", "are", "was", "were", "be", "been", "being",
            "have", "has", "had", "do", "does", "did", "will", "would", "could",
            "should", "may", "might", "must", "can", "this", "that", "these", "those"
        }
        
        return [token for token in tokens if token.lower() not in stopwords]
    
    def _add_special_tokens(self, tokens: List[str]) -> List[str]:
        """Add special tokens to the token list."""
        result = []
        
        # Add beginning of sequence token
        if self.config.bos_token != "[BOS]":
            result.append(self.config.bos_token)
        
        result.extend(tokens)
        
        # Add end of sequence token
        if self.config.eos_token != "[EOS]":
            result.append(self.config.eos_token)
        
        return result
    
    def _create_result(self, tokens: List[str], original_text: str) -> TokenizationResult:
        """Create TokenizationResult with all requested information."""
        # Generate token IDs (override in subclasses if needed)
        token_ids = self._tokens_to_ids(tokens) if self.config.return_token_type_ids is not False else None
        
        # Generate offsets
        offsets = self._generate_offsets(tokens, original_text) if self.config.return_offsets else None
        
        # Generate attention mask
        attention_mask = [1] * len(tokens) if self.config.return_attention_mask else None
        
        # Generate special tokens mask
        special_tokens_mask = self._generate_special_tokens_mask(tokens) if self.config.return_special_tokens_mask else None
        
        # Generate token type IDs (simplified)
        token_type_ids = [0] * len(tokens) if self.config.return_token_type_ids else None
        
        return TokenizationResult(
            tokens=tokens,
            token_ids=token_ids,
            offsets=offsets,
            token_type_ids=token_type_ids,
            attention_mask=attention_mask,
            special_tokens_mask=special_tokens_mask,
            metadata={
                "original_length": len(original_text),
                "processed_length": len(' '.join(tokens)),
                "tokenization_strategy": self.config.strategy.value,
                "config_used": self.config.to_dict()
            }
        )
    
    def _tokens_to_ids(self, tokens: List[str]) -> List[int]:
        """Convert tokens to IDs (simplified implementation)."""
        # This should be implemented by subclasses with actual vocabularies
        return list(range(len(tokens)))
    
    def _generate_offsets(self, tokens: List[str], original_text: str) -> List[Tuple[int, int]]:
        """Generate character offsets for each token."""
        offsets = []
        current_pos = 0
        
        for token in tokens:
            # Find token in original text
            token_start = original_text.find(token, current_pos)
            if token_start == -1:
                token_start = current_pos
            
            token_end = token_start + len(token)
            offsets.append((token_start, token_end))
            current_pos = token_end
        
        return offsets
    
    def _generate_special_tokens_mask(self, tokens: List[str]) -> List[int]:
        """Generate mask for special tokens."""
        mask = []
        for token in tokens:
            if token in self.special_tokens.values():
                mask.append(1)
            else:
                mask.append(0)
        return mask
    
    def _get_cache_key(self, text: str) -> str:
        """Generate cache key for text."""
        import hashlib
        key_string = f"{self.config.strategy.value}:{self.config.mode.value}:{text}"
        return hashlib.md5(key_string.encode()).hexdigest()
    
    def _cache_result(self, cache_key: str, result: TokenizationResult) -> None:
        """Cache tokenization result."""
        if len(self._cache) >= self.config.cache_size:
            # Remove oldest entry (simple FIFO)
            oldest_key = next(iter(self._cache))
            del self._cache[oldest_key]
        
        self._cache[cache_key] = result
    
    def _update_stats(self, text: str, result: TokenizationResult) -> None:
        """Update tokenization statistics."""
        self._stats["total_tokenized"] += 1
        self._stats["total_characters"] += len(text)
        
        # Update average tokens per text
        current_avg = self._stats["average_tokens_per_text"]
        count = self._stats["total_tokenized"]
        self._stats["average_tokens_per_text"] = (
            (current_avg * (count - 1) + result.token_count) / count
        )
        
        # Update cache hit rate
        total_requests = self._cache_hits + self._cache_misses
        if total_requests > 0:
            self._stats["cache_hit_rate"] = self._cache_hits / total_requests
    
    def decode(self, tokens: List[str], skip_special_tokens: bool = True) -> str:
        """
        Decode tokens back to text.
        
        Args:
            tokens: List of tokens to decode
            skip_special_tokens: Whether to skip special tokens
            
        Returns:
            Decoded text string
        """
        if skip_special_tokens:
            tokens = [token for token in tokens 
                     if token not in self.special_tokens.values()]
        
        return ' '.join(tokens)
    
    def encode(self, text: str) -> List[int]:
        """
        Encode text to token IDs.
        
        Args:
            text: Input text
            
        Returns:
            List of token IDs
        """
        result = self.tokenize(text)
        return result.token_ids or []
    
    def get_vocabulary(self) -> Dict[str, int]:
        """Get vocabulary mapping (to be implemented by subclasses)."""
        return {}
    
    def get_special_tokens(self) -> Dict[str, str]:
        """Get special tokens."""
        return self.special_tokens.copy()
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get tokenizer statistics."""
        return {
            **self._stats,
            "cache_size": len(self._cache),
            "config": self.config.to_dict()
        }
    
    def clear_cache(self) -> None:
        """Clear the tokenization cache."""
        self._cache.clear()
        self._cache_hits = 0
        self._cache_misses = 0
