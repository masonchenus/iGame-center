"""
Character Tokenizer for AI Backend

Implements character-level tokenization for fine-grained text processing.
"""

import re
import string
from typing import Dict, Any, List, Optional, Tuple, Set
from .base_tokenizer import BaseTokenizer, TokenizationResult
from .config import TokenizerConfig, get_config


class CharacterTokenizer(BaseTokenizer):
    """
    Character-level tokenizer implementation.
    
    Features:
    - Character-by-character tokenization
    - Unicode support
    - Configurable character filtering
    - Efficient for character-level models
    - Preserves exact character sequence
    """
    
    def __init__(self, config: Optional[TokenizerConfig] = None):
        """Initialize the character tokenizer."""
        super().__init__(config)
        
        if self.config.strategy.value != "character":
            raise ValueError(f"CharacterTokenizer requires CHARACTER strategy, got {self.config.strategy.value}")
        
        # Character sets
        self._ascii_chars = set(chr(i) for i in range(32, 127))  # Printable ASCII
        self._extended_chars = set(chr(i) for i in range(128, 256))  # Extended ASCII
        self._unicode_chars = set()  # Will be populated as needed
        
        # Character filtering patterns
        self._filter_patterns = self._compile_filter_patterns()
        
        # Character categories
        self._character_categories = self._initialize_character_categories()
    
    def _compile_filter_patterns(self) -> List[re.Pattern]:
        """Compile patterns for character filtering."""
        patterns = []
        
        if self.config.remove_punctuation:
            patterns.append(re.compile(r'[^\w\s]'))
        
        if self.config.remove_numbers:
            patterns.append(re.compile(r'[0-9]'))
        
        # Unicode normalization patterns
        if self.config.normalize_unicode:
            # Pattern to match combining characters
            patterns.append(re.compile(r'[\u0300-\u036f]'))  # Combining diacritical marks
        
        return patterns
    
    def _initialize_character_categories(self) -> Dict[str, Set[str]]:
        """Initialize character categories for analysis."""
        categories = {
            'letters': set(string.ascii_letters),
            'digits': set(string.digits),
            'punctuation': set(string.punctuation),
            'whitespace': set(string.whitespace),
            'symbols': set('!@#$%^&*()_+-=[]{}|;:,.<>?'),
            'special': set('~`"\'')
        }
        
        # Add extended character sets
        categories['ascii_extended'] = self._extended_chars
        
        # Add Unicode categories if needed
        if self.config.multi_language:
            # Add common Unicode ranges for various languages
            unicode_ranges = [
                (0x00C0, 0x00FF),  # Latin-1 Supplement
                (0x0100, 0x017F),  # Latin Extended-A
                (0x0400, 0x04FF),  # Cyrillic
                (0x0590, 0x05FF),  # Hebrew
                (0x0600, 0x06FF),  # Arabic
                (0x4E00, 0x9FFF),  # CJK Unified Ideographs
                (0x3040, 0x309F),  # Hiragana
                (0x30A0, 0x30FF),  # Katakana
            ]
            
            for start, end in unicode_ranges:
                for codepoint in range(start, end + 1):
                    try:
                        char = chr(codepoint)
                        categories.setdefault('unicode_extended', set()).add(char)
                    except ValueError:
                        continue
        
        return categories
    
    def _do_tokenize(self, text: str) -> List[str]:
        """Perform character-level tokenization."""
        # Handle special modes
        if self.config.mode.value == "basic":
            return self._tokenize_basic(text)
        elif self.config.mode.value == "preserve_punctuation":
            return self._tokenize_preserve_punctuation(text)
        elif self.config.mode.value == "preserve_case":
            return self._tokenize_preserve_case(text)
        elif self.config.mode.value == "advanced":
            return self._tokenize_advanced(text)
        else:
            return self._tokenize_basic(text)
    
    def _tokenize_basic(self, text: str) -> List[str]:
        """Basic character tokenization."""
        # Apply filtering if configured
        filtered_text = self._apply_character_filtering(text)
        
        # Split into individual characters
        return list(filtered_text)
    
    def _tokenize_preserve_punctuation(self, text: str) -> List[str]:
        """Character tokenization preserving punctuation."""
        # Split into characters but keep punctuation as separate tokens
        tokens = []
        
        for char in text:
            if char.isspace():
                # Handle whitespace specially
                if self.config.preserve_punctuation:
                    tokens.append(char)
                else:
                    # Skip whitespace if not preserving
                    continue
            elif char in string.punctuation:
                # Keep punctuation as separate tokens
                tokens.append(char)
            else:
                # Regular character
                tokens.append(char)
        
        return tokens
    
    def _tokenize_preserve_case(self, text: str) -> List[str]:
        """Character tokenization preserving case."""
        # Basic character tokenization with case preservation
        return list(text)
    
    def _tokenize_advanced(self, text: str) -> List[str]:
        """Advanced character tokenization with Unicode support."""
        tokens = []
        i = 0
        
        while i < len(text):
            char = text[i]
            
            # Handle multi-byte Unicode characters
            if ord(char) > 127:
                # This is a Unicode character, find the complete character
                unicode_char = self._extract_unicode_char(text, i)
                tokens.append(unicode_char)
                i += len(unicode_char)
            else:
                # ASCII character
                if char.isspace():
                    if self.config.preserve_punctuation:
                        tokens.append(char)
                elif char in string.punctuation:
                    if self.config.preserve_punctuation:
                        tokens.append(char)
                    else:
                        # Skip punctuation if not preserving
                        pass
                else:
                    tokens.append(char)
                i += 1
        
        return tokens
    
    def _extract_unicode_char(self, text: str, start_index: int) -> str:
        """Extract a complete Unicode character."""
        char = text[start_index]
        
        # For basic implementation, just return the single character
        # A more sophisticated implementation would handle UTF-8 sequences
        # and combining characters properly
        
        if ord(char) < 128:
            return char
        
        # For now, just return the single character
        # In a full implementation, this would:
        # 1. Check if it's a multi-byte UTF-8 sequence
        # 2. Handle combining characters
        # 3. Handle surrogate pairs for characters outside the BMP
        
        return char
    
    def _apply_character_filtering(self, text: str) -> str:
        """Apply character filtering based on configuration."""
        filtered_text = text
        
        for pattern in self._filter_patterns:
            filtered_text = pattern.sub('', filtered_text)
        
        return filtered_text
    
    def _post_process_tokens(self, tokens: List[str]) -> List[str]:
        """Post-process character tokens."""
        processed = []
        
        for token in tokens:
            # Skip empty tokens
            if not token:
                continue
            
            # Apply length filtering
            if len(token) < self.config.min_token_length:
                continue
            
            if len(token) > self.config.max_token_length:
                # Truncate if too long
                token = token[:self.config.max_token_length]
            
            processed.append(token)
        
        # Add special tokens if configured
        if self.config.add_special_tokens:
            processed = self._add_special_tokens(processed)
        
        # Truncate if necessary
        if len(processed) > self.config.max_tokens:
            processed = processed[:self.config.max_tokens]
        
        return processed
    
    def _tokens_to_ids(self, tokens: List[str]) -> List[int]:
        """Convert character tokens to IDs."""
        # Create vocabulary for characters
        if not hasattr(self, '_char_vocab'):
            self._build_character_vocabulary()
        
        token_ids = []
        for token in tokens:
            token_id = self._char_vocab.get(token, self._char_vocab[self.config.unk_token])
            token_ids.append(token_id)
        
        return token_ids
    
    def _build_character_vocabulary(self) -> None:
        """Build vocabulary for character tokens."""
        self._char_vocab = {}
        id_counter = 0
        
        # Add special tokens first
        for name, token in self.special_tokens.items():
            self._char_vocab[token] = id_counter
            id_counter += 1
        
        # Add all characters that might appear
        char_set = set()
        
        # Add ASCII characters
        char_set.update(string.ascii_letters)
        char_set.update(string.digits)
        char_set.update(string.punctuation)
        char_set.update(string.whitespace)
        
        # Add extended ASCII
        char_set.update(self._extended_chars)
        
        # Add Unicode characters from categories
        for category_chars in self._character_categories.values():
            char_set.update(category_chars)
        
        # Sort characters for consistent ordering
        for char in sorted(char_set):
            if char not in self._char_vocab:
                self._char_vocab[char] = id_counter
                id_counter += 1
    
    def get_character_statistics(self) -> Dict[str, Any]:
        """Get character-specific statistics."""
        stats = {
            "character_mode": self.config.mode.value,
            "unicode_support": self.config.multi_language,
            "preserve_punctuation": self.config.preserve_punctuation,
            "preserve_case": self.config.preserve_case,
            "filter_patterns": len(self._filter_patterns),
            "character_categories": len(self._character_categories),
            "ascii_chars": len(string.ascii_letters),
            "digits": len(string.digits),
            "punctuation": len(string.punctuation),
            "whitespace": len(string.whitespace),
            "extended_ascii": len(self._extended_chars)
        }
        
        # Add Unicode category sizes
        for category, chars in self._character_categories.items():
            stats[f"{category}_count"] = len(chars)
        
        return stats
    
    def analyze_character_distribution(self, text: str) -> Dict[str, int]:
        """Analyze the distribution of characters in text."""
        char_count = {}
        
        for char in text:
            char_count[char] = char_count.get(char, 0) + 1
        
        return dict(sorted(char_count.items(), key=lambda x: x[1], reverse=True))
    
    def get_character_categories(self, char: str) -> List[str]:
        """Get categories that a character belongs to."""
        categories = []
        
        for category, chars in self._character_categories.items():
            if char in chars:
                categories.append(category)
        
        return categories
    
    def is_valid_character(self, char: str) -> bool:
        """Check if a character is valid according to configuration."""
        # Check basic validity
        if not char:
            return False
        
        # Check if it's filtered out
        for pattern in self._filter_patterns:
            if pattern.search(char):
                return False
        
        return True
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get comprehensive character tokenizer statistics."""
        base_stats = super().get_statistics()
        char_stats = self.get_character_statistics()
        
        # Add vocabulary information
        if hasattr(self, '_char_vocab'):
            char_stats['vocabulary_size'] = len(self._char_vocab)
        
        return {**base_stats, **char_stats}
