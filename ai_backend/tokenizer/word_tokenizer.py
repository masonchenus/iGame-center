"""
Word Tokenizer for AI Backend

Implements word-level tokenization with various word splitting strategies.
"""

import re
import string
from typing import Dict, Any, List, Optional, Tuple, Set
from .base_tokenizer import BaseTokenizer, TokenizationResult
from .config import TokenizerConfig, get_config


class WordTokenizer(BaseTokenizer):
    """
    Word-level tokenizer implementation.
    
    Features:
    - Multiple word splitting strategies
    - Punctuation handling
    - Case preservation options
    - Contraction handling
    - Language-specific rules
    """
    
    def __init__(self, config: Optional[TokenizerConfig] = None):
        """Initialize the word tokenizer."""
        super().__init__(config)
        
        if self.config.strategy.value != "word":
            raise ValueError(f"WordTokenizer requires WORD strategy, got {self.config.strategy.value}")
        
        # Word splitting patterns
        self._word_patterns = self._compile_word_patterns()
        
        # Punctuation sets
        self._punctuation = set(string.punctuation)
        self._sentence_endings = {'.', '!', '?'}
        self._word_boundaries = {'-', '_'}
        
        # Common abbreviations that shouldn't be split
        self._abbreviations = {
            'mr.', 'mrs.', 'dr.', 'prof.', 'sr.', 'jr.', 'vs.', 'etc.', 'e.g.', 'i.e.',
            'u.s.', 'u.k.', 'u.n.', 'e.u.', 'p.s.', 'a.m.', 'p.m.', 'st.', 'rd.', 'ave.'
        }
        
        # Contraction patterns
        self._contraction_patterns = self._compile_contraction_patterns()
    
    def _compile_word_patterns(self) -> Dict[str, re.Pattern]:
        """Compile word splitting patterns based on configuration."""
        patterns = {}
        
        if self.config.mode.value == "basic":
            # Simple whitespace splitting
            patterns['main'] = re.compile(r'\s+')
            
        elif self.config.mode.value == "preserve_punctuation":
            # Split on whitespace but keep punctuation with words
            patterns['main'] = re.compile(r'(\s+)|([^\w\s]+)')
            patterns['word'] = re.compile(r'\w+')
            
        elif self.config.mode.value == "preserve_case":
            # Split on whitespace, preserve case
            patterns['main'] = re.compile(r'\s+')
            
        elif self.config.mode.value == "advanced":
            # Advanced word splitting with multiple rules
            patterns['main'] = re.compile(
                r'(\s+)|' +  # Whitespace
                r'([^\w\s]+)|' +  # Punctuation
                r'([A-Z][a-z]+(?=[A-Z])|[A-Z]+(?=[0-9])|[a-z]+(?=[0-9]))|' +  # CamelCase and mixed
                r'([0-9]+(?:\.[0-9]+)?(?:[eE][+-]?[0-9]+)?)'  # Numbers (including scientific notation)
            )
            
        else:  # Default advanced
            patterns['main'] = re.compile(r'\s+')
        
        return patterns
    
    def _compile_contraction_patterns(self) -> List[Tuple[re.Pattern, str]]:
        """Compile patterns for handling contractions."""
        patterns = []
        
        # Common English contractions
        contractions = [
            (r"won't", "will not"),
            (r"can't", "cannot"),
            (r"n't", " not"),
            (r"'re", " are"),
            (r"'ve", " have"),
            (r"'ll", " will"),
            (r"'d", " would"),
            (r"'m", " am"),
            (r"'s", " is"),  # This is ambiguous (could be "is" or possessive)
        ]
        
        for pattern, replacement in contractions:
            compiled_pattern = re.compile(pattern, re.IGNORECASE)
            patterns.append((compiled_pattern, replacement))
        
        return patterns
    
    def _do_tokenize(self, text: str) -> List[str]:
        """Perform word-level tokenization."""
        tokens = []
        
        # Handle contractions first
        processed_text = self._expand_contractions(text)
        
        # Apply word splitting based on mode
        if self.config.mode.value == "basic":
            tokens = self._split_basic(processed_text)
        elif self.config.mode.value == "preserve_punctuation":
            tokens = self._split_preserve_punctuation(processed_text)
        elif self.config.mode.value == "preserve_case":
            tokens = self._split_preserve_case(processed_text)
        elif self.config.mode.value == "advanced":
            tokens = self._split_advanced(processed_text)
        else:
            tokens = self._split_basic(processed_text)
        
        # Post-process tokens
        tokens = self._post_process_word_tokens(tokens)
        
        return tokens
    
    def _expand_contractions(self, text: str) -> str:
        """Expand contractions in text."""
        if not self.config.handle_contractions:
            return text
        
        expanded_text = text
        for pattern, replacement in self._contraction_patterns:
            expanded_text = pattern.sub(replacement, expanded_text)
        
        return expanded_text
    
    def _split_basic(self, text: str) -> List[str]:
        """Basic whitespace splitting."""
        return text.split()
    
    def _split_preserve_punctuation(self, text: str) -> List[str]:
        """Split preserving punctuation with words."""
        tokens = []
        
        # Split on whitespace while keeping punctuation
        parts = re.split(r'(\s+)', text)
        
        for part in parts:
            if part.strip():  # Skip pure whitespace
                if re.match(r'^\s+$', part):
                    # This is whitespace, add as separator token
                    tokens.append(part.strip())
                else:
                    # This is content (words and/or punctuation)
                    # Further split to separate words from punctuation
                    word_punct_parts = re.findall(r'\w+|[^\w\s]+', part)
                    tokens.extend(word_punct_parts)
        
        return tokens
    
    def _split_preserve_case(self, text: str) -> List[str]:
        """Split preserving case information."""
        # Use basic splitting but don't modify case
        return text.split()
    
    def _split_advanced(self, text: str) -> List[str]:
        """Advanced word splitting with multiple rules."""
        tokens = []
        
        # Use the advanced pattern to split
        parts = self._word_patterns['main'].split(text)
        
        for part in parts:
            if part and not part.isspace():
                # Handle different types of parts
                if re.match(r'^\w+$', part):
                    # Pure word - check for CamelCase splitting
                    tokens.extend(self._split_camel_case_word(part))
                elif re.match(r'^[0-9]+(?:\.[0-9]+)?(?:[eE][+-]?[0-9]+)?$', part):
                    # Number (including scientific notation)
                    tokens.append(part)
                elif re.match(r'^[^\w\s]+$', part):
                    # Pure punctuation
                    tokens.append(part)
                else:
                    # Mixed content - split further
                    sub_tokens = re.findall(r'\w+|[^\w\s]+', part)
                    tokens.extend(sub_tokens)
        
        return tokens
    
    def _split_camel_case_word(self, word: str) -> List[str]:
        """Split CamelCase words into separate tokens."""
        if not self.config.split_camel_case:
            return [word]
        
        # Pattern to split camelCase
        # This splits at transitions from lowercase to uppercase
        pattern = re.compile(r'([a-z])([A-Z])')
        split_word = pattern.sub(r'\1 \2', word)
        
        # Also handle sequences of uppercase letters
        pattern2 = re.compile(r'([A-Z]+)([A-Z][a-z])')
        split_word = pattern2.sub(r'\1 \2', split_word)
        
        return split_word.split()
    
    def _post_process_word_tokens(self, tokens: List[str]) -> List[str]:
        """Post-process word tokens."""
        processed = []
        
        for token in tokens:
            # Skip empty tokens
            if not token.strip():
                continue
            
            # Handle punctuation-based splitting
            if self.config.preserve_punctuation:
                processed.append(token)
            else:
                # Split punctuation from words
                processed.extend(self._split_punctuation_from_word(token))
        
        # Remove duplicates while preserving order
        seen = set()
        unique_tokens = []
        for token in processed:
            if token not in seen:
                seen.add(token)
                unique_tokens.append(token)
        
        return unique_tokens
    
    def _split_punctuation_from_word(self, token: str) -> List[str]:
        """Split punctuation characters from word tokens."""
        if not token:
            return []
        
        # Find boundaries between letters/digits and punctuation
        parts = re.findall(r'\w+|[^\w\s]+', token)
        
        # Filter out empty parts
        return [part for part in parts if part.strip()]
    
    def _tokens_to_ids(self, tokens: List[str]) -> List[int]:
        """Convert tokens to IDs (simplified - no actual vocabulary)."""
        # For now, use a simple hash-based encoding
        # In a real implementation, this would use an actual vocabulary
        token_ids = []
        for token in tokens:
            # Simple hash to ID conversion
            token_hash = hash(token) % 10000  # Limited range for demo
            token_ids.append(token_hash)
        return token_ids
    
    def get_vocabulary(self) -> Dict[str, int]:
        """Get vocabulary (simplified implementation)."""
        # In a real implementation, this would build from training data
        vocab = {}
        id_counter = 0
        
        # Add special tokens
        for name, token in self.special_tokens.items():
            vocab[token] = id_counter
            id_counter += 1
        
        # Add common English words (simplified)
        common_words = [
            'the', 'be', 'to', 'of', 'and', 'a', 'in', 'that', 'have', 'i',
            'it', 'for', 'not', 'on', 'with', 'he', 'as', 'you', 'do', 'at',
            'this', 'but', 'his', 'by', 'from', 'they', 'we', 'say', 'her', 'she',
            'or', 'an', 'will', 'my', 'one', 'all', 'would', 'there', 'their', 'what',
            'so', 'up', 'out', 'if', 'about', 'who', 'get', 'which', 'go', 'me'
        ]
        
        for word in common_words:
            if word not in vocab:
                vocab[word] = id_counter
                id_counter += 1
        
        return vocab
    
    def get_word_statistics(self) -> Dict[str, Any]:
        """Get word-specific statistics."""
        stats = {
            "word_splitting_mode": self.config.mode.value,
            "preserve_punctuation": self.config.preserve_punctuation,
            "handle_contractions": self.config.handle_contractions,
            "split_camel_case": self.config.split_camel_case,
            "contractions_patterns": len(self._contraction_patterns),
            "abbreviations": len(self._abbreviations)
        }
        return stats
    
    def tokenize_sentences(self, text: str) -> List[List[str]]:
        """
        Tokenize text into sentences, then into words.
        
        Args:
            text: Input text to sentence-tokenize
            
        Returns:
            List of sentences, each containing word tokens
        """
        sentences = self._split_into_sentences(text)
        tokenized_sentences = []
        
        for sentence in sentences:
            tokens = self.tokenize(sentence)
            tokenized_sentences.append(tokens.tokens)
        
        return tokenized_sentences
    
    def _split_into_sentences(self, text: str) -> List[str]:
        """Split text into sentences."""
        # Simple sentence splitting on sentence endings
        # This is a basic implementation - more sophisticated ones would handle
        # abbreviations, quotes, etc.
        
        sentences = []
        current_sentence = ""
        
        for char in text:
            current_sentence += char
            
            # Check if this could be end of sentence
            if char in self._sentence_endings:
                # Look ahead to see if next non-whitespace char is likely sentence end
                next_chars = text[text.index(char) + 1:text.index(char) + 3]
                if re.match(r'^\s*[.!?]?\s*[A-Z"\'\(]', next_chars):
                    sentences.append(current_sentence.strip())
                    current_sentence = ""
        
        # Add remaining text
        if current_sentence.strip():
            sentences.append(current_sentence.strip())
        
        return sentences
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get comprehensive word tokenizer statistics."""
        base_stats = super().get_statistics()
        word_stats = self.get_word_statistics()
        
        return {**base_stats, **word_stats}
