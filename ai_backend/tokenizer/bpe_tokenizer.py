"""
BPE (Byte-Pair Encoding) Tokenizer for AI Backend

Implements Byte-Pair Encoding tokenization algorithm for subword tokenization.
"""

import re
from typing import Dict, Any, List, Optional, Tuple, Set
from collections import Counter, defaultdict
from .base_tokenizer import BaseTokenizer, TokenizationResult
from .config import TokenizerConfig, get_config


class BPETokenizer(BaseTokenizer):
    """
    BPE Tokenizer implementation.
    
    Features:
    - Learns BPE merges from training data
    - Supports subword tokenization
    - Handles unknown tokens with UNK token
    - Efficient vocabulary management
    """
    
    def __init__(self, config: Optional[TokenizerConfig] = None):
        """Initialize the BPE tokenizer."""
        super().__init__(config)
        
        if self.config.strategy.value != "bpe":
            raise ValueError(f"BPETokenizer requires BPE strategy, got {self.config.strategy.value}")
        
        # BPE-specific attributes
        self.merges = []
        self.vocab = {}
        self.reverse_vocab = {}
        self.word_vocab = {}
        
        # Training data for building vocabulary
        self._training_texts = []
        
        # Initialize with basic vocabulary
        self._initialize_basic_vocabulary()
    
    def _initialize_basic_vocabulary(self) -> None:
        """Initialize with basic character-level vocabulary."""
        # Add all characters that might appear
        chars = set()
        
        # Add basic ASCII characters
        for i in range(32, 127):  # Printable ASCII
            chars.add(chr(i))
        
        # Add some Unicode characters
        for i in range(128, 256):
            chars.add(chr(i))
        
        # Add common whitespace
        chars.update(['\n', '\r', '\t', '\f', '\v'])
        
        # Initialize vocabulary
        self.vocab = {char: i for i, char in enumerate(sorted(chars))}
        self.vocab.update({
            self.config.unk_token: len(self.vocab),
            self.config.pad_token: len(self.vocab),
            self.config.bos_token: len(self.vocab),
            self.config.eos_token: len(self.vocab),
            self.config.sep_token: len(self.vocab),
            self.config.cls_token: len(self.vocab),
            self.config.mask_token: len(self.vocab)
        })
        
        # Build reverse vocabulary
        self.reverse_vocab = {v: k for k, v in self.vocab.items()}
    
    def train(self, texts: List[str], vocab_size: Optional[int] = None) -> None:
        """
        Train BPE tokenizer on provided texts.
        
        Args:
            texts: List of training texts
            vocab_size: Target vocabulary size
        """
        if vocab_size is None:
            vocab_size = self.config.vocab_size
        
        print(f"Training BPE tokenizer with target vocab size: {vocab_size}")
        
        # Preprocess texts
        processed_texts = []
        for text in texts:
            processed_text = self._preprocess_text(text)
            processed_texts.append(processed_text)
        
        # Build word vocabulary
        self._build_word_vocabulary(processed_texts)
        
        # Learn BPE merges
        self._learn_bpe_merges(vocab_size)
        
        # Build final vocabulary
        self._build_final_vocabulary()
        
        print(f"BPE training complete. Vocabulary size: {len(self.vocab)}")
    
    def _build_word_vocabulary(self, texts: List[str]) -> None:
        """Build word-level vocabulary from texts."""
        word_freq = Counter()
        
        for text in texts:
            # Split into words (simple whitespace split for now)
            words = text.split()
            for word in words:
                word_freq[word] += 1
        
        # Filter by frequency
        min_freq = self.config.min_frequency
        filtered_words = {word: freq for word, freq in word_freq.items() 
                         if freq >= min_freq}
        
        self.word_vocab = filtered_words
        print(f"Word vocabulary size: {len(self.word_vocab)}")
    
    def _learn_bpe_merges(self, target_vocab_size: int) -> None:
        """Learn BPE merge operations."""
        # Initialize word pairs with counts
        word_pairs = self._get_word_pairs()
        
        # Learn merges until target vocabulary size is reached
        while len(self.vocab) < target_vocab_size and word_pairs:
            # Find most frequent pair
            most_frequent_pair = max(word_pairs, key=word_pairs.get)
            pair_freq = word_pairs[most_frequent_pair]
            
            # Perform merge
            self._merge_pair(most_frequent_pair)
            
            # Update word pairs
            word_pairs = self._update_word_pairs(most_frequent_pair)
            
            # Log progress
            if len(self.vocab) % 1000 == 0:
                print(f"Vocabulary size: {len(self.vocab)}")
        
        print(f"Learned {len(self.merges)} BPE merges")
    
    def _get_word_pairs(self) -> Dict[Tuple[str, str], int]:
        """Get all word pairs and their frequencies."""
        pair_counts = Counter()
        
        for word, freq in self.word_vocab.items():
            symbols = word.split()
            
            # Add special symbols for BPE
            symbols = ['<w>'] + symbols + ['</w>']
            
            # Count pairs
            for i in range(len(symbols) - 1):
                pair = (symbols[i], symbols[i + 1])
                pair_counts[pair] += freq
        
        return dict(pair_counts)
    
    def _merge_pair(self, pair: Tuple[str, str]) -> None:
        """Merge a pair of symbols."""
        # Add merge to list
        self.merges.append(pair)
        
        # Create new symbol
        new_symbol = ''.join(pair)
        
        # Update word vocabulary
        new_word_vocab = {}
        
        for word, freq in self.word_vocab.items():
            new_word = word.replace(' '.join(pair), new_symbol)
            new_word_vocab[new_word] = freq
        
        self.word_vocab = new_word_vocab
    
    def _update_word_pairs(self, merged_pair: Tuple[str, str]) -> Dict[Tuple[str, str], int]:
        """Update word pairs after a merge."""
        new_symbol = ''.join(merged_pair)
        old_pair = ' '.join(merged_pair)
        
        # Update existing word pairs
        pair_counts = Counter()
        
        for word, freq in self.word_vocab.items():
            symbols = word.split()
            
            # Add special symbols
            symbols = ['<w>'] + symbols + ['</w>']
            
            # Count pairs
            for i in range(len(symbols) - 1):
                pair = (symbols[i], symbols[i + 1])
                
                # Skip the merged pair
                if pair != merged_pair:
                    pair_counts[pair] += freq
        
        return dict(pair_counts)
    
    def _build_final_vocabulary(self) -> None:
        """Build final vocabulary from learned merges."""
        # Start with character vocabulary
        final_vocab = set(self.vocab.keys())
        
        # Add all subwords from word vocabulary
        for word in self.word_vocab.keys():
            final_vocab.update(word.split())
        
        # Add special tokens
        special_tokens = [
            self.config.unk_token,
            self.config.pad_token,
            self.config.bos_token,
            self.config.eos_token,
            self.config.sep_token,
            self.config.cls_token,
            self.config.mask_token
        ]
        final_vocab.update(special_tokens)
        
        # Create final vocabulary mapping
        self.vocab = {token: i for i, token in enumerate(sorted(final_vocab))}
        self.reverse_vocab = {v: k for k, v in self.vocab.items()}
    
    def _do_tokenize(self, text: str) -> List[str]:
        """Perform BPE tokenization."""
        if not self.vocab:
            raise ValueError("Tokenizer not trained. Call train() first.")
        
        # Split text into words
        words = text.split()
        tokens = []
        
        for word in words:
            # Add word boundary symbols
            word = '<w>' + word + '</w>'
            
            # Tokenize word using BPE
            word_tokens = self._tokenize_word_bpe(word)
            tokens.extend(word_tokens)
        
        return tokens
    
    def _tokenize_word_bpe(self, word: str) -> List[str]:
        """Tokenize a single word using BPE."""
        if word in self.vocab:
            return [word]
        
        # If word not in vocabulary, split into characters and apply BPE
        symbols = list(word)
        
        # Apply BPE merges
        while len(symbols) > 1:
            # Find best pair to merge
            pairs = [(symbols[i], symbols[i + 1]) for i in range(len(symbols) - 1)]
            
            # Find pair in merges
            pair_to_merge = None
            for pair in pairs:
                if pair in self.merges:
                    pair_to_merge = pair
                    break
            
            if pair_to_merge is None:
                break  # No more merges possible
            
            # Find position to merge
            i = pairs.index(pair_to_merge)
            
            # Merge the pair
            new_symbols = []
            for j, symbol in enumerate(symbols):
                if j == i:
                    new_symbols.append(''.join(pair_to_merge))
                elif j == i + 1:
                    continue  # Skip next symbol as it's merged
                else:
                    new_symbols.append(symbol)
            
            symbols = new_symbols
        
        # Convert to tokens, using UNK for unknown symbols
        tokens = []
        for symbol in symbols:
            if symbol in self.vocab:
                tokens.append(symbol)
            else:
                tokens.append(self.config.unk_token)
        
        return tokens
    
    def _tokens_to_ids(self, tokens: List[str]) -> List[int]:
        """Convert tokens to IDs using BPE vocabulary."""
        return [self.vocab.get(token, self.vocab[self.config.unk_token]) for token in tokens]
    
    def _ids_to_tokens(self, ids: List[int]) -> List[str]:
        """Convert IDs back to tokens."""
        return [self.reverse_vocab.get(id, self.config.unk_token) for id in ids]
    
    def encode(self, text: str) -> List[int]:
        """Encode text to token IDs."""
        result = self.tokenize(text)
        return result.token_ids or []
    
    def decode(self, tokens: List[int], skip_special_tokens: bool = True) -> str:
        """Decode token IDs back to text."""
        token_strings = self._ids_to_tokens(tokens)
        
        if skip_special_tokens:
            # Remove special tokens
            special_tokens = set(self.special_tokens.values())
            token_strings = [token for token in token_strings if token not in special_tokens]
        
        # Join tokens and clean up
        text = ''.join(token_strings)
        
        # Remove word boundary symbols
        text = text.replace('<w>', '').replace('</w>', '')
        
        # Clean up whitespace
        text = re.sub(r'\s+', ' ', text).strip()
        
        return text
    
    def get_vocabulary(self) -> Dict[str, int]:
        """Get the vocabulary mapping."""
        return self.vocab.copy()
    
    def get_merges(self) -> List[Tuple[str, str]]:
        """Get the list of BPE merges."""
        return self.merges.copy()
    
    def save_vocabulary(self, vocab_path: str, merges_path: str) -> None:
        """Save vocabulary and merges to files."""
        import json
        
        # Save vocabulary
        with open(vocab_path, 'w', encoding='utf-8') as f:
            json.dump(self.vocab, f, ensure_ascii=False, indent=2)
        
        # Save merges
        with open(merges_path, 'w', encoding='utf-8') as f:
            json.dump(self.merges, f, ensure_ascii=False, indent=2)
    
    def load_vocabulary(self, vocab_path: str, merges_path: Optional[str] = None) -> None:
        """Load vocabulary and merges from files."""
        import json
        
        # Load vocabulary
        with open(vocab_path, 'r', encoding='utf-8') as f:
            self.vocab = json.load(f)
        
        self.reverse_vocab = {v: k for k, v in self.vocab.items()}
        
        # Load merges if provided
        if merges_path:
            with open(merges_path, 'r', encoding='utf-8') as f:
                self.merges = json.load(f)
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get BPE tokenizer statistics."""
        base_stats = super().get_statistics()
        
        bpe_stats = {
            "vocabulary_size": len(self.vocab),
            "merges_learned": len(self.merges),
            "word_vocabulary_size": len(self.word_vocab),
            "unk_token": self.config.unk_token,
            "unk_count": 0  # Could be tracked if needed
        }
        
        return {**base_stats, **bpe_stats}
