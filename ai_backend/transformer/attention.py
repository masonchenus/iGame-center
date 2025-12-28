"""
Multi-Head Attention for AI Backend Transformer

Implements scaled dot-product attention and multi-head attention mechanisms.
"""

import math
from typing import Optional, Tuple, Dict, Any
import numpy as np
from .config import TransformerConfig, get_config


class ScaledDotProductAttention:
    """
    Scaled Dot-Product Attention mechanism.
    
    Computes attention scores using the formula:
    Attention(Q, K, V) = softmax(QK^T / sqrt(d_k))V
    """
    
    def __init__(self, config: Optional[TransformerConfig] = None):
        """Initialize scaled dot-product attention."""
        self.config = config or get_config()
        self.scale = 1.0 / math.sqrt(self.config.d_model // self.config.n_heads)
        
    def forward(self, 
                query: np.ndarray,
                key: np.ndarray,
                value: np.ndarray,
                mask: Optional[np.ndarray] = None,
                dropout_p: float = 0.0) -> Tuple[np.ndarray, np.ndarray]:
        """
        Forward pass of scaled dot-product attention.
        
        Args:
            query: Query tensor [batch_size, seq_len, d_model]
            key: Key tensor [batch_size, seq_len, d_model]
            value: Value tensor [batch_size, seq_len, d_model]
            mask: Optional attention mask
            dropout_p: Dropout probability
            
        Returns:
            Tuple of (attention_output, attention_weights)
        """
        d_k = query.shape[-1]
        
        # Calculate attention scores
        scores = np.matmul(query, key.transpose(0, 2, 1)) / math.sqrt(d_k)
        
        # Apply mask if provided
        if mask is not None:
            scores = np.where(mask == 0, -1e9, scores)
        
        # Apply softmax
        attention_weights = self._softmax(scores)
        
        # Apply dropout during training
        if dropout_p > 0:
            attention_weights = self._dropout(attention_weights, dropout_p)
        
        # Apply attention to values
        output = np.matmul(attention_weights, value)
        
        return output, attention_weights
    
    def _softmax(self, x: np.ndarray) -> np.ndarray:
        """Apply softmax function."""
        # Subtract max for numerical stability
        x_max = np.max(x, axis=-1, keepdims=True)
        exp_x = np.exp(x - x_max)
        return exp_x / np.sum(exp_x, axis=-1, keepdims=True)
    
    def _dropout(self, x: np.ndarray, dropout_p: float) -> np.ndarray:
        """Apply dropout."""
        if dropout_p > 0:
            mask = np.random.binomial(1, 1 - dropout_p, x.shape) / (1 - dropout_p)
            return x * mask
        return x


class MultiHeadAttention:
    """
    Multi-Head Attention mechanism.
    
    Projects queries, keys, and values through multiple attention heads
    and combines the results.
    """
    
    def __init__(self, config: Optional[TransformerConfig] = None):
        """Initialize multi-head attention."""
        self.config = config or get_config()
        self.d_model = config.d_model if config else 512
        self.n_heads = config.n_heads if config else 8
        self.d_k = self.d_model // self.n_heads
        self.d_v = self.d_k
        
        # Validate configuration
        if self.d_model % self.n_heads != 0:
            raise ValueError(f"d_model ({self.d_model}) must be divisible by n_heads ({self.n_heads})")
        
        # Linear projections for Q, K, V
        self.w_q = self._init_linear(self.d_model, self.d_model)
        self.w_k = self._init_linear(self.d_model, self.d_model)
        self.w_v = self._init_linear(self.d_model, self.d_model)
        
        # Output linear projection
        self.w_o = self._init_linear(self.d_model, self.d_model)
        
        # Scaled dot-product attention
        self.attention = ScaledDotProductAttention(config)
        
        # Dropout
        self.dropout_p = config.attention_dropout if config else 0.1
    
    def _init_linear(self, in_dim: int, out_dim: int) -> np.ndarray:
        """Initialize linear layer weights."""
        # Xavier initialization
        limit = math.sqrt(6.0 / (in_dim + out_dim))
        return np.random.uniform(-limit, limit, (in_dim, out_dim))
    
    def forward(self, 
                query: np.ndarray,
                key: np.ndarray,
                value: np.ndarray,
                mask: Optional[np.ndarray] = None,
                return_attention_weights: bool = False) -> np.ndarray:
        """
        Forward pass of multi-head attention.
        
        Args:
            query: Query tensor [batch_size, seq_len, d_model]
            key: Key tensor [batch_size, seq_len, d_model]
            value: Value tensor [batch_size, seq_len, d_model]
            mask: Optional attention mask
            return_attention_weights: Whether to return attention weights
            
        Returns:
            Multi-head attention output
        """
        batch_size, seq_len, d_model = query.shape
        
        # Linear projections for all heads
        Q = np.matmul(query, self.w_q)  # [batch_size, seq_len, d_model]
        K = np.matmul(key, self.w_k)    # [batch_size, seq_len, d_model]
        V = np.matmul(value, self.w_v)  # [batch_size, seq_len, d_model]
        
        # Reshape for multi-head attention
        Q = Q.reshape(batch_size, seq_len, self.n_heads, self.d_k).transpose(0, 2, 1, 3)
        K = K.reshape(batch_size, seq_len, self.n_heads, self.d_k).transpose(0, 2, 1, 3)
        V = V.reshape(batch_size, seq_len, self.n_heads, self.d_v).transpose(0, 2, 1, 3)
        
        # Apply scaled dot-product attention
        if mask is not None:
            # Expand mask for multi-head attention
            mask = mask.reshape(batch_size, 1, seq_len, 1)  # Broadcast across heads
            mask = np.repeat(mask, self.n_heads, axis=1)     # Repeat for each head
        
        attn_output, attention_weights = self.attention.forward(
            Q, K, V, mask, self.dropout_p
        )
        
        # Concatenate heads
        attn_output = attn_output.transpose(0, 2, 1, 3).reshape(
            batch_size, seq_len, d_model
        )
        
        # Final linear projection
        output = np.matmul(attn_output, self.w_o)
        
        if return_attention_weights:
            return output, attention_weights
        else:
            return output
    
    def create_causal_mask(self, seq_len: int) -> np.ndarray:
        """Create causal (triangular) mask for decoder attention."""
        mask = np.triu(np.full((seq_len, seq_len), -np.inf), k=1)
        return mask
    
    def create_padding_mask(self, input_ids: np.ndarray, pad_token_id: int = 0) -> np.ndarray:
        """Create padding mask for sequences."""
        mask = (input_ids == pad_token_id).astype(np.float32)
        # Convert to negative for masking in attention
        return np.where(mask == 1, -np.inf, 0.0)
    
    def combine_masks(self, *masks: Optional[np.ndarray]) -> Optional[np.ndarray]:
        """Combine multiple masks using element-wise minimum."""
        valid_masks = [mask for mask in masks if mask is not None]
        if not valid_masks:
            return None
        
        combined = valid_masks[0]
        for mask in valid_masks[1:]:
            combined = np.minimum(combined, mask)
        
        return combined
    
    def get_attention_weights(self, 
                             query: np.ndarray,
                             key: np.ndarray,
                             mask: Optional[np.ndarray] = None) -> np.ndarray:
        """Get attention weights without applying them to values."""
        batch_size, seq_len, d_model = query.shape
        
        # Linear projections
        Q = np.matmul(query, self.w_q)
        K = np.matmul(key, self.w_k)
        
        # Reshape for multi-head attention
        Q = Q.reshape(batch_size, seq_len, self.n_heads, self.d_k).transpose(0, 2, 1, 3)
        K = K.reshape(batch_size, seq_len, self.n_heads, self.d_k).transpose(0, 2, 1, 3)
        
        # Calculate attention scores
        scores = np.matmul(Q, K.transpose(0, 1, 3, 2)) / math.sqrt(self.d_k)
        
        # Apply mask if provided
        if mask is not None:
            mask = mask.reshape(batch_size, 1, seq_len, 1)
            mask = np.repeat(mask, self.n_heads, axis=1)
            scores = np.where(mask == -np.inf, -np.inf, scores)
        
        # Apply softmax to get weights
        attention_weights = self.attention._softmax(scores)
        
        return attention_weights
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get attention mechanism statistics."""
        return {
            "d_model": self.d_model,
            "n_heads": self.n_heads,
            "d_k": self.d_k,
            "d_v": self.d_v,
            "scale": self.scale,
            "dropout_p": self.dropout_p,
            "total_parameters": (
                self.w_q.size + self.w_k.size + self.w_v.size + self.w_o.size
            )
        }
