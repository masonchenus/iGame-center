"""
Transformer Encoder for AI Backend

Implements the transformer encoder layer and full encoder stack.
"""

import math
import numpy as np
from typing import Optional, Tuple, Dict, Any
from .attention import MultiHeadAttention
from .config import TransformerConfig, get_config


class LayerNorm:
    """
    Layer Normalization implementation.
    
    Applies layer normalization to stabilize training.
    """
    
    def __init__(self, d_model: int, epsilon: float = 1e-6):
        """Initialize layer normalization."""
        self.epsilon = epsilon
        self.gamma = np.ones(d_model)
        self.beta = np.zeros(d_model)
    
    def forward(self, x: np.ndarray) -> np.ndarray:
        """
        Apply layer normalization.
        
        Args:
            x: Input tensor
            
        Returns:
            Normalized tensor
        """
        mean = np.mean(x, axis=-1, keepdims=True)
        variance = np.var(x, axis=-1, keepdims=True)
        
        # Normalize
        normalized = (x - mean) / np.sqrt(variance + self.epsilon)
        
        # Scale and shift
        return self.gamma * normalized + self.beta


class FeedForward:
    """
    Position-wise Feed-Forward Network.
    
    Two linear transformations with a ReLU activation in between.
    """
    
    def __init__(self, d_model: int, d_ff: int, activation: str = "gelu", use_bias: bool = True):
        """Initialize feed-forward network."""
        self.d_model = d_model
        self.d_ff = d_ff
        self.activation = activation
        self.use_bias = use_bias
        
        # Linear layers
        self.w1 = self._init_linear(d_model, d_ff)
        self.w2 = self._init_linear(d_ff, d_model)
        
        if use_bias:
            self.b1 = np.zeros(d_ff)
            self.b2 = np.zeros(d_model)
        else:
            self.b1 = None
            self.b2 = None
    
    def _init_linear(self, in_dim: int, out_dim: int) -> np.ndarray:
        """Initialize linear layer weights."""
        # Xavier initialization
        limit = math.sqrt(6.0 / (in_dim + out_dim))
        return np.random.uniform(-limit, limit, (in_dim, out_dim))
    
    def _activation(self, x: np.ndarray) -> np.ndarray:
        """Apply activation function."""
        if self.activation == "relu":
            return np.maximum(0, x)
        elif self.activation == "gelu":
            # GELU approximation
            return 0.5 * x * (1 + np.tanh(math.sqrt(2 / math.pi) * (x + 0.044715 * np.power(x, 3))))
        elif self.activation == "swish":
            return x * (1 / (1 + np.exp(-x)))
        elif self.activation == "leaky_relu":
            return np.where(x > 0, x, 0.01 * x)
        elif self.activation == "elu":
            return np.where(x >= 0, x, np.exp(x) - 1)
        else:
            return x  # Linear
    
    def forward(self, x: np.ndarray) -> np.ndarray:
        """
        Forward pass of feed-forward network.
        
        Args:
            x: Input tensor
            
        Returns:
            Output tensor
        """
        # First linear transformation
        hidden = np.matmul(x, self.w1)
        if self.b1 is not None:
            hidden += self.b1
        
        # Activation
        hidden = self._activation(hidden)
        
        # Second linear transformation
        output = np.matmul(hidden, self.w2)
        if self.b2 is not None:
            output += self.b2
        
        return output


class EncoderLayer:
    """
    Single Transformer Encoder Layer.
    
    Contains multi-head self-attention and position-wise feed-forward network.
    """
    
    def __init__(self, config: Optional[TransformerConfig] = None):
        """Initialize encoder layer."""
        self.config = config or get_config()
        self.d_model = self.config.d_model
        self.n_heads = self.config.n_heads
        self.d_ff = self.config.d_ff
        self.dropout_rate = self.config.dropout_rate
        
        # Multi-head self-attention
        self.self_attention = MultiHeadAttention(config)
        
        # Feed-forward network
        self.feed_forward = FeedForward(
            self.d_model, 
            self.d_ff, 
            self.config.activation.value,
            self.config.use_bias
        )
        
        # Layer normalization
        self.norm1 = LayerNorm(self.d_model, self.config.layer_norm_epsilon)
        self.norm2 = LayerNorm(self.d_model, self.config.layer_norm_epsilon)
        
        # Dropout
        self.dropout_rate = self.config.dropout_rate
    
    def forward(self, 
                x: np.ndarray,
                mask: Optional[np.ndarray] = None,
                training: bool = True) -> np.ndarray:
        """
        Forward pass of encoder layer.
        
        Args:
            x: Input tensor [batch_size, seq_len, d_model]
            mask: Attention mask
            training: Whether in training mode
            
        Returns:
            Output tensor
        """
        # Multi-head self-attention with residual connection
        attn_output = self.self_attention.forward(x, x, x, mask)
        
        # Apply dropout during training
        if training and self.dropout_rate > 0:
            attn_output = self._apply_dropout(attn_output)
        
        # First residual connection and layer norm
        if self.config.use_layer_norm:
            attn_output = self.norm1.forward(x + attn_output)
        else:
            attn_output = x + attn_output
        
        # Feed-forward network
        ff_output = self.feed_forward.forward(attn_output)
        
        # Apply dropout during training
        if training and self.dropout_rate > 0:
            ff_output = self._apply_dropout(ff_output)
        
        # Second residual connection and layer norm
        if self.config.use_layer_norm:
            output = self.norm2.forward(attn_output + ff_output)
        else:
            output = attn_output + ff_output
        
        return output
    
    def _apply_dropout(self, x: np.ndarray) -> np.ndarray:
        """Apply dropout to tensor."""
        if self.dropout_rate > 0:
            mask = np.random.binomial(1, 1 - self.dropout_rate, x.shape) / (1 - self.dropout_rate)
            return x * mask
        return x


class TransformerEncoder:
    """
    Full Transformer Encoder.
    
    Stacks multiple encoder layers with optional positional encoding.
    """
    
    def __init__(self, config: Optional[TransformerConfig] = None):
        """Initialize transformer encoder."""
        self.config = config or get_config()
        self.vocab_size = self.config.vocab_size
        self.d_model = self.config.d_model
        self.n_layers = self.config.n_layers
        self.max_seq_length = self.config.max_seq_length
        
        # Token embedding
        self.token_embedding = self._init_embedding(self.vocab_size, self.d_model)
        
        # Positional encoding
        if self.config.use_positional_encoding:
            self.pos_encoding = self._init_positional_encoding(
                self.max_seq_length, self.d_model
            )
        else:
            self.pos_encoding = None
        
        # Encoder layers
        self.layers = [EncoderLayer(config) for _ in range(self.n_layers)]
        
        # Final layer normalization
        if self.config.layer_norm_type == "post":
            self.final_layer_norm = LayerNorm(self.d_model, self.config.layer_norm_epsilon)
        else:
            self.final_layer_norm = None
        
        # Dropout
        self.dropout_rate = self.config.dropout_rate
    
    def _init_embedding(self, vocab_size: int, d_model: int) -> np.ndarray:
        """Initialize token embedding."""
        limit = 1.0 / math.sqrt(vocab_size)
        return np.random.uniform(-limit, limit, (vocab_size, d_model))
    
    def _init_positional_encoding(self, max_seq_len: int, d_model: int) -> np.ndarray:
        """Initialize sinusoidal positional encoding."""
        pe = np.zeros((max_seq_len, d_model))
        
        for pos in range(max_seq_len):
            for i in range(0, d_model, 2):
                # Even dimensions: sin
                if i < d_model:
                    pe[pos, i] = math.sin(pos / math.pow(10000, (2 * i) / d_model))
                
                # Odd dimensions: cos
                if i + 1 < d_model:
                    pe[pos, i + 1] = math.cos(pos / math.pow(10000, (2 * i) / d_model))
        
        return pe
    
    def create_padding_mask(self, input_ids: np.ndarray, pad_token_id: int = 0) -> np.ndarray:
        """Create padding mask for encoder attention."""
        mask = (input_ids == pad_token_id).astype(np.float32)
        # Convert to negative for masking in attention
        return np.where(mask == 1, -np.inf, 0.0)
    
    def forward(self, 
                input_ids: np.ndarray,
                attention_mask: Optional[np.ndarray] = None,
                training: bool = True) -> np.ndarray:
        """
        Forward pass of transformer encoder.
        
        Args:
            input_ids: Input token IDs [batch_size, seq_len]
            attention_mask: Attention mask
            training: Whether in training mode
            
        Returns:
            Encoder output [batch_size, seq_len, d_model]
        """
        batch_size, seq_len = input_ids.shape
        
        # Token embeddings
        x = self.token_embedding[input_ids]  # [batch_size, seq_len, d_model]
        
        # Scale embeddings
        x = x * math.sqrt(self.d_model)
        
        # Add positional encoding
        if self.pos_encoding is not None:
            # Truncate positional encoding if sequence is too long
            pos_encoding = self.pos_encoding[:seq_len]  # [seq_len, d_model]
            x = x + pos_encoding  # Broadcasting: [batch_size, seq_len, d_model] + [seq_len, d_model]
        
        # Apply dropout to embeddings
        if training and self.dropout_rate > 0:
            x = self._apply_dropout(x)
        
        # Create attention mask
        if attention_mask is None:
            attention_mask = self.create_padding_mask(input_ids)
        
        # Pass through encoder layers
        for layer in self.layers:
            x = layer.forward(x, attention_mask, training)
        
        # Final layer normalization
        if self.final_layer_norm is not None:
            x = self.final_layer_norm.forward(x)
        
        return x
    
    def _apply_dropout(self, x: np.ndarray) -> np.ndarray:
        """Apply dropout to tensor."""
        if self.dropout_rate > 0:
            mask = np.random.binomial(1, 1 - self.dropout_rate, x.shape) / (1 - self.dropout_rate)
            return x * mask
        return x
    
    def get_attention_weights(self, 
                             input_ids: np.ndarray,
                             layer_idx: int = 0) -> np.ndarray:
        """Get attention weights from a specific layer."""
        if layer_idx >= len(self.layers):
            raise ValueError(f"Layer index {layer_idx} exceeds number of layers {len(self.layers)}")
        
        # Get embeddings (simplified - in practice would need to extract from forward pass)
        x = self.token_embedding[input_ids] * math.sqrt(self.d_model)
        
        if self.pos_encoding is not None:
            seq_len = input_ids.shape[1]
            pos_encoding = self.pos_encoding[:seq_len]
            x = x + pos_encoding
        
        # Get attention weights from the specified layer
        attention_weights = self.layers[layer_idx].self_attention.get_attention_weights(x, x)
        
        return attention_weights
    
    def get_layer_outputs(self, 
                         input_ids: np.ndarray,
                         training: bool = True) -> Dict[int, np.ndarray]:
        """Get outputs from all encoder layers."""
        batch_size, seq_len = input_ids.shape
        
        # Token embeddings
        x = self.token_embedding[input_ids] * math.sqrt(self.d_model)
        
        if self.pos_encoding is not None:
            pos_encoding = self.pos_encoding[:seq_len]
            x = x + pos_encoding
        
        if training and self.dropout_rate > 0:
            x = self._apply_dropout(x)
        
        layer_outputs = {}
        
        for i, layer in enumerate(self.layers):
            x = layer.forward(x, training=training)
            layer_outputs[i] = x.copy()
        
        if self.final_layer_norm is not None:
            x = self.final_layer_norm.forward(x)
            layer_outputs['final'] = x
        
        return layer_outputs
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get encoder statistics."""
        total_params = 0
        
        # Token embedding parameters
        total_params += self.token_embedding.size
        
        # Positional encoding parameters
        if self.pos_encoding is not None:
            total_params += self.pos_encoding.size
        
        # Encoder layer parameters
        for layer in self.layers:
            layer_stats = layer.self_attention.get_statistics()
            total_params += layer_stats['total_parameters']
            total_params += layer.feed_forward.w1.size + layer.feed_forward.w2.size
        
        # Final layer norm parameters
        if self.final_layer_norm is not None:
            total_params += self.final_layer_norm.gamma.size + self.final_layer_norm.beta.size
        
        return {
            "vocab_size": self.vocab_size,
            "d_model": self.d_model,
            "n_layers": self.n_layers,
            "max_seq_length": self.max_seq_length,
            "use_positional_encoding": self.config.use_positional_encoding,
            "total_parameters": total_params,
            "layer_norm_type": self.config.layer_norm_type,
            "activation": self.config.activation.value
        }
