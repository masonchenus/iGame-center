"""
Transformer Decoder for AI Backend

Implements the transformer decoder layer and full decoder stack.
"""

import math
import numpy as np
from typing import Optional, Tuple, Dict, Any
from .attention import MultiHeadAttention
from .encoder import LayerNorm, FeedForward
from .config import TransformerConfig, get_config


class DecoderLayer:
    """
    Single Transformer Decoder Layer.
    
    Contains:
    - Masked multi-head self-attention
    - Multi-head cross-attention (with encoder output)
    - Position-wise feed-forward network
    """
    
    def __init__(self, config: Optional[TransformerConfig] = None):
        """Initialize decoder layer."""
        self.config = config or get_config()
        self.d_model = self.config.d_model
        self.n_heads = self.config.n_heads
        self.d_ff = self.config.d_ff
        self.dropout_rate = self.config.dropout_rate
        
        # Masked self-attention (causal attention)
        self.self_attention = MultiHeadAttention(config)
        
        # Cross-attention (encoder-decoder attention)
        self.cross_attention = MultiHeadAttention(config)
        
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
        self.norm3 = LayerNorm(self.d_model, self.config.layer_norm_epsilon)
    
    def forward(self, 
                x: np.ndarray,
                encoder_output: Optional[np.ndarray] = None,
                self_attention_mask: Optional[np.ndarray] = None,
                cross_attention_mask: Optional[np.ndarray] = None,
                training: bool = True) -> np.ndarray:
        """
        Forward pass of decoder layer.
        
        Args:
            x: Input tensor [batch_size, seq_len, d_model]
            encoder_output: Encoder output for cross-attention
            self_attention_mask: Mask for self-attention (causal mask)
            cross_attention_mask: Mask for cross-attention
            training: Whether in training mode
            
        Returns:
            Output tensor
        """
        batch_size, seq_len, d_model = x.shape
        
        # 1. Masked self-attention with residual connection
        self_attn_output = self.self_attention.forward(x, x, x, self_attention_mask)
        
        # Apply dropout during training
        if training and self.dropout_rate > 0:
            self_attn_output = self._apply_dropout(self_attn_output)
        
        # First residual connection and layer norm
        if self.config.use_layer_norm:
            x = self.norm1.forward(x + self_attn_output)
        else:
            x = x + self_attn_output
        
        # 2. Cross-attention with residual connection
        if encoder_output is not None:
            cross_attn_output = self.cross_attention.forward(
                x, encoder_output, encoder_output, cross_attention_mask
            )
            
            # Apply dropout during training
            if training and self.dropout_rate > 0:
                cross_attn_output = self._apply_dropout(cross_attn_output)
            
            # Second residual connection and layer norm
            if self.config.use_layer_norm:
                x = self.norm2.forward(x + cross_attn_output)
            else:
                x = x + cross_attn_output
        
        # 3. Feed-forward network with residual connection
        ff_output = self.feed_forward.forward(x)
        
        # Apply dropout during training
        if training and self.dropout_rate > 0:
            ff_output = self._apply_dropout(ff_output)
        
        # Third residual connection and layer norm
        if self.config.use_layer_norm:
            output = self.norm3.forward(x + ff_output)
        else:
            output = x + ff_output
        
        return output
    
    def _apply_dropout(self, x: np.ndarray) -> np.ndarray:
        """Apply dropout to tensor."""
        if self.dropout_rate > 0:
            mask = np.random.binomial(1, 1 - self.dropout_rate, x.shape) / (1 - self.dropout_rate)
            return x * mask
        return x


class TransformerDecoder:
    """
    Full Transformer Decoder.
    
    Stacks multiple decoder layers with optional positional encoding.
    """
    
    def __init__(self, config: Optional[TransformerConfig] = None):
        """Initialize transformer decoder."""
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
        
        # Decoder layers
        self.layers = [DecoderLayer(config) for _ in range(self.n_layers)]
        
        # Final layer normalization
        if self.config.layer_norm_type == "post":
            self.final_layer_norm = LayerNorm(self.d_model, self.config.layer_norm_epsilon)
        else:
            self.final_layer_norm = None
        
        # Output projection (for language modeling)
        self.output_projection = self._init_linear(self.d_model, self.vocab_size)
        
        # Dropout
        self.dropout_rate = self.config.dropout_rate
    
    def _init_embedding(self, vocab_size: int, d_model: int) -> np.ndarray:
        """Initialize token embedding."""
        limit = 1.0 / math.sqrt(vocab_size)
        return np.random.uniform(-limit, limit, (vocab_size, d_model))
    
    def _init_linear(self, in_dim: int, out_dim: int) -> np.ndarray:
        """Initialize linear layer weights."""
        # Xavier initialization
        limit = math.sqrt(6.0 / (in_dim + out_dim))
        return np.random.uniform(-limit, limit, (in_dim, out_dim))
    
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
    
    def create_causal_mask(self, seq_len: int) -> np.ndarray:
        """Create causal (triangular) mask for self-attention."""
        mask = np.triu(np.full((seq_len, seq_len), -np.inf), k=1)
        return mask
    
    def create_padding_mask(self, input_ids: np.ndarray, pad_token_id: int = 0) -> np.ndarray:
        """Create padding mask for decoder attention."""
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
    
    def forward(self, 
                input_ids: np.ndarray,
                encoder_output: Optional[np.ndarray] = None,
                attention_mask: Optional[np.ndarray] = None,
                encoder_attention_mask: Optional[np.ndarray] = None,
                training: bool = True) -> np.ndarray:
        """
        Forward pass of transformer decoder.
        
        Args:
            input_ids: Input token IDs [batch_size, seq_len]
            encoder_output: Encoder output for cross-attention
            attention_mask: Attention mask for decoder
            encoder_attention_mask: Attention mask for encoder
            training: Whether in training mode
            
        Returns:
            Decoder output [batch_size, seq_len, vocab_size] (if output_projection applied)
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
            x = x + pos_encoding  # Broadcasting
        
        # Apply dropout to embeddings
        if training and self.dropout_rate > 0:
            x = self._apply_dropout(x)
        
        # Create masks
        causal_mask = self.create_causal_mask(seq_len)
        padding_mask = self.create_padding_mask(input_ids)
        
        # Combine masks
        self_attention_mask = self.combine_masks(causal_mask, padding_mask, attention_mask)
        
        # Cross-attention mask (encoder padding mask)
        cross_attention_mask = encoder_attention_mask
        
        # Pass through decoder layers
        for layer in self.layers:
            x = layer.forward(
                x, 
                encoder_output, 
                self_attention_mask, 
                cross_attention_mask, 
                training
            )
        
        # Final layer normalization
        if self.final_layer_norm is not None:
            x = self.final_layer_norm.forward(x)
        
        # Output projection for language modeling
        output = np.matmul(x, self.output_projection)  # [batch_size, seq_len, vocab_size]
        
        return output
    
    def generate(self,
                input_ids: np.ndarray,
                encoder_output: Optional[np.ndarray] = None,
                max_length: int = 100,
                temperature: float = 1.0,
                top_k: int = 50,
                top_p: float = 0.9,
                training: bool = False) -> np.ndarray:
        """
        Generate text using the decoder.
        
        Args:
            input_ids: Starting input IDs [batch_size, seq_len]
            encoder_output: Encoder output for cross-attention
            max_length: Maximum length to generate
            temperature: Sampling temperature
            top_k: Top-k sampling parameter
            top_p: Top-p (nucleus) sampling parameter
            training: Whether in training mode
            
        Returns:
            Generated token IDs
        """
        batch_size = input_ids.shape[0]
        current_length = input_ids.shape[1]
        generated_ids = input_ids.copy()
        
        for _ in range(max_length - current_length):
            # Forward pass
            logits = self.forward(
                generated_ids, 
                encoder_output, 
                training=training
            )
            
            # Get next token probabilities
            next_token_logits = logits[:, -1, :] / temperature  # [batch_size, vocab_size]
            
            # Apply sampling
            next_token_probs = self._sample(next_token_logits, top_k, top_p)
            
            # Sample next token
            next_tokens = np.random.choice(
                self.vocab_size, 
                size=batch_size, 
                p=next_token_probs
            )
            
            # Append to generated sequence
            generated_ids = np.concatenate([generated_ids, next_tokens[:, None]], axis=1)
            
            # Stop if all sequences generate EOS token
            if np.all(generated_ids[:, -1] == self.config.eos_token_id):
                break
        
        return generated_ids
    
    def _sample(self, logits: np.ndarray, top_k: int = 50, top_p: float = 0.9) -> np.ndarray:
        """Apply top-k and top-p sampling to logits."""
        # Top-k filtering
        if top_k > 0:
            indices_to_remove = logits < np.topk(logits, top_k)[0][:, -1, None]
            logits[indices_to_remove] = -np.inf
        
        # Top-p (nucleus) filtering
        if top_p < 1.0:
            sorted_logits = np.sort(logits, axis=-1)[:, ::-1]
            cumulative_probs = np.cumsum(self._softmax(sorted_logits), axis=-1)
            
            # Remove tokens with cumulative probability above threshold
            sorted_indices_to_remove = cumulative_probs > top_p
            sorted_indices_to_remove[:, 1:] = sorted_indices_to_remove[:, :-1].copy()
            sorted_indices_to_remove[:, 0] = 0
            
            for i in range(logits.shape[0]):
                indices_to_remove = sorted_indices_to_remove[i]
                logits[i, indices_to_remove] = -np.inf
        
        # Convert to probabilities and sample
        probs = self._softmax(logits)
        return probs
    
    def _softmax(self, x: np.ndarray) -> np.ndarray:
        """Apply softmax function."""
        # Subtract max for numerical stability
        x_max = np.max(x, axis=-1, keepdims=True)
        exp_x = np.exp(x - x_max)
        return exp_x / np.sum(exp_x, axis=-1, keepdims=True)
    
    def _apply_dropout(self, x: np.ndarray) -> np.ndarray:
        """Apply dropout to tensor."""
        if self.dropout_rate > 0:
            mask = np.random.binomial(1, 1 - self.dropout_rate, x.shape) / (1 - self.dropout_rate)
            return x * mask
        return x
    
    def get_attention_weights(self, 
                             input_ids: np.ndarray,
                             layer_idx: int = 0,
                             attention_type: str = "self") -> np.ndarray:
        """Get attention weights from a specific layer."""
        if layer_idx >= len(self.layers):
            raise ValueError(f"Layer index {layer_idx} exceeds number of layers {len(self.layers)}")
        
        # Get embeddings (simplified)
        x = self.token_embedding[input_ids] * math.sqrt(self.d_model)
        
        if self.pos_encoding is not None:
            seq_len = input_ids.shape[1]
            pos_encoding = self.pos_encoding[:seq_len]
            x = x + pos_encoding
        
        # Get attention weights from the specified layer
        if attention_type == "self":
            attention_weights = self.layers[layer_idx].self_attention.get_attention_weights(x, x)
        elif attention_type == "cross":
            # For cross-attention, we need encoder output
            attention_weights = self.layers[layer_idx].cross_attention.get_attention_weights(x, x)
        else:
            raise ValueError(f"Unknown attention type: {attention_type}")
        
        return attention_weights
    
    def get_layer_outputs(self, 
                         input_ids: np.ndarray,
                         encoder_output: Optional[np.ndarray] = None,
                         training: bool = True) -> Dict[int, np.ndarray]:
        """Get outputs from all decoder layers."""
        batch_size, seq_len = input_ids.shape
        
        # Token embeddings
        x = self.token_embedding[input_ids] * math.sqrt(self.d_model)
        
        if self.pos_encoding is not None:
            pos_encoding = self.pos_encoding[:seq_len]
            x = x + pos_encoding
        
        if training and self.dropout_rate > 0:
            x = self._apply_dropout(x)
        
        layer_outputs = {}
        
        # Create masks
        causal_mask = self.create_causal_mask(seq_len)
        padding_mask = self.create_padding_mask(input_ids)
        self_attention_mask = self.combine_masks(causal_mask, padding_mask)
        
        for i, layer in enumerate(self.layers):
            x = layer.forward(
                x, 
                encoder_output, 
                self_attention_mask, 
                training=training
            )
            layer_outputs[i] = x.copy()
        
        if self.final_layer_norm is not None:
            x = self.final_layer_norm.forward(x)
            layer_outputs['final'] = x
        
        return layer_outputs
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get decoder statistics."""
        total_params = 0
        
        # Token embedding parameters
        total_params += self.token_embedding.size
        
        # Positional encoding parameters
        if self.pos_encoding is not None:
            total_params += self.pos_encoding.size
        
        # Output projection parameters
        total_params += self.output_projection.size
        
        # Decoder layer parameters
        for layer in self.layers:
            # Self-attention parameters
            self_stats = layer.self_attention.get_statistics()
            total_params += self_stats['total_parameters']
            
            # Cross-attention parameters
            cross_stats = layer.cross_attention.get_statistics()
            total_params += cross_stats['total_parameters']
            
            # Feed-forward parameters
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
            "activation": self.config.activation.value,
            "generation_support": True
        }
