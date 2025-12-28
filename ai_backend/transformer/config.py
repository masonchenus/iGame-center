"""
Transformer Configuration

Configuration settings for the AI transformer architecture system.
"""

import os
from typing import Dict, Any, List, Optional
from dataclasses import dataclass, field
from enum import Enum
import json


class TransformerArchitecture(Enum):
    """Transformer architecture types."""
    ENCODER_ONLY = "encoder_only"
    DECODER_ONLY = "decoder_only"
    ENCODER_DECODER = "encoder_decoder"
    GPT_STYLE = "gpt_style"
    BERT_STYLE = "bert_style"
    T5_STYLE = "t5_style"


class AttentionType(Enum):
    """Attention mechanism types."""
    FULL = "full"
    LOCAL = "local"
    STRUCTURED = "structured"
    LINEAR = "linear"
    CONVOLUTIONAL = "convolutional"


class ActivationFunction(Enum):
    """Activation functions for transformer layers."""
    RELU = "relu"
    GELU = "gelu"
    SWISH = "swish"
    LEAKY_RELU = "leaky_relu"
    ELU = "elu"


@dataclass
class TransformerConfig:
    """Configuration class for AI transformer architecture."""
    
    # Core architecture settings
    architecture: TransformerArchitecture = TransformerArchitecture.ENCODER_DECODER
    d_model: int = 512  # Model dimension
    n_heads: int = 8  # Number of attention heads
    n_layers: int = 6  # Number of transformer layers
    d_ff: int = 2048  # Feed-forward dimension
    
    # Attention settings
    attention_type: AttentionType = AttentionType.FULL
    max_seq_length: int = 512
    dropout_rate: float = 0.1
    attention_dropout: float = 0.1
    
    # Feed-forward settings
    activation: ActivationFunction = ActivationFunction.GELU
    use_bias: bool = True
    layer_norm_epsilon: float = 1e-6
    
    # Embedding settings
    vocab_size: int = 50000
    pad_token_id: int = 0
    bos_token_id: int = 1
    eos_token_id: int = 2
    unk_token_id: int = 3
    
    # Position encoding settings
    use_positional_encoding: bool = True
    positional_encoding_type: str = "sinusoidal"  # "sinusoidal" or "learned"
    max_position_embeddings: int = 512
    
    # Layer normalization settings
    use_layer_norm: bool = True
    layer_norm_type: str = "pre"  # "pre" or "post"
    
    # Regularization settings
    use_residual_connections: bool = True
    use_dropout: bool = True
    use_weight_tying: bool = False
    
    # Optimization settings
    learning_rate: float = 1e-4
    weight_decay: float = 0.01
    adam_beta1: float = 0.9
    adam_beta2: float = 0.999
    adam_epsilon: float = 1e-8
    
    # Training settings
    batch_size: int = 32
    max_epochs: int = 100
    gradient_clip_norm: float = 1.0
    warmup_steps: int = 4000
    
    # Inference settings
    temperature: float = 1.0
    top_k: int = 50
    top_p: float = 0.9
    repetition_penalty: float = 1.0
    
    # Memory and performance settings
    use_mixed_precision: bool = False
    gradient_checkpointing: bool = False
    compile_model: bool = False
    parallel_layers: bool = False
    
    # Model saving and loading
    save_every_n_epochs: int = 10
    save_best_only: bool = False
    load_pretrained: bool = False
    pretrained_model_path: Optional[str] = None
    
    # Logging and monitoring
    log_level: str = "INFO"
    log_every_n_steps: int = 100
    save_metrics_history: bool = True
    track_memory_usage: bool = True
    
    # Advanced settings
    use_alibi: bool = False  # ALiBi position encoding
    use_rotary: bool = False  # RoPE position encoding
    use_gated_linear_units: bool = False  # GLU variants
    use_parallel_attention: bool = False  # Parallel attention computation
    
    # Multi-modal settings (for future expansion)
    multimodal: bool = False
    image_embedding_dim: Optional[int] = None
    audio_embedding_dim: Optional[int] = None
    
    @classmethod
    def from_dict(cls, config_dict: Dict[str, Any]) -> 'TransformerConfig':
        """Create config from dictionary."""
        # Convert string enums if needed
        if 'architecture' in config_dict and isinstance(config_dict['architecture'], str):
            config_dict['architecture'] = TransformerArchitecture(config_dict['architecture'])
        
        if 'attention_type' in config_dict and isinstance(config_dict['attention_type'], str):
            config_dict['attention_type'] = AttentionType(config_dict['attention_type'])
        
        if 'activation' in config_dict and isinstance(config_dict['activation'], str):
            config_dict['activation'] = ActivationFunction(config_dict['activation'])
        
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
    def get_default_config(cls) -> 'TransformerConfig':
        """Get default configuration."""
        return cls()
    
    @classmethod
    def get_gpt_config(cls, vocab_size: int = 50257, n_layers: int = 12) -> 'TransformerConfig':
        """Get GPT-style configuration."""
        return cls(
            architecture=TransformerArchitecture.DECODER_ONLY,
            d_model=768,
            n_heads=12,
            n_layers=n_layers,
            d_ff=3072,
            vocab_size=vocab_size,
            use_positional_encoding=True,
            positional_encoding_type="learned",
            activation=ActivationFunction.GELU,
            dropout_rate=0.1,
            max_seq_length=1024
        )
    
    @classmethod
    def get_bert_config(cls, vocab_size: int = 30000, n_layers: int = 12) -> 'TransformerConfig':
        """Get BERT-style configuration."""
        return cls(
            architecture=TransformerArchitecture.ENCODER_ONLY,
            d_model=768,
            n_heads=12,
            n_layers=n_layers,
            d_ff=3072,
            vocab_size=vocab_size,
            use_positional_encoding=True,
            positional_encoding_type="sinusoidal",
            activation=ActivationFunction.GELU,
            dropout_rate=0.1,
            layer_norm_type="pre"
        )
    
    @classmethod
    def get_t5_config(cls, vocab_size: int = 32128, n_layers: int = 12) -> 'TransformerConfig':
        """Get T5-style configuration."""
        return cls(
            architecture=TransformerArchitecture.ENCODER_DECODER,
            d_model=768,
            n_heads=12,
            n_layers=n_layers,
            d_ff=3072,
            vocab_size=vocab_size,
            use_positional_encoding=True,
            positional_encoding_type="relative",
            activation=ActivationFunction.RELU,
            dropout_rate=0.1,
            layer_norm_type="post"
        )
    
    @classmethod
    def get_small_config(cls) -> 'TransformerConfig':
        """Get small configuration for testing."""
        return cls(
            architecture=TransformerArchitecture.ENCODER_ONLY,
            d_model=256,
            n_heads=4,
            n_layers=3,
            d_ff=1024,
            vocab_size=10000,
            max_seq_length=256,
            batch_size=16
        )
    
    @classmethod
    def get_large_config(cls) -> 'TransformerConfig':
        """Get large configuration for production."""
        return cls(
            architecture=TransformerArchitecture.ENCODER_DECODER,
            d_model=1024,
            n_heads=16,
            n_layers=12,
            d_ff=4096,
            vocab_size=50000,
            max_seq_length=2048,
            batch_size=64,
            use_mixed_precision=True
        )
    
    def validate(self) -> List[str]:
        """Validate configuration and return list of warnings/errors."""
        warnings = []
        
        # Check model dimension and attention heads compatibility
        if self.d_model % self.n_heads != 0:
            warnings.append(f"d_model ({self.d_model}) should be divisible by n_heads ({self.n_heads})")
        
        # Check feed-forward dimension
        if self.d_ff <= self.d_model:
            warnings.append(f"d_ff ({self.d_ff}) should be larger than d_model ({self.d_model})")
        
        # Check dropout rates
        if not 0 <= self.dropout_rate <= 1:
            warnings.append(f"dropout_rate ({self.dropout_rate}) should be between 0 and 1")
        
        if not 0 <= self.attention_dropout <= 1:
            warnings.append(f"attention_dropout ({self.attention_dropout}) should be between 0 and 1")
        
        # Check sequence length limits
        if self.max_seq_length > self.max_position_embeddings:
            warnings.append(f"max_seq_length ({self.max_seq_length}) exceeds max_position_embeddings ({self.max_position_embeddings})")
        
        # Check vocabulary size
        if self.vocab_size < 100:
            warnings.append(f"vocab_size ({self.vocab_size}) seems too small for practical use")
        
        # Check learning rate
        if self.learning_rate <= 0:
            warnings.append(f"learning_rate ({self.learning_rate}) should be positive")
        
        # Check batch size
        if self.batch_size < 1:
            warnings.append(f"batch_size ({self.batch_size}) should be at least 1")
        
        # Check warmup steps
        if self.warmup_steps < 0:
            warnings.append(f"warmup_steps ({self.warmup_steps}) should be non-negative")
        
        return warnings
    
    def get_model_size_estimate(self) -> Dict[str, Any]:
        """Get estimate of model size and parameters."""
        # Estimate number of parameters
        # This is a simplified calculation
        
        # Embedding parameters
        embedding_params = self.vocab_size * self.d_model
        
        # Position encoding parameters
        pos_encoding_params = 0
        if self.use_positional_encoding:
            if self.positional_encoding_type == "learned":
                pos_encoding_params = self.max_position_embeddings * self.d_model
        
        # Transformer layer parameters
        # Each layer has: attention + feed-forward
        attention_params_per_layer = (
            3 * self.d_model * self.d_model +  # Q, K, V projections
            self.n_heads * (self.d_model // self.n_heads) * self.d_model +  # Output projection
            2 * self.d_model  # Layer norm parameters
        )
        
        feed_forward_params_per_layer = (
            2 * self.d_model * self.d_ff +  # Linear layers
            2 * self.d_model  # Layer norm parameters
        )
        
        layer_params = self.n_layers * (attention_params_per_layer + feed_forward_params_per_layer)
        
        # Total parameters
        total_params = embedding_params + pos_encoding_params + layer_params
        
        # Memory estimates (rough)
        param_memory_mb = total_params * 4 / (1024 * 1024)  # 4 bytes per float32
        
        # Forward pass memory (rough estimate)
        seq_memory_mb = self.d_model * self.max_seq_length * 4 / (1024 * 1024)
        
        return {
            "total_parameters": total_params,
            "embedding_parameters": embedding_params,
            "position_encoding_parameters": pos_encoding_params,
            "layer_parameters": layer_params,
            "parameter_memory_mb": param_memory_mb,
            "sequence_memory_mb": seq_memory_mb,
            "estimated_total_memory_mb": param_memory_mb + seq_memory_mb
        }


# Global configuration instance
_config = None


def get_config() -> TransformerConfig:
    """Get the global transformer configuration."""
    global _config
    if _config is None:
        _config = TransformerConfig.get_default_config()
    return _config


def set_config(config: TransformerConfig) -> None:
    """Set the global transformer configuration."""
    global _config
    _config = config


def reset_config() -> None:
    """Reset configuration to default."""
    global _config
    _config = TransformerConfig.get_default_config()


def load_config_from_file(config_path: str) -> TransformerConfig:
    """Load configuration from JSON file."""
    try:
        with open(config_path, 'r', encoding='utf-8') as f:
            config_dict = json.load(f)
        return TransformerConfig.from_dict(config_dict)
    except Exception as e:
        raise ValueError(f"Failed to load config from {config_path}: {str(e)}")


def save_config_to_file(config: TransformerConfig, config_path: str) -> None:
    """Save configuration to JSON file."""
    try:
        with open(config_path, 'w', encoding='utf-8') as f:
            json.dump(config.to_dict(), f, indent=2, ensure_ascii=False)
    except Exception as e:
        raise ValueError(f"Failed to save config to {config_path}: {str(e)}")
