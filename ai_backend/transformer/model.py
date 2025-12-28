"""
Full Transformer Model for AI Backend

Combines encoder, decoder, and other components into a complete transformer model.
"""

import math
import numpy as np
from typing import Optional, Dict, Any, Tuple, List
from .encoder import TransformerEncoder
from .decoder import TransformerDecoder
from .config import TransformerConfig, get_config, TransformerArchitecture
from ..tokenizer import TokenizerConfig, get_tokenizer_config


class TransformerModel:
    """
    Complete Transformer Model.
    
    Supports multiple architectures:
    - Encoder-only (BERT-style)
    - Decoder-only (GPT-style) 
    - Encoder-Decoder (T5-style)
    """
    
    def __init__(self, config: Optional[TransformerConfig] = None):
        """Initialize the transformer model."""
        self.config = config or get_config()
        self.architecture = self.config.architecture
        
        # Initialize model components based on architecture
        if self.architecture in [TransformerArchitecture.ENCODER_ONLY, TransformerArchitecture.BERT_STYLE]:
            self._init_encoder_only()
        elif self.architecture in [TransformerArchitecture.DECODER_ONLY, TransformerArchitecture.GPT_STYLE]:
            self._init_decoder_only()
        elif self.architecture in [TransformerArchitecture.ENCODER_DECODER, TransformerArchitecture.T5_STYLE]:
            self._init_encoder_decoder()
        else:
            raise ValueError(f"Unsupported architecture: {self.architecture}")
        
        # Initialize tokenizer configuration
        self.tokenizer_config = get_tokenizer_config()
        
        # Model state
        self.is_training = False
        self._statistics = {}
    
    def _init_encoder_only(self) -> None:
        """Initialize encoder-only model (BERT-style)."""
        self.encoder = TransformerEncoder(self.config)
        self.decoder = None
        
        # Language model head for BERT
        if self.architecture == TransformerArchitecture.BERT_STYLE:
            self.lm_head = self._init_linear(self.config.d_model, self.config.vocab_size)
        else:
            self.lm_head = None
    
    def _init_decoder_only(self) -> None:
        """Initialize decoder-only model (GPT-style)."""
        self.encoder = None
        self.decoder = TransformerDecoder(self.config)
        
        # Language model head (usually built into decoder)
        if self.architecture == TransformerArchitecture.DECODER_ONLY:
            self.lm_head = None  # Already in decoder
        else:
            self.lm_head = None
    
    def _init_encoder_decoder(self) -> None:
        """Initialize encoder-decoder model (T5-style)."""
        self.encoder = TransformerEncoder(self.config)
        self.decoder = TransformerDecoder(self.config)
        
        # No separate LM head needed (decoder has its own)
        self.lm_head = None
    
    def _init_linear(self, in_dim: int, out_dim: int) -> np.ndarray:
        """Initialize linear layer weights."""
        # Xavier initialization
        limit = math.sqrt(6.0 / (in_dim + out_dim))
        return np.random.uniform(-limit, limit, (in_dim, out_dim))
    
    def forward(self, 
                input_ids: np.ndarray,
                decoder_input_ids: Optional[np.ndarray] = None,
                attention_mask: Optional[np.ndarray] = None,
                decoder_attention_mask: Optional[np.ndarray] = None,
                encoder_attention_mask: Optional[np.ndarray] = None,
                training: bool = True) -> Dict[str, np.ndarray]:
        """
        Forward pass of the transformer model.
        
        Args:
            input_ids: Input token IDs
            decoder_input_ids: Decoder input IDs (for encoder-decoder models)
            attention_mask: Attention mask for inputs
            decoder_attention_mask: Attention mask for decoder
            encoder_attention_mask: Attention mask for encoder
            training: Whether in training mode
            
        Returns:
            Dictionary containing model outputs
        """
        self.is_training = training
        
        outputs = {}
        
        if self.architecture in [TransformerArchitecture.ENCODER_ONLY, TransformerArchitecture.BERT_STYLE]:
            # Encoder-only models (BERT)
            encoder_output = self.encoder.forward(
                input_ids, 
                attention_mask, 
                training
            )
            outputs['encoder_output'] = encoder_output
            
            # Apply language model head if needed
            if self.lm_head is not None:
                logits = np.matmul(encoder_output, self.lm_head)
                outputs['logits'] = logits
                
                # For BERT, we need to mask padding positions
                if attention_mask is not None:
                    # Create mask for valid positions
                    mask = attention_mask.unsqueeze(-1)  # [batch_size, seq_len, 1]
                    masked_logits = np.where(mask == 0, -10000.0, logits)
                    outputs['logits'] = masked_logits
            
            # Return sequence output for BERT
            outputs['sequence_output'] = encoder_output
            
        elif self.architecture in [TransformerArchitecture.DECODER_ONLY, TransformerArchitecture.GPT_STYLE]:
            # Decoder-only models (GPT)
            if decoder_input_ids is None:
                decoder_input_ids = input_ids
            
            logits = self.decoder.forward(
                decoder_input_ids,
                encoder_output=None,
                attention_mask=decoder_attention_mask,
                training=training
            )
            outputs['logits'] = logits
            
        elif self.architecture in [TransformerArchitecture.ENCODER_DECODER, TransformerArchitecture.T5_STYLE]:
            # Encoder-decoder models (T5)
            if decoder_input_ids is None:
                raise ValueError("decoder_input_ids required for encoder-decoder models")
            
            # Encoder forward pass
            encoder_output = self.encoder.forward(
                input_ids,
                encoder_attention_mask,
                training
            )
            outputs['encoder_output'] = encoder_output
            
            # Decoder forward pass
            logits = self.decoder.forward(
                decoder_input_ids,
                encoder_output=encoder_output,
                attention_mask=decoder_attention_mask,
                encoder_attention_mask=encoder_attention_mask,
                training=training
            )
            outputs['logits'] = logits
        
        return outputs
    
    def generate(self,
                input_ids: np.ndarray,
                decoder_start_token_id: Optional[int] = None,
                max_length: int = 100,
                temperature: float = 1.0,
                top_k: int = 50,
                top_p: float = 0.9,
                repetition_penalty: float = 1.0,
                pad_token_id: Optional[int] = None) -> np.ndarray:
        """
        Generate text using the transformer model.
        
        Args:
            input_ids: Input token IDs
            decoder_start_token_id: Start token for decoder
            max_length: Maximum generation length
            temperature: Sampling temperature
            top_k: Top-k sampling parameter
            top_p: Top-p (nucleus) sampling parameter
            repetition_penalty: Repetition penalty
            pad_token_id: Padding token ID
            
        Returns:
            Generated token IDs
        """
        if self.architecture in [TransformerArchitecture.ENCODER_ONLY, TransformerArchitecture.BERT_STYLE]:
            raise ValueError("Generation not supported for encoder-only models")
        
        if self.architecture in [TransformerArchitecture.DECODER_ONLY, TransformerArchitecture.GPT_STYLE]:
            # Decoder-only generation (GPT-style)
            if decoder_start_token_id is None:
                decoder_start_token_id = self.config.bos_token_id
            
            # Start with start token
            batch_size = input_ids.shape[0]
            generated_ids = np.full(
                (batch_size, 1), 
                decoder_start_token_id, 
                dtype=np.int32
            )
            
            # Generate autoregressively
            for _ in range(max_length - 1):
                # Forward pass
                outputs = self.forward(
                    generated_ids,
                    training=False
                )
                logits = outputs['logits'][:, -1, :]  # Last position
                
                # Apply temperature
                logits = logits / temperature
                
                # Apply repetition penalty
                if repetition_penalty != 1.0:
                    logits = self._apply_repetition_penalty(
                        logits, generated_ids, repetition_penalty
                    )
                
                # Apply sampling
                next_token_probs = self._sample(logits, top_k, top_p)
                
                # Sample next token
                next_tokens = np.random.choice(
                    self.config.vocab_size,
                    size=batch_size,
                    p=next_token_probs
                )
                
                # Check for EOS token
                if pad_token_id is not None:
                    eos_mask = next_tokens == self.config.eos_token_id
                    if np.all(eos_mask):
                        break
                
                # Append to generated sequence
                generated_ids = np.concatenate([generated_ids, next_tokens[:, None]], axis=1)
            
            return generated_ids
            
        elif self.architecture in [TransformerArchitecture.ENCODER_DECODER, TransformerArchitecture.T5_STYLE]:
            # Encoder-decoder generation (T5-style)
            if decoder_start_token_id is None:
                decoder_start_token_id = self.config.decoder_start_token_id or self.config.bos_token_id
            
            batch_size = input_ids.shape[0]
            generated_ids = np.full(
                (batch_size, 1),
                decoder_start_token_id,
                dtype=np.int32
            )
            
            # Generate autoregressively
            for _ in range(max_length - 1):
                # Forward pass
                outputs = self.forward(
                    input_ids,
                    decoder_input_ids=generated_ids,
                    training=False
                )
                logits = outputs['logits'][:, -1, :]
                
                # Apply sampling and temperature
                logits = logits / temperature
                next_token_probs = self._sample(logits, top_k, top_p)
                
                # Sample next token
                next_tokens = np.random.choice(
                    self.config.vocab_size,
                    size=batch_size,
                    p=next_token_probs
                )
                
                # Check for EOS token
                if np.all(next_tokens == self.config.eos_token_id):
                    break
                
                # Append to generated sequence
                generated_ids = np.concatenate([generated_ids, next_tokens[:, None]], axis=1)
            
            return generated_ids
    
    def _apply_repetition_penalty(self, 
                                 logits: np.ndarray, 
                                 generated_ids: np.ndarray, 
                                 penalty: float) -> np.ndarray:
        """Apply repetition penalty to logits."""
        for i in range(len(generated_ids[0])):
            for token_id in set(generated_ids[:, i]):
                if logits[0, token_id] < 0:
                    logits[0, token_id] *= penalty
                else:
                    logits[0, token_id] /= penalty
        return logits
    
    def _sample(self, logits: np.ndarray, top_k: int = 50, top_p: float = 0.9) -> np.ndarray:
        """Apply top-k and top-p sampling to logits."""
        batch_size = logits.shape[0]
        
        # Top-k filtering
        if top_k > 0:
            top_k = min(top_k, logits.shape[-1])
            indices_to_remove = logits < np.topk(logits, top_k)[0][:, -1, None]
            logits = np.where(indices_to_remove, -np.inf, logits)
        
        # Top-p (nucleus) filtering
        if top_p < 1.0:
            sorted_logits = np.sort(logits, axis=-1)[:, ::-1]
            cumulative_probs = np.cumsum(self._softmax(sorted_logits), axis=-1)
            
            # Remove tokens with cumulative probability above threshold
            sorted_indices_to_remove = cumulative_probs > top_p
            sorted_indices_to_remove[:, 1:] = sorted_indices_to_remove[:, :-1].copy()
            sorted_indices_to_remove[:, 0] = 0
            
            for i in range(batch_size):
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
    
    def get_attention_weights(self, 
                             input_ids: np.ndarray,
                             layer_idx: int = 0,
                             attention_type: str = "self") -> np.ndarray:
        """Get attention weights from a specific layer."""
        if self.architecture in [TransformerArchitecture.ENCODER_ONLY, TransformerArchitecture.BERT_STYLE]:
            return self.encoder.get_attention_weights(input_ids, layer_idx)
        elif self.architecture in [TransformerArchitecture.DECODER_ONLY, TransformerArchitecture.GPT_STYLE]:
            return self.decoder.get_attention_weights(input_ids, layer_idx, attention_type)
        elif self.architecture in [TransformerArchitecture.ENCODER_DECODER, TransformerArchitecture.T5_STYLE]:
            if attention_type == "self":
                return self.decoder.get_attention_weights(input_ids, layer_idx, "self")
            elif attention_type == "cross":
                return self.decoder.get_attention_weights(input_ids, layer_idx, "cross")
            else:
                raise ValueError(f"Unknown attention type: {attention_type}")
    
    def get_model_info(self) -> Dict[str, Any]:
        """Get comprehensive model information."""
        info = {
            "architecture": self.config.architecture.value,
            "config": self.config.to_dict(),
            "training_mode": self.is_training
        }
        
        # Component information
        if self.encoder is not None:
            info["encoder"] = self.encoder.get_statistics()
        
        if self.decoder is not None:
            info["decoder"] = self.decoder.get_statistics()
        
        # Parameter count
        total_params = self._count_parameters()
        info["total_parameters"] = total_params
        
        # Model size estimate
        model_size = self.config.get_model_size_estimate()
        info["model_size_estimate"] = model_size
        
        return info
    
    def _count_parameters(self) -> int:
        """Count total parameters in the model."""
        total_params = 0
        
        if self.encoder is not None:
            encoder_stats = self.encoder.get_statistics()
            total_params += encoder_stats.get("total_parameters", 0)
        
        if self.decoder is not None:
            decoder_stats = self.decoder.get_statistics()
            total_params += decoder_stats.get("total_parameters", 0)
        
        if self.lm_head is not None:
            total_params += self.lm_head.size
        
        return total_params
    
    def save_model(self, path: str) -> None:
        """Save model weights and configuration."""
        import pickle
        
        model_data = {
            "config": self.config,
            "state_dict": self._get_state_dict(),
            "tokenizer_config": self.tokenizer_config
        }
        
        with open(path, 'wb') as f:
            pickle.dump(model_data, f)
    
    def load_model(self, path: str) -> None:
        """Load model weights and configuration."""
        import pickle
        
        with open(path, 'rb') as f:
            model_data = pickle.load(f)
        
        self.config = model_data["config"]
        self._load_state_dict(model_data["state_dict"])
        self.tokenizer_config = model_data.get("tokenizer_config")
        
        # Reinitialize components with new config
        self.__init__(self.config)
    
    def _get_state_dict(self) -> Dict[str, Any]:
        """Get model state dictionary."""
        state_dict = {}
        
        if self.encoder is not None:
            # Save encoder weights
            state_dict["encoder.token_embedding"] = self.encoder.token_embedding
            if self.encoder.pos_encoding is not None:
                state_dict["encoder.pos_encoding"] = self.encoder.pos_encoding
            # Save layer weights (simplified)
        
        if self.decoder is not None:
            # Save decoder weights
            state_dict["decoder.token_embedding"] = self.decoder.token_embedding
            if self.decoder.pos_encoding is not None:
                state_dict["decoder.pos_encoding"] = self.decoder.pos_encoding
            state_dict["decoder.output_projection"] = self.decoder.output_projection
        
        if self.lm_head is not None:
            state_dict["lm_head"] = self.lm_head
        
        return state_dict
    
    def _load_state_dict(self, state_dict: Dict[str, Any]) -> None:
        """Load model state dictionary."""
        # This would load weights back into the model
        # Implementation depends on how weights are stored
        pass
    
    @classmethod
    def from_pretrained(cls, model_name: str) -> 'TransformerModel':
        """Load a pretrained model."""
        # This would download and load a pretrained model
        # For now, return a default model
        config = TransformerConfig.get_default_config()
        return cls(config)
    
    def train(self) -> None:
        """Set model to training mode."""
        self.is_training = True
    
    def eval(self) -> None:
        """Set model to evaluation mode."""
        self.is_training = False


# Utility functions
def create_model_from_config(config: TransformerConfig) -> TransformerModel:
    """Create a transformer model from configuration."""
    return TransformerModel(config)


def create_gpt_model(vocab_size: int = 50257, n_layers: int = 12) -> TransformerModel:
    """Create a GPT-style model."""
    config = TransformerConfig.get_gpt_config(vocab_size, n_layers)
    return TransformerModel(config)


def create_bert_model(vocab_size: int = 30000, n_layers: int = 12) -> TransformerModel:
    """Create a BERT-style model."""
    config = TransformerConfig.get_bert_config(vocab_size, n_layers)
    return TransformerModel(config)


def create_t5_model(vocab_size: int = 32128, n_layers: int = 12) -> TransformerModel:
    """Create a T5-style model."""
    config = TransformerConfig.get_t5_config(vocab_size, n_layers)
    return TransformerModel(config)
