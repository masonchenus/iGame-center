"""
Advanced Parameter Manager for AI Models
Handles w and b parameters in y = w*x + b formula with fine-tuning capabilities
"""
import json
import os
import numpy as np
from typing import Dict, List, Tuple, Optional, Any
from dataclasses import dataclass, asdict
from ai_backend.virtual_array import VirtualLargeArray
import hashlib
import logging

@dataclass
class ParameterConfig:
    """Configuration for AI parameters"""
    w_learning_rate: float = 0.001
    b_learning_rate: float = 0.001
    w_regularization: float = 0.01
    b_regularization: float = 0.01
    parameter_decay: float = 0.999
    adaptive_lr: bool = True
    min_lr: float = 0.0001
    max_lr: float = 0.1
    tuning_enabled: bool = True

@dataclass
class ParameterState:
    """Current state of parameters"""
    w_current: List[float]
    b_current: List[float]
    w_gradients: List[float]
    b_gradients: List[float]
    training_steps: int
    last_update: str
    performance_score: float

class ParameterManager:
    """Manages w and b parameters for AI models with advanced tuning capabilities"""
    
    def __init__(self, model_name: str = "nexus", config: ParameterConfig = None):
        self.model_name = model_name
        self.config = config or ParameterConfig()
        self.logger = logging.getLogger(__name__)
        
        # Initialize virtual arrays for massive parameter storage
        self.w_virtual_array = VirtualLargeArray(148_000_000_000_000, seed=f"{model_name}_w_v2")
        self.b_virtual_array = VirtualLargeArray(148_000_000_000_000, seed=f"{model_name}_b_v2")
        
        # Active parameters (subset of virtual arrays for performance)
        self.active_w_size = 10000  # Active parameters for computation
        self.active_b_size = 10000
        
        self.state_file = f"ai_backend/models/config/{model_name}_parameters.json"
        self.checkpoint_dir = f"ai_backend/models/checkpoints/{model_name}"
        
        # Initialize state
        self.state = self._load_state()
        
        # Performance tracking
        self.performance_history = []
        self.tuning_history = []
        
    def _load_state(self) -> ParameterState:
        """Load parameter state from file or create new"""
        try:
            if os.path.exists(self.state_file):
                with open(self.state_file, 'r') as f:
                    data = json.load(f)
                return ParameterState(**data)
        except Exception as e:
            self.logger.warning(f"Could not load state: {e}")
            
        # Create new state
        return ParameterState(
            w_current=self._initialize_parameters(self.active_w_size),
            b_current=self._initialize_parameters(self.active_b_size),
            w_gradients=[0.0] * self.active_w_size,
            b_gradients=[0.0] * self.active_b_size,
            training_steps=0,
            last_update="",
            performance_score=0.0
        )
    
    def _initialize_parameters(self, size: int) -> List[float]:
        """Initialize parameters with smart seeding based on model name"""
        seed = hashlib.md5(self.model_name.encode()).digest()
        np.random.seed(int.from_bytes(seed[:4], 'big'))
        return np.random.normal(0, 0.1, size).tolist()
    
    def _save_state(self):
        """Save current parameter state"""
        try:
            os.makedirs(os.path.dirname(self.state_file), exist_ok=True)
            with open(self.state_file, 'w') as f:
                json.dump(asdict(self.state), f, indent=2)
        except Exception as e:
            self.logger.error(f"Could not save state: {e}")
    
    def get_parameters(self, x_values: List[float]) -> Tuple[List[float], float]:
        """
        Get w and b parameters for given x values using y = w*x + b
        
        Args:
            x_values: Input values for which to compute parameters
            
        Returns:
            Tuple of (computed_y_values, computed_b)
        """
        if not x_values:
            return [], 0.0
            
        # Use virtual arrays to get w and b values
        w_sample = self.w_virtual_array.sample(len(x_values), start=0)
        b_sample = self.b_virtual_array.sample(1, start=0)[0]
        
        # Compute y = w*x + b
        y_values = [w * x + b_sample for w, x in zip(w_sample, x_values)]
        
        return y_values, b_sample
    
    def update_parameters(self, x_values: List[float], y_targets: List[float], 
                         learning_rate: Optional[float] = None) -> Dict[str, float]:
        """
        Update parameters using gradient descent
        
        Args:
            x_values: Input values
            y_targets: Target outputs
            learning_rate: Learning rate (uses config if None)
            
        Returns:
            Dictionary with update metrics
        """
        if not x_values or not y_targets or len(x_values) != len(y_targets):
            return {"error": "Invalid input data"}
            
        lr_w = learning_rate or self.config.w_learning_rate
        lr_b = learning_rate or self.config.b_learning_rate
        
        # Get current predictions
        y_pred, b_current = self.get_parameters(x_values)
        
        # Compute gradients
        errors = [pred - target for pred, target in zip(y_pred, y_targets)]
        mse = sum(e**2 for e in errors) / len(errors)
        
        # Update gradients (simplified)
        w_grad = sum(e * x for e, x in zip(errors, x_values)) / len(errors)
        b_grad = sum(errors) / len(errors)
        
        # Update parameters
        for i in range(min(len(self.state.w_current), len(x_values))):
            if i < len(self.state.w_gradients):
                self.state.w_gradients[i] = w_grad
                self.state.w_current[i] -= lr_w * w_grad
                
        for i in range(len(self.state.b_gradients)):
            self.state.b_gradients[i] = b_grad
            self.state.b_current[i] -= lr_b * b_grad
        
        # Update state
        self.state.training_steps += 1
        self.state.performance_score = 1.0 / (1.0 + mse)  # Higher is better
        self.state.last_update = str(np.datetime64('now'))
        
        # Save state
        self._save_state()
        
        return {
            "mse": mse,
            "w_gradient": w_grad,
            "b_gradient": b_grad,
            "training_steps": self.state.training_steps,
            "performance_score": self.state.performance_score
        }
    
    def fine_tune_parameters(self, tuning_data: List[Dict[str, Any]]) -> Dict[str, Any]:
        """
        Fine-tune parameters using provided tuning data
        
        Args:
            tuning_data: List of dictionaries with 'x' and 'y' keys
            
        Returns:
            Tuning results and metrics
        """
        if not tuning_data:
            return {"error": "No tuning data provided"}
            
        x_values = [item['x'] for item in tuning_data]
        y_values = [item['y'] for item in tuning_data]
        
        results = []
        for i in range(10):  # Fine-tune for 10 iterations
            result = self.update_parameters(x_values, y_values)
            results.append(result)
            
        # Calculate improvement
        initial_mse = results[0]['mse']
        final_mse = results[-1]['mse']
        improvement = (initial_mse - final_mse) / initial_mse if initial_mse > 0 else 0
        
        tuning_result = {
            "initial_mse": initial_mse,
            "final_mse": final_mse,
            "improvement": improvement,
            "iterations": len(results),
            "final_performance": results[-1]['performance_score']
        }
        
        # Save tuning history
        self.tuning_history.append({
            "timestamp": str(np.datetime64('now')),
            "data_points": len(tuning_data),
            **tuning_result
        })
        
        return tuning_result
    
    def get_parameter_stats(self) -> Dict[str, Any]:
        """Get comprehensive parameter statistics"""
        w_sample = self.w_virtual_array.sample(100, start=0)
        b_sample = self.b_virtual_array.sample(100, start=0)
        
        return {
            "model_name": self.model_name,
            "virtual_array_sizes": {
                "w": len(self.w_virtual_array),
                "b": len(self.b_virtual_array)
            },
            "active_parameters": {
                "w_size": len(self.state.w_current),
                "b_size": len(self.state.b_current)
            },
            "statistics": {
                "w_mean": np.mean(w_sample),
                "w_std": np.std(w_sample),
                "b_mean": np.mean(b_sample),
                "b_std": np.std(b_sample)
            },
            "training": {
                "steps": self.state.training_steps,
                "performance_score": self.state.performance_score,
                "last_update": self.state.last_update
            },
            "tuning_history": len(self.tuning_history),
            "config": asdict(self.config)
        }
    
    def reset_parameters(self):
        """Reset parameters to initial state"""
        self.state = ParameterState(
            w_current=self._initialize_parameters(self.active_w_size),
            b_current=self._initialize_parameters(self.active_b_size),
            w_gradients=[0.0] * self.active_w_size,
            b_gradients=[0.0] * self.active_b_size,
            training_steps=0,
            last_update="",
            performance_score=0.0
        )
        self._save_state()
        self.logger.info(f"Parameters reset for model {self.model_name}")
    
    def export_parameters(self, filepath: str):
        """Export current parameters to file"""
        export_data = {
            "model_name": self.model_name,
            "timestamp": str(np.datetime64('now')),
            "state": asdict(self.state),
            "config": asdict(self.config),
            "statistics": self.get_parameter_stats()
        }
        
        with open(filepath, 'w') as f:
            json.dump(export_data, f, indent=2)
            
        self.logger.info(f"Parameters exported to {filepath}")
    
    def import_parameters(self, filepath: str) -> bool:
        """Import parameters from file"""
        try:
            with open(filepath, 'r') as f:
                data = json.load(f)
                
            if "state" in data:
                self.state = ParameterState(**data["state"])
            if "config" in data:
                self.config = ParameterConfig(**data["config"])
                
            self._save_state()
            self.logger.info(f"Parameters imported from {filepath}")
            return True
            
        except Exception as e:
            self.logger.error(f"Failed to import parameters: {e}")
            return False
