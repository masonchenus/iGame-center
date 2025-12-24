"""
Enhanced AI Model Trainer
Provides advanced training capabilities for AI models with parameter optimization
"""
import json
import os
import numpy as np
from typing import Dict, List, Tuple, Optional, Any, Callable
from dataclasses import dataclass, asdict
from datetime import datetime
import logging
from ai_backend.models.enhanced.parameter_manager import ParameterManager, ParameterConfig
from ai_backend.virtual_array import VirtualLargeArray

@dataclass
class TrainingConfig:
    """Configuration for training process"""
    epochs: int = 100
    batch_size: int = 32
    validation_split: float = 0.2
    learning_rate: float = 0.001
    early_stopping: bool = True
    patience: int = 10
    save_checkpoints: bool = True
    checkpoint_frequency: int = 10
    verbose: bool = True

@dataclass
class TrainingMetrics:
    """Metrics tracked during training"""
    epoch: int
    train_loss: float
    val_loss: float
    train_accuracy: float
    val_accuracy: float
    learning_rate: 50
    timestamp: str

class ModelTrainer:
    """Enhanced model trainer with comprehensive training capabilities"""
    
    def __init__(self, model_name: str = "nexus", config: TrainingConfig = None):
        self.model_name = model_name
        self.config = config or TrainingConfig()
        self.logger = logging.getLogger(__name__)
        
        # Initialize parameter manager
        self.parameter_manager = ParameterManager(model_name)
        
        # Training state
        self.current_epoch = 0
        self.best_val_loss = float('inf')
        self.training_history = []
        self.checkpoint_dir = f"ai_backend/models/training/checkpoints/{model_name}"
        
        # Ensure checkpoint directory exists
        os.makedirs(self.checkpoint_dir, exist_ok=True)
        
    def prepare_training_data(self, data: List[Dict[str, Any]]) -> Tuple[List[float], List[float]]:
        """
        Prepare training data from various formats
        
        Args:
            data: List of dictionaries with 'x' and 'y' keys, or raw x,y pairs
            
        Returns:
            Tuple of (x_values, y_values)
        """
        if not data:
            return [], []
            
        # Handle different data formats
        if isinstance(data[0], dict):
            # Dictionary format: {'x': value, 'y': value}
            x_values = [item.get('x', 0) for item in data]
            y_values = [item.get('y', 0) for item in data]
        elif isinstance(data[0], (list, tuple)) and len(data[0]) >= 2:
            # List/tuple format: [x, y] or (x, y)
            x_values = [item[0] for item in data]
            y_values = [item[1] for item in data]
        else:
            raise ValueError("Unsupported data format. Use [{'x': val, 'y': val}] or [[x,y]]")
            
        return x_values, y_values
    
    def split_data(self, x_values: List[float], y_values: List[float]) -> Tuple:
        """Split data into training and validation sets"""
        if len(x_values) != len(y_values):
            raise ValueError("x_values and y_values must have same length")
            
        n_samples = len(x_values)
        split_idx = int(n_samples * (1 - self.config.validation_split))
        
        # Shuffle indices for random split
        indices = np.random.permutation(n_samples)
        train_indices = indices[:split_idx]
        val_indices = indices[split_idx:]
        
        return (
            [x_values[i] for i in train_indices], [y_values[i] for i in train_indices],
            [x_values[i] for i in val_indices], [y_values[i] for i in val_indices]
        )
    
    def compute_loss(self, y_true: List[float], y_pred: List[float]) -> float:
        """Compute Mean Squared Error loss"""
        if len(y_true) != len(y_pred):
            raise ValueError("y_true and y_pred must have same length")
            
        mse = sum((true - pred) ** 2 for true, pred in zip(y_true, y_pred)) / len(y_true)
        return mse
    
    def compute_accuracy(self, y_true: List[float], y_pred: List[float], tolerance: float = 0.1) -> float:
        """Compute accuracy within tolerance"""
        if len(y_true) != len(y_pred):
            return 0.0
            
        correct = sum(1 for true, pred in zip(y_true, y_pred) if abs(true - pred) <= tolerance)
        return correct / len(y_true)
    
    def train_epoch(self, x_train: List[float], y_train: List[float], learning_rate: float) -> Dict[str, float]:
        """Train for one epoch"""
        # Get predictions using current parameters
        y_pred, b_bias = self.parameter_manager.get_parameters(x_train)
        
        # Update parameters using gradient descent
        update_result = self.parameter_manager.update_parameters(x_train, y_train, learning_rate)
        
        # Compute metrics
        train_loss = self.compute_loss(y_train, y_pred)
        train_accuracy = self.compute_accuracy(y_train, y_pred)
        
        return {
            "train_loss": train_loss,
            "train_accuracy": train_accuracy,
            **update_result
        }
    
    def validate(self, x_val: List[float], y_val: List[float]) -> Dict[str, float]:
        """Validate model on validation set"""
        # Get predictions using current parameters
        y_pred, b_bias = self.parameter_manager.get_parameters(x_val)
        
        # Compute metrics
        val_loss = self.compute_loss(y_val, y_pred)
        val_accuracy = self.compute_accuracy(y_val, y_pred)
        
        return {
            "val_loss": val_loss,
            "val_accuracy": val_accuracy
        }
    
    def save_checkpoint(self, epoch: int, metrics: TrainingMetrics):
        """Save training checkpoint"""
        if not self.config.save_checkpoints:
            return
            
        checkpoint_data = {
            "model_name": self.model_name,
            "epoch": epoch,
            "timestamp": datetime.now().isoformat(),
            "metrics": asdict(metrics),
            "parameter_state": asdict(self.parameter_manager.state),
            "config": asdict(self.config)
        }
        
        checkpoint_path = os.path.join(self.checkpoint_dir, f"checkpoint_epoch_{epoch}.json")
        
        try:
            with open(checkpoint_path, 'w') as f:
                json.dump(checkpoint_data, f, indent=2)
                
            if self.config.verbose:
                self.logger.info(f"Checkpoint saved: epoch {epoch}")
                
        except Exception as e:
            self.logger.error(f"Failed to save checkpoint: {e}")
    
    def load_checkpoint(self, checkpoint_path: str) -> bool:
        """Load training checkpoint"""
        try:
            with open(checkpoint_path, 'r') as f:
                checkpoint_data = json.load(f)
                
            # Load parameter state
            if "parameter_state" in checkpoint_data:
                self.parameter_manager.state = self.parameter_manager.ParameterState(**checkpoint_data["parameter_state"])
                
            # Load training state
            if "epoch" in checkpoint_data:
                self.current_epoch = checkpoint_data["epoch"]
                
            self.logger.info(f"Checkpoint loaded from {checkpoint_path}")
            return True
            
        except Exception as e:
            self.logger.error(f"Failed to load checkpoint: {e}")
            return False
    
    def train(self, data: List[Dict[str, Any]], validation_data: Optional[List[Dict[str, Any]]] = None) -> Dict[str, Any]:
        """
        Main training function
        
        Args:
            data: Training data
            validation_data: Optional validation data
            
        Returns:
            Training results and history
        """
        self.logger.info(f"Starting training for model {self.model_name}")
        
        # Prepare training data
        x_train, y_train = self.prepare_training_data(data)
        
        if not x_train:
            return {"error": "No training data provided"}
            
        # Prepare validation data
        if validation_data:
            x_val, y_val = self.prepare_training_data(validation_data)
        else:
            # Split data for validation
            x_train, y_train, x_val, y_val = self.split_data(x_train, y_train)
            
        # Training loop
        no_improvement_count = 0
        
        for epoch in range(self.config.epochs):
            self.current_epoch = epoch
            
            # Train one epoch
            train_metrics = self.train_epoch(x_train, y_train, self.config.learning_rate)
            
            # Validate
            val_metrics = self.validate(x_val, y_val)
            
            # Create metrics object
            metrics = TrainingMetrics(
                epoch=epoch,
                train_loss=train_metrics["train_loss"],
                val_loss=val_metrics["val_loss"],
                train_accuracy=train_metrics["train_accuracy"],
                val_accuracy=val_metrics["val_accuracy"],
                learning_rate=self.config.learning_rate,
                timestamp=datetime.now().isoformat()
            )
            
            self.training_history.append(metrics)
            
            # Print progress
            if self.config.verbose and epoch % 10 == 0:
                self.logger.info(
                    f"Epoch {epoch}: train_loss={train_metrics['train_loss']:.4f}, "
                    f"val_loss={val_metrics['val_loss']:.4f}, "
                    f"train_acc={train_metrics['train_accuracy']:.4f}, "
                    f"val_acc={val_metrics['val_accuracy']:.4f}"
                )
            
            # Early stopping
            if self.config.early_stopping:
                if val_metrics["val_loss"] < self.best_val_loss:
                    self.best_val_loss = val_metrics["val_loss"]
                    no_improvement_count = 0
                else:
                    no_improvement_count += 1
                    
                if no_improvement_count >= self.config.patience:
                    self.logger.info(f"Early stopping at epoch {epoch}")
                    break
            
            # Save checkpoint
            if self.config.save_checkpoints and epoch % self.config.checkpoint_frequency == 0:
                self.save_checkpoint(epoch, metrics)
        
        # Final results
        final_metrics = self.training_history[-1] if self.training_history else None
        
        training_results = {
            "model_name": self.model_name,
            "total_epochs": len(self.training_history),
            "final_metrics": asdict(final_metrics) if final_metrics else None,
            "best_val_loss": self.best_val_loss,
            "parameter_stats": self.parameter_manager.get_parameter_stats(),
            "training_complete": True
        }
        
        # Save final model
        final_checkpoint_path = os.path.join(self.checkpoint_dir, "final_model.json")
        self.parameter_manager.export_parameters(final_checkpoint_path)
        
        self.logger.info(f"Training completed for model {self.model_name}")
        return training_results
    
    def get_training_summary(self) -> Dict[str, Any]:
        """Get comprehensive training summary"""
        if not self.training_history:
            return {"error": "No training history available"}
            
        # Calculate summary statistics
        train_losses = [m.train_loss for m in self.training_history]
        val_losses = [m.val_loss for m in self.training_history]
        train_accuracies = [m.train_accuracy for m in self.training_history]
        val_accuracies = [m.val_accuracy for m in self.training_history]
        
        return {
            "model_name": self.model_name,
            "total_epochs": len(self.training_history),
            "best_epoch": min(range(len(val_losses)), key=lambda i: val_losses[i]),
            "best_val_loss": min(val_losses),
            "best_train_accuracy": max(train_accuracies),
            "best_val_accuracy": max(val_accuracies),
            "final_metrics": asdict(self.training_history[-1]),
            "parameter_stats": self.parameter_manager.get_parameter_stats(),
            "improvements": {
                "train_loss_reduction": train_losses[0] - train_losses[-1],
                "val_loss_reduction": val_losses[0] - val_losses[-1],
                "train_accuracy_improvement": train_accuracies[-1] - train_accuracies[0],
                "val_accuracy_improvement": val_accuracies[-1] - val_accuracies[0]
            }
        }
    
    def export_training_history(self, filepath: str):
        """Export training history to file"""
        history_data = {
            "model_name": self.model_name,
            "export_timestamp": datetime.now().isoformat(),
            "training_config": asdict(self.config),
            "training_history": [asdict(metric) for metric in self.training_history],
            "summary": self.get_training_summary()
        }
        
        with open(filepath, 'w') as f:
            json.dump(history_data, f, indent=2)
            
        self.logger.info(f"Training history exported to {filepath}")
