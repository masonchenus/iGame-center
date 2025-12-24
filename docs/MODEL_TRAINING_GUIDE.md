# AI Model Training Guide

This comprehensive guide will show you how to train your AI model using the built-in training system.

## Overview

Your AI system uses a mathematical foundation based on the `y = w*x + b` formula with:
- **148 trillion virtual parameters** for w and b arrays
- **Advanced parameter management** with gradient descent
- **Memory-efficient training** using virtual arrays
- **Real-time monitoring** and optimization

## Prerequisites

```bash
# Ensure you have the required dependencies
pip install numpy pathlib dataclasses
```

## Step 1: Prepare Your Training Data

Your training data should be in one of these formats:

### Format 1: Dictionary Format
```python
training_data = [
    {"x": 1.0, "y": 2.5},
    {"x": 2.0, "y": 5.1},
    {"x": 3.0, "y": 7.8},
    # ... more data points
]
```

### Format 2: List/Tuple Format
```python
training_data = [
    [1.0, 2.5],
    [2.0, 5.1],
    [3.0, 7.8],
    # ... more data points
]
```

## Step 2: Configure Training Parameters

### Basic Configuration
```python
from ai_backend.models.enhanced.model_trainer import ModelTrainer, TrainingConfig

# Create training configuration
config = TrainingConfig(
    epochs=100,              # Number of training epochs
    batch_size=32,           # Batch size for training
    validation_split=0.2,    # 20% of data for validation
    learning_rate=0.001,     # Learning rate for gradient descent
    early_stopping=True,     # Stop if no improvement
    patience=10,             # Epochs to wait before early stopping
    save_checkpoints=True,   # Save model checkpoints
    checkpoint_frequency=10, # Save every 10 epochs
    verbose=True             # Print training progress
)
```

### Advanced Configuration
```python
# For production training
production_config = TrainingConfig(
    epochs=500,
    batch_size=64,
    learning_rate=0.0001,
    validation_split=0.1,
    early_stopping=True,
    patience=20,
    save_checkpoints=True,
    checkpoint_frequency=5
)

# For quick testing
quick_config = TrainingConfig(
    epochs=10,
    batch_size=16,
    learning_rate=0.01,
    validation_split=0.3,
    early_stopping=False,
    save_checkpoints=False,
    verbose=True
)
```

## Step 3: Initialize and Train Your Model

### Basic Training Example
```python
from ai_backend.models.enhanced.model_trainer import ModelTrainer

# Initialize trainer
trainer = ModelTrainer(model_name="my_custom_model", config=config)

# Prepare training data
training_data = [
    {"x": 1.0, "y": 2.5},
    {"x": 2.0, "y": 5.1},
    {"x": 3.0, "y": 7.8},
    {"x": 4.0, "y": 10.2},
    {"x": 5.0, "y": 12.9},
    # Add more data points...
]

# Run training
print("Starting training...")
results = trainer.train(training_data)

# Print results
print(f"Training completed!")
print(f"Model: {results['model_name']}")
print(f"Total epochs: {results['total_epochs']}")
print(f"Best validation loss: {results['best_val_loss']:.6f}")
```

### Training with Custom Validation Data
```python
# Separate training and validation data
train_data = training_data[:80]  # First 80% for training
val_data = training_data[80:]    # Last 20% for validation

# Train with separate validation data
results = trainer.train(train_data, validation_data=val_data)
```

## Step 4: Monitor Training Progress

### Get Training Summary
```python
# Get comprehensive training summary
summary = trainer.get_training_summary()
print(f"Best epoch: {summary['best_epoch']}")
print(f"Best validation loss: {summary['best_val_loss']:.6f}")
print(f"Final training accuracy: {summary['best_train_accuracy']:.4f}")
print(f"Final validation accuracy: {summary['best_val_accuracy']:.4f}")
```

### Export Training History
```python
# Export detailed training history
trainer.export_training_history("my_model_training_history.json")
print("Training history exported!")
```

## Step 5: Save and Load Models

### Automatic Saving
The trainer automatically saves checkpoints during training and the final model:
- Checkpoints: `ai_backend/models/training/checkpoints/{model_name}/`
- Final model: `ai_backend/models/training/checkpoints/{model_name}/final_model.json`

### Manual Export
```python
# Export final trained model
trainer.parameter_manager.export_parameters("my_trained_model.json")
print("Model exported successfully!")
```

### Load Previously Trained Model
```python
from ai_backend.models.enhanced.parameter_manager import ParameterManager

# Load model parameters
param_manager = ParameterManager("my_custom_model")
param_manager.import_parameters("my_trained_model.json")

# Create new trainer and load state
trainer = ModelTrainer("my_custom_model")
trainer.parameter_manager = param_manager
```

## Step 6: Fine-tune Existing Models

```python
# Fine-tune with new data
tuning_data = [
    {"x": 10.0, "y": 25.0},
    {"x": 11.0, "y": 27.5},
    # ... new data points
]

# Fine-tune the model
fine_tune_result = trainer.parameter_manager.fine_tune_parameters(tuning_data)
print(f"Fine-tuning improvement: {fine_tune_result['improvement']:.2%}")
```

## Complete Training Script Example

```python
#!/usr/bin/env python3
"""
Complete training script example
"""
import sys
from pathlib import Path

# Add project root to path
ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT))

from ai_backend.models.enhanced.model_trainer import ModelTrainer, TrainingConfig

def main():
    print("🚀 Starting AI Model Training")
    
    # Step 1: Configure training
    config = TrainingConfig(
        epochs=200,
        batch_size=32,
        learning_rate=0.001,
        validation_split=0.2,
        early_stopping=True,
        patience=15,
        save_checkpoints=True,
        verbose=True
    )
    
    # Step 2: Initialize trainer
    trainer = ModelTrainer(model_name="my_ai_model", config=config)
    
    # Step 3: Prepare training data (replace with your data)
    training_data = [
        {"x": i, "y": 2.5 * i + 1.3 + (i % 3 - 1) * 0.1} 
        for i in range(1, 101)  # 100 data points
    ]
    
    # Step 4: Run training
    print("Starting training process...")
    results = trainer.train(training_data)
    
    # Step 5: Show results
    if results.get("training_complete"):
        print("✅ Training completed successfully!")
        print(f"   Model: {results['model_name']}")
        print(f"   Epochs: {results['total_epochs']}")
        print(f"   Best validation loss: {results['best_val_loss']:.6f}")
        
        # Get detailed summary
        summary = trainer.get_training_summary()
        print(f"   Final training accuracy: {summary['best_train_accuracy']:.4f}")
        print(f"   Final validation accuracy: {summary['best_val_accuracy']:.4f}")
        
        # Export training history
        trainer.export_training_history("training_results.json")
        print("📊 Training history saved to 'training_results.json'")
        
    else:
        print(f"❌ Training failed: {results.get('error')}")

if __name__ == "__main__":
    main()
```

## Parameter Configuration Options

### Learning Rate Tuning
```python
from ai_backend.models.config.ai_config import ai_config

# Set custom learning rates
ai_config.update({
    "parameter_learning_rate": 0.002,
    "w_parameter_size": 148_000_000_000_000,
    "b_parameter_size": 148_000_000_000_000
})
```

### Training Presets
```python
from ai_backend.models.config.ai_config import apply_preset

# Apply preset configurations
apply_preset("development")  # Fast training, more verbose
apply_preset("production")   # Optimized for production
apply_preset("training")     # Extended training runs
apply_preset("performance")  # Performance optimized
```

## Troubleshooting

### Common Issues and Solutions

1. **"No training data provided"**
   - Ensure your data is in the correct format
   - Check that you have at least 2 data points

2. **"Invalid input data"**
   - Verify x_values and y_values have the same length
   - Ensure all values are numeric

3. **Training too slow**
   - Reduce epochs for testing
   - Increase batch size
   - Use "development" preset

4. **Model not converging**
   - Try different learning rates (0.001, 0.01, 0.0001)
   - Check if your data has a linear relationship
   - Increase patience for early stopping

## Next Steps

After training your model:

1. **Evaluate Performance**: Use the evaluator module to assess model quality
2. **Deploy Model**: Integrate with your application
3. **Monitor Performance**: Use the performance tracker for ongoing monitoring
4. **Fine-tune**: Continuously improve with new data

## Advanced Features

- **Genetic Algorithm Optimization**: Use `ai_manager.optimize_parameters()`
- **Performance Monitoring**: Real-time tracking with `PerformanceTracker`
- **Parameter Statistics**: Detailed analytics with `get_parameter_stats()`
- **Model Export**: Export for deployment in other systems

Your AI model training system is now ready to use! 🚀
