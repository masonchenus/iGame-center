#!/usr/bin/env python3
"""
Practical AI Model Training Script
Train your own AI model with custom data using the built-in training system
"""
import sys
import json
import time
from pathlib import Path

# Add the project root to the path
ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT))

from ai_backend.models.enhanced.model_trainer import ModelTrainer, TrainingConfig
from ai_backend.models.config.ai_config import apply_preset

def generate_sample_data(num_points=100, noise_level=0.1):
    """Generate sample training data with optional noise"""
    import random
    import math
    
    data = []
    for i in range(num_points):
        # Generate data following y = 2.5*x + 1.3 + noise
        x = i / 10.0  # Scale to 0-10 range
        true_y = 2.5 * x + 1.3  # True relationship
        noise = (random.random() - 0.5) * noise_level * 2  # Add noise
        y = true_y + noise
        
        data.append({"x": x, "y": y})
    
    return data

def load_training_data(filepath):
    """Load training data from JSON file"""
    try:
        with open(filepath, 'r') as f:
            data = json.load(f)
        
        # Validate data format
        if not isinstance(data, list):
            raise ValueError("Data must be a list")
        
        if not data:
            raise ValueError("Data list is empty")
        
        # Check format
        if isinstance(data[0], dict):
            if 'x' not in data[0] or 'y' not in data[0]:
                raise ValueError("Dictionary format must have 'x' and 'y' keys")
        elif isinstance(data[0], (list, tuple)):
            if len(data[0]) < 2:
                raise ValueError("List format must have at least 2 elements [x, y]")
        else:
            raise ValueError("Unsupported data format")
        
        return data
        
    except FileNotFoundError:
        print(f"❌ Data file not found: {filepath}")
        return None
    except Exception as e:
        print(f"❌ Error loading data: {e}")
        return None

def train_model_with_custom_data(model_name, training_data, config=None, validation_data=None):
    """Train model with provided data"""
    print(f"🚀 Training model '{model_name}'")
    print(f"📊 Training data points: {len(training_data)}")
    
    if validation_data:
        print(f"📊 Validation data points: {len(validation_data)}")
    
    # Initialize trainer
    trainer = ModelTrainer(model_name=model_name, config=config)
    
    # Start timing
    start_time = time.time()
    
    # Train the model
    print("🔄 Starting training process...")
    results = trainer.train(training_data, validation_data=validation_data)
    
    # Calculate training time
    training_time = time.time() - start_time
    
    return results, trainer, training_time

def display_training_results(results, trainer, training_time):
    """Display comprehensive training results"""
    print("\n" + "="*60)
    print("🎯 TRAINING RESULTS")
    print("="*60)
    
    if results.get("training_complete"):
        print("✅ Training completed successfully!")
        
        # Basic results
        print(f"📈 Model: {results['model_name']}")
        print(f"⏱️  Training time: {training_time:.2f} seconds")
        print(f"🔄 Total epochs: {results['total_epochs']}")
        print(f"🎯 Best validation loss: {results['best_val_loss']:.6f}")
        
        # Get detailed summary
        summary = trainer.get_training_summary()
        
        print(f"\n📊 PERFORMANCE METRICS:")
        print(f"   Best epoch: {summary['best_epoch']}")
        print(f"   Best training accuracy: {summary['best_train_accuracy']:.4f}")
        print(f"   Best validation accuracy: {summary['best_val_accuracy']:.4f}")
        
        print(f"\n📈 IMPROVEMENTS:")
        print(f"   Training loss reduction: {summary['improvements']['train_loss_reduction']:.6f}")
        print(f"   Validation loss reduction: {summary['improvements']['val_loss_reduction']:.6f}")
        print(f"   Training accuracy improvement: {summary['improvements']['train_accuracy_improvement']:.4f}")
        print(f"   Validation accuracy improvement: {summary['improvements']['val_accuracy_improvement']:.4f}")
        
        # Parameter statistics
        param_stats = trainer.parameter_manager.get_parameter_stats()
        print(f"\n⚙️  PARAMETER STATISTICS:")
        print(f"   Virtual W array size: {param_stats['virtual_array_sizes']['w']:,}")
        print(f"   Virtual B array size: {param_stats['virtual_array_sizes']['b']:,}")
        print(f"   Active parameters: {param_stats['active_parameters']['w_size']:,}")
        print(f"   Training steps: {param_stats['training']['steps']:,}")
        print(f"   Performance score: {param_stats['training']['performance_score']:.4f}")
        
        return True
    else:
        print(f"❌ Training failed: {results.get('error')}")
        return False

def save_training_artifacts(trainer, model_name, results):
    """Save training artifacts and exports"""
    print(f"\n💾 Saving training artifacts...")
    
    # Export training history
    history_file = f"{model_name}_training_history.json"
    trainer.export_training_history(history_file)
    print(f"📊 Training history saved: {history_file}")
    
    # Export final model
    model_file = f"{model_name}_final_model.json"
    trainer.parameter_manager.export_parameters(model_file)
    print(f"🤖 Final model saved: {model_file}")
    
    # Save training summary
    summary_file = f"{model_name}_training_summary.json"
    summary = trainer.get_training_summary()
    
    summary_data = {
        "model_name": model_name,
        "timestamp": time.strftime("%Y-%m-%d %H:%M:%S"),
        "training_results": results,
        "summary": summary
    }
    
    with open(summary_file, 'w') as f:
        json.dump(summary_data, f, indent=2)
    
    print(f"📋 Training summary saved: {summary_file}")
    
    return history_file, model_file, summary_file

def main():
    """Main training function"""
    print("🤖 AI Model Training Script")
    print("="*50)
    
    # Configuration options
    print("\n📋 Configuration Options:")
    print("1. Quick test (10 epochs, high learning rate)")
    print("2. Standard training (100 epochs, balanced)")
    print("3. Extended training (500 epochs, fine-tuned)")
    print("4. Custom configuration")
    print("5. Use sample data (generated automatically)")
    
    choice = input("\nSelect option (1-5): ").strip()
    
    if choice == "1":
        # Quick test configuration
        config = TrainingConfig(
            epochs=10,
            batch_size=16,
            learning_rate=0.01,
            validation_split=0.3,
            early_stopping=False,
            save_checkpoints=False,
            verbose=True
        )
        model_name = "quick_test_model"
        use_sample_data = True
        
    elif choice == "2":
        # Standard configuration
        config = TrainingConfig(
            epochs=100,
            batch_size=32,
            learning_rate=0.001,
            validation_split=0.2,
            early_stopping=True,
            patience=15,
            save_checkpoints=True,
            verbose=True
        )
        model_name = "standard_model"
        use_sample_data = True
        
    elif choice == "3":
        # Extended training configuration
        config = TrainingConfig(
            epochs=500,
            batch_size=64,
            learning_rate=0.0005,
            validation_split=0.15,
            early_stopping=True,
            patience=25,
            save_checkpoints=True,
            checkpoint_frequency=10,
            verbose=True
        )
        model_name = "extended_model"
        use_sample_data = True
        
    elif choice == "4":
        # Custom configuration
        model_name = input("Enter model name: ").strip() or "custom_model"
        
        epochs = int(input("Number of epochs (default 100): ") or "100")
        learning_rate = float(input("Learning rate (default 0.001): ") or "0.001")
        batch_size = int(input("Batch size (default 32): ") or "32")
        validation_split = float(input("Validation split (default 0.2): ") or "0.2")
        
        config = TrainingConfig(
            epochs=epochs,
            batch_size=batch_size,
            learning_rate=learning_rate,
            validation_split=validation_split,
            early_stopping=True,
            patience=10,
            save_checkpoints=True,
            verbose=True
        )
        
        data_file = input("Path to training data file (or press Enter for sample): ").strip()
        use_sample_data = not data_file
        training_data = None
        
        if not use_sample_data:
            training_data = load_training_data(data_file)
            if not training_data:
                print("❌ Failed to load training data. Using sample data instead.")
                use_sample_data = True
        
    else:
        # Default to option 2
        config = TrainingConfig(epochs=100, verbose=True)
        model_name = "default_model"
        use_sample_data = True
    
    # Generate or load training data
    if use_sample_data:
        print("\n📊 Generating sample training data...")
        training_data = generate_sample_data(150, noise_level=0.2)
        print(f"✅ Generated {len(training_data)} sample data points")
        
        # Ask if user wants to use separate validation data
        use_separate_val = input("Use separate validation data? (y/N): ").lower().strip() == 'y'
        
        if use_separate_val:
            # Use last 20% as validation
            split_point = int(len(training_data) * 0.8)
            validation_data = training_data[split_point:]
            training_data = training_data[:split_point]
        else:
            validation_data = None
    else:
        validation_data = None
    
    # Apply configuration preset
    apply_preset("training")
    
    # Train the model
    try:
        results, trainer, training_time = train_model_with_custom_data(
            model_name, training_data, config, validation_data
        )
        
        # Display results
        success = display_training_results(results, trainer, training_time)
        
        if success:
            # Save artifacts
            history_file, model_file, summary_file = save_training_artifacts(
                trainer, model_name, results
            )
            
            print(f"\n🎉 Training completed successfully!")
            print(f"\n📁 Generated files:")
            print(f"   • {history_file} - Detailed training history")
            print(f"   • {model_file} - Trained model parameters")
            print(f"   • {summary_file} - Training summary")
            
            # Next steps
            print(f"\n🚀 Next steps:")
            print(f"   1. Load your trained model: ParameterManager('{model_name}')")
            print(f"   2. Use the model for predictions")
            print(f"   3. Fine-tune with additional data")
            print(f"   4. Deploy in your application")
            
        else:
            print(f"\n❌ Training failed. Check the error messages above.")
            
    except Exception as e:
        print(f"\n❌ Unexpected error during training: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    main()
