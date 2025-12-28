#!/usr/bin/env python3
"""
Quick AI Model Training Example
A simple, runnable example to train your AI model immediately
"""
import sys
import json
from pathlib import Path

# Add the project root to the path
ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT))

from ai_backend.models.enhanced.model_trainer import ModelTrainer, TrainingConfig

def create_sample_data():
    """Create sample training data"""
    data = []
    # Generate data following y = 3.2*x + 0.7 + noise
    for i in range(1, 51):  # 50 data points
        x = i / 5.0  # Scale to 0.2-10 range
        true_y = 3.2 * x + 0.7  # True relationship
        import random
        noise = (random.random() - 0.5) * 0.5  # Add some noise
        y = true_y + noise
        
        data.append({"x": round(x, 2), "y": round(y, 3)})
    
    return data

def main():
    print("🚀 Quick AI Model Training Example")
    print("="*50)
    
    # Step 1: Create sample training data
    print("\n📊 Creating sample training data...")
    training_data = create_sample_data()
    print(f"✅ Generated {len(training_data)} data points")
    print("   Sample data:")
    for i, point in enumerate(training_data[:5]):
        print(f"     {i+1}. x={point['x']}, y={point['y']}")
    print("     ...")
    
    # Step 2: Configure training
    print("\n⚙️  Configuring training...")
    config = TrainingConfig(
        epochs=50,              # Quick training
        batch_size=16,
        learning_rate=0.01,     # Faster convergence
        validation_split=0.2,   # 20% for validation
        early_stopping=True,
        patience=10,
        save_checkpoints=True,
        verbose=True
    )
    print("✅ Training configuration set")
    
    # Step 3: Initialize trainer
    print("\n🤖 Initializing model trainer...")
    model_name = "quick_example_model"
    trainer = ModelTrainer(model_name=model_name, config=config)
    print(f"✅ Trainer initialized for model '{model_name}'")
    
    # Step 4: Train the model
    print(f"\n🔥 Starting training for '{model_name}'...")
    print("-" * 30)
    
    results = trainer.train(training_data)
    
    # Step 5: Display results
    print("-" * 30)
    if results.get("training_complete"):
        print("🎉 Training completed successfully!")
        
        summary = trainer.get_training_summary()
        
        print(f"\n📈 Results Summary:")
        print(f"   Model: {results['model_name']}")
        print(f"   Epochs: {results['total_epochs']}")
        print(f"   Best validation loss: {results['best_val_loss']:.6f}")
        print(f"   Best epoch: {summary['best_epoch']}")
        print(f"   Final training accuracy: {summary['best_train_accuracy']:.4f}")
        print(f"   Final validation accuracy: {summary['best_val_accuracy']:.4f}")
        
        # Parameter statistics
        param_stats = trainer.parameter_manager.get_parameter_stats()
        print(f"\n⚙️  Model Parameters:")
        print(f"   Training steps: {param_stats['training']['steps']:,}")
        print(f"   Performance score: {param_stats['training']['performance_score']:.4f}")
        print(f"   W parameter std: {param_stats['statistics']['w_std']:.4f}")
        print(f"   B parameter mean: {param_stats['statistics']['b_mean']:.4f}")
        
        # Step 6: Test predictions
        print(f"\n🔮 Testing predictions with trained model...")
        test_x_values = [0.5, 1.0, 2.5, 5.0, 7.8]
        predictions, bias = trainer.parameter_manager.get_parameters(test_x_values)
        
        print("   Predictions (y = w*x + b):")
        for x, y in zip(test_x_values, predictions):
            print(f"     x={x:4.1f} → y={y:7.3f}")
        print(f"   Bias (b): {bias:.4f}")
        
        # Step 7: Save model
        print(f"\n💾 Saving trained model...")
        model_file = f"{model_name}.json"
        trainer.parameter_manager.export_parameters(model_file)
        print(f"✅ Model saved to: {model_file}")
        
        # Step 8: Export training history
        history_file = f"{model_name}_history.json"
        trainer.export_training_history(history_file)
        print(f"✅ Training history saved to: {history_file}")
        
        print(f"\n🎯 What you can do next:")
        print(f"   1. Load this model in another script: ParameterManager('{model_name}')")
        print(f"   2. Use it for making predictions on new data")
        print(f"   3. Fine-tune with additional training data")
        print(f"   4. Run the comprehensive training script: python train_my_model.py")
        
        return True
        
    else:
        print(f"❌ Training failed: {results.get('error')}")
        return False

def demonstrate_fine_tuning(trainer):
    """Demonstrate fine-tuning with additional data"""
    print(f"\n🔧 Demonstrating fine-tuning...")
    
    # Create additional training data
    additional_data = []
    for i in range(1, 21):  # 20 new data points
        x = i / 2.0  # Different range
        true_y = 3.2 * x + 0.7  # Same relationship
        import random
        noise = (random.random() - 0.5) * 0.3
        y = true_y + noise
        additional_data.append({"x": round(x, 2), "y": round(y, 3)})
    
    print(f"   Fine-tuning with {len(additional_data)} additional data points...")
    
    # Fine-tune the model
    fine_tune_result = trainer.parameter_manager.fine_tune_parameters(additional_data)
    
    print(f"✅ Fine-tuning completed:")
    print(f"   Initial MSE: {fine_tune_result['initial_mse']:.6f}")
    print(f"   Final MSE: {fine_tune_result['final_mse']:.6f}")
    print(f"   Improvement: {fine_tune_result['improvement']:.2%}")

if __name__ == "__main__":
    success = main()
    
    if success:
        print(f"\n" + "="*50)
        print("🎊 Quick example completed successfully!")
        print("Your AI model is now trained and ready to use!")
        print("="*50)
    else:
        print(f"\n❌ Example failed. Check the error messages above.")
