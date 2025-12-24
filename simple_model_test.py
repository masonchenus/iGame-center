#!/usr/bin/env python3
"""
Simple AI Model Test
Creates, trains, loads and tests an AI model in one script
"""
import sys
from pathlib import Path

# Add the project root to the path
ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT))

from ai_backend.models.enhanced.model_trainer import ModelTrainer, TrainingConfig
from ai_backend.models.enhanced.parameter_manager import ParameterManager

def main():
    print("🤖 Simple AI Model Test")
    print("="*40)
    
    # Step 1: Create and train a model
    print("\n1️⃣ Creating and training model...")
    
    # Simple training data (y = 2x + 1)
    training_data = [
        {"x": 1, "y": 3},
        {"x": 2, "y": 5}, 
        {"x": 3, "y": 7},
        {"x": 4, "y": 9},
        {"x": 5, "y": 11}
    ]
    
    # Configure and train
    config = TrainingConfig(epochs=20, learning_rate=0.01, verbose=True)
    trainer = ModelTrainer(model_name="simple_test", config=config)
    
    print(f"   Training with {len(training_data)} data points...")
    results = trainer.train(training_data)
    
    if not results.get("training_complete"):
        print(f"❌ Training failed: {results.get('error')}")
        return
    
    print(f"✅ Training completed in {results['total_epochs']} epochs")
    
    # Step 2: Save the model
    print("\n2️⃣ Saving model...")
    model_file = "simple_test_model.json"
    trainer.parameter_manager.export_parameters(model_file)
    print(f"✅ Model saved to {model_file}")
    
    # Step 3: Load the model in a new ParameterManager
    print("\n3️⃣ Loading model...")
    param_manager = ParameterManager("simple_test")
    param_manager.import_parameters(model_file)
    print(f"✅ Model loaded from {model_file}")
    
    # Step 4: Test the model with predictions
    print("\n4️⃣ Testing model predictions...")
    test_values = [1.5, 2.5, 6.0, 8.0]
    predictions, bias = param_manager.get_parameters(test_values)
    
    print(f"   Model bias (b): {bias:.3f}")
    print("   Predictions:")
    for x, y in zip(test_values, predictions):
        print(f"     x = {x:4.1f} → y = {y:7.3f}")
    
    # Step 5: Test model performance
    print("\n5️⃣ Testing model accuracy...")
    test_data = [
        {"x": 6, "y": 13},  # Expected: ~13
        {"x": 7, "y": 15},  # Expected: ~15
        {"x": 8, "y": 17}   # Expected: ~17
    ]
    
    x_values = [item["x"] for item in test_data]
    y_true = [item["y"] for item in test_data]
    y_pred, _ = param_manager.get_parameters(x_values)
    
    # Calculate accuracy
    errors = [abs(true - pred) for true, pred in zip(y_true, y_pred)]
    avg_error = sum(errors) / len(errors)
    
    print("   Test Results:")
    for x, true_y, pred_y, error in zip(x_values, y_true, y_pred, errors):
        print(f"     x={x}: true={true_y:4.1f}, predicted={pred_y:6.1f}, error={error:4.1f}")
    
    print(f"   Average error: {avg_error:.2f}")
    
    # Step 6: Show model stats
    print("\n6️⃣ Model statistics...")
    stats = param_manager.get_parameter_stats()
    print(f"   Training steps: {stats['training']['steps']}")
    print(f"   Performance score: {stats['training']['performance_score']:.4f}")
    print(f"   W parameter stats: mean={stats['statistics']['w_mean']:.3f}, std={stats['statistics']['w_std']:.3f}")
    
    print("\n🎉 Simple AI model test completed!")
    print(f"✅ Model created, saved, loaded, and tested successfully")
    print(f"📁 Model file: {model_file}")

if __name__ == "__main__":
    main()
