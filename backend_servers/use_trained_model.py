#!/usr/bin/env python3
"""
Use Your Trained AI Model
Example script showing how to load and use your trained model for predictions
"""
import sys
import json
from pathlib import Path

# Add the project root to the path
ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT))

from ai_backend.models.enhanced.parameter_manager import ParameterManager

def load_trained_model(model_name, model_file=None):
    """Load a previously trained model"""
    print(f"🤖 Loading trained model '{model_name}'...")
    
    # Initialize parameter manager
    param_manager = ParameterManager(model_name)
    
    # If model file is provided, load it
    if model_file and Path(model_file).exists():
        success = param_manager.import_parameters(model_file)
        if success:
            print(f"✅ Model loaded from {model_file}")
        else:
            print(f"⚠️  Failed to load from {model_file}, using current state")
    else:
        print(f"📂 Using current model state (no file specified or file not found)")
    
    return param_manager

def make_predictions(param_manager, x_values):
    """Make predictions using the loaded model"""
    predictions, bias = param_manager.get_parameters(x_values)
    return predictions, bias

def evaluate_model_performance(param_manager, test_data):
    """Evaluate model performance on test data"""
    if not test_data:
        return None
    
    x_values = [item['x'] for item in test_data]
    y_true = [item['y'] for item in test_data]
    
    y_pred, bias = make_predictions(param_manager, x_values)
    
    # Calculate metrics
    mse = sum((true - pred) ** 2 for true, pred in zip(y_true, y_pred)) / len(y_true)
    
    # Calculate accuracy within tolerance
    tolerance = 0.1
    correct = sum(1 for true, pred in zip(y_true, y_pred) if abs(true - pred) <= tolerance)
    accuracy = correct / len(y_true)
    
    return {
        "mse": mse,
        "accuracy": accuracy,
        "predictions": list(zip(x_values, y_true, y_pred)),
        "bias": bias
    }

def interactive_prediction_demo(param_manager):
    """Interactive demonstration of making predictions"""
    print(f"\n🔮 Interactive Prediction Demo")
    print("Enter x values to get predictions (or 'quit' to exit)")
    
    while True:
        user_input = input("\nEnter x value: ").strip()
        
        if user_input.lower() in ['quit', 'exit', 'q']:
            break
        
        try:
            x_value = float(user_input)
            predictions, bias = make_predictions(param_manager, [x_value])
            
            print(f"   Input: x = {x_value}")
            print(f"   Prediction: y = {predictions[0]:.4f}")
            print(f"   Model bias: {bias:.4f}")
            
            # Show the formula
            print(f"   Formula: y = w*x + b = {predictions[0]:.4f}")
            
        except ValueError:
            print("❌ Please enter a valid number")
        except Exception as e:
            print(f"❌ Error making prediction: {e}")

def batch_prediction_demo(param_manager):
    """Demonstrate batch predictions"""
    print(f"\n📊 Batch Prediction Demo")
    
    # Create batch of test values
    test_x_values = [0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0]
    
    predictions, bias = make_predictions(param_manager, test_x_values)
    
    print(f"Making predictions for {len(test_x_values)} values:")
    print("-" * 40)
    print(f"{'Input (x)':<10} {'Prediction (y)':<15} {'Formula'}")
    print("-" * 40)
    
    for x, y in zip(test_x_values, predictions):
        print(f"{x:<10.1f} {y:<15.4f} y = w*{x} + b")
    
    print(f"\nModel bias (b): {bias:.4f}")

def main():
    """Main function demonstrating model usage"""
    print("🤖 Using Your Trained AI Model")
    print("="*50)
    
    # Step 1: Load a model
    model_name = input("Enter model name to load (or press Enter for 'quick_example_model'): ").strip()
    if not model_name:
        model_name = "quick_example_model"
    
    model_file = input(f"Enter model file path (or press Enter to use '{model_name}.json'): ").strip()
    if not model_file:
        model_file = f"{model_name}.json"
    
    param_manager = load_trained_model(model_name, model_file)
    
    # Step 2: Show model statistics
    print(f"\n📈 Model Statistics:")
    stats = param_manager.get_parameter_stats()
    print(f"   Model name: {stats['model_name']}")
    print(f"   Training steps: {stats['training']['steps']:,}")
    print(f"   Performance score: {stats['training']['performance_score']:.4f}")
    print(f"   W parameter mean: {stats['statistics']['w_mean']:.4f}")
    print(f"   W parameter std: {stats['statistics']['w_std']:.4f}")
    print(f"   B parameter mean: {stats['statistics']['b_mean']:.4f}")
    print(f"   Last update: {stats['training']['last_update']}")
    
    # Step 3: Batch predictions
    batch_prediction_demo(param_manager)
    
    # Step 4: Test with some sample data
    print(f"\n🧪 Testing with sample data...")
    sample_test_data = [
        {"x": 1.0, "y": 3.9},  # Expected around 3.9
        {"x": 2.0, "y": 7.1},  # Expected around 7.1
        {"x": 3.0, "y": 10.3}, # Expected around 10.3
    ]
    
    performance = evaluate_model_performance(param_manager, sample_test_data)
    
    if performance:
        print(f"✅ Performance on test data:")
        print(f"   Mean Squared Error: {performance['mse']:.6f}")
        print(f"   Accuracy (within 0.1): {performance['accuracy']:.2%}")
        print(f"   Model bias: {performance['bias']:.4f}")
        
        print(f"\n   Predictions vs Actual:")
        for x, y_true, y_pred in performance['predictions']:
            error = abs(y_true - y_pred)
            print(f"     x={x:.1f}: predicted={y_pred:.3f}, actual={y_true:.3f}, error={error:.3f}")
    
    # Step 5: Interactive demo
    demo_choice = input(f"\nTry interactive predictions? (y/N): ").strip().lower()
    if demo_choice == 'y':
        interactive_prediction_demo(param_manager)
    
    # Step 6: Show how to integrate into applications
    print(f"\n🚀 Integration Examples:")
    
    # Create integration examples with proper variable substitution
    example1_code = f"""# Example 1: Basic prediction
from ai_backend.models.enhanced.parameter_manager import ParameterManager

param_manager = ParameterManager('{model_name}')
param_manager.import_parameters('{model_file}')

# Make a prediction
x_value = 2.5
predictions, bias = param_manager.get_parameters([x_value])
y_prediction = predictions[0]

print(f"Prediction for x={{x_value}}: y={{y_prediction:.4f}}")"""
    
    example2_code = """# Example 2: Batch predictions
x_values = [1.0, 2.0, 3.0, 4.0, 5.0]
predictions, bias = param_manager.get_parameters(x_values)

for x, y in zip(x_values, predictions):
    print(f"x={x} → y={y:.4f}")"""
    
    example3_code = """# Example 3: Fine-tune with new data
new_training_data = [
    {"x": 6.0, "y": 19.5},
    {"x": 7.0, "y": 22.8},
    {"x": 8.0, "y": 25.9}
]

result = param_manager.fine_tune_parameters(new_training_data)
print(f"Fine-tuning improvement: {result['improvement']:.2%}")"""
    
    print(example1_code)
    print()
    print(example2_code)
    print()
    print(example3_code)
    
    print(f"\n🎯 Summary:")
    print(f"✅ Model '{model_name}' loaded successfully")
    print(f"✅ Ready for predictions and fine-tuning")
    print(f"✅ Model file: {model_file}")
    
    return param_manager

if __name__ == "__main__":
    param_manager = main()
    print(f"\n🎉 Model usage demonstration complete!")
    print(f"Your trained model is ready for production use!")
