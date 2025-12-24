#!/usr/bin/env python3
"""
Enhanced AI System Demo
Demonstrates the comprehensive AI enhancement capabilities
"""
import sys
import os
from pathlib import Path

# Add the project root to the path
ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT))

from models.unified_ai_manager import get_ai_manager, initialize_enhanced_ai
def demo_enhanced_ai():
    """Demonstrate the enhanced AI system"""
    print("🚀 Enhanced AI System Demo")
    print("=" * 50)
    
    # Initialize the enhanced AI system
    print("\n1. Initializing Enhanced AI System...")
    init_result = initialize_enhanced_ai("nexus")
    
    if init_result["status"] == "success":
        print("✅ AI System Initialized Successfully!")
        print(f"   Model: {init_result['model_name']}")
        print(f"   Session: {init_result['session_info']['session_id']}")
        print(f"   Capabilities: {', '.join(init_result['capabilities'])}")
    else:
        print(f"❌ Initialization Failed: {init_result.get('error')}")
        return
    
    # Get the AI manager
    ai_manager = get_ai_manager()
    
    # Demo 1: Process AI request with enhanced capabilities
    print("\n2. Processing AI Request...")
    test_prompt = "Explain the mathematical foundation of AI using y = w*x + b"
    parameters = {
        "w_learning_rate": 0.002,
        "b_learning_rate": 0.001,
        "w_regularization": 0.01,
        "b_regularization": 0.01
    }
    
    response = ai_manager.process_ai_request(test_prompt, parameters)
    
    if "error" not in response:
        print("✅ AI Request Processed!")
        print(f"   Response: {response['response'][:100]}...")
        print(f"   X Value: {response['ai_enhancement']['x_value']:.4f}")
        print(f"   Processing Time: {response['ai_enhancement']['processing_time_ms']:.1f}ms")
        print(f"   Token Count: {response['token_count']}")
    else:
        print(f"❌ Request Failed: {response['error']}")
    
    # Demo 2: Parameter management
    print("\n3. Demonstrating Parameter Management...")
    param_stats = ai_manager.parameter_manager.get_parameter_stats()
    print("✅ Parameter Statistics Retrieved:")
    print(f"   W Array Size: {param_stats['virtual_array_sizes']['w']:,}")
    print(f"   B Array Size: {param_stats['virtual_array_sizes']['b']:,}")
    print(f"   Active W Parameters: {param_stats['active_parameters']['w_size']:,}")
    print(f"   Active B Parameters: {param_stats['active_parameters']['b_size']:,}")
    print(f"   Training Steps: {param_stats['training']['steps']:,}")
    
    # Demo 3: Performance monitoring
    print("\n4. Performance Monitoring Status...")
    perf_status = ai_manager.performance_tracker.get_current_status()
    print("✅ Performance Status:")
    print(f"   System Health: {perf_status['system_health']}")
    print(f"   Monitoring Active: {perf_status['monitoring_active']}")
    print(f"   Uptime: {perf_status['uptime_hours']:.1f} hours")
    
    # Demo 4: System status
    print("\n5. Comprehensive System Status...")
    status = ai_manager.get_system_status()
    if status["status"] == "success":
        print("✅ System Status Retrieved:")
        print(f"   Parameter Efficiency: {status['system_status']['parameter_status']['statistics']['w_std']:.4f}")
        print(f"   Model Training: {'Available' if status['system_status']['training_summary'] else 'Not Available'}")
        print(f"   Evaluations: {len(status['system_status']['evaluation_summary']) if isinstance(status['system_status']['evaluation_summary'], list) else 'N/A'}")
    else:
        print(f"❌ Status Check Failed: {status['error']}")
    
    # Demo 5: Export system state
    print("\n6. Exporting System State...")
    export_path = f"demo_export_{ai_manager.model_name}_{int(__import__('time').time())}.json"
    export_result = ai_manager.export_system_state(export_path)
    
    if export_result["status"] == "success":
        print("✅ System State Exported:")
        print(f"   File: {export_result['export_file']}")
        print(f"   Size: {export_result['export_size_mb']:.2f} MB")
    else:
        print(f"❌ Export Failed: {export_result['error']}")
    
    # Demo 6: Quick optimization test (if data available)
    print("\n7. Testing Parameter Optimization...")
    try:
        # Generate some test data
        test_data = [{"x": i/10.0, "y": 2.5 * (i/10.0) + 1.3 + (hash(str(i)) % 100 - 50) / 100.0} for i in range(50)]
        
        optimization_result = ai_manager.optimize_parameters(test_data, algorithm="genetic")
        
        if optimization_result["status"] == "success":
            opt_result = optimization_result["optimization_result"]
            print("✅ Parameter Optimization Completed:")
            print(f"   Algorithm: {opt_result['algorithm']}")
            print(f"   Best Score: {opt_result['best_score']:.4f}")
            print(f"   Optimization Time: {opt_result['optimization_time_seconds']:.2f}s")
            print(f"   Convergence: Iteration {opt_result['convergence_iteration']}")
        else:
            print(f"❌ Optimization Failed: {optimization_result['error']}")
            
    except Exception as e:
        print(f"⚠️  Optimization Test Skipped: {e}")
    
    # Cleanup
    print("\n8. Shutting Down System...")
    shutdown_result = ai_manager.shutdown_system()
    
    if shutdown_result["status"] == "success":
        print("✅ System Shutdown Successfully")
        print(f"   Final Export: {shutdown_result['final_export']}")
    else:
        print(f"❌ Shutdown Failed: {shutdown_result['error']}")
    
    print("\n🎉 Enhanced AI System Demo Complete!")
    print("=" * 50)
    print("\nKey Features Demonstrated:")
    print("• Advanced parameter management with w and b arrays")
    print("• Real-time performance monitoring")
    print("• AI request processing with mathematical foundation")
    print("• Parameter optimization using genetic algorithms")
    print("• Comprehensive system state export")
    print("• Session management and health monitoring")
    print("\nNext Steps:")
    print("1. Start the FastAPI server: python server.py")
    print("2. Open the enhanced AI interface: src/AI/enhanced_interface.html")
    print("3. Explore the mathematical AI capabilities!")

if __name__ == "__main__":
    demo_enhanced_ai()

