#!/usr/bin/env python3
"""
Lightweight AI Performance Demo
Optimized for low CPU and memory usage with comprehensive real-time metrics
"""
import sys
import os
import time
import json
import psutil
from datetime import datetime
import threading

# Simple demo without complex imports
def demo_lightweight_ai():
    """Demonstrate lightweight AI system with optimized performance"""
    print("🚀 Lightweight AI Performance Demo")
    print("=" * 60)
    
    # Performance monitoring setup
    start_time = time.time()
    
    def get_system_metrics():
        """Get comprehensive system metrics"""
        cpu_percent = psutil.cpu_percent(interval=0.1)
        memory = psutil.virtual_memory()
        disk = psutil.disk_usage('/')
        
        return {
            "cpu_percent": cpu_percent,
            "memory_percent": memory.percent,
            "memory_used_gb": memory.used / (1024**3),
            "memory_available_gb": memory.available / (1024**3),
            "disk_percent": (disk.used / disk.total) * 100,
            "disk_free_gb": disk.free / (1024**3),
            "process_count": len(psutil.pids()),
            "load_average": os.getloadavg() if hasattr(os, 'getloadavg') else [0, 0, 0],
            "boot_time": psutil.boot_time(),
            "uptime_hours": (time.time() - psutil.boot_time()) / 3600
        }
    
    def simulate_ai_processing(prompt):
        """Simulate AI processing with y = w*x + b formula"""
        # Generate x from prompt hash (deterministic)
        import hashlib
        prompt_hash = hashlib.sha256(prompt.encode()).digest()
        x = int.from_bytes(prompt_hash[:4], 'big') / (2**32 - 1) * 10  # 0-10 range
        
        # Simulate w and b parameters (virtual arrays concept)
        w_sample = [(hash(f"w_{i}_{prompt}") % 1000) / 100.0 for i in range(10)]
        b_sample = [(hash(f"b_{i}_{prompt}") % 1000) / 100.0 for i in range(10)]
        
        # Calculate y = w*x + b
        y_values = [w * x + b for w, b in zip(w_sample, b_sample)]
        
        return {
            "x": x,
            "w_sample": w_sample,
            "b_sample": b_sample,
            "y_values": y_values,
            "response": f"AI processing complete for: {prompt[:50]}..."
        }
    
    # Initialize metrics
    print("\n1. Initializing Lightweight AI System...")
    initial_metrics = get_system_metrics()
    print(f"✅ System initialized")
    print(f"   CPU: {initial_metrics['cpu_percent']:.1f}%")
    print(f"   Memory: {initial_metrics['memory_percent']:.1f}% ({initial_metrics['memory_used_gb']:.1f}GB used)")
    print(f"   Disk: {initial_metrics['disk_percent']:.1f}% free")
    
    # AI Request Simulation
    print("\n2. Processing AI Request...")
    prompt = "Explain the mathematical foundation of AI using y = w*x + b"
    
    start_process = time.time()
    result = simulate_ai_processing(prompt)
    process_time = (time.time() - start_process) * 1000
    
    print(f"✅ AI Request Processed!")
    print(f"   Prompt: {prompt}")
    print(f"   X Value: {result['x']:.4f}")
    print(f"   Processing Time: {process_time:.1f}ms")
    print(f"   W Sample: [{', '.join(map(str, result['w_sample'][:3]))}...]")
    print(f"   B Sample: [{', '.join(map(str, result['b_sample'][:3]))}...]")
    print(f"   Y Values: [{', '.join(map(str, result['y_values'][:3]))}...]")
    
    # Real-time Performance Metrics
    print("\n3. Real-time Performance Metrics:")
    
    metrics_samples = []
    for i in range(5):  # Sample every 2 seconds
        metrics = get_system_metrics()
        metrics_samples.append(metrics)
        
        print(f"   Sample {i+1}:")
        print(f"     CPU Usage: {metrics['cpu_percent']:.1f}%")
        print(f"     Memory Usage: {metrics['memory_percent']:.1f}% ({metrics['memory_used_gb']:.1f}GB)")
        print(f"     Memory Available: {metrics['memory_available_gb']:.1f}GB")
        print(f"     Disk Free: {metrics['disk_free_gb']:.1f}GB")
        print(f"     Process Count: {metrics['process_count']}")
        print(f"     Load Average: {metrics['load_average'][0]:.2f}")
        print(f"     System Uptime: {metrics['uptime_hours']:.1f} hours")
        
        if i < 4:  # Don't sleep on last iteration
            time.sleep(2)
    
    # Performance Analysis
    print("\n4. Performance Analysis:")
    
    avg_cpu = sum(m['cpu_percent'] for m in metrics_samples) / len(metrics_samples)
    avg_memory = sum(m['memory_percent'] for m in metrics_samples) / len(metrics_samples)
    max_cpu = max(m['cpu_percent'] for m in metrics_samples)
    max_memory = max(m['memory_percent'] for m in metrics_samples)
    
    print(f"   Average CPU Usage: {avg_cpu:.1f}%")
    print(f"   Peak CPU Usage: {max_cpu:.1f}%")
    print(f"   Average Memory Usage: {avg_memory:.1f}%")
    print(f"   Peak Memory Usage: {max_memory:.1f}%")
    
    # System Health Assessment
    health_score = 100
    if avg_cpu > 80:
        health_score -= 20
    elif avg_cpu > 60:
        health_score -= 10
    
    if avg_memory > 85:
        health_score -= 25
    elif avg_memory > 70:
        health_score -= 15
    
    if max_cpu > 95 or max_memory > 95:
        health_score -= 30
    
    if health_score >= 90:
        health_status = "Excellent"
    elif health_score >= 75:
        health_status = "Good"
    elif health_score >= 60:
        health_status = "Fair"
    else:
        health_status = "Poor"
    
    print(f"   System Health Score: {health_score}/100 ({health_status})")
    
    # Network and I/O Metrics
    print("\n5. Additional Real-time Metrics:")
    
    try:
        # Network I/O
        net_io = psutil.net_io_counters()
        print(f"   Network - Bytes Sent: {net_io.bytes_sent / (1024**2):.1f}MB")
        print(f"   Network - Bytes Received: {net_io.bytes_recv / (1024**2):.1f}MB")
        
        # Disk I/O
        disk_io = psutil.disk_io_counters()
        if disk_io:
            print(f"   Disk I/O - Read: {disk_io.read_bytes / (1024**2):.1f}MB")
            print(f"   Disk I/O - Write: {disk_io.write_bytes / (1024**2):.1f}MB")
        
        # CPU frequency
        cpu_freq = psutil.cpu_freq()
        if cpu_freq:
            print(f"   CPU Frequency: {cpu_freq.current:.0f}MHz")
        
        # Temperature (if available)
        try:
            temps = psutil.sensors_temperatures()
            if temps:
                for name, entries in temps.items():
                    if entries:
                        temp = entries[0].current
                        print(f"   Temperature ({name}): {temp:.1f}°C")
        except:
            pass
        
    except Exception as e:
        print(f"   Additional metrics unavailable: {e}")
    
    # AI Performance Summary
    print("\n6. AI Mathematical Processing Summary:")
    print(f"   Formula: y = w × x + b")
    print(f"   W Array Concept: 148,000,000,000,000 elements (virtual)")
    print(f"   B Array Concept: 148,000,000,000,000 elements (virtual)")
    print(f"   Current X: {result['x']:.4f}")
    print(f"   Sample Computations: {len(result['y_values'])} performed")
    
    # Export metrics
    print("\n7. Exporting Performance Data...")
    export_data = {
        "timestamp": datetime.now().isoformat(),
        "demo_duration_seconds": time.time() - start_time,
        "ai_processing_time_ms": process_time,
        "system_metrics": metrics_samples,
        "ai_result": result,
        "performance_summary": {
            "avg_cpu_percent": avg_cpu,
            "max_cpu_percent": max_cpu,
            "avg_memory_percent": avg_memory,
            "max_memory_percent": max_memory,
            "health_score": health_score,
            "health_status": health_status
        }
    }
    
    export_file = f"ai_performance_demo_{int(time.time())}.json"
    try:
        with open(export_file, 'w') as f:
            json.dump(export_data, f, indent=2)
        print(f"✅ Performance data exported to: {export_file}")
    except Exception as e:
        print(f"❌ Export failed: {e}")
    
    # Final status
    total_time = time.time() - start_time
    print(f"\n🎉 Lightweight AI Demo Complete!")
    print("=" * 60)
    print(f"Total Runtime: {total_time:.1f} seconds")
    print(f"Final CPU: {metrics_samples[-1]['cpu_percent']:.1f}%")
    print(f"Final Memory: {metrics_samples[-1]['memory_percent']:.1f}%")
    
    # Recommendations
    print("\n💡 Performance Recommendations:")
    if avg_cpu > 70:
        print("   • Consider reducing computational complexity")
        print("   • Implement more efficient algorithms")
    if avg_memory > 80:
        print("   • Monitor memory usage patterns")
        print("   • Consider memory optimization techniques")
    if process_time > 100:
        print("   • AI processing time could be optimized")
        print("   • Consider caching results")
    
    print("\n🔧 Next Steps:")
    print("   • Monitor system performance continuously")
    print("   • Implement adaptive parameter tuning")
    print("   • Add predictive performance analytics")
    print("   • Consider distributed processing for heavy loads")

if __name__ == "__main__":
    demo_lightweight_ai()

