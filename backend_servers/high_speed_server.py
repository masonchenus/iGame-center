"""
High-Speed AI Server
Ultra-fast server optimized for maximum performance
"""

import uvicorn
import logging
from pathlib import Path
import sys
from ai_backend.high_performance_orchestrator import create_high_performance_app, fast_orchestrator

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("high_speed_server")

def main():
    """Start the ultra-fast AI server"""
    
    print("🚀 Starting HIGH-SPEED AI Server...")
    print("⚡ Optimized for maximum performance")
    print("🔥 Developer users get priority fast lane")
    print("💾 Intelligent response caching enabled")
    print("⚙️  Timeout protection: 10 seconds")
    
    # Create the high-performance app
    app = create_high_performance_app()
    
    # Configure uvicorn for maximum speed
    config = uvicorn.Config(
        app,
        host="0.0.0.0",
        port=8000,
        log_level="info",
        access_log=True,
        loop="auto",
        http="httptools",  # Fast HTTP parser
        lifespan="on",
        workers=1,  # Single worker for better performance
        timeout_keep_alive=30,
        max_requests=10000,  # Restart after 10K requests
        max_requests_jitter=1000,
        limit_concurrency=1000,  # Allow high concurrency
        limit_max_requests=50000,  # Allow 50K requests before restart
    )
    
    server = uvicorn.Server(config)
    
    try:
        logger.info("🔥 HIGH-SPEED AI Server starting...")
        print("\n" + "="*50)
        print("🚀 HIGH-SPEED AI SERVER READY")
        print("="*50)
        print("📍 Server: http://localhost:8000")
        print("⚡ Performance Mode: ULTRA-FAST")
        print("🎯 Fast Lane: Developer users (masonchenus, dev, owner)")
        print("💾 Cache: Intelligent response caching")
        print("🛡️ Security: Full protection maintained")
        print("\n📊 Endpoints:")
        print("   GET  /health")
        print("   POST /api/run (Ultra-fast AI processing)")
        print("   POST /api/run-sync (Synchronous for maximum speed)")
        print("   GET  /api/performance/metrics")
        print("="*50)
        
        server.run()
        
    except KeyboardInterrupt:
        logger.info("🛑 Server shutdown requested")
        print("\n⚡ High-speed server stopped")
    except Exception as e:
        logger.error(f"❌ Server error: {e}")
        print(f"❌ Server failed to start: {e}")

if __name__ == "__main__":
    main()

