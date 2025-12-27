#!/usr/bin/env python3
"""
Enhanced AI Server Startup Script
Starts the FastAPI server with all AI tools and features
"""

import os
import sys
import uvicorn
import subprocess
from pathlib import Path

# Add the project root to Python path
ROOT_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT_DIR))

def check_dependencies():
    """Check if required dependencies are installed"""
    required_packages = [
        'fastapi',
        'uvicorn',
        'pydantic',
        'aiofiles',
        'jinja2'
    ]
    
    missing_packages = []
    for package in required_packages:
        try:
            __import__(package)
        except ImportError:
            missing_packages.append(package)
    
    if missing_packages:
        print(f"Missing packages: {', '.join(missing_packages)}")
        print("Installing missing packages...")
        subprocess.check_call([
            sys.executable, "-m", "pip", "install"
        ] + missing_packages)
        print("Dependencies installed successfully!")

def setup_directories():
    """Create necessary directories"""
    directories = [
        "ai_backend/logs",
        "ai_backend/cache",
        "temp",
        "generated_code"
    ]
    
    for directory in directories:
        Path(directory).mkdir(parents=True, exist_ok=True)
        print(f"Created directory: {directory}")

def start_server():
    """Start the FastAPI server"""
    print("🚀 Starting Enhanced AI Server...")
    print("📍 Server will be available at: http://localhost:8000")
    print("📖 API Documentation: http://localhost:8000/docs")
    print("🔧 Tools API: http://localhost:8000/api/tools")
    print("💬 Enhanced Chat: http://localhost:8000/chat")
    print("\n" + "="*50)
    
    # Configure uvicorn
    config = uvicorn.Config(
        "server:app",
        host="127.0.0.1",
        port=8000,
        reload=True,
        log_level="info"
    )
    
    server = uvicorn.Server(config)
    
    try:
        server.run()
    except KeyboardInterrupt:
        print("\n🛑 Server stopped by user")
    except Exception as e:
        print(f"❌ Server error: {e}")
        return 1
    
    return 0

def main():
    """Main function"""
    print("🧠 Enhanced AI System Startup")
    print("=" * 40)
    
    # Check current directory
    if not Path("server.py").exists():
        print("❌ Error: server.py not found in current directory")
        print("Please run this script from the project root directory")
        return 1
    
    # Setup environment
    print("🔧 Checking dependencies...")
    check_dependencies()
    
    print("📁 Setting up directories...")
    setup_directories()
    
    # Start server
    print("🚀 Starting server...")
    return start_server()

if __name__ == "__main__":
    exit_code = main()
    sys.exit(exit_code)

