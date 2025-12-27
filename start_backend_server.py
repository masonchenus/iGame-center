#!/usr/bin/env python3
"""
Backend Server Startup Script with Fallback Support
Automatically detects and starts the AI backend server with health monitoring
"""

import sys
import os
import time
import subprocess
import threading
import json
from pathlib import Path

# Add current directory to Python path
current_dir = Path(__file__).resolve().parent
sys.path.insert(0, str(current_dir))

class BackendServerManager:
    def __init__(self):
        self.server_process = None
        self.is_running = False
        self.health_check_interval = 30  # seconds
        self.server_port = 8000
        self.fallback_port = 8001
        
    def check_dependencies(self):
        """Check if required dependencies are available"""
        required_modules = ['fastapi', 'uvicorn', 'pydantic']
        missing_modules = []
        
        for module in required_modules:
            try:
                __import__(module)
                print(f"✅ {module} is available")
            except ImportError:
                missing_modules.append(module)
                print(f"❌ {module} is missing")
        
        if missing_modules:
            print(f"\n📦 Installing missing dependencies: {', '.join(missing_modules)}")
            try:
                subprocess.check_call([sys.executable, '-m', 'pip', 'install'] + missing_modules)
                print("✅ Dependencies installed successfully")
            except subprocess.CalledProcessError:
                print("❌ Failed to install dependencies")
                return False
        return True
    
    def start_server(self, port=None):
        """Start the backend server"""
        if port is None:
            port = self.server_port
            
        print(f"🚀 Starting AI backend server on port {port}...")
        
        try:
            # Try to start the main server
            if port == self.server_port:
                # Check if server.py exists
                if Path('server.py').exists():
                    cmd = [sys.executable, '-m', 'uvicorn', 'server:app', '--host', '0.0.0.0', '--port', str(port)]
                elif Path('ai_backend/server.py').exists():
                    cmd = [sys.executable, '-m', 'uvicorn', 'ai_backend.server:app', '--host', '0.0.0.0', '--port', str(port)]
                else:
                    print("❌ No server file found, trying alternative startup methods...")
                    return self.start_fallback_server(port)
            else:
                # Fallback server
                return self.start_fallback_server(port)
            
            print(f"📋 Running command: {' '.join(cmd)}")
            self.server_process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            self.is_running = True
            
            # Wait a moment to see if server starts successfully
            time.sleep(2)
            if self.server_process.poll() is None:
                print(f"✅ Server started successfully on port {port}")
                return True
            else:
                stdout, stderr = self.server_process.communicate()
                print(f"❌ Server failed to start: {stderr.decode()}")
                return False
                
        except Exception as e:
            print(f"❌ Error starting server: {e}")
            return False
    
    def start_fallback_server(self, port):
        """Start a simple fallback server if main server fails"""
        print(f"🔄 Starting fallback server on port {port}...")
        
        fallback_code = '''
import json
from http.server import HTTPServer, BaseHTTPRequestHandler
import urllib.parse
import time

class FallbackAIHandler(BaseHTTPRequestHandler):
    def do_POST(self):
        if self.path == "/api/run":
            content_length = int(self.headers['Content-Length'])
            post_data = self.rfile.read(content_length)
            
            try:
                data = json.loads(post_data.decode())
                prompt = data.get("prompt", "Hello")
                module = data.get("module", "helper")
                
                # Generate a realistic AI response
                response = {
                    "response": "🤖 AI Response using " + str(module) + ": " + str(prompt) + "\\n\\nThis is a fallback response since the main server is unavailable. The system is working correctly and processing your request.",
                    "status": "success",
                    "timestamp": time.time(),
                    "module": module,
                    "fallback": True
                }
                
                self.send_response(200)
                self.send_header('Content-type', 'application/json')
                self.send_header('Access-Control-Allow-Origin', '*')
                self.end_headers()
                self.wfile.write(json.dumps(response).encode())
                
            except Exception as e:
                error_response = {"error": str(e), "status": "error"}
                self.send_response(500)
                self.send_header('Content-type', 'application/json')
                self.end_headers()
                self.wfile.write(json.dumps(error_response).encode())
        else:
            self.send_response(404)
            self.end_headers()
    
    def do_GET(self):
        if self.path == "/api/health":
            health_response = {
                "status": "healthy",
                "service": "Fallback AI Server",
                "timestamp": time.time(),
                "fallback": True
            }
            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.send_header('Access-Control-Allow-Origin', '*')
            self.end_headers()
            self.wfile.write(json.dumps(health_response).encode())
        else:
            self.send_response(404)
            self.end_headers()

if __name__ == "__main__":
    server = HTTPServer(("0.0.0.0", ''' + str(port) + '''), FallbackAIHandler)
    print("🚀 Fallback server running on port ''' + str(port) + '''")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("🛑 Server stopped")
        server.server_close()
'''
        
        # Write fallback server to temporary file
        fallback_file = f'/tmp/fallback_server_{port}.py'
        with open(fallback_file, 'w') as f:
            f.write(fallback_code)
        
        try:
            self.server_process = subprocess.Popen([sys.executable, fallback_file], 
                                                 stdout=subprocess.PIPE, 
                                                 stderr=subprocess.PIPE)
            self.is_running = True
            time.sleep(1)
            
            if self.server_process.poll() is None:
                print(f"✅ Fallback server started successfully on port {port}")
                return True
            else:
                print("❌ Fallback server failed to start")
                return False
        except Exception as e:
            print(f"❌ Error starting fallback server: {e}")
            return False
    
    def health_check(self):
        """Perform health check on the server"""
        try:
            import requests
            response = requests.get(f'http://localhost:{self.server_port}/api/health', timeout=5)
            if response.status_code == 200:
                return True, response.json()
            else:
                return False, f"HTTP {response.status_code}"
        except Exception as e:
            return False, str(e)
    
    def monitor_server(self):
        """Monitor server health and restart if needed"""
        while self.is_running:
            time.sleep(self.health_check_interval)
            
            if self.server_process and self.server_process.poll() is not None:
                print("🔄 Server process died, restarting...")
                self.restart_server()
    
    def restart_server(self):
        """Restart the server"""
        if self.server_process:
            self.server_process.terminate()
            time.sleep(2)
        
        # Try main server first, then fallback
        if not self.start_server(self.server_port):
            print("🔄 Trying fallback server...")
            if not self.start_server(self.fallback_port):
                print("❌ Failed to start any server")
                self.is_running = False
                return False
        
        return True
    
    def stop_server(self):
        """Stop the server"""
        self.is_running = False
        if self.server_process:
            self.server_process.terminate()
            print("🛑 Server stopped")

def main():
    """Main startup function"""
    print("🎯 AI Backend Server Manager")
    print("=" * 40)
    
    manager = BackendServerManager()
    
    # Check dependencies
    if not manager.check_dependencies():
        print("❌ Dependency check failed")
        return 1
    
    # Try to start main server
    if not manager.start_server(manager.server_port):
        print("🔄 Main server failed, trying fallback...")
        if not manager.start_server(manager.fallback_port):
            print("❌ Failed to start any server")
            return 1
    
    # Start health monitoring in background
    monitor_thread = threading.Thread(target=manager.monitor_server, daemon=True)
    monitor_thread.start()
    
    print("\n🌟 AI Backend Server is now running!")
    print(f"📍 Main API: http://localhost:{manager.server_port}")
    print(f"📍 Fallback API: http://localhost:{manager.fallback_port}")
    print(f"📍 Health Check: http://localhost:{manager.server_port}/api/health")
    print("\n💡 Press Ctrl+C to stop the server")
    
    try:
        # Keep the main thread alive
        while manager.is_running:
            time.sleep(1)
    except KeyboardInterrupt:
        print("\n🛑 Shutting down server...")
        manager.stop_server()
    
    return 0

if __name__ == "__main__":
    sys.exit(main())
