#!/bin/bash
# Quick Backend Server Startup Script

echo "🚀 Starting AI Backend Server..."
echo "=================================="

# Kill any existing server processes on ports 8000 or 8001
echo "🧹 Cleaning up existing processes..."
lsof -ti:8000 | xargs kill -9 2>/dev/null || true
lsof -ti:8001 | xargs kill -9 2>/dev/null || true

# Start the backend server with fallback support
echo "🎯 Starting backend server manager..."
python3 start_backend_server.py

echo "✅ Server startup complete!"
