#!/bin/bash

# Enhanced Game Center Server Startup Script
echo "🚀 Starting Enhanced Game Center..."

# Function to cleanup background processes
cleanup() {
    echo "🛑 Stopping servers..."
    kill $FASTAPI_PID $FRONTEND_PID 2>/dev/null
    exit
}

# Set trap to cleanup on script exit
trap cleanup SIGINT SIGTERM

# Start FastAPI Backend Server
echo "🔧 Starting FastAPI backend server on http://localhost:8000..."
cd /Users/mason/Game Center Project
uvicorn server:app --host 0.0.0.0 --port 8000 --reload &
FASTAPI_PID=$!

# Wait a moment for FastAPI to start
sleep 3

# Start Frontend Static Server (for development)
echo "🌐 Starting frontend server on http://localhost:8080..."
python3 -m http.server 8080 &
FRONTEND_PID=$!

echo ""
echo "✅ Enhanced Game Center is running!"
echo "📊 API Server: http://localhost:8000"
echo "🎮 Frontend: http://localhost:8080"
echo "📖 API Docs: http://localhost:8000/docs"
echo ""
echo "Press Ctrl+C to stop all servers"

# Keep script running
wait
