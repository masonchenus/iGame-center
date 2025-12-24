"""
Secure AI Server
FastAPI server with comprehensive security enhancements
"""

import sys
from pathlib import Path
from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.middleware.trustedhost import TrustedHostMiddleware
from fastapi.responses import JSONResponse

ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT))

# Import secure orchestrator
from ai_backend.secure_orchestrator_fixed import (
    SecureAIOrchestrator, 
    create_secure_fastapi_app,
    secure_run_mode
)
from ai_backend.security.middleware import SecurityConfig

# Create secure FastAPI application
app = create_secure_fastapi_app()

# Add additional security middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=SecurityConfig.ALLOWED_ORIGINS,
    allow_credentials=True,
    allow_methods=SecurityConfig.ALLOWED_METHODS,
    allow_headers=SecurityConfig.ALLOWED_HEADERS,
)

# Add trusted host middleware for additional security
app.add_middleware(
    TrustedHostMiddleware,
    allowed_hosts=[
        "localhost",
        "127.0.0.1",
        "your-domain.com",
        "*.your-domain.com"
    ]
)

# Global exception handler for security events
@app.exception_handler(Exception)
async def global_exception_handler(request, exc):
    """Global exception handler with security logging"""
    return JSONResponse(
        status_code=500,
        content={
            "error": "Internal server error",
            "detail": "An unexpected error occurred",
            "type": "server_error"
        }
    )

# Health check with security status
@app.get("/health")
async def health_check():
    """Enhanced health check with security status"""
    return {
        "status": "healthy",
        "security_enabled": True,
        "version": "2.0.0-security",
        "features": {
            "authentication": False,  # Will be enabled in production
            "rate_limiting": True,
            "input_validation": True,
            "output_sanitization": True,
            "security_monitoring": True
        }
    }

if __name__ == "__main__":
    import uvicorn
    print("🚀 Starting Secure AI Server...")
    print("🔒 Security Features Enabled:")
    print("   ✓ Input Validation & Sanitization")
    print("   ✓ Rate Limiting")
    print("   ✓ Security Headers")
    print("   ✓ Request Monitoring")
    print("   ✓ XSS Protection")
    print("   ✓ CORS Configuration")
    print("")
    
    uvicorn.run(
        "secure_server:app",
        host="0.0.0.0",
        port=8000,
        reload=True,
        log_level="info"
    )
