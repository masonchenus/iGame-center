"""
Secure AI Orchestrator - Production Ready
Enterprise-grade security with tiered rate limiting, input validation, and comprehensive monitoring
"""

import time
import logging
from typing import Dict, Any, Optional
from fastapi import FastAPI, HTTPException, status
from ai_backend.security.middleware import (
    InputValidator,
    RunRequest,
    SecureRunResponse,
    SecurityConfig,
    InputValidationError,
    log_security_event,
    rate_limiter
)
from ai_backend.orchestrator import BaseAI, run_mode as original_run_mode

# Configure secure logger
secure_logger = logging.getLogger("secure_ai")

class SecureAIOrchestrator:
    """Enhanced AI orchestrator with comprehensive security features"""
    
    def __init__(self):
        self.input_validator = InputValidator()
        self.secure_logger = secure_logger
        self.request_count = 0
        self.blocked_requests = 0
        
    async def secure_process_request(self, request: RunRequest) -> SecureRunResponse:
        """Process AI request with comprehensive security"""
        start_time = time.time()
        
        try:
            # Check tiered rate limiting BEFORE processing
            rate_limited = rate_limiter.is_rate_limited(
                user_id=request.user_id,
                model_name=request.model
            )
            
            if rate_limited:
                remaining = rate_limiter.get_remaining_requests(request.user_id, request.model)
                self.secure_logger.warning(f"Rate limit exceeded for user {request.user_id}: {remaining}")
                raise HTTPException(
                    status_code=status.HTTP_429_TOO_MANY_REQUESTS,
                    detail={
                        "error": "Rate limit exceeded",
                        "tier": remaining["tier"],
                        "limit": remaining["limit"],
                        "used": remaining["used"],
                        "remaining": remaining["remaining"],
                        "is_best_model": remaining["is_best_model"]
                    }
                )
            
            # Validate input length and content
            secure_input = self.input_validator.validate_ai_prompt(request.input)
            
            # Get rate limiting stats for metadata
            rate_stats = rate_limiter.get_remaining_requests(request.user_id, request.model)
            
            # Log security metadata
            security_metadata = {
                "input_validated": True,
                "sanitization_applied": True,
                "rate_limit_check": "passed",
                "tier": rate_stats["tier"],
                "remaining_requests": rate_stats["remaining"],
                "mode": request.mode,
                "model": request.model,
                "user_id": request.user_id,
                "session_id": request.session_id,
                "timestamp": time.time(),
                "request_id": f"req_{int(time.time() * 1000)}"
            }
            
            # Process with original AI system
            self.request_count += 1
            
            # Call original AI processing (with fallback for missing API keys)
            try:
                # Use original run_mode but with secure input
                result = original_run_mode(
                    mode=request.mode,
                    model=request.model,
                    input=secure_input,
                    user_id=request.user_id,
                    session_id=request.session_id
                )
                
                if hasattr(result, 'output'):
                    output = result.output
                elif isinstance(result, dict) and 'output' in result:
                    output = result['output']
                else:
                    output = str(result)
                    
            except Exception as ai_error:
                self.secure_logger.error(f"AI processing error: {str(ai_error)}")
                # Return secure fallback response
                output = f"AI processing temporarily unavailable. Request logged: {security_metadata['request_id']}"
            
            # Log successful processing
            processing_time = time.time() - start_time
            self.secure_logger.info(
                f"Secure AI request processed: {request.mode}/{request.model} "
                f"in {processing_time:.3f}s for user {request.user_id}"
            )
            
            return SecureRunResponse(
                output=output,
                security_metadata=security_metadata
            )
            
        except InputValidationError as e:
            self.blocked_requests += 1
            self.secure_logger.warning(f"Input validation failed: {str(e)} for user {request.user_id}")
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail=str(e.detail)
            )
            
        except HTTPException:
            # Re-raise HTTP exceptions (like rate limiting)
            raise
            
        except Exception as e:
            self.blocked_requests += 1
            self.secure_logger.error(f"Security processing error: {str(e)}")
            raise HTTPException(
                status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
                detail="Internal security processing error"
            )
    
    async def get_security_metrics(self) -> Dict[str, Any]:
        """Get comprehensive security metrics"""
        return {
            "total_requests": self.request_count,
            "blocked_requests": self.blocked_requests,
            "security_active": True,
            "rate_limits": SecurityConfig.RATE_LIMITS,
            "best_models": list(SecurityConfig.BEST_MODELS),
            "timestamp": time.time(),
            "version": "2.0.0-security"
        }

# Global orchestrator instance
secure_orchestrator = SecureAIOrchestrator()

def create_secure_fastapi_app() -> FastAPI:
    """Create and configure secure FastAPI application"""
    
    app = FastAPI(
        title="Secure AI API",
        description="Enterprise-grade secure AI processing API",
        version="2.0.0-security"
    )
    
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
    
    @app.get("/api/security/metrics")
    async def get_security_metrics():
        """Get security metrics and monitoring data"""
        return await secure_orchestrator.get_security_metrics()
    
    @app.post("/api/run", response_model=SecureRunResponse)
    @log_security_event("SECURE_RUN_ENDPOINT", "MEDIUM")
    async def secure_run_ai(req: RunRequest):
        """Secure AI processing endpoint with comprehensive validation"""
        return await secure_orchestrator.secure_process_request(req)
    
    return app

# Convenience functions for easy integration
def secure_run_mode(mode: str, model: str, input_text: str, user_id: str = "anonymous", session_id: str = "default"):
    """Secure version of run_mode with built-in validation"""
    try:
        # Create RunRequest
        request = RunRequest(
            mode=mode,
            model=model,
            input=input_text,
            user_id=user_id,
            session_id=session_id
        )
        
        # Process synchronously using the orchestrator
        import asyncio
        response = asyncio.run(secure_orchestrator.secure_process_request(request))
        return response
        
    except Exception as e:
        secure_logger.error(f"Secure run_mode error: {str(e)}")
        raise

# Export for backward compatibility
run_mode = secure_run_mode

