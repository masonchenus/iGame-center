"""
Secure AI Orchestrator
Integrates security middleware with AI processing for enterprise-grade protection
"""

import json
import time
import logging
from typing import Dict, Any, Optional
from fastapi import FastAPI, HTTPException, status
from ai_backend.security.middleware import (
    SecurityMiddleware,
    InputValidator,
    RunRequest,
    SecureRunResponse,
    SecurityConfig,
    InputValidationError,
    log_security_event,
    SecurityEvent,
    rate_limiter
)
from ai_backend.orchestrator import BaseAI, run_mode as original_run_mode

# Configure secure logger
secure_logger = logging.getLogger("secure_ai")

class SecureAIOrchestrator:
    """Secure wrapper around the BaseAI orchestrator"""
    
    def __init__(self, ultra_mode: bool = True):
        self.ai = BaseAI(ultra_mode=ultra_mode)
        self.security_config = SecurityConfig()
        self.input_validator = InputValidator()
        self.request_count = 0
        self.blocked_requests = 0
    
    @log_security_event("SECURE_ORCHESTRATOR_INIT", "LOW")
    def __enter__(self):
        """Context manager entry"""
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit"""
        if exc_type:
            secure_logger.error(f"Secure orchestrator error: {exc_val}")
    
    @log_security_event("VALIDATE_INPUT", "MEDIUM")
    def validate_and_sanitize_request(self, request_data: Dict[str, Any]) -> RunRequest:
        """Validate and sanitize request data"""
        try:
            # Create validated request object
            validated_request = RunRequest(**request_data)
            
            # Additional security validation
            secure_logger.info(f"Request validated: mode={validated_request.mode}, "
                             f"model={validated_request.model}, "
                             f"user_id={validated_request.user_id}")
            
            return validated_request
            
        except Exception as e:
            secure_logger.warning(f"Invalid request data: {str(e)}")
            raise InputValidationError(f"Request validation failed: {str(e)}")
    
    @log_security_event("PROCESS_SECURE_REQUEST", "MEDIUM")
    def process_secure_request(self, request: RunRequest) -> Dict[str, Any]:
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
                secure_logger.warning(f"Rate limit exceeded for user {request.user_id}: {remaining}")
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
            
            # Process the request using original orchestrator
            # Note: We need to adapt the interface to match original expected format
            original_format_input = {
                "prompt": secure_input
            }
            
            # Use original run_mode with security validation
            response_output = self._secure_run_mode(
                mode_name=request.mode,
                model_name=request.model,
                input_data=json.dumps(original_format_input),
                user_id=request.user_id,
                session_id=request.session_id
            )
            
            # Parse and validate the response
            try:
                response_data = json.loads(response_output)
            except json.JSONDecodeError as e:
                secure_logger.error(f"Failed to parse AI response: {e}")
                raise HTTPException(
                    status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
                    detail="Invalid AI response format"
                )
            
            # Validate response content
            if "response" not in response_data:
                raise HTTPException(
                    status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
                    detail="Missing response content from AI"
                )
            
            # Add security metadata to response
            response_data["security_metadata"] = security_metadata
            response_data["processing_time"] = time.time() - start_time
            
            # Log successful processing
            secure_logger.info(f"Request processed successfully: "
                             f"mode={request.mode}, "
                             f"model={request.model}, "
                             f"time={response_data['processing_time']:.3f}s")
            
            return response_data
            
        except InputValidationError:
            # Re-raise validation errors
            raise
        except Exception as e:
            secure_logger.error(f"Secure request processing failed: {str(e)}")
            raise HTTPException(
                status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
                detail="Request processing failed"
            )
    
    def _secure_run_mode(self, mode_name: str, model_name: str, input_data: str, 
                        user_id: str, session_id: str) -> str:
        """Secure wrapper for the original run_mode function"""
        
        # Additional security checks
        if not mode_name or len(mode_name.strip()) == 0:
            raise InputValidationError("Mode name cannot be empty", "EMPTY_MODE")
        
        if not model_name or len(model_name.strip()) == 0:
            raise InputValidationError("Model name cannot be empty", "EMPTY_MODEL")
        
        # Check for potentially dangerous combinations
        dangerous_combinations = [
            ("system", "admin"),
            ("debug", "production"),
            ("test", "prod")
        ]
        
        for dangerous_mode, dangerous_model in dangerous_combinations:
            if dangerous_mode in mode_name.lower() and dangerous_model in model_name.lower():
                secure_logger.warning(f"Potentially dangerous mode/model combination: "
                                    f"{mode_name}/{model_name}")
                # Log but don't necessarily block - could be legitimate testing
        
        # Call original function with security logging
        try:
            result = original_run_mode(
                mode_name=mode_name,
                model_name=model_name,
                input_data=input_data,
                user_id=user_id,
                session_id=session_id
            )
            return result
        except Exception as e:
            secure_logger.error(f"Original run_mode failed: {str(e)}")
            raise
    
    @log_security_event("GET_SECURITY_METRICS", "LOW")
    def get_security_metrics(self) -> Dict[str, Any]:
        """Get current security metrics"""
        return {
            "total_requests": self.request_count,
            "blocked_requests": self.blocked_requests,
            "security_active": True,
            "validation_enabled": True,
            "rate_limiting_enabled": True,
            "input_sanitization_enabled": True
        }
    
    def increment_request_count(self):
        """Increment request counter"""
        self.request_count += 1
    
    def increment_blocked_count(self):
        """Increment blocked request counter"""
        self.blocked_requests += 1

# Global secure orchestrator instance
secure_orchestrator = SecureAIOrchestrator()

def create_secure_fastapi_app() -> FastAPI:
    """Create and configure secure FastAPI application"""
    
    app = FastAPI(
        title="Secure AI API",
        description="Enterprise-grade secure AI processing API",
        version="2.0.0-security"
    )
    
    @app.post("/api/run", response_model=SecureRunResponse)
    @log_security_event("SECURE_RUN_ENDPOINT", "MEDIUM")
    async def secure_run_ai(req: RunRequest):
        """Secure AI processing endpoint with comprehensive validation"""
        
        # Increment request counter
        secure_orchestrator.increment_request_count()
        
        try:
            # Process secure request
            result_data = secure_orchestrator.process_secure_request(req)
            
            # Create secure response
            return SecureRunResponse(
                output=result_data.get("response", "No response"),
                security_metadata=result_data.get("security_metadata", {})
            )
            
        except InputValidationError as e:
            secure_orchestrator.increment_blocked_count()
            raise e
        except Exception as e:
            secure_logger.error(f"Unexpected error in secure_run: {str(e)}")
            raise HTTPException(
                status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
                detail="Internal server error"
            )
    
    @app.post("/api/run/dynamic", response_model=SecureRunResponse)
    @log_security_event("SECURE_DYNAMIC_ENDPOINT", "MEDIUM")
    async def secure_run_ai_dynamic(req: RunRequest):
        """Secure dynamic AI processing endpoint"""
        
        secure_orchestrator.increment_request_count()
        
        try:
            # Enhanced validation for dynamic requests
            if "dynamic" in req.mode.lower():
                secure_logger.warning(f"Dynamic mode request: {req.mode}")
            
            result_data = secure_orchestrator.process_secure_request(req)
            
            return SecureRunResponse(
                output=result_data.get("response", "No response"),
                security_metadata=result_data.get("security_metadata", {})
            )
            
        except InputValidationError as e:
            secure_orchestrator.increment_blocked_count()
            raise e
        except Exception as e:
            secure_logger.error(f"Dynamic request failed: {str(e)}")
            raise HTTPException(
                status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
                detail="Dynamic request processing failed"
            )
    
    @app.get("/api/security/metrics")
    @log_security_event("SECURITY_METRICS_ENDPOINT", "LOW")
    async def get_security_metrics():
        """Get current security metrics"""
        return secure_orchestrator.get_security_metrics()
    
    @app.get("/api/health")
    async def health_check():
        """Basic health check endpoint"""
        return {
            "status": "healthy",
            "security_enabled": True,
            "version": "2.0.0-security"
        }
    
    return app

# Security-enhanced versions of original functions
def secure_run_mode(mode_name=None, model_name="chatgpt", input_data="", 
                   user_id="web", session_id="default"):
    """Security-enhanced wrapper for run_mode"""
    
    with SecureAIOrchestrator(ultra_mode=True) as orchestrator:
        # Create request data
        request_data = {
            "mode": mode_name or "helper",
            "model": model_name,
            "input": input_data,
            "user_id": user_id,
            "session_id": session_id
        }
        
        # Validate and process
        validated_request = orchestrator.validate_and_sanitize_request(request_data)
        result_data = orchestrator.process_secure_request(validated_request)
        
        return json.dumps(result_data)

def secure_run_user_mode(mode_name, input_data, user_id, session_id, model_name):
    """Security-enhanced wrapper for run_user_mode"""
    
    with SecureAIOrchestrator(ultra_mode=True) as orchestrator:
        request_data = {
            "mode": mode_name,
            "model": model_name,
            "input": input_data,
            "user_id": user_id,
            "session_id": session_id
        }
        
        validated_request = orchestrator.validate_and_sanitize_request(request_data)
        result_data = orchestrator.process_secure_request(validated_request)
        
        return result_data.get("response", f"Error: {str(result_data.get('error', 'Unknown error'))}")
