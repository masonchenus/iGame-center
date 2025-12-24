"""
Security Middleware for AI Backend
Provides authentication, input validation, rate limiting, and security headers
"""
import time
import secrets
import hashlib
import hmac
import jwt
from typing import Dict, List, Optional, Any
from fastapi import Request, HTTPException, status, Depends
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from pydantic import BaseModel, Field, validator
import re
import bleach
from functools import wraps
import logging

# Configure security logger
security_logger = logging.getLogger("security")

class SecurityConfig:
    """Security configuration settings"""
    JWT_SECRET_KEY = "CHANGE_THIS_TO_ENVIRONMENT_VARIABLE_IN_PRODUCTION"
    JWT_ALGORITHM = "HS256"
    JWT_EXPIRATION_MINUTES = 30
    
    # Tiered rate limiting (requests per day by user tier)
    RATE_LIMITS = {
        "developer": 800000,    # Dev/Owner tier
        "free": 300,            # Free tier
        "free_best": 30,        # Free tier for best models
        "pro": 1200,            # Pro tier
        "pro_best": 600,        # Pro tier for best models  
        "ultra": 50000,         # Ultra tier
        "ultra_best": 30000,    # Ultra tier for best models
        "default": 100          # Default fallback
    }
    
    # Best models list
    BEST_MODELS = {"gpt-4", "gpt-5", "claude-3-opus", "claude-3.5-sonnet", "gemini-ultra", "grok-premium"}
    
    # Input validation limits
    MAX_INPUT_LENGTH = 10000
    MAX_RESPONSE_LENGTH = 50000
    BANNED_PATTERNS = [
        r'<script[^>]*>.*?</script>',  # XSS
        r'javascript:',  # JavaScript protocol
        r'on\w+\s*=',  # Event handlers
        r'union\s+select',  # SQL injection
        r'eval\s*\(',  # Code injection
        r'exec\s*\(',  # Code execution
        r'system\s*\(',  # System calls
        r'shell_exec',  # Shell execution
        r'passwd',  # Password files
        r'/etc/passwd',  # Passwd file access
        r'..\/',  # Path traversal
        r'\.\.\%2F',  # URL encoded path traversal
    ]
    
    # CORS settings
    ALLOWED_ORIGINS = [
        "https://your-domain.com",
        "http://localhost:3000",  # Development
        "http://localhost:8080",  # Development
    ]
    
    ALLOWED_METHODS = ["GET", "POST", "OPTIONS"]
    ALLOWED_HEADERS = ["*"]

class InputValidationError(HTTPException):
    """Custom exception for input validation failures"""
    def __init__(self, detail: str, error_code: str = "INVALID_INPUT"):
        super().__init__(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail={
                "error": detail,
                "error_code": error_code,
                "type": "validation_error"
            }
        )

class SecurityEvent(BaseModel):
    """Security event logging model"""
    timestamp: float
    event_type: str
    severity: str  # LOW, MEDIUM, HIGH, CRITICAL
    source_ip: str
    user_id: Optional[str] = None
    endpoint: Optional[str] = None
    details: Dict[str, Any] = {}

class TieredRateLimiter:
    """Tiered rate limiter supporting different user tiers and model types"""
    
    def __init__(self):
        self.requests: Dict[str, Dict[str, List[float]]] = {}
        self.user_tiers: Dict[str, str] = {}  # user_id -> tier
        self.daily_counts: Dict[str, Dict[str, int]] = {}  # user_id -> {tier: count}
    
    def get_user_tier(self, user_id: str, user_agent: str = "") -> str:
        """Determine user tier based on user ID and request patterns"""
        
        # Developer/Owner detection (you can customize this)
        if user_id in ["dev", "developer", "owner", "admin"] or "masonchenus" in user_id:
            return "developer"
        
        # Check if user tier is cached
        if user_id in self.user_tiers:
            return self.user_tiers[user_id]
        
        # In a real implementation, this would check your billing database
        # For now, we'll use a simple heuristic based on user_id patterns
        if user_id.startswith("ultra_"):
            tier = "ultra"
        elif user_id.startswith("pro_"):
            tier = "pro"
        elif user_id.startswith("free_"):
            tier = "free"
        else:
            tier = "free"  # Default tier
        
        self.user_tiers[user_id] = tier
        return tier
    
    def is_best_model(self, model_name: str) -> bool:
        """Check if the model is considered a 'best' model"""
        model_lower = model_name.lower()
        return any(best_model in model_lower for best_model in SecurityConfig.BEST_MODELS)
    
    def get_rate_limit(self, user_id: str, model_name: str) -> int:
        """Get the appropriate rate limit for a user and model combination"""
        tier = self.get_user_tier(user_id)
        is_best = self.is_best_model(model_name)
        
        if is_best and tier in ["free", "pro"]:
            # Use best model limits for free/pro tiers
            return SecurityConfig.RATE_LIMITS.get(f"{tier}_best", SecurityConfig.RATE_LIMITS["default"])
        
        return SecurityConfig.RATE_LIMITS.get(tier, SecurityConfig.RATE_LIMITS["default"])
    
    def is_rate_limited(self, user_id: str, model_name: str, window_hours: int = 24) -> bool:
        """Check if user is rate limited for their tier and model"""
        now = time.time()
        window_seconds = window_hours * 3600
        
        # Initialize user data if needed
        if user_id not in self.requests:
            self.requests[user_id] = {}
        if user_id not in self.daily_counts:
            self.daily_counts[user_id] = {}
        
        tier = self.get_user_tier(user_id)
        is_best = self.is_best_model(model_name)
        
        # Get appropriate limit
        if is_best and tier in ["free", "pro"]:
            rate_key = f"{tier}_best"
        else:
            rate_key = tier
        
        limit = SecurityConfig.RATE_LIMITS.get(rate_key, SecurityConfig.RATE_LIMITS["default"])
        
        # Initialize daily count for this tier
        if rate_key not in self.daily_counts[user_id]:
            self.daily_counts[user_id][rate_key] = 0
        
        # Check if we've exceeded the daily limit
        if self.daily_counts[user_id][rate_key] >= limit:
            return True
        
        # Add current request
        self.daily_counts[user_id][rate_key] += 1
        
        return False
    
    def get_remaining_requests(self, user_id: str, model_name: str) -> Dict[str, int]:
        """Get remaining requests for a user and model"""
        tier = self.get_user_tier(user_id)
        is_best = self.is_best_model(model_name)
        
        if is_best and tier in ["free", "pro"]:
            rate_key = f"{tier}_best"
        else:
            rate_key = tier
        
        limit = SecurityConfig.RATE_LIMITS.get(rate_key, SecurityConfig.RATE_LIMITS["default"])
        
        if user_id in self.daily_counts and rate_key in self.daily_counts[user_id]:
            used = self.daily_counts[user_id][rate_key]
        else:
            used = 0
        
        return {
            "remaining": max(0, limit - used),
            "limit": limit,
            "used": used,
            "tier": tier,
            "is_best_model": is_best
        }
    
    def reset_daily_counts(self):
        """Reset daily request counts (call this daily)"""
        self.daily_counts.clear()
    
    def get_user_stats(self, user_id: str) -> Dict[str, Any]:
        """Get comprehensive user statistics"""
        tier = self.get_user_tier(user_id)
        return {
            "user_id": user_id,
            "tier": tier,
            "daily_usage": self.daily_counts.get(user_id, {}),
            "rate_limits": SecurityConfig.RATE_LIMITS,
            "best_models": list(SecurityConfig.BEST_MODELS)
        }

# Global rate limiter instance
rate_limiter = TieredRateLimiter()

class InputValidator:
    """Comprehensive input validation and sanitization"""
    
    @staticmethod
    def sanitize_html(text: str) -> str:
        """Sanitize HTML content"""
        return bleach.clean(text, tags=[], strip=True)
    
    @staticmethod
    def validate_input_length(text: str, max_length: int) -> str:
        """Validate and truncate input length"""
        if len(text) > max_length:
            raise InputValidationError(
                f"Input too long. Maximum {max_length} characters allowed.",
                "INPUT_TOO_LONG"
            )
        return text
    
    @staticmethod
    def check_banned_patterns(text: str) -> None:
        """Check for banned patterns in input"""
        for pattern in SecurityConfig.BANNED_PATTERNS:
            if re.search(pattern, text, re.IGNORECASE | re.DOTALL):
                raise InputValidationError(
                    f"Input contains forbidden pattern: {pattern}",
                    "FORBIDDEN_PATTERN"
                )
    
    @staticmethod
    def validate_ai_prompt(prompt: str) -> str:
        """Specialized validation for AI prompts"""
        # Basic length check
        prompt = InputValidator.validate_input_length(prompt, SecurityConfig.MAX_INPUT_LENGTH)
        
        # Check for banned patterns
        InputValidator.check_banned_patterns(prompt)
        
        # Sanitize HTML
        prompt = InputValidator.sanitize_html(prompt)
        
        # Additional AI-specific checks
        if len(prompt.strip()) < 1:
            raise InputValidationError("Prompt cannot be empty", "EMPTY_PROMPT")
        
        # Check for potential prompt injection
        injection_patterns = [
            r'ignore\s+previous\s+instructions',
            r'disregard\s+above\s+instructions',
            r'system\s*:\s*',
            r'human\s*:\s*',
            r'assistant\s*:\s*',
            r'you\s+are\s+now\s+',
            r'pretend\s+you\s+are\s+',
        ]
        
        for pattern in injection_patterns:
            if re.search(pattern, prompt, re.IGNORECASE):
                security_logger.warning(f"Potential prompt injection detected: {pattern}")
                # Don't reject, but log for monitoring
                break
        
        return prompt

class RunRequest(BaseModel):
    """Enhanced input validation for AI requests"""
    mode: str = Field(..., min_length=1, max_length=50)
    model: str = Field(..., min_length=1, max_length=50)
    input: str = Field(..., min_length=1, max_length=10000)
    user_id: str = Field(default="anonymous", min_length=1, max_length=100)
    session_id: str = Field(default="default", min_length=1, max_length=100)
    
    @validator('mode')
    def validate_mode(cls, v):
        allowed_modes = {
            'game', 'math', 'slides', 'helper', 'agent', 'tester', 'codegen',
            'explain', 'summarizer', 'story', 'dialogue', 'brainstorming',
            'outline', 'quiz', 'trivia', 'fact-check', 'translator',
            'lyrics', 'poem', 'joke', 'cheat-sheet', 'code-review',
            'optimization', 'design', 'diagram', 'debug', 'math-explain',
            'data-analysis', 'recommendation', 'productivity', 'health',
            'research', 'coding-challenge', 'game-tips', 'ranking',
            'simulation', 'test-creator', 'logic-puzzle', 'riddle',
            'comparison', 'planning', 'checklist', 'prompt-idea',
            'generator', 'naming', 'stats-analyzer', 'visualization',
            'feedback', 'creativity', 'documentation', 'python-ml',
            'javascript-ts', 'go', 'rust', 'java'
        }
        if v not in allowed_modes:
            raise ValueError(f"Invalid mode. Allowed modes: {', '.join(sorted(allowed_modes))}")
        return v
    
    @validator('model')
    def validate_model(cls, v):
        allowed_models = {'chatgpt', 'claude', 'grok', 'gemini', 'nexus', 'flash', 'coherence'}
        if v not in allowed_models:
            raise ValueError(f"Invalid model. Allowed models: {', '.join(sorted(allowed_models))}")
        return v
    
    @validator('input')
    def validate_input(cls, v):
        return InputValidator.validate_ai_prompt(v)

class SecurityMiddleware:
    """Comprehensive security middleware"""
    
    def __init__(self, app):
        self.app = app
        self.security = HTTPBearer(auto_error=False)
    
    async def __call__(self, request: Request, call_next):
        start_time = time.time()
        
        # Get client information
        client_ip = self._get_client_ip(request)
        user_agent = request.headers.get("user-agent", "")
        
        # For now, use simple rate limiting (can be enhanced later)
        # This would be replaced with user-tier based rate limiting
        if self._is_basic_rate_limited(client_ip):
            self._log_security_event(
                "RATE_LIMIT_EXCEEDED", "HIGH", client_ip, request.url.path
            )
            raise HTTPException(
                status_code=status.HTTP_429_TOO_MANY_REQUESTS,
                detail="Rate limit exceeded. Please try again later."
            )
        
        # Security headers check
        await self._check_security_headers(request)
        
        # Process request
        response = await call_next(request)
        
        # Add security headers to response
        self._add_security_headers(response)
        
        # Log request
        processing_time = time.time() - start_time
        self._log_request(client_ip, request.method, request.url.path, processing_time)
        
        return response
    
    def _is_basic_rate_limited(self, client_ip: str) -> bool:
        """Basic rate limiting check (to be enhanced with tiered system)"""
        # Simple implementation - in production would use the tiered system
        now = time.time()
        window = 60  # 1 minute window
        
        if not hasattr(self, '_basic_requests'):
            self._basic_requests = {}
        
        if client_ip not in self._basic_requests:
            self._basic_requests[client_ip] = []
        
        # Remove old requests
        self._basic_requests[client_ip] = [
            req_time for req_time in self._basic_requests[client_ip]
            if now - req_time < window
        ]
        
        # Check limit (100 requests per minute)
        if len(self._basic_requests[client_ip]) >= 100:
            return True
        
        self._basic_requests[client_ip].append(now)
        return False
    
    def _get_client_ip(self, request: Request) -> str:
        """Get real client IP address"""
        # Check for forwarded headers first (for reverse proxy)
        forwarded_for = request.headers.get("x-forwarded-for")
        if forwarded_for:
            return forwarded_for.split(",")[0].strip()
        
        real_ip = request.headers.get("x-real-ip")
        if real_ip:
            return real_ip
        
        return request.client.host if request.client else "unknown"
    
    async def _check_security_headers(self, request: Request):
        """Check for required security headers"""
        # This would be expanded for specific security requirements
        pass
    
    def _add_security_headers(self, response):
        """Add comprehensive security headers"""
        headers = {
            "X-Content-Type-Options": "nosniff",
            "X-Frame-Options": "DENY",
            "X-XSS-Protection": "1; mode=block",
            "Referrer-Policy": "strict-origin-when-cross-origin",
            "Permissions-Policy": "geolocation=(), microphone=(), camera=()",
            "Strict-Transport-Security": "max-age=31536000; includeSubDomains; preload",
            "Content-Security-Policy": (
                "default-src 'self'; "
                "script-src 'self' 'unsafe-inline' https://cdn.tailwindcss.com; "
                "style-src 'self' 'unsafe-inline' https://cdn.tailwindcss.com; "
                "img-src 'self' data: https:; "
                "font-src 'self' https://cdn.tailwindcss.com; "
                "connect-src 'self' https:; "
                "frame-ancestors 'none'; "
                "base-uri 'self'; "
                "form-action 'self'"
            ),
            "Cache-Control": "no-cache, no-store, must-revalidate",
            "Pragma": "no-cache",
            "Expires": "0"
        }
        
        for header, value in headers.items():
            response.headers[header] = value
    
    def _log_security_event(self, event_type: str, severity: str, 
                          source_ip: str, endpoint: str, details: Dict = None):
        """Log security events"""
        event = SecurityEvent(
            timestamp=time.time(),
            event_type=event_type,
            severity=severity,
            source_ip=source_ip,
            endpoint=endpoint,
            details=details or {}
        )
        
        # In production, this would send to a SIEM or logging system
        security_logger.warning(f"SECURITY EVENT: {event.dict()}")
    
    def _log_request(self, client_ip: str, method: str, path: str, processing_time: float):
        """Log request for monitoring"""
        if processing_time > 5.0:  # Log slow requests
            security_logger.info(f"SLOW_REQUEST: {client_ip} {method} {path} {processing_time:.2f}s")

def require_auth(credentials: HTTPAuthorizationCredentials = Depends(HTTPBearer(auto_error=False))):
    """Authentication dependency for protected endpoints"""
    if not credentials:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Authentication required",
            headers={"WWW-Authenticate": "Bearer"},
        )
    
    try:
        # In production, verify JWT token here
        payload = jwt.decode(
            credentials.credentials, 
            SecurityConfig.JWT_SECRET_KEY, 
            algorithms=[SecurityConfig.JWT_ALGORITHM]
        )
        return payload
    except jwt.ExpiredSignatureError:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Token has expired"
        )
    except jwt.JWTError:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Invalid token"
        )

def log_security_event(event_type: str, severity: str = "MEDIUM"):
    """Decorator to log security events"""
    def decorator(func):
        @wraps(func)
        async def wrapper(*args, **kwargs):
            try:
                return await func(*args, **kwargs)
            except Exception as e:
                security_logger.error(f"SECURITY_EVENT in {func.__name__}: {event_type} - {str(e)}")
                raise
        return wrapper
    return decorator

# Enhanced response model with security metadata
class SecureRunResponse(BaseModel):
    output: str
    security_metadata: Dict[str, Any] = Field(default_factory=dict)
    
    class Config:
        schema_extra = {
            "example": {
                "output": "AI response here",
                "security_metadata": {
                    "input_validated": True,
                    "sanitization_applied": True,
                    "rate_limiting": "passed",
                    "timestamp": "2024-01-01T00:00:00Z"
                }
            }
        }
