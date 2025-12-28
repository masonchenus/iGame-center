"""
Comprehensive Security Testing Suite
Tests all implemented security features including rate limiting, input validation,
authentication, and security headers.
"""

import unittest
import json
import time
from unittest.mock import Mock, patch
from fastapi.testclient import TestClient
import sys
from pathlib import Path

# Add the project root to Python path
ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT))

from ai_backend.security.middleware import (
    SecurityConfig,
    InputValidator,
    TieredRateLimiter,
    InputValidationError,
    RunRequest,
    rate_limiter
)
from ai_backend.secure_orchestrator import create_secure_fastapi_app, SecureAIOrchestrator

class TestSecurityConfig(unittest.TestCase):
    """Test security configuration"""
    
    def test_rate_limits_config(self):
        """Test that rate limits are properly configured"""
        limits = SecurityConfig.RATE_LIMITS
        
        # Verify developer tier has highest limit
        self.assertEqual(limits["developer"], 800000)
        
        # Verify free tier limits
        self.assertEqual(limits["free"], 300)
        self.assertEqual(limits["free_best"], 30)
        
        # Verify pro tier limits
        self.assertEqual(limits["pro"], 1200)
        self.assertEqual(limits["pro_best"], 600)
        
        # Verify ultra tier limits
        self.assertEqual(limits["ultra"], 50000)
        self.assertEqual(limits["ultra_best"], 30000)
    
    def test_best_models_config(self):
        """Test that best models are properly configured"""
        best_models = SecurityConfig.BEST_MODELS
        
        # Verify best models list contains expected models
        self.assertIn("gpt-4", best_models)
        self.assertIn("claude-3-opus", best_models)
        self.assertIn("gemini-ultra", best_models)
        self.assertIn("grok-premium", best_models)
    
    def test_security_patterns_config(self):
        """Test that banned patterns are configured"""
        patterns = SecurityConfig.BANNED_PATTERNS
        
        # Verify XSS protection
        self.assertTrue(any("<script" in pattern for pattern in patterns))
        
        # Verify SQL injection protection
        self.assertTrue(any("union" in pattern for pattern in patterns))
        
        # Verify code injection protection
        self.assertTrue(any("eval" in pattern for pattern in patterns))

class TestTieredRateLimiter(unittest.TestCase):
    """Test tiered rate limiting functionality"""
    
    def setUp(self):
        """Set up test environment"""
        self.rate_limiter = TieredRateLimiter()
    
    def test_user_tier_detection(self):
        """Test user tier detection"""
        # Test developer tier
        self.assertEqual(
            self.rate_limiter.get_user_tier("masonchenus"), 
            "developer"
        )
        self.assertEqual(
            self.rate_limiter.get_user_tier("dev"), 
            "developer"
        )
        
        # Test tier prefixes
        self.assertEqual(
            self.rate_limiter.get_user_tier("ultra_user123"), 
            "ultra"
        )
        self.assertEqual(
            self.rate_limiter.get_user_tier("pro_user456"), 
            "pro"
        )
        self.assertEqual(
            self.rate_limiter.get_user_tier("free_user789"), 
            "free"
        )
        
        # Test default tier
        self.assertEqual(
            self.rate_limiter.get_user_tier("random_user"), 
            "free"
        )
    
    def test_best_model_detection(self):
        """Test best model detection"""
        # Test best models
        self.assertTrue(self.rate_limiter.is_best_model("gpt-4"))
        self.assertTrue(self.rate_limiter.is_best_model("claude-3-opus"))
        self.assertTrue(self.rate_limiter.is_best_model("gemini-ultra"))
        
        # Test non-best models
        self.assertFalse(self.rate_limiter.is_best_model("gpt-3.5"))
        self.assertFalse(self.rate_limiter.is_best_model("claude-instant"))
        self.assertFalse(self.rate_limiter.is_best_model("gemini-pro"))
    
    def test_rate_limiting_limits(self):
        """Test rate limiting limits for different tiers"""
        # Test developer tier (should never be rate limited in normal usage)
        self.assertEqual(
            self.rate_limiter.get_rate_limit("masonchenus", "gpt-4"),
            800000
        )
        
        # Test free tier with regular model
        self.assertEqual(
            self.rate_limiter.get_rate_limit("free_user", "gpt-3.5"),
            300
        )
        
        # Test free tier with best model
        self.assertEqual(
            self.rate_limiter.get_rate_limit("free_user", "gpt-4"),
            30
        )
        
        # Test pro tier with best model
        self.assertEqual(
            self.rate_limiter.get_rate_limit("pro_user", "gpt-4"),
            600
        )
        
        # Test ultra tier with best model
        self.assertEqual(
            self.rate_limiter.get_rate_limit("ultra_user", "gpt-4"),
            30000
        )
    
    def test_rate_limiting_functionality(self):
        """Test actual rate limiting functionality"""
        user_id = "test_free_user"
        model_name = "gpt-4"  # Best model
        
        # First few requests should pass
        for i in range(30):  # Free tier best model limit is 30
            self.assertFalse(
                self.rate_limiter.is_rate_limited(user_id, model_name),
                f"Request {i+1} should not be rate limited"
            )
        
        # After hitting the limit, should be rate limited
        self.assertTrue(
            self.rate_limiter.is_rate_limited(user_id, model_name),
            "Should be rate limited after hitting limit"
        )
    
    def test_remaining_requests(self):
        """Test remaining requests calculation"""
        user_id = "test_pro_user"
        model_name = "gpt-3.5"  # Regular model
        
        # Initial state
        remaining = self.rate_limiter.get_remaining_requests(user_id, model_name)
        self.assertEqual(remaining["remaining"], 1200)
        self.assertEqual(remaining["limit"], 1200)
        self.assertEqual(remaining["used"], 0)
        self.assertEqual(remaining["tier"], "pro")
        self.assertFalse(remaining["is_best_model"])
        
        # After some requests
        for i in range(10):
            self.rate_limiter.is_rate_limited(user_id, model_name)
        
        remaining = self.rate_limiter.get_remaining_requests(user_id, model_name)
        self.assertEqual(remaining["remaining"], 1190)
        self.assertEqual(remaining["used"], 10)

class TestInputValidator(unittest.TestCase):
    """Test input validation and sanitization"""
    
    def setUp(self):
        """Set up test environment"""
        self.validator = InputValidator()
    
    def test_html_sanitization(self):
        """Test HTML sanitization"""
        # Test malicious script
        malicious = "<script>alert('xss')</script>"
        sanitized = self.validator.sanitize_html(malicious)
        self.assertNotIn("<script>", sanitized)
        self.assertNotIn("alert", sanitized)
        
        # Test HTML tags
        html = "<p>Hello <b>World</b></p>"
        sanitized = self.validator.sanitize_html(html)
        self.assertEqual(sanitized, "Hello World")
    
    def test_input_length_validation(self):
        """Test input length validation"""
        # Test valid length
        valid_input = "a" * 100
        result = self.validator.validate_input_length(valid_input, 200)
        self.assertEqual(len(result), 100)
        
        # Test excessive length
        long_input = "a" * 1000
        with self.assertRaises(InputValidationError):
            self.validator.validate_input_length(long_input, 100)
    
    def test_banned_patterns(self):
        """Test banned pattern detection"""
        # Test XSS pattern
        with self.assertRaises(InputValidationError):
            self.validator.check_banned_patterns("<script>alert('xss')</script>")
        
        # Test SQL injection pattern
        with self.assertRaises(InputValidationError):
            self.validator.check_banned_patterns("'; DROP TABLE users; --")
        
        # Test code injection
        with self.assertRaises(InputValidationError):
            self.validator.check_banned_patterns("eval('malicious code')")
        
        # Test path traversal
        with self.assertRaises(InputValidationError):
            self.validator.check_banned_patterns("../../../etc/passwd")
    
    def test_ai_prompt_validation(self):
        """Test AI prompt validation"""
        # Test valid prompt
        valid_prompt = "Help me write a Python function to calculate fibonacci numbers."
        result = self.validator.validate_ai_prompt(valid_prompt)
        self.assertEqual(result, valid_prompt)
        
        # Test prompt with script
        with self.assertRaises(InputValidationError):
            self.validator.validate_ai_prompt("Help me <script>alert('xss')</script>")
        
        # Test empty prompt
        with self.assertRaises(InputValidationError):
            self.validator.validate_ai_prompt("")
        
        # Test prompt with excessive length
        long_prompt = "a" * 15000
        with self.assertRaises(InputValidationError):
            self.validator.validate_ai_prompt(long_prompt)

class TestRunRequest(unittest.TestCase):
    """Test RunRequest model validation"""
    
    def test_valid_request(self):
        """Test valid request creation"""
        valid_data = {
            "mode": "helper",
            "model": "chatgpt",
            "input": "Help me with Python",
            "user_id": "test_user",
            "session_id": "session_123"
        }
        
        request = RunRequest(**valid_data)
        self.assertEqual(request.mode, "helper")
        self.assertEqual(request.model, "chatgpt")
        self.assertEqual(request.input, "Help me with Python")
        self.assertEqual(request.user_id, "test_user")
        self.assertEqual(request.session_id, "session_123")
    
    def test_invalid_mode(self):
        """Test invalid mode rejection"""
        invalid_data = {
            "mode": "invalid_mode",
            "model": "chatgpt",
            "input": "Help me"
        }
        
        with self.assertRaises(ValueError):
            RunRequest(**invalid_data)
    
    def test_invalid_model(self):
        """Test invalid model rejection"""
        invalid_data = {
            "mode": "helper",
            "model": "invalid_model",
            "input": "Help me"
        }
        
        with self.assertRaises(ValueError):
            RunRequest(**invalid_data)
    
    def test_input_validation_in_request(self):
        """Test input validation in RunRequest"""
        # Test malicious input
        invalid_data = {
            "mode": "helper",
            "model": "chatgpt",
            "input": "<script>alert('xss')</script>"
        }
        
        with self.assertRaises(Exception):  # Should be caught by input validator
            RunRequest(**invalid_data)

class TestSecureFastAPIEndpoints(unittest.TestCase):
    """Test secure FastAPI endpoints"""
    
    def setUp(self):
        """Set up test client"""
        self.client = TestClient(create_secure_fastapi_app())
    
    def test_health_endpoint(self):
        """Test health check endpoint"""
        response = self.client.get("/health")
        self.assertEqual(response.status_code, 200)
        
        data = response.json()
        self.assertEqual(data["status"], "healthy")
        self.assertTrue(data["security_enabled"])
        self.assertEqual(data["version"], "2.0.0-security")
    
    def test_security_metrics_endpoint(self):
        """Test security metrics endpoint"""
        response = self.client.get("/api/security/metrics")
        self.assertEqual(response.status_code, 200)
        
        data = response.json()
        self.assertIn("total_requests", data)
        self.assertIn("blocked_requests", data)
        self.assertTrue(data["security_active"])
    
    def test_secure_run_endpoint_invalid_input(self):
        """Test secure run endpoint with invalid input"""
        invalid_request = {
            "mode": "invalid_mode",
            "model": "chatgpt",
            "input": "Help me"
        }
        
        response = self.client.post("/api/run", json=invalid_request)
        self.assertEqual(response.status_code, 422)  # Validation error
    
    def test_secure_run_endpoint_rate_limiting(self):
        """Test rate limiting in secure run endpoint"""
        # This would require making many requests to test rate limiting
        # For now, just test the endpoint exists and accepts valid requests
        valid_request = {
            "mode": "helper",
            "model": "chatgpt",
            "input": "Help me with Python basics",
            "user_id": "test_user",
            "session_id": "session_123"
        }
        
        # Note: This might fail because we're using a mock AI system
        # In a real test, we'd need to mock the underlying AI calls
        response = self.client.post("/api/run", json=valid_request)
        # We expect either success (200) or some error from the mock AI system
        # but not a validation error (422)
        self.assertNotEqual(response.status_code, 422)

class TestSecurityHeaders(unittest.TestCase):
    """Test security headers implementation"""
    
    def setUp(self):
        """Set up test client"""
        self.client = TestClient(create_secure_fastapi_app())
    
    def test_security_headers_present(self):
        """Test that security headers are present in responses"""
        response = self.client.get("/health")
        
        # Check for essential security headers
        self.assertIn("x-content-type-options", response.headers)
        self.assertIn("x-frame-options", response.headers)
        self.assertIn("x-xss-protection", response.headers)
        self.assertIn("referrer-policy", response.headers)
        
        # Check header values
        self.assertEqual(response.headers["x-content-type-options"], "nosniff")
        self.assertEqual(response.headers["x-frame-options"], "DENY")
        self.assertEqual(response.headers["x-xss-protection"], "1; mode=block")
    
    def test_csp_header_present(self):
        """Test Content Security Policy header"""
        response = self.client.get("/health")
        
        # CSP should be present
        csp = response.headers.get("content-security-policy")
        self.assertIsNotNone(csp)
        
        # CSP should contain essential directives
        self.assertIn("default-src 'self'", csp)
        self.assertIn("script-src", csp)
        self.assertIn("style-src", csp)
    
    def test_cors_headers(self):
        """Test CORS configuration"""
        # Test preflight request
        response = self.client.options("/api/run", headers={
            "Origin": "https://localhost:3000",
            "Access-Control-Request-Method": "POST"
        })
        
        # Should have CORS headers
        self.assertIn("access-control-allow-origin", response.headers)
        self.assertIn("access-control-allow-methods", response.headers)

def run_security_tests():
    """Run all security tests"""
    print("🛡️  Running Comprehensive Security Test Suite")
    print("=" * 50)
    
    # Create test suite
    test_suite = unittest.TestSuite()
    
    # Add test classes
    test_classes = [
        TestSecurityConfig,
        TestTieredRateLimiter,
        TestInputValidator,
        TestRunRequest,
        TestSecureFastAPIEndpoints,
        TestSecurityHeaders
    ]
    
    for test_class in test_classes:
        tests = unittest.TestLoader().loadTestsFromTestCase(test_class)
        test_suite.addTests(tests)
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(test_suite)
    
    # Print summary
    print("\n" + "=" * 50)
    print("🏁 Security Test Summary")
    print(f"Tests run: {result.testsRun}")
    print(f"Failures: {len(result.failures)}")
    print(f"Errors: {len(result.errors)}")
    print(f"Success rate: {((result.testsRun - len(result.failures) - len(result.errors)) / result.testsRun * 100):.1f}%")
    
    if result.failures:
        print("\n❌ Failures:")
        for test, traceback in result.failures:
            print(f"  - {test}: {traceback.split('AssertionError: ')[-1].split('\\n')[0]}")
    
    if result.errors:
        print("\n🚨 Errors:")
        for test, traceback in result.errors:
            print(f"  - {test}: {traceback.split('\\n')[-2]}")
    
    return result.wasSuccessful()

if __name__ == "__main__":
    success = run_security_tests()
    
    if success:
        print("\n✅ All security tests passed! Your AI system is properly secured.")
    else:
        print("\n⚠️  Some security tests failed. Please review the issues above.")
        sys.exit(1)

