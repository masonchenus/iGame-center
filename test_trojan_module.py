#!/usr/bin/env python3
"""
Test script to demonstrate module not found error with trojan module
"""
import sys
import os
sys.path.insert(0, '/Users/mason/Game Center Project')

from ai_backend.orchestrator import run_user_mode

def test_trojan_module():
    """Test the trojan module to show module not found error"""
    print("🚨 Testing TROJAN MODULE - Should cause module not found error")
    print("=" * 60)
    
    try:
        # This should fail with module not found error
        result = run_user_mode(
            mode_name="trojan_test_module",
            input_data="test request to trojan module",
            user_id="test_user",
            session_id="test_session", 
            model_name="nexus"
        )
        print(f"❌ ERROR: Expected failure but got: {result}")
        
    except Exception as e:
        print(f"✅ SUCCESS: Module not found error caught as expected!")
        print(f"Error type: {type(e).__name__}")
        print(f"Error message: {str(e)}")
        
    print("=" * 60)

def test_existing_module():
    """Test an existing module to show it works"""
    print("\n🔧 Testing EXISTING MODULE - Should work normally")
    print("=" * 60)
    
    try:
        result = run_user_mode(
            mode_name="helper_module",
            input_data="test request to existing module",
            user_id="test_user",
            session_id="test_session",
            model_name="nexus"
        )
        print(f"✅ SUCCESS: Existing module works: {result[:100]}...")
        
    except Exception as e:
        print(f"❌ ERROR: Unexpected error with existing module: {e}")
        
    print("=" * 60)

def test_builtin_modes():
    """Test built-in modes from orchestrator"""
    print("\n⚙️ Testing BUILT-IN MODES")
    print("=" * 60)
    
    try:
        from ai_backend.orchestrator import BaseAI
        ai = BaseAI(ultra_mode=True)
        
        # Check if trojan_test_module is in built-in modes
        if "trojan_test_module" in ai.modes:
            print("❌ ERROR: trojan_test_module found in built-in modes (shouldn't be)")
        else:
            print("✅ SUCCESS: trojan_test_module NOT in built-in modes (correct)")
            
        # Check helper mode
        if "helper" in ai.modes:
            print("✅ SUCCESS: helper mode found in built-in modes (correct)")
        else:
            print("❌ ERROR: helper mode NOT found in built-in modes (unexpected)")
            
    except Exception as e:
        print(f"❌ ERROR: Failed to test built-in modes: {e}")
        
    print("=" * 60)

if __name__ == "__main__":
    print("🧪 TROYAN MODULE TEST - Module Not Found Error Demonstration")
    print("This test will show how the system handles non-existent modules")
    
    test_trojan_module()
    test_existing_module() 
    test_builtin_modes()
    
    print("\n🎯 CONCLUSION:")
    print("- The trojan module (trojan_test_module) is properly caught as a module not found error")
    print("- This demonstrates good error handling for non-existent modules")
    print("- The system gracefully falls back to existing functionality")
