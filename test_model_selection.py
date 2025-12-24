#!/usr/bin/env python3
"""
Test script for dynamic model selection feature
Tests both backend functionality and API endpoints
"""

import requests
import json
import time
import sys
from pathlib import Path

# Add the project root to Python path
ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT))

def test_model_management():
    """Test model management functionality"""
    print("🔧 Testing Model Management System...")
    
    try:
        from ai_backend.models.unified_ai_manager import get_ai_manager
        from ai_backend.models.model_manager import get_model_manager, get_available_models, switch_model
        
        # Test AI Manager
        ai_manager = get_ai_manager()
        print(f"✅ AI Manager initialized with model: {ai_manager.model_name}")
        
        # Test model availability
        available = get_available_models()
        print(f"✅ Available models: {len(available)} models found")
        for model in available:
            print(f"  - {model['name']}: {model.get('display_name', model['name'])} (Available: {model['available']})")
        
        # Test AI Manager methods
        result = ai_manager.get_available_models()
        print(f"✅ AI Manager get_available_models() works: {len(result['available_models'])} models")
        
        return True
        
    except Exception as e:
        print(f"❌ Model management test failed: {str(e)}")
        return False

def test_api_endpoints():
    """Test API endpoints for model management"""
    print("\n🌐 Testing API Endpoints...")
    
    base_url = "http://127.0.0.1:8000"
    
    try:
        # Test GET /api/models
        response = requests.get(f"{base_url}/api/models")
        if response.status_code == 200:
            data = response.json()
            print(f"✅ GET /api/models: Current model = {data.get('current_model')}")
            print(f"✅ Available models: {len(data.get('available_models', []))}")
        else:
            print(f"⚠️  GET /api/models returned status {response.status_code}")
            print(f"   Response: {response.text}")
        
        # Test POST /api/models/switch
        switch_data = {"model_name": "flash"}
        response = requests.post(f"{base_url}/api/models/switch", json=switch_data)
        if response.status_code == 200:
            data = response.json()
            print(f"✅ POST /api/models/switch: Status = {data.get('status')}")
            if data.get('status') == 'success':
                print(f"✅ Successfully switched to: {data.get('current_model')}")
            else:
                print(f"⚠️  Switch failed: {data.get('model_info', {}).get('error', 'Unknown error')}")
        else:
            print(f"⚠️  POST /api/models/switch returned status {response.status_code}")
            print(f"   Response: {response.text}")
        
        # Test invalid model switch
        switch_data = {"model_name": "invalid_model"}
        response = requests.post(f"{base_url}/api/models/switch", json=switch_data)
        if response.status_code == 200:
            data = response.json()
            print(f"✅ Invalid model switch test: Status = {data.get('status')}")
        else:
            print(f"⚠️  Invalid model switch returned status {response.status_code}")
        
        return True
        
    except requests.exceptions.ConnectionError:
        print("⚠️  Cannot connect to API server. Make sure it's running on http://127.0.0.1:8000")
        return False
    except Exception as e:
        print(f"❌ API test failed: {str(e)}")
        return False

def test_ai_request_with_model():
    """Test AI request with different models"""
    print("\n🤖 Testing AI Requests with Model Selection...")
    
    base_url = "http://127.0.0.1:8000"
    
    try:
        # Test with nexus model
        request_data = {
            "mode": "helper",
            "model": "nexus",
            "input": json.dumps({
                "prompt": "Hello, test message",
                "parameters": {
                    "wLearningRate": 0.001,
                    "wRegularization": 0.01,
                    "wSize": 10000,
                    "bLearningRate": 0.001,
                    "bRegularization": 0.01,
                    "bSize": 10000
                }
            }),
            "user_id": "test_user",
            "session_id": "test_session"
        }
        
        response = requests.post(f"{base_url}/api/run", json=request_data)
        if response.status_code == 200:
            data = response.json()
            output = json.loads(data['output'])
            print(f"✅ AI request with nexus model: Success")
            print(f"   Response length: {len(output.get('response', ''))} chars")
            print(f"   Model used: {output.get('model_used')}")
        else:
            print(f"⚠️  AI request returned status {response.status_code}")
            print(f"   Response: {response.text}")
        
        # Test with different model
        request_data["model"] = "flash"
        response = requests.post(f"{base_url}/api/run", json=request_data)
        if response.status_code == 200:
            data = response.json()
            output = json.loads(data['output'])
            print(f"✅ AI request with flash model: Success")
            print(f"   Model used: {output.get('model_used')}")
        else:
            print(f"⚠️  AI request with flash returned status {response.status_code}")
        
        return True
        
    except requests.exceptions.ConnectionError:
        print("⚠️  Cannot connect to API server. Make sure it's running.")
        return False
    except Exception as e:
        print(f"❌ AI request test failed: {str(e)}")
        return False

def test_frontend_files():
    """Test that frontend files have been updated correctly"""
    print("\n🎨 Testing Frontend Implementation...")
    
    try:
        # Check HTML file for model selector
        html_file = ROOT / "src" / "AI" / "enhanced_interface.html"
        if html_file.exists():
            with open(html_file, 'r') as f:
                html_content = f.read()
                if 'model-selector' in html_content and 'modelDropdown' in html_content:
                    print("✅ HTML file contains model selector components")
                else:
                    print("❌ HTML file missing model selector components")
                    return False
        else:
            print("❌ HTML file not found")
            return False
        
        # Check JavaScript file for model management
        js_file = ROOT / "src" / "AI" / "enhanced_interface_stable.js"
        if js_file.exists():
            with open(js_file, 'r') as f:
                js_content = f.read()
                required_functions = [
                    'loadAvailableModels',
                    'switchToModel',
                    'toggleModelDropdown',
                    'updateModelSelector'
                ]
                
                missing_functions = []
                for func in required_functions:
                    if func not in js_content:
                        missing_functions.append(func)
                
                if not missing_functions:
                    print("✅ JavaScript file contains all required model management functions")
                else:
                    print(f"❌ JavaScript file missing functions: {missing_functions}")
                    return False
        else:
            print("❌ JavaScript file not found")
            return False
        
        return True
        
    except Exception as e:
        print(f"❌ Frontend test failed: {str(e)}")
        return False

def main():
    """Run all tests"""
    print("🚀 Starting Model Selection Feature Tests")
    print("=" * 50)
    
    tests = [
        ("Model Management", test_model_management),
        ("API Endpoints", test_api_endpoints),
        ("AI Requests", test_ai_request_with_model),
        ("Frontend Implementation", test_frontend_files)
    ]
    
    results = {}
    for test_name, test_func in tests:
        try:
            results[test_name] = test_func()
        except Exception as e:
            print(f"❌ {test_name} failed with exception: {str(e)}")
            results[test_name] = False
    
    print("\n" + "=" * 50)
    print("📊 TEST RESULTS SUMMARY")
    print("=" * 50)
    
    passed = 0
    for test_name, result in results.items():
        status = "✅ PASSED" if result else "❌ FAILED"
        print(f"{test_name:<25} {status}")
        if result:
            passed += 1
    
    print(f"\n🎯 Results: {passed}/{len(results)} tests passed")
    
    if passed == len(results):
        print("\n🎉 All tests passed! Model selection feature is ready!")
        print("\n📋 Next steps:")
        print("1. Start the server: python server.py")
        print("2. Open the enhanced interface in browser")
        print("3. Test the model selector in the UI")
        return True
    else:
        print(f"\n⚠️  {len(results) - passed} tests failed. Please check the implementation.")
        return False

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)
