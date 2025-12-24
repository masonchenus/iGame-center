"""
Model Manager
Handles availability, initialization, and switching between different AI models
"""
import os
import importlib
import inspect
from typing import Dict, List, Optional, Any
from dataclasses import dataclass
import logging

@dataclass
class ModelInfo:
    """Information about an AI model"""
    name: str
    display_name: str
    description: str
    requires_api_key: bool
    available: bool
    api_key_env_var: Optional[str] = None
    module_path: Optional[str] = None
    class_name: Optional[str] = None

class ModelManager:
    """Manages AI model availability and switching"""
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        self.models: Dict[str, ModelInfo] = {}
        self.loaded_models: Dict[str, Any] = {}
        self.current_model = "nexus"
        self._initialize_models()
    
    def _initialize_models(self):
        """Initialize all available models"""
        model_configs = [
            ModelInfo(
                name="nexus",
                display_name="Nexus AI",
                description="Default system model with enhanced parameters",
                requires_api_key=False,
                available=True,
                module_path="ai_backend.models.unified_ai_manager",
                class_name="UnifiedAIManager"
            ),
            ModelInfo(
                name="chatgpt",
                display_name="ChatGPT",
                description="OpenAI's GPT models",
                requires_api_key=True,
                available=self._check_api_key("OPENAI_API_KEY"),
                api_key_env_var="OPENAI_API_KEY",
                module_path="ai_backend.ai_models.chatgpt_model",
                class_name="ChatGPTModel"
            ),
            ModelInfo(
                name="claude",
                display_name="Claude",
                description="Anthropic's Claude models",
                requires_api_key=True,
                available=self._check_api_key("ANTHROPIC_API_KEY"),
                api_key_env_var="ANTHROPIC_API_KEY",
                module_path="ai_backend.ai_models.claude_model",
                class_name="ClaudeModel"
            ),
            ModelInfo(
                name="grok",
                display_name="Grok",
                description="X.AI's Grok models",
                requires_api_key=True,
                available=self._check_api_key("GROK_API_KEY"),
                api_key_env_var="GROK_API_KEY",
                module_path="ai_backend.ai_models.grok_model",
                class_name="GrokModel"
            ),
            ModelInfo(
                name="gemini",
                display_name="Gemini",
                description="Google's Gemini models",
                requires_api_key=True,
                available=self._check_api_key("GEMINI_API_KEY"),
                api_key_env_var="GEMINI_API_KEY",
                module_path="ai_backend.ai_models.gemini_model",
                class_name="GeminiModel"
            ),
            ModelInfo(
                name="cohere",
                display_name="Cohere",
                description="Cohere's language models",
                requires_api_key=True,
                available=self._check_api_key("COHERE_API_KEY"),
                api_key_env_var="COHERE_API_KEY",
                module_path="ai_backend.ai_models.coherence_model",
                class_name="CohereModel"
            )
        ]
        
        for config in model_configs:
            self.models[config.name] = config
        
        self.logger.info(f"Initialized {len(self.models)} models")
    
    def _check_api_key(self, env_var: str) -> bool:
        """Check if API key is available"""
        return bool(os.getenv(env_var))
    
    def get_available_models(self) -> List[Dict[str, Any]]:
        """Get list of all models with availability status"""
        available_models = []
        
        for model_name, model_info in self.models.items():
            model_data = {
                "name": model_name,
                "display_name": model_info.display_name,
                "description": model_info.description,
                "available": model_info.available,
                "requires_api_key": model_info.requires_api_key,
                "api_key_env_var": model_info.api_key_env_var,
                "is_current": model_name == self.current_model
            }
            
            # Add availability status message
            if not model_info.available and model_info.requires_api_key:
                model_data["status_message"] = f"Requires {model_info.api_key_env_var} to be set"
            elif model_info.available:
                model_data["status_message"] = "Ready to use"
            else:
                model_data["status_message"] = "Not available"
                
            available_models.append(model_data)
        
        return available_models
    
    def get_current_model(self) -> str:
        """Get currently selected model"""
        return self.current_model
    
    def set_current_model(self, model_name: str) -> Dict[str, Any]:
        """Set the current model"""
        if model_name not in self.models:
            return {
                "success": False,
                "error": f"Model '{model_name}' not found",
                "available_models": list(self.models.keys())
            }
        
        model_info = self.models[model_name]
        
        if not model_info.available:
            return {
                "success": False,
                "error": f"Model '{model_name}' is not available",
                "reason": f"Missing required API key: {model_info.api_key_env_var}" if model_info.requires_api_key else "Model not available"
            }
        
        self.current_model = model_name
        self.logger.info(f"Switched to model: {model_name}")
        
        return {
            "success": True,
            "previous_model": None,  # Could store previous model if needed
            "current_model": model_name,
            "model_info": {
                "display_name": model_info.display_name,
                "description": model_info.description
            }
        }
    
    def get_model_instance(self, model_name: str) -> Optional[Any]:
        """Get an instance of the specified model"""
        if model_name not in self.models:
            return None
        
        model_info = self.models[model_name]
        
        if not model_info.available:
            return None
        
        # Return cached instance if available
        if model_name in self.loaded_models:
            return self.loaded_models[model_name]
        
        try:
            # Load the model class
            if model_name == "nexus":
                # Special handling for nexus model
                from ai_backend.models.unified_ai_manager import UnifiedAIManager
                instance = UnifiedAIManager(model_name)
            else:
                # Load dynamic module
                module = importlib.import_module(model_info.module_path)
                model_class = getattr(module, model_info.class_name)
                instance = model_class()
            
            # Cache the instance
            self.loaded_models[model_name] = instance
            return instance
            
        except Exception as e:
            self.logger.error(f"Failed to load model {model_name}: {e}")
            return None
    
    def generate_with_model(self, model_name: str, prompt: str, **kwargs) -> Dict[str, Any]:
        """Generate response using specified model"""
        model_instance = self.get_model_instance(model_name)
        
        if not model_instance:
            return {
                "success": False,
                "error": f"Model '{model_name}' not available",
                "response": f"[{model_name}-error] Model not available or not properly configured"
            }
        
        try:
            if model_name == "nexus":
                # Special handling for nexus model
                response = model_instance.process_ai_request(prompt, kwargs)
                return {
                    "success": True,
                    "response": response.get("response", ""),
                    "model_used": model_name,
                    "additional_data": response
                }
            else:
                # Standard model interface
                if hasattr(model_instance, 'generate'):
                    response = model_instance.generate(prompt, **kwargs)
                    return {
                        "success": True,
                        "response": response,
                        "model_used": model_name
                    }
                else:
                    return {
                        "success": False,
                        "error": f"Model '{model_name}' does not have generate method"
                    }
        except Exception as e:
            self.logger.error(f"Generation failed for model {model_name}: {e}")
            return {
                "success": False,
                "error": str(e),
                "response": f"[{model_name}-error] {str(e)}"
            }
    
    def check_model_health(self, model_name: str) -> Dict[str, Any]:
        """Check the health/status of a specific model"""
        if model_name not in self.models:
            return {
                "status": "error",
                "message": f"Model '{model_name}' not found"
            }
        
        model_info = self.models[model_name]
        
        if not model_info.available:
            return {
                "status": "unavailable",
                "message": f"Model requires API key: {model_info.api_key_env_var}"
            }
        
        # Try to get an instance to verify it's working
        model_instance = self.get_model_instance(model_name)
        if model_instance:
            return {
                "status": "healthy",
                "message": f"Model '{model_name}' is ready to use"
            }
        else:
            return {
                "status": "error",
                "message": f"Model '{model_name}' failed to initialize"
            }
    
    def refresh_model_availability(self):
        """Refresh availability status for all models"""
        for model_info in self.models.values():
            if model_info.requires_api_key:
                model_info.available = self._check_api_key(model_info.api_key_env_var)
        
        self.logger.info("Refreshed model availability status")
    
    def get_model_statistics(self) -> Dict[str, Any]:
        """Get statistics about models"""
        total_models = len(self.models)
        available_models = sum(1 for m in self.models.values() if m.available)
        models_with_api_keys = sum(1 for m in self.models.values() if m.requires_api_key)
        
        return {
            "total_models": total_models,
            "available_models": available_models,
            "models_with_api_keys": models_with_api_keys,
            "current_model": self.current_model,
            "model_usage": {
                model_name: {"available": info.available, "requires_key": info.requires_api_key}
                for model_name, info in self.models.items()
            }
        }

# Global model manager instance
_model_manager = None

def get_model_manager() -> ModelManager:
    """Get or create global model manager instance"""
    global _model_manager
    if _model_manager is None:
        _model_manager = ModelManager()
    return _model_manager

def get_available_models() -> List[Dict[str, Any]]:
    """Get list of available models"""
    return get_model_manager().get_available_models()

def switch_model(model_name: str) -> Dict[str, Any]:
    """Switch to specified model"""
    return get_model_manager().set_current_model(model_name)

def get_current_model() -> str:
    """Get current model name"""
    return get_model_manager().get_current_model()

def generate_with_current_model(prompt: str, **kwargs) -> Dict[str, Any]:
    """Generate response using current model"""
    return get_model_manager().generate_with_model(
        get_model_manager().get_current_model(), 
        prompt, 
        **kwargs
    )
