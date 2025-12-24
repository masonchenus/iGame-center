"""
AI Configuration Management
Centralized configuration for all AI systems
"""
import json
import os
from typing import Dict, Any, Optional
from dataclasses import dataclass, asdict
from pathlib import Path

@dataclass
class AIConfig:
    """Main AI configuration class"""
    # Model Settings
    default_model: str = "nexus"
    model_timeout: int = 30
    max_tokens: int = 4000
    temperature: float = 0.7
    
    # Parameter Settings
    w_parameter_size: int = 148_000_000_000_000
    b_parameter_size: int = 148_000_000_000_000
    active_parameter_size: int = 10000
    parameter_learning_rate: float = 0.001
    parameter_regularization: float = 0.01
    
    # Training Settings
    default_epochs: int = 100
    default_batch_size: int = 32
    validation_split: float = 0.2
    early_stopping: bool = True
    patience: int = 10
    
    # API Settings
    api_rate_limit: int = 100
    api_timeout: int = 60
    cors_enabled: bool = True
    debug_mode: bool = False
    
    # Monitoring Settings
    enable_monitoring: bool = True
    metrics_retention_days: int = 30
    alert_thresholds: Dict[str, float] = None
    
    # Virtual Array Settings
    virtual_array_seed: str = "ai_backend_v3"
    array_sample_size: int = 100
    memory_efficient: bool = True
    
    def __post_init__(self):
        if self.alert_thresholds is None:
            self.alert_thresholds = {
                "cpu_usage": 80.0,
                "memory_usage": 85.0,
                "response_time": 5.0,
                "error_rate": 5.0
            }

class AIConfigManager:
    """Manages AI configuration with file persistence"""
    
    def __init__(self, config_file: str = "ai_backend/models/config/ai_system_config.json"):
        self.config_file = config_file
        self.config = self._load_config()
        
    def _load_config(self) -> AIConfig:
        """Load configuration from file or create default"""
        try:
            if os.path.exists(self.config_file):
                with open(self.config_file, 'r') as f:
                    data = json.load(f)
                return AIConfig(**data)
        except Exception as e:
            print(f"Warning: Could not load config: {e}")
            
        # Return default configuration
        return AIConfig()
    
    def save_config(self):
        """Save current configuration to file"""
        try:
            os.makedirs(os.path.dirname(self.config_file), exist_ok=True)
            with open(self.config_file, 'w') as f:
                json.dump(asdict(self.config), f, indent=2)
        except Exception as e:
            print(f"Error saving config: {e}")
    
    def get(self, key: str, default: Any = None) -> Any:
        """Get configuration value"""
        return getattr(self.config, key, default)
    
    def set(self, key: str, value: Any):
        """Set configuration value"""
        setattr(self.config, key, value)
        self.save_config()
    
    def update(self, updates: Dict[str, Any]):
        """Update multiple configuration values"""
        for key, value in updates.items():
            self.set(key, value)
    
    def reset_to_defaults(self):
        """Reset configuration to defaults"""
        self.config = AIConfig()
        self.save_config()
    
    def export_config(self, filepath: str):
        """Export configuration to file"""
        with open(filepath, 'w') as f:
            json.dump(asdict(self.config), f, indent=2)
    
    def import_config(self, filepath: str) -> bool:
        """Import configuration from file"""
        try:
            with open(filepath, 'r') as f:
                data = json.load(f)
            self.config = AIConfig(**data)
            self.save_config()
            return True
        except Exception as e:
            print(f"Error importing config: {e}")
            return False
    
    def get_model_config(self, model_name: str) -> Dict[str, Any]:
        """Get configuration for specific model"""
        base_config = asdict(self.config)
        base_config.update({
            "model_name": model_name,
            "w_array_seed": f"{self.config.virtual_array_seed}_{model_name}_w",
            "b_array_seed": f"{self.config.virtual_array_seed}_{model_name}_b"
        })
        return base_config

# Global configuration instance
ai_config = AIConfigManager()

# Configuration presets for different use cases
PRESETS = {
    "development": AIConfig(
        default_model="nexus",
        debug_mode=True,
        api_timeout=120,
        metrics_retention_days=7,
        default_epochs=10,
        validation_split=0.3
    ),
    
    "production": AIConfig(
        default_model="nexus",
        debug_mode=False,
        api_timeout=30,
        metrics_retention_days=90,
        early_stopping=True,
        api_rate_limit=1000
    ),
    
    "training": AIConfig(
        default_model="nexus",
        debug_mode=True,
        default_epochs=500,
        default_batch_size=64,
        validation_split=0.2,
        early_stopping=False
    ),
    
    "performance": AIConfig(
        default_model="nexus",
        debug_mode=False,
        max_tokens=8000,
        temperature=0.5,
        api_timeout=90,
        memory_efficient=True
    )
}

def apply_preset(preset_name: str):
    """Apply a configuration preset"""
    if preset_name in PRESETS:
        ai_config.config = PRESETS[preset_name]
        ai_config.save_config()
        print(f"Applied {preset_name} preset")
    else:
        print(f"Unknown preset: {preset_name}")
        print(f"Available presets: {list(PRESETS.keys())}")

