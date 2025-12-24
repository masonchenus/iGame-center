"""
Unified AI Manager
Integrates all AI enhancement components into a single interface with model selection support
"""
import json
import os
from typing import Dict, List, Optional, Any
from datetime import datetime
import logging

from ai_backend.models.enhanced.parameter_manager import ParameterManager, ParameterConfig
from ai_backend.models.enhanced.model_trainer import ModelTrainer, TrainingConfig
from ai_backend.models.enhanced.evaluator import ModelEvaluator
from ai_backend.models.enhanced.optimizer import ParameterOptimizer, OptimizationConfig
from ai_backend.monitoring.performance_tracker import PerformanceTracker
from ai_backend.models.config.ai_config import ai_config, AIConfig
from ai_backend.models.model_manager import get_model_manager, get_available_models, switch_model
class UnifiedAIManager:
    """Unified interface for all AI enhancement capabilities"""
    
    def __init__(self, model_name: str = "nexus"):
        self.model_name = model_name
        self.logger = logging.getLogger(__name__)
        
        # Initialize all components
        self.parameter_manager = ParameterManager(model_name)
        self.trainer = ModelTrainer(model_name)
        self.evaluator = ModelEvaluator(model_name)
        self.optimizer = ParameterOptimizer(model_name)
        self.performance_tracker = PerformanceTracker(model_name)
        
        # AI system state
        self.is_initialized = False
        self.current_session = None
        
        # Configuration
        self.config = ai_config.get_model_config(model_name)
        
    def initialize_system(self) -> Dict[str, Any]:
        """Initialize the complete AI system"""
        try:
            self.logger.info(f"Initializing AI system for model {self.model_name}")
            
            # Initialize parameter manager
            param_stats = self.parameter_manager.get_parameter_stats()
            
            # Start performance monitoring
            self.performance_tracker.start_monitoring()
            
            # Set system state
            self.is_initialized = True
            self.current_session = {
                "session_id": f"session_{datetime.now().strftime('%Y%m%d_%H%M%S')}",
                "start_time": datetime.now().isoformat(),
                "model_name": self.model_name
            }
            
            init_result = {
                "status": "success",
                "model_name": self.model_name,
                "session_info": self.current_session,
                "parameter_stats": param_stats,
                "system_config": self.config,
                "capabilities": [
                    "parameter_management",
                    "model_training", 
                    "model_evaluation",
                    "parameter_optimization",
                    "performance_monitoring",
                    "real_time_metrics"
                ]
            }
            
            self.logger.info("AI system initialized successfully")
            return init_result
            
        except Exception as e:
            self.logger.error(f"Failed to initialize AI system: {e}")
            return {
                "status": "error",
                "error": str(e)
            }
    
    def process_ai_request(self, prompt: str, parameters: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Process AI request with enhanced capabilities"""
        if not self.is_initialized:
            return {"error": "AI system not initialized"}
        
        start_time = datetime.now()
        
        try:
            # Apply custom parameters if provided
            if parameters:
                self._apply_request_parameters(parameters)
            
            # Record request for performance tracking
            self.performance_tracker.record_api_request(0, True)  # Will update with actual time
            
            # Use the existing orchestrator for core AI processing
            from ai_backend.orchestrator import run_mode
            import time
            
            # Generate x internally (as in original system)
            import hashlib
            prompt_hash = hashlib.sha256(prompt.encode("utf-8")).digest()
            x_seed = int.from_bytes(prompt_hash[:8], "big") % 1_000_000
            x = x_seed / 100_000.0
            
            # Process with enhanced AI
            result = run_mode(
                mode_name="helper",
                model_name=self.model_name,
                input_data=json.dumps({"prompt": prompt}),
                user_id="enhanced_ai",
                session_id=self.current_session["session_id"]
            )
            
            # Parse result
            output = json.loads(result)
            
            # Enhance with additional AI system info
            enhanced_result = {
                "response": output.get("response", ""),
                "model_used": output.get("model_used", self.model_name),
                "mode": output.get("mode", "helper"),
                "tokens": output.get("tokens", []),
                "token_count": output.get("token_count", 0),
                "ai_enhancement": {
                    "x_value": x,
                    "parameter_stats": self.parameter_manager.get_parameter_stats(),
                    "performance_metrics": self.performance_tracker.get_current_status(),
                    "processing_time_ms": (datetime.now() - start_time).total_seconds() * 1000
                }
            }
            
            # Add virtual arrays info if available
            if "virtual_arrays" in output:
                enhanced_result["virtual_arrays"] = output["virtual_arrays"]
            
            return enhanced_result
            
        except Exception as e:
            self.logger.error(f"AI request processing failed: {e}")
            return {
                "error": str(e),
                "processing_time_ms": (datetime.now() - start_time).total_seconds() * 1000
            }
    
    def _apply_request_parameters(self, parameters: Dict[str, Any]):
        """Apply parameters from request"""
        if "w_learning_rate" in parameters:
            self.parameter_manager.config.w_learning_rate = parameters["w_learning_rate"]
        if "b_learning_rate" in parameters:
            self.parameter_manager.config.b_learning_rate = parameters["b_learning_rate"]
        if "w_regularization" in parameters:
            self.parameter_manager.config.w_regularization = parameters["w_regularization"]
        if "b_regularization" in parameters:
            self.parameter_manager.config.b_regularization = parameters["b_regularization"]
    
    def train_model(self, training_data: List[Dict[str, Any]], config: Optional[TrainingConfig] = None) -> Dict[str, Any]:
        """Train the model with provided data"""
        if not self.is_initialized:
            return {"error": "AI system not initialized"}
        
        try:
            self.trainer.config = config or self.trainer.config
            result = self.trainer.train(training_data)
            return {
                "status": "success",
                "training_result": result,
                "parameter_improvements": self.parameter_manager.get_parameter_stats()
            }
        except Exception as e:
            return {"error": f"Training failed: {str(e)}"}
    
    def evaluate_model(self, evaluation_data: List[Dict[str, Any]], tolerance: float = 0.1) -> Dict[str, Any]:
        """Evaluate model performance"""
        if not self.is_initialized:
            return {"error": "AI system not initialized"}
        
        try:
            metrics = self.evaluator.evaluate_model(evaluation_data, tolerance)
            return {
                "status": "success",
                "evaluation_metrics": {
                    "mse": metrics.mse,
                    "r2_score": metrics.r2_score,
                    "accuracy": metrics.accuracy_within_tolerance,
                    "inference_time_ms": metrics.inference_time_ms,
                    "parameter_efficiency": metrics.parameter_efficiency
                },
                "evaluation_summary": self.evaluator.get_evaluation_summary()
            }
        except Exception as e:
            return {"error": f"Evaluation failed: {str(e)}"}
    
    def optimize_parameters(self, optimization_data: List[Dict[str, Any]], algorithm: str = "genetic") -> Dict[str, Any]:
        """Optimize model parameters"""
        if not self.is_initialized:
            return {"error": "AI system not initialized"}
        
        try:
            result = self.optimizer.optimize(optimization_data, algorithm)
            
            # Apply optimized parameters
            self.optimizer.apply_optimized_parameters(result)
            
            return {
                "status": "success",
                "optimization_result": {
                    "algorithm": result.algorithm,
                    "best_score": result.best_score,
                    "best_parameters": result.best_parameters,
                    "optimization_time_seconds": result.optimization_time_seconds,
                    "convergence_iteration": result.convergence_iteration
                },
                "parameter_updates": self.parameter_manager.get_parameter_stats()
            }
        except Exception as e:
            return {"error": f"Optimization failed: {str(e)}"}
    
    def get_system_status(self) -> Dict[str, Any]:
        """Get comprehensive system status"""
        if not self.is_initialized:
            return {"error": "AI system not initialized"}
        
        try:
            status = {
                "system_info": {
                    "model_name": self.model_name,
                    "is_initialized": self.is_initialized,
                    "current_session": self.current_session,
                    "config": self.config
                },
                "parameter_status": self.parameter_manager.get_parameter_stats(),
                "performance_status": self.performance_tracker.get_current_status(),
                "evaluation_summary": self.evaluator.get_evaluation_summary(),
                "training_summary": self.trainer.get_training_summary() if self.trainer.training_history else None
            }
            
            return {
                "status": "success",
                "system_status": status
            }
            
        except Exception as e:
            return {"error": f"Status check failed: {str(e)}"}
    
    def update_parameters(self, parameter_updates: Dict[str, Any]) -> Dict[str, Any]:
        """Update system parameters"""
        if not self.is_initialized:
            return {"error": "AI system not initialized"}
        
        try:
            # Update parameter manager
            if "w_learning_rate" in parameter_updates:
                self.parameter_manager.config.w_learning_rate = parameter_updates["w_learning_rate"]
            if "b_learning_rate" in parameter_updates:
                self.parameter_manager.config.b_learning_rate = parameter_updates["b_learning_rate"]
            if "w_regularization" in parameter_updates:
                self.parameter_manager.config.w_regularization = parameter_updates["w_regularization"]
            if "b_regularization" in parameter_updates:
                self.parameter_manager.config.b_regularization = parameter_updates["b_regularization"]
            if "w_size" in parameter_updates:
                self.parameter_manager.active_w_size = parameter_updates["w_size"]
            if "b_size" in parameter_updates:
                self.parameter_manager.active_b_size = parameter_updates["b_size"]
            
            # Save updated parameters
            self.parameter_manager._save_state()
            
            return {
                "status": "success",
                "updated_parameters": self.parameter_manager.get_parameter_stats()
            }
            
        except Exception as e:
            return {"error": f"Parameter update failed: {str(e)}"}
    
    def export_system_state(self, filepath: str) -> Dict[str, Any]:
        """Export complete system state"""
        if not self.is_initialized:
            return {"error": "AI system not initialized"}
        
        try:
            export_data = {
                "export_timestamp": datetime.now().isoformat(),
                "model_name": self.model_name,
                "system_status": self.get_system_status(),
                "parameter_manager_state": self.parameter_manager.state.__dict__,
                "parameter_config": self.parameter_manager.config.__dict__,
                "performance_metrics": self.performance_tracker.get_performance_summary(24),
                "evaluation_history": [metric.__dict__ for metric in self.evaluator.evaluation_history],
                "training_history": [metric.__dict__ for metric in self.trainer.training_history] if self.trainer.training_history else []
            }
            
            with open(filepath, 'w') as f:
                json.dump(export_data, f, indent=2)
            
            return {
                "status": "success",
                "export_file": filepath,
                "export_size_mb": os.path.getsize(filepath) / (1024 * 1024)
            }
            
        except Exception as e:
            return {"error": f"Export failed: {str(e)}"}
    
    def shutdown_system(self) -> Dict[str, Any]:
        """Shutdown the AI system gracefully"""
        try:
            # Stop performance monitoring
            self.performance_tracker.stop_monitoring()
            
            # Save final state
            self.parameter_manager._save_state()
            
            # Export final state
            export_path = f"ai_backend/models/exports/{self.model_name}_final_state_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
            self.export_system_state(export_path)
            
            self.is_initialized = False
            
            return {
                "status": "success",
                "shutdown_time": datetime.now().isoformat(),
                "final_export": export_path
            }
            
        except Exception as e:
            return {"error": f"Shutdown failed: {str(e)}"}
    
    def compare_optimization_algorithms(self, test_data: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Compare different optimization algorithms"""
        if not self.is_initialized:
            return {"error": "AI system not initialized"}
        
        try:
            comparison_result = self.optimizer.compare_algorithms(test_data)
            return {
                "status": "success",
                "algorithm_comparison": comparison_result,
                "recommendation": f"Best algorithm: {comparison_result['best_algorithm']} with score {comparison_result['best_score']:.4f}"
            }
        except Exception as e:
            return {"error": f"Algorithm comparison failed: {str(e)}"}
    
    def get_available_models(self) -> Dict[str, Any]:
        """Get list of available AI models"""
        try:
            available_models = get_available_models()
            return {
                "status": "success",
                "available_models": available_models,
                "current_model": get_model_manager().get_current_model()
            }
        except Exception as e:
            return {"error": f"Failed to get available models: {str(e)}"}
    
    def switch_model(self, model_name: str) -> Dict[str, Any]:
        """Switch to a different AI model"""
        try:
            result = switch_model(model_name)
            
            if result["success"]:
                # Update our model name
                self.model_name = model_name
                self.logger.info(f"Switched to model: {model_name}")
                
                # Update configuration for new model
                self.config = ai_config.get_model_config(model_name)
                
                return {
                    "status": "success",
                    "model_switched": model_name,
                    "previous_model": result.get("previous_model"),
                    "new_config": self.config
                }
            else:
                return {
                    "status": "error",
                    "error": result["error"],
                    "available_models": [m["name"] for m in get_available_models() if m["available"]]
                }
        except Exception as e:
            return {"error": f"Model switch failed: {str(e)}"}
    
    def process_with_model(self, prompt: str, model_name: Optional[str] = None, parameters: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Process AI request with specified model (or current model)"""
        try:
            target_model = model_name or self.model_name
            
            if target_model == "nexus":
                # Use the enhanced nexus processing
                return self.process_ai_request(prompt, parameters)
            else:
                # Use the model manager for other models
                model_manager = get_model_manager()
                result = model_manager.generate_with_model(target_model, prompt, **(parameters or {}))
                
                if result["success"]:
                    return {
                        "response": result["response"],
                        "model_used": result["model_used"],
                        "mode": "external_model",
                        "ai_enhancement": {
                            "parameter_stats": self.parameter_manager.get_parameter_stats(),
                            "performance_metrics": self.performance_tracker.get_current_status()
                        }
                    }
                else:
                    return {
                        "error": result["error"],
                        "response": result.get("response", ""),
                        "model_used": target_model
                    }
        except Exception as e:
            return {"error": f"Model processing failed: {str(e)}"}

# Global instance for easy access
global_ai_manager = None

def get_ai_manager(model_name: str = "nexus") -> UnifiedAIManager:
    """Get or create global AI manager instance"""
    global global_ai_manager
    if global_ai_manager is None or global_ai_manager.model_name != model_name:
        global_ai_manager = UnifiedAIManager(model_name)
    return global_ai_manager

def initialize_enhanced_ai(model_name: str = "nexus") -> Dict[str, Any]:
    """Initialize enhanced AI system"""
    manager = get_ai_manager(model_name)
    return manager.initialize_system()

def process_enhanced_request(prompt: str, parameters: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
    """Process request with enhanced AI capabilities"""
    manager = get_ai_manager()
    return manager.process_ai_request(prompt, parameters)

