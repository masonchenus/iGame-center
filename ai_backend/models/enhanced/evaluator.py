"""
Model Evaluator
Comprehensive evaluation system for AI models with performance metrics
"""
import json
import os
import numpy as np
from typing import Dict, List, Tuple, Optional, Any, Callable
from dataclasses import dataclass, asdict
from datetime import datetime, timedelta
import logging
from ai_backend.models.enhanced.parameter_manager import ParameterManager
from ai_backend.models.config.ai_config import ai_config

@dataclass
class EvaluationMetrics:
    """Comprehensive evaluation metrics"""
    model_name: str
    evaluation_timestamp: str
    
    # Core Metrics
    mse: float
    mae: float
    rmse: float
    r2_score: float
    
    # Parameter Metrics
    parameter_efficiency: float
    convergence_rate: float
    stability_score: float
    
    # Performance Metrics
    inference_time_ms: float
    memory_usage_mb: float
    throughput_samples_per_second: float
    
    # Virtual Array Metrics
    w_array_utilization: float
    b_array_utilization: float
    array_access_patterns: Dict[str, Any]
    
    # Model-Specific Metrics
    accuracy_within_tolerance: float
    prediction_confidence: float
    overfitting_score: float

class ModelEvaluator:
    """Comprehensive model evaluation system"""
    
    def __init__(self, model_name: str = "nexus"):
        self.model_name = model_name
        self.logger = logging.getLogger(__name__)
        
        # Initialize parameter manager for evaluation
        self.parameter_manager = ParameterManager(model_name)
        
        # Evaluation history
        self.evaluation_history = []
        self.evaluation_dir = f"ai_backend/models/evaluation/{model_name}"
        
        # Ensure evaluation directory exists
        os.makedirs(self.evaluation_dir, exist_ok=True)
        
    def prepare_evaluation_data(self, data: List[Dict[str, Any]]) -> Tuple[List[float], List[float]]:
        """Prepare evaluation data from various formats"""
        if not data:
            return [], []
            
        # Handle different data formats
        if isinstance(data[0], dict):
            # Dictionary format: {'x': value, 'y': value}
            x_values = [item.get('x', 0) for item in data]
            y_values = [item.get('y', 0) for item in data]
        elif isinstance(data[0], (list, tuple)) and len(data[0]) >= 2:
            # List/tuple format: [x, y] or (x, y)
            x_values = [item[0] for item in data]
            y_values = [item[1] for item in data]
        else:
            raise ValueError("Unsupported data format. Use [{'x': val, 'y': val}] or [[x,y]]")
            
        return x_values, y_values
    
    def compute_core_metrics(self, y_true: List[float], y_pred: List[float]) -> Dict[str, float]:
        """Compute core evaluation metrics"""
        if len(y_true) != len(y_pred):
            raise ValueError("y_true and y_pred must have same length")
            
        n = len(y_true)
        if n == 0:
            return {"mse": 0, "mae": 0, "rmse": 0, "r2_score": 0}
        
        # Mean Squared Error
        mse = sum((true - pred) ** 2 for true, pred in zip(y_true, y_pred)) / n
        
        # Mean Absolute Error
        mae = sum(abs(true - pred) for true, pred in zip(y_true, y_pred)) / n
        
        # Root Mean Squared Error
        rmse = np.sqrt(mse)
        
        # R-squared (coefficient of determination)
        y_mean = sum(y_true) / n
        ss_tot = sum((y - y_mean) ** 2 for y in y_true)
        ss_res = sum((true - pred) ** 2 for true, pred in zip(y_true, y_pred))
        r2_score = 1 - (ss_res / ss_tot) if ss_tot > 0 else 0
        
        return {
            "mse": mse,
            "mae": mae,
            "rmse": rmse,
            "r2_score": max(0, r2_score)  # Ensure non-negative
        }
    
    def compute_parameter_metrics(self) -> Dict[str, float]:
        """Compute parameter-specific metrics"""
        param_stats = self.parameter_manager.get_parameter_stats()
        
        # Parameter efficiency (based on performance vs parameter count)
        performance_score = param_stats.get("training", {}).get("performance_score", 0)
        parameter_count = (
            param_stats.get("active_parameters", {}).get("w_size", 0) +
            param_stats.get("active_parameters", {}).get("b_size", 0)
        )
        
        parameter_efficiency = performance_score / max(parameter_count, 1) if parameter_count > 0 else 0
        
        # Convergence rate (improvement per training step)
        training_steps = param_stats.get("training", {}).get("steps", 0)
        convergence_rate = performance_score / max(training_steps, 1) if training_steps > 0 else 0
        
        # Stability score (based on parameter variance)
        w_std = param_stats.get("statistics", {}).get("w_std", 0)
        b_std = param_stats.get("statistics", {}).get("b_std", 0)
        stability_score = 1.0 / (1.0 + w_std + b_std)  # Lower variance = higher stability
        
        return {
            "parameter_efficiency": parameter_efficiency,
            "convergence_rate": convergence_rate,
            "stability_score": stability_score
        }
    
    def compute_performance_metrics(self, x_values: List[float], y_true: List[float]) -> Dict[str, float]:
        """Compute performance and efficiency metrics"""
        import time
        
        # Measure inference time
        start_time = time.time()
        
        # Perform predictions
        y_pred, _ = self.parameter_manager.get_parameters(x_values)
        
        end_time = time.time()
        inference_time_ms = (end_time - start_time) * 1000
        
        # Calculate throughput
        n_samples = len(x_values)
        throughput = n_samples / max(inference_time_ms / 1000, 0.001)  # samples per second
        
        # Estimate memory usage (simplified)
        memory_usage_mb = (n_samples * 8 * 2) / (1024 * 1024)  # Rough estimate for arrays
        
        return {
            "inference_time_ms": inference_time_ms,
            "memory_usage_mb": memory_usage_mb,
            "throughput_samples_per_second": throughput
        }
    
    def compute_virtual_array_metrics(self) -> Dict[str, Any]:
        """Compute metrics related to virtual array usage"""
        param_stats = self.parameter_manager.get_parameter_stats()
        
        # Array utilization
        w_size = param_stats.get("virtual_array_sizes", {}).get("w", 0)
        b_size = param_stats.get("virtual_array_sizes", {}).get("b", 0)
        active_w = param_stats.get("active_parameters", {}).get("w_size", 0)
        active_b = param_stats.get("active_parameters", {}).get("b_size", 0)
        
        w_utilization = active_w / max(w_size, 1) if w_size > 0 else 0
        b_utilization = active_b / max(b_size, 1) if b_size > 0 else 0
        
        # Array access patterns (simulated)
        access_patterns = {
            "sequential_accesses": np.random.poisson(100),
            "random_accesses": np.random.poisson(50),
            "cache_hits": np.random.uniform(0.7, 0.95),
            "cache_misses": np.random.uniform(0.05, 0.3)
        }
        
        return {
            "w_array_utilization": w_utilization,
            "b_array_utilization": b_utilization,
            "array_access_patterns": access_patterns
        }
    
    def compute_model_specific_metrics(self, y_true: List[float], y_pred: List[float], 
                                     tolerance: float = 0.1) -> Dict[str, float]:
        """Compute model-specific evaluation metrics"""
        if len(y_true) != len(y_pred):
            return {"accuracy_within_tolerance": 0, "prediction_confidence": 0, "overfitting_score": 0}
        
        n = len(y_true)
        if n == 0:
            return {"accuracy_within_tolerance": 0, "prediction_confidence": 0, "overfitting_score": 0}
        
        # Accuracy within tolerance
        correct_predictions = sum(1 for true, pred in zip(y_true, y_pred) 
                                if abs(true - pred) <= tolerance)
        accuracy_within_tolerance = correct_predictions / n
        
        # Prediction confidence (based on prediction variance)
        predictions = np.array(y_pred)
        prediction_confidence = 1.0 / (1.0 + np.std(predictions))
        
        # Overfitting score (simplified - compares training vs validation performance)
        # This is a simplified metric; in practice, you'd need separate training/validation sets
        train_var = np.var(y_true)
        pred_var = np.var(y_pred)
        overfitting_score = abs(train_var - pred_var) / max(train_var, 1)
        
        return {
            "accuracy_within_tolerance": accuracy_within_tolerance,
            "prediction_confidence": prediction_confidence,
            "overfitting_score": overfitting_score
        }
    
    def evaluate_model(self, evaluation_data: List[Dict[str, Any]], 
                      tolerance: float = 0.1) -> EvaluationMetrics:
        """
        Comprehensive model evaluation
        
        Args:
            evaluation_data: Data for evaluation with 'x' and 'y' keys
            tolerance: Tolerance for accuracy calculations
            
        Returns:
            Comprehensive evaluation metrics
        """
        self.logger.info(f"Starting evaluation for model {self.model_name}")
        
        # Prepare data
        x_values, y_true = self.prepare_evaluation_data(evaluation_data)
        
        if not x_values or not y_true:
            raise ValueError("No valid evaluation data provided")
        
        # Get predictions
        y_pred, b_bias = self.parameter_manager.get_parameters(x_values)
        
        # Compute all metric categories
        core_metrics = self.compute_core_metrics(y_true, y_pred)
        parameter_metrics = self.compute_parameter_metrics()
        performance_metrics = self.compute_performance_metrics(x_values, y_true)
        virtual_array_metrics = self.compute_virtual_array_metrics()
        model_specific_metrics = self.compute_model_specific_metrics(y_true, y_pred, tolerance)
        
        # Combine all metrics
        evaluation_metrics = EvaluationMetrics(
            model_name=self.model_name,
            evaluation_timestamp=datetime.now().isoformat(),
            
            # Core metrics
            mse=core_metrics["mse"],
            mae=core_metrics["mae"],
            rmse=core_metrics["rmse"],
            r2_score=core_metrics["r2_score"],
            
            # Parameter metrics
            parameter_efficiency=parameter_metrics["parameter_efficiency"],
            convergence_rate=parameter_metrics["convergence_rate"],
            stability_score=parameter_metrics["stability_score"],
            
            # Performance metrics
            inference_time_ms=performance_metrics["inference_time_ms"],
            memory_usage_mb=performance_metrics["memory_usage_mb"],
            throughput_samples_per_second=performance_metrics["throughput_samples_per_second"],
            
            # Virtual array metrics
            w_array_utilization=virtual_array_metrics["w_array_utilization"],
            b_array_utilization=virtual_array_metrics["b_array_utilization"],
            array_access_patterns=virtual_array_metrics["array_access_patterns"],
            
            # Model-specific metrics
            accuracy_within_tolerance=model_specific_metrics["accuracy_within_tolerance"],
            prediction_confidence=model_specific_metrics["prediction_confidence"],
            overfitting_score=model_specific_metrics["overfitting_score"]
        )
        
        # Save evaluation to history
        self.evaluation_history.append(evaluation_metrics)
        self._save_evaluation(evaluation_metrics)
        
        self.logger.info(f"Evaluation completed for model {self.model_name}")
        return evaluation_metrics
    
    def _save_evaluation(self, metrics: EvaluationMetrics):
        """Save evaluation results to file"""
        try:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            eval_file = os.path.join(self.evaluation_dir, f"evaluation_{timestamp}.json")
            
            with open(eval_file, 'w') as f:
                json.dump(asdict(metrics), f, indent=2)
                
        except Exception as e:
            self.logger.error(f"Failed to save evaluation: {e}")
    
    def get_evaluation_summary(self) -> Dict[str, Any]:
        """Get comprehensive evaluation summary"""
        if not self.evaluation_history:
            return {"error": "No evaluations available"}
        
        latest_eval = self.evaluation_history[-1]
        
        # Calculate improvements over time
        improvements = {}
        if len(self.evaluation_history) > 1:
            previous_eval = self.evaluation_history[-2]
            improvements = {
                "mse_change": latest_eval.mse - previous_eval.mse,
                "r2_score_change": latest_eval.r2_score - previous_eval.r2_score,
                "performance_change": latest_eval.throughput_samples_per_second - previous_eval.throughput_samples_per_second
            }
        
        return {
            "model_name": self.model_name,
            "total_evaluations": len(self.evaluation_history),
            "latest_evaluation": asdict(latest_eval),
            "improvements": improvements,
            "performance_grade": self._calculate_performance_grade(latest_eval),
            "recommendations": self._generate_recommendations(latest_eval)
        }
    
    def _calculate_performance_grade(self, metrics: EvaluationMetrics) -> str:
        """Calculate overall performance grade"""
        score = 0
        
        # R2 score (0-40 points)
        score += min(metrics.r2_score * 40, 40)
        
        # Accuracy (0-30 points)
        score += metrics.accuracy_within_tolerance * 30
        
        # Performance (0-20 points)
        score += min(metrics.throughput_samples_per_second / 1000 * 20, 20)
        
        # Stability (0-10 points)
        score += metrics.stability_score * 10
        
        # Convert to grade
        if score >= 90:
            return "A+"
        elif score >= 80:
            return "A"
        elif score >= 70:
            return "B"
        elif score >= 60:
            return "C"
        else:
            return "D"
    
    def _generate_recommendations(self, metrics: EvaluationMetrics) -> List[str]:
        """Generate improvement recommendations"""
        recommendations = []
        
        if metrics.r2_score < 0.7:
            recommendations.append("Consider improving model parameters or increasing training data")
        
        if metrics.inference_time_ms > 100:
            recommendations.append("Optimize parameter retrieval or reduce active parameter size")
        
        if metrics.overfitting_score > 0.5:
            recommendations.append("Model may be overfitting; consider regularization or more data")
        
        if metrics.stability_score < 0.5:
            recommendations.append("Parameter updates are unstable; reduce learning rate")
        
        if metrics.accuracy_within_tolerance < 0.8:
            recommendations.append("Increase tolerance or improve model precision")
        
        if not recommendations:
            recommendations.append("Model performance is good; continue monitoring")
        
        return recommendations
    
    def compare_models(self, other_evaluators: List['ModelEvaluator']) -> Dict[str, Any]:
        """Compare current model with other models"""
        if not self.evaluation_history:
            return {"error": "No evaluations available for comparison"}
        
        current_metrics = self.evaluation_history[-1]
        comparison_results = {
            "current_model": self.model_name,
            "comparison_timestamp": datetime.now().isoformat()
        }
        
        for evaluator in other_evaluators:
            if not evaluator.evaluation_history:
                continue
                
            other_metrics = evaluator.evaluation_history[-1]
            comparison_results[evaluator.model_name] = {
                "r2_score": other_metrics.r2_score,
                "mse": other_metrics.mse,
                "accuracy": other_metrics.accuracy_within_tolerance,
                "performance": other_metrics.throughput_samples_per_second,
                "grade": self._calculate_performance_grade(other_metrics)
            }
        
        # Determine best model
        best_model = self.model_name
        best_score = current_metrics.r2_score
        
        for evaluator in other_evaluators:
            if not evaluator.evaluation_history:
                continue
            other_metrics = evaluator.evaluation_history[-1]
            if other_metrics.r2_score > best_score:
                best_score = other_metrics.r2_score
                best_model = evaluator.model_name
        
        comparison_results["best_model"] = best_model
        comparison_results["best_score"] = best_score
        
        return comparison_results

