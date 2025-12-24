"""
Performance Tracker
Real-time monitoring and performance tracking for AI systems
"""
import json
import os
import time
import psutil
from typing import Dict, List, Optional, Any, Callable
from dataclasses import dataclass, asdict
from datetime import datetime, timedelta
import threading
import logging
from collections import deque
from ai_backend.models.config.ai_config import ai_config

@dataclass
class PerformanceMetrics:
    """Real-time performance metrics"""
    timestamp: str
    cpu_usage_percent: float
    memory_usage_mb: float
    memory_usage_percent: float
    disk_usage_percent: float
    active_connections: int
    api_requests_per_minute: float
    average_response_time_ms: float
    error_rate_percent: float
    model_inference_time_ms: float
    parameter_access_count: int
    virtual_array_operations: int

@dataclass
class AlertRule:
    """Alert rule configuration"""
    name: str
    metric: str
    threshold: float
    comparison: str  # 'greater_than', 'less_than', 'equals'
    severity: str    # 'low', 'medium', 'high', 'critical'
    enabled: bool = True
    cooldown_minutes: int = 5

class PerformanceTracker:
    """Real-time performance monitoring for AI systems"""
    
    def __init__(self, model_name: str = "nexus"):
        self.model_name = model_name
        self.logger = logging.getLogger(__name__)
        
        # Monitoring configuration
        self.monitoring_dir = f"ai_backend/monitoring/{model_name}"
        self.metrics_file = os.path.join(self.monitoring_dir, "performance_metrics.json")
        self.alerts_file = os.path.join(self.monitoring_dir, "alerts.json")
        
        # Ensure monitoring directory exists
        os.makedirs(self.monitoring_dir, exist_ok=True)
        
        # Metrics storage (keep last 1000 entries in memory)
        self.metrics_history = deque(maxlen=1000)
        self.current_metrics = None
        
        # Alert system
        self.alert_rules = self._load_alert_rules()
        self.active_alerts = []
        self.alert_cooldowns = {}
        
        # Real-time tracking
        self.request_times = deque(maxlen=100)
        self.api_request_count = 0
        self.error_count = 0
        self.start_time = time.time()
        
        # Threading for background monitoring
        self.monitoring_active = False
        self.monitoring_thread = None
        
    def _load_alert_rules(self) -> List[AlertRule]:
        """Load alert rules from configuration"""
        default_rules = [
            AlertRule("High CPU Usage", "cpu_usage_percent", 80.0, "greater_than", "medium"),
            AlertRule("High Memory Usage", "memory_usage_percent", 85.0, "greater_than", "high"),
            AlertRule("Slow Response Time", "average_response_time_ms", 5000.0, "greater_than", "medium"),
            AlertRule("High Error Rate", "error_rate_percent", 10.0, "greater_than", "critical"),
            AlertRule("Low Disk Space", "disk_usage_percent", 90.0, "greater_than", "critical"),
        ]
        
        try:
            if os.path.exists(self.alerts_file):
                with open(self.alerts_file, 'r') as f:
                    data = json.load(f)
                return [AlertRule(**rule) for rule in data]
        except Exception as e:
            self.logger.warning(f"Could not load alert rules: {e}")
        
        return default_rules
    
    def _save_alert_rules(self):
        """Save alert rules to file"""
        try:
            with open(self.alerts_file, 'w') as f:
                json.dump([asdict(rule) for rule in self.alert_rules], f, indent=2)
        except Exception as e:
            self.logger.error(f"Failed to save alert rules: {e}")
    
    def get_system_metrics(self) -> Dict[str, float]:
        """Get current system metrics"""
        # CPU usage
        cpu_percent = psutil.cpu_percent(interval=1)
        
        # Memory usage
        memory = psutil.virtual_memory()
        memory_mb = memory.used / (1024 * 1024)
        memory_percent = memory.percent
        
        # Disk usage
        disk = psutil.disk_usage('/')
        disk_percent = (disk.used / disk.total) * 100
        
        return {
            "cpu_usage_percent": cpu_percent,
            "memory_usage_mb": memory_mb,
            "memory_usage_percent": memory_percent,
            "disk_usage_percent": disk_percent
        }
    
    def get_ai_metrics(self) -> Dict[str, Any]:
        """Get AI-specific metrics"""
        uptime = time.time() - self.start_time
        
        # Calculate request rate (requests per minute)
        requests_per_minute = (self.api_request_count / uptime) * 60 if uptime > 0 else 0
        
        # Calculate average response time
        avg_response_time = sum(self.request_times) / len(self.request_times) if self.request_times else 0
        
        # Calculate error rate
        error_rate = (self.error_count / max(self.api_request_count, 1)) * 100
        
        return {
            "uptime_seconds": uptime,
            "api_requests_per_minute": requests_per_minute,
            "average_response_time_ms": avg_response_time,
            "error_rate_percent": error_rate,
            "total_requests": self.api_request_count,
            "total_errors": self.error_count
        }
    
    def collect_metrics(self) -> PerformanceMetrics:
        """Collect all performance metrics"""
        # Get system metrics
        system_metrics = self.get_system_metrics()
        ai_metrics = self.get_ai_metrics()
        
        # Simulate some AI-specific metrics
        active_connections = 1  # Simulated
        model_inference_time = 10.0 + (system_metrics["cpu_usage_percent"] * 0.1)  # Simulated
        parameter_access_count = len(self.request_times) * 5  # Simulated
        virtual_array_operations = parameter_access_count * 10  # Simulated
        
        # Create metrics object
        metrics = PerformanceMetrics(
            timestamp=datetime.now().isoformat(),
            cpu_usage_percent=system_metrics["cpu_usage_percent"],
            memory_usage_mb=system_metrics["memory_usage_mb"],
            memory_usage_percent=system_metrics["memory_usage_percent"],
            disk_usage_percent=system_metrics["disk_usage_percent"],
            active_connections=active_connections,
            api_requests_per_minute=ai_metrics["api_requests_per_minute"],
            average_response_time_ms=ai_metrics["average_response_time_ms"],
            error_rate_percent=ai_metrics["error_rate_percent"],
            model_inference_time_ms=model_inference_time,
            parameter_access_count=parameter_access_count,
            virtual_array_operations=virtual_array_operations
        )
        
        # Store current metrics
        self.current_metrics = metrics
        self.metrics_history.append(metrics)
        
        # Save to file periodically
        self._save_metrics(metrics)
        
        return metrics
    
    def _save_metrics(self, metrics: PerformanceMetrics):
        """Save metrics to file"""
        try:
            # Load existing metrics
            all_metrics = []
            if os.path.exists(self.metrics_file):
                with open(self.metrics_file, 'r') as f:
                    all_metrics = json.load(f)
            
            # Add new metrics (keep last 1000)
            all_metrics.append(asdict(metrics))
            if len(all_metrics) > 1000:
                all_metrics = all_metrics[-1000:]
            
            # Save updated metrics
            with open(self.metrics_file, 'w') as f:
                json.dump(all_metrics, f, indent=2)
                
        except Exception as e:
            self.logger.error(f"Failed to save metrics: {e}")
    
    def record_api_request(self, response_time_ms: float, success: bool = True):
        """Record API request for metrics"""
        self.request_times.append(response_time_ms)
        self.api_request_count += 1
        
        if not success:
            self.error_count += 1
    
    def check_alerts(self) -> List[Dict[str, Any]]:
        """Check all alert rules and return triggered alerts"""
        if not self.current_metrics:
            return []
        
        triggered_alerts = []
        
        for rule in self.alert_rules:
            if not rule.enabled:
                continue
            
            # Check cooldown
            last_triggered = self.alert_cooldowns.get(rule.name, 0)
            time_since_last = time.time() - last_triggered
            if time_since_last < (rule.cooldown_minutes * 60):
                continue
            
            # Get metric value
            metric_value = getattr(self.current_metrics, rule.metric, 0)
            
            # Check threshold
            trigger_alert = False
            if rule.comparison == "greater_than" and metric_value > rule.threshold:
                trigger_alert = True
            elif rule.comparison == "less_than" and metric_value < rule.threshold:
                trigger_alert = True
            elif rule.comparison == "equals" and abs(metric_value - rule.threshold) < 0.01:
                trigger_alert = True
            
            if trigger_alert:
                alert = {
                    "rule_name": rule.name,
                    "metric": rule.metric,
                    "threshold": rule.threshold,
                    "actual_value": metric_value,
                    "severity": rule.severity,
                    "timestamp": datetime.now().isoformat(),
                    "message": f"{rule.name}: {metric_value:.2f} {rule.comparison} {rule.threshold}"
                }
                
                triggered_alerts.append(alert)
                self.alert_cooldowns[rule.name] = time.time()
                
                self.logger.warning(f"Alert triggered: {alert['message']}")
        
        return triggered_alerts
    
    def start_monitoring(self, interval_seconds: int = 30):
        """Start background monitoring"""
        if self.monitoring_active:
            return
        
        self.monitoring_active = True
        
        def monitoring_loop():
            while self.monitoring_active:
                try:
                    self.collect_metrics()
                    self.check_alerts()
                    time.sleep(interval_seconds)
                except Exception as e:
                    self.logger.error(f"Monitoring error: {e}")
                    time.sleep(interval_seconds)
        
        self.monitoring_thread = threading.Thread(target=monitoring_loop, daemon=True)
        self.monitoring_thread.start()
        self.logger.info("Performance monitoring started")
    
    def stop_monitoring(self):
        """Stop background monitoring"""
        self.monitoring_active = False
        if self.monitoring_thread:
            self.monitoring_thread.join(timeout=5)
        self.logger.info("Performance monitoring stopped")
    
    def get_current_status(self) -> Dict[str, Any]:
        """Get current system status"""
        if not self.current_metrics:
            self.collect_metrics()
        
        alerts = self.check_alerts()
        
        return {
            "model_name": self.model_name,
            "monitoring_active": self.monitoring_active,
            "current_metrics": asdict(self.current_metrics) if self.current_metrics else None,
            "active_alerts": len(alerts),
            "recent_alerts": alerts[-5:] if alerts else [],
            "system_health": self._calculate_system_health(),
            "uptime_hours": (time.time() - self.start_time) / 3600
        }
    
    def _calculate_system_health(self) -> str:
        """Calculate overall system health"""
        if not self.current_metrics:
            return "unknown"
        
        health_score = 100
        
        # Deduct points for high usage
        if self.current_metrics.cpu_usage_percent > 80:
            health_score -= 20
        elif self.current_metrics.cpu_usage_percent > 60:
            health_score -= 10
        
        if self.current_metrics.memory_usage_percent > 85:
            health_score -= 20
        elif self.current_metrics.memory_usage_percent > 70:
            health_score -= 10
        
        if self.current_metrics.disk_usage_percent > 90:
            health_score -= 15
        elif self.current_metrics.disk_usage_percent > 80:
            health_score -= 5
        
        if self.current_metrics.error_rate_percent > 10:
            health_score -= 25
        elif self.current_metrics.error_rate_percent > 5:
            health_score -= 10
        
        # Determine health status
        if health_score >= 90:
            return "excellent"
        elif health_score >= 75:
            return "good"
        elif health_score >= 60:
            return "fair"
        elif health_score >= 40:
            return "poor"
        else:
            return "critical"
    
    def get_performance_summary(self, hours: int = 24) -> Dict[str, Any]:
        """Get performance summary for specified time period"""
        cutoff_time = datetime.now() - timedelta(hours=hours)
        
        # Filter metrics for time period
        recent_metrics = [
            m for m in self.metrics_history 
            if datetime.fromisoformat(m.timestamp) > cutoff_time
        ]
        
        if not recent_metrics:
            return {"error": "No metrics available for the specified period"}
        
        # Calculate summary statistics
        cpu_values = [m.cpu_usage_percent for m in recent_metrics]
        memory_values = [m.memory_usage_percent for m in recent_metrics]
        response_times = [m.average_response_time_ms for m in recent_metrics]
        error_rates = [m.error_rate_percent for m in recent_metrics]
        
        return {
            "period_hours": hours,
            "data_points": len(recent_metrics),
            "averages": {
                "cpu_usage_percent": sum(cpu_values) / len(cpu_values),
                "memory_usage_percent": sum(memory_values) / len(memory_values),
                "response_time_ms": sum(response_times) / len(response_times),
                "error_rate_percent": sum(error_rates) / len(error_rates)
            },
            "peaks": {
                "max_cpu_percent": max(cpu_values),
                "max_memory_percent": max(memory_values),
                "max_response_time_ms": max(response_times),
                "max_error_rate_percent": max(error_rates)
            },
            "recommendations": self._generate_performance_recommendations(recent_metrics)
        }
    
    def _generate_performance_recommendations(self, metrics: List[PerformanceMetrics]) -> List[str]:
        """Generate performance improvement recommendations"""
        recommendations = []
        
        avg_cpu = sum(m.cpu_usage_percent for m in metrics) / len(metrics)
        avg_memory = sum(m.memory_usage_percent for m in metrics) / len(metrics)
        avg_response = sum(m.average_response_time_ms for m in metrics) / len(metrics)
        avg_error = sum(m.error_rate_percent for m in metrics) / len(metrics)
        
        if avg_cpu > 70:
            recommendations.append("Consider optimizing CPU-intensive operations or scaling resources")
        
        if avg_memory > 80:
            recommendations.append("High memory usage detected; consider reducing parameter cache size")
        
        if avg_response > 2000:
            recommendations.append("Slow response times; optimize parameter retrieval or model inference")
        
        if avg_error > 5:
            recommendations.append("High error rate; review error handling and input validation")
        
        if not recommendations:
            recommendations.append("System performance is optimal")
        
        return recommendations
    
    def add_alert_rule(self, rule: AlertRule):
        """Add new alert rule"""
        self.alert_rules.append(rule)
        self._save_alert_rules()
    
    def remove_alert_rule(self, rule_name: str):
        """Remove alert rule by name"""
        self.alert_rules = [r for r in self.alert_rules if r.name != rule_name]
        self._save_alert_rules()
    
    def export_metrics(self, filepath: str, hours: int = 24):
        """Export metrics to file"""
        cutoff_time = datetime.now() - timedelta(hours=hours)
        
        recent_metrics = [
            m for m in self.metrics_history 
            if datetime.fromisoformat(m.timestamp) > cutoff_time
        ]
        
        export_data = {
            "model_name": self.model_name,
            "export_timestamp": datetime.now().isoformat(),
            "period_hours": hours,
            "metrics": [asdict(m) for m in recent_metrics],
            "summary": self.get_performance_summary(hours)
        }
        
        with open(filepath, 'w') as f:
            json.dump(export_data, f, indent=2)
        
        self.logger.info(f"Metrics exported to {filepath}")

