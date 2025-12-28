"""
Scheduler Configuration

Configuration settings for the AI task scheduling and resource management system.
"""

import os
from typing import Dict, Any, List
from dataclasses import dataclass, field
from enum import Enum
import threading
import time


class SchedulingStrategy(Enum):
    """Task scheduling strategies."""
    FIFO = "fifo"  # First In, First Out
    PRIORITY = "priority"  # Priority-based
    ROUND_ROBIN = "round_robin"  # Round-robin
    FAIR_SHARE = "fair_share"  # Fair share scheduling
    SHORTEST_JOB_FIRST = "sjf"  # Shortest Job First
    MULTI_LEVEL_QUEUE = "mlq"  # Multi-Level Queue


class ResourcePolicy(Enum):
    """Resource allocation policies."""
    CONSERVATIVE = "conservative"  # Conservative resource usage
    BALANCED = "balanced"  # Balanced resource usage
    AGGRESSIVE = "aggressive"  # Aggressive resource usage
    ADAPTIVE = "adaptive"  # Adaptive resource allocation


@dataclass
class SchedulerConfig:
    """Configuration class for AI task scheduling."""
    
    # Core scheduling settings
    scheduling_strategy: SchedulingStrategy = SchedulingStrategy.PRIORITY
    max_concurrent_tasks: int = 10
    task_timeout_seconds: int = 300
    max_queue_size: int = 1000
    
    # Resource management settings
    resource_policy: ResourcePolicy = ResourcePolicy.BALANCED
    max_cpu_percent: float = 80.0
    max_memory_mb: int = 2048
    max_disk_io_mb_per_sec: int = 100
    
    # Performance optimization settings
    enable_caching: bool = True
    cache_size: int = 500
    batch_processing: bool = True
    batch_size: int = 5
    worker_threads: int = 4
    
    # Priority handling settings
    enable_priority: bool = True
    priority_levels: int = 5
    starvation_prevention: bool = True
    starvation_threshold_seconds: float = 60.0
    
    # Monitoring and logging settings
    enable_monitoring: bool = True
    monitoring_interval_seconds: float = 5.0
    log_performance_stats: bool = True
    detailed_logging: bool = False
    
    # Health and recovery settings
    health_check_enabled: bool = True
    health_check_interval_seconds: float = 30.0
    auto_recovery: bool = True
    max_retry_attempts: int = 3
    
    # Task-specific settings
    default_task_priority: int = 3  # 1=highest, 5=lowest
    task_cleanup_interval_seconds: float = 300.0  # 5 minutes
    dead_task_timeout_seconds: float = 1800.0  # 30 minutes
    
    # Statistics and analytics settings
    collect_detailed_stats: bool = True
    stats_retention_hours: int = 24
    enable_performance_prediction: bool = False
    
    # Resource pool settings
    enable_resource_pooling: bool = True
    pool_size: int = 8
    resource_reuse: bool = True
    prewarm_resources: bool = False
    
    @classmethod
    def from_dict(cls, config_dict: Dict[str, Any]) -> 'SchedulerConfig':
        """Create config from dictionary."""
        # Convert string enums if needed
        if 'scheduling_strategy' in config_dict and isinstance(config_dict['scheduling_strategy'], str):
            config_dict['scheduling_strategy'] = SchedulingStrategy(config_dict['scheduling_strategy'])
        
        if 'resource_policy' in config_dict and isinstance(config_dict['resource_policy'], str):
            config_dict['resource_policy'] = ResourcePolicy(config_dict['resource_policy'])
        
        return cls(**config_dict)
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert config to dictionary."""
        result = {}
        for key, value in self.__dict__.items():
            if isinstance(value, Enum):
                result[key] = value.value
            else:
                result[key] = value
        return result
    
    @classmethod
    def get_default_config(cls) -> 'SchedulerConfig':
        """Get default configuration."""
        return cls()
    
    @classmethod
    def get_high_throughput_config(cls) -> 'SchedulerConfig':
        """Get configuration optimized for high throughput."""
        return cls(
            scheduling_strategy=SchedulingStrategy.FIFO,
            max_concurrent_tasks=20,
            batch_processing=True,
            batch_size=10,
            worker_threads=8,
            enable_caching=True,
            cache_size=1000,
            resource_policy=ResourcePolicy.AGGRESSIVE,
            max_cpu_percent=90.0,
            max_memory_mb=4096
        )
    
    @classmethod
    def get_low_latency_config(cls) -> 'SchedulerConfig':
        """Get configuration optimized for low latency."""
        return cls(
            scheduling_strategy=SchedulingStrategy.PRIORITY,
            max_concurrent_tasks=5,
            task_timeout_seconds=60,
            worker_threads=2,
            batch_processing=False,
            resource_policy=ResourcePolicy.CONSERVATIVE,
            max_cpu_percent=60.0,
            max_memory_mb=1024,
            priority_levels=10,
            starvation_prevention=True
        )
    
    @classmethod
    def get_fair_share_config(cls) -> 'SchedulerConfig':
        """Get configuration for fair share scheduling."""
        return cls(
            scheduling_strategy=SchedulingStrategy.FAIR_SHARE,
            enable_priority=True,
            priority_levels=5,
            starvation_prevention=True,
            resource_policy=ResourcePolicy.BALANCED,
            max_concurrent_tasks=8,
            worker_threads=4,
            collect_detailed_stats=True,
            enable_performance_prediction=True
        )


# Global configuration instance
_config = None
_config_lock = threading.Lock()


def get_config() -> SchedulerConfig:
    """Get the global scheduler configuration."""
    global _config
    if _config is None:
        with _config_lock:
            if _config is None:
                _config = SchedulerConfig.get_default_config()
    return _config


def set_config(config: SchedulerConfig) -> None:
    """Set the global scheduler configuration."""
    global _config
    with _config_lock:
        _config = config


def reset_config() -> None:
    """Reset configuration to default."""
    global _config
    with _config_lock:
        _config = SchedulerConfig.get_default_config()


def load_config_from_file(config_path: str) -> SchedulerConfig:
    """Load configuration from JSON file."""
    import json
    
    try:
        with open(config_path, 'r') as f:
            config_dict = json.load(f)
        return SchedulerConfig.from_dict(config_dict)
    except Exception as e:
        raise ValueError(f"Failed to load config from {config_path}: {str(e)}")


def save_config_to_file(config: SchedulerConfig, config_path: str) -> None:
    """Save configuration to JSON file."""
    import json
    
    try:
        with open(config_path, 'w') as f:
            json.dump(config.to_dict(), f, indent=2)
    except Exception as e:
        raise ValueError(f"Failed to save config to {config_path}: {str(e)}")


def validate_config(config: SchedulerConfig) -> List[str]:
    """Validate configuration and return list of warnings/errors."""
    warnings = []
    
    # Validate resource limits
    if config.max_cpu_percent > 100.0:
        warnings.append("max_cpu_percent should not exceed 100.0")
    
    if config.max_memory_mb < 100:
        warnings.append("max_memory_mb should be at least 100 MB")
    
    # Validate thread counts
    if config.worker_threads < 1:
        warnings.append("worker_threads should be at least 1")
    
    if config.max_concurrent_tasks < config.worker_threads:
        warnings.append("max_concurrent_tasks should be >= worker_threads for optimal performance")
    
    # Validate timeouts
    if config.task_timeout_seconds < 10:
        warnings.append("task_timeout_seconds should be at least 10 seconds")
    
    if config.starvation_threshold_seconds > config.task_timeout_seconds:
        warnings.append("starvation_threshold_seconds should be < task_timeout_seconds")
    
    # Validate queue sizes
    if config.max_queue_size < config.max_concurrent_tasks:
        warnings.append("max_queue_size should be >= max_concurrent_tasks")
    
    # Validate batch settings
    if config.batch_processing and config.batch_size < 1:
        warnings.append("batch_size should be at least 1 when batch_processing is enabled")
    
    return warnings
