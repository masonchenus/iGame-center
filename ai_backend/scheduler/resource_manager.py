"""
Resource Manager for AI Backend

Manages system resources (CPU, memory, disk I/O) for task execution.
"""

import time
import threading
import psutil
import os
from typing import Dict, Any, List, Optional, Set
from dataclasses import dataclass, field
from .config import SchedulerConfig, get_config, ResourcePolicy


@dataclass
class ResourceAllocation:
    """Represents a resource allocation for a task."""
    task_id: str
    cpu_percent: float
    memory_mb: float
    disk_io_mb_per_sec: float
    allocated_at: float = field(default_factory=time.time)
    last_used: float = field(default_factory=time.time)


@dataclass
class ResourceStats:
    """Resource usage statistics."""
    cpu_percent: float
    memory_mb: float
    memory_percent: float
    disk_io_read_mb_per_sec: float
    disk_io_write_mb_per_sec: float
    available_memory_mb: float
    timestamp: float = field(default_factory=time.time)


class ResourceManager:
    """
    Manages system resources for AI task execution.
    
    Responsibilities:
    - Monitor system resource usage
    - Allocate resources to tasks
    - Prevent resource over-allocation
    - Optimize resource usage patterns
    - Provide resource usage statistics
    """
    
    def __init__(self, config: Optional[SchedulerConfig] = None):
        """Initialize the resource manager."""
        self.config = config or get_config()
        
        # Current resource allocations
        self._allocations: Dict[str, ResourceAllocation] = {}
        self._allocation_lock = threading.Lock()
        
        # System resource monitoring
        self._monitoring_active = False
        self._monitoring_thread = None
        self._current_stats = self._get_system_stats()
        
        # Resource usage history for optimization
        self._stats_history: List[ResourceStats] = []
        self._max_history_size = 100
        
        # Task resource requirements tracking
        self._task_requirements: Dict[str, Dict[str, float]] = {}
        
        # Start monitoring if enabled
        if self.config.enable_monitoring:
            self._start_monitoring()
    
    def can_allocate_resources(self, task_id: str, 
                             cpu_percent: Optional[float] = None,
                             memory_mb: Optional[float] = None,
                             disk_io_mb_per_sec: Optional[float] = None) -> bool:
        """
        Check if resources can be allocated for a task.
        
        Args:
            task_id: Task identifier
            cpu_percent: CPU percentage required
            memory_mb: Memory in MB required
            disk_io_mb_per_sec: Disk I/O rate required
            
        Returns:
            True if resources can be allocated
        """
        # Use task-specific requirements if provided
        if task_id in self._task_requirements:
            req = self._task_requirements[task_id]
            cpu_percent = req.get('cpu_percent', cpu_percent or 0)
            memory_mb = req.get('memory_mb', memory_mb or 0)
            disk_io_mb_per_sec = req.get('disk_io_mb_per_sec', disk_io_mb_per_sec or 0)
        
        # Use defaults if not specified
        cpu_percent = cpu_percent or self._get_default_cpu_requirement()
        memory_mb = memory_mb or self._get_default_memory_requirement()
        disk_io_mb_per_sec = disk_io_mb_per_sec or self._get_default_disk_io_requirement()
        
        with self._allocation_lock:
            # Check current resource usage
            current_usage = self._get_current_usage()
            
            # Check against limits
            new_cpu = current_usage['cpu_percent'] + cpu_percent
            new_memory = current_usage['memory_mb'] + memory_mb
            new_disk_io = current_usage['disk_io_mb_per_sec'] + disk_io_mb_per_sec
            
            # Apply resource policy limits
            max_cpu = self._get_max_cpu_limit()
            max_memory = self._get_max_memory_limit()
            max_disk_io = self._get_max_disk_io_limit()
            
            return (new_cpu <= max_cpu and 
                   new_memory <= max_memory and 
                   new_disk_io <= max_disk_io)
    
    def allocate_resources(self, task_id: str,
                          cpu_percent: Optional[float] = None,
                          memory_mb: Optional[float] = None,
                          disk_io_mb_per_sec: Optional[float] = None) -> bool:
        """
        Allocate resources for a task.
        
        Args:
            task_id: Task identifier
            cpu_percent: CPU percentage to allocate
            memory_mb: Memory in MB to allocate
            disk_io_mb_per_sec: Disk I/O rate to allocate
            
        Returns:
            True if allocation successful
        """
        if not self.can_allocate_resources(task_id, cpu_percent, memory_mb, disk_io_mb_per_sec):
            return False
        
        # Use task-specific requirements if available
        if task_id in self._task_requirements:
            req = self._task_requirements[task_id]
            cpu_percent = req.get('cpu_percent', cpu_percent or 0)
            memory_mb = req.get('memory_mb', memory_mb or 0)
            disk_io_mb_per_sec = req.get('disk_io_mb_per_sec', disk_io_mb_per_sec or 0)
        else:
            # Use defaults
            cpu_percent = cpu_percent or self._get_default_cpu_requirement()
            memory_mb = memory_mb or self._get_default_memory_requirement()
            disk_io_mb_per_sec = disk_io_mb_per_sec or self._get_default_disk_io_requirement()
        
        allocation = ResourceAllocation(
            task_id=task_id,
            cpu_percent=cpu_percent,
            memory_mb=memory_mb,
            disk_io_mb_per_sec=disk_io_mb_per_sec
        )
        
        with self._allocation_lock:
            self._allocations[task_id] = allocation
            return True
    
    def release_resources(self, task_id: str) -> bool:
        """Release resources allocated to a task."""
        with self._allocation_lock:
            if task_id in self._allocations:
                del self._allocations[task_id]
                return True
            return False
    
    def get_usage_stats(self) -> Dict[str, Any]:
        """Get current resource usage statistics."""
        current_usage = self._get_current_usage()
        allocations = list(self._allocations.values())
        
        return {
            "current_usage": {
                "cpu_percent": current_usage['cpu_percent'],
                "memory_mb": current_usage['memory_mb'],
                "memory_percent": current_usage['memory_percent'],
                "disk_io_mb_per_sec": current_usage['disk_io_mb_per_sec']
            },
            "allocations": {
                "count": len(allocations),
                "total_cpu_percent": sum(a.cpu_percent for a in allocations),
                "total_memory_mb": sum(a.memory_mb for a in allocations),
                "total_disk_io_mb_per_sec": sum(a.disk_io_mb_per_sec for a in allocations)
            },
            "limits": {
                "max_cpu_percent": self._get_max_cpu_limit(),
                "max_memory_mb": self._get_max_memory_limit(),
                "max_disk_io_mb_per_sec": self._get_max_disk_io_limit()
            },
            "available": {
                "cpu_percent": max(0, self._get_max_cpu_limit() - current_usage['cpu_percent']),
                "memory_mb": max(0, self._get_max_memory_limit() - current_usage['memory_mb']),
                "disk_io_mb_per_sec": max(0, self._get_max_disk_io_limit() - current_usage['disk_io_mb_per_sec'])
            }
        }
    
    def set_task_requirements(self, task_id: str, requirements: Dict[str, float]) -> None:
        """Set specific resource requirements for a task."""
        self._task_requirements[task_id] = requirements
    
    def get_task_allocation(self, task_id: str) -> Optional[ResourceAllocation]:
        """Get the current allocation for a task."""
        with self._allocation_lock:
            return self._allocations.get(task_id)
    
    def optimize_allocation(self) -> Dict[str, Any]:
        """Optimize resource allocation based on usage patterns."""
        # Analyze usage patterns
        if len(self._stats_history) < 10:
            return {"status": "insufficient_data", "optimizations": []}
        
        optimizations = []
        
        # Check for CPU usage patterns
        recent_cpu = [s.cpu_percent for s in self._stats_history[-10:]]
        avg_cpu = sum(recent_cpu) / len(recent_cpu)
        max_cpu = max(recent_cpu)
        
        if max_cpu - avg_cpu < 10:  # Low variance
            optimizations.append({
                "type": "cpu_optimization",
                "suggestion": "Consider reducing max_cpu_percent limit",
                "current_avg": avg_cpu,
                "potential_saving": max_cpu - avg_cpu
            })
        
        # Check for memory usage patterns
        recent_memory = [s.memory_mb for s in self._stats_history[-10:]]
        avg_memory = sum(recent_memory) / len(recent_memory)
        peak_memory = max(recent_memory)
        
        if peak_memory - avg_memory < 100:  # Low variance
            optimizations.append({
                "type": "memory_optimization",
                "suggestion": "Consider reducing max_memory_mb limit",
                "current_avg": avg_memory,
                "potential_saving": peak_memory - avg_memory
            })
        
        # Check allocation efficiency
        if self._allocations:
            total_allocated = sum(a.memory_mb for a in self._allocations.values())
            current_usage = self._get_current_usage()
            
            if total_allocated > current_usage['memory_mb'] * 1.5:
                optimizations.append({
                    "type": "allocation_efficiency",
                    "suggestion": "Consider more aggressive resource deallocation",
                    "allocated": total_allocated,
                    "used": current_usage['memory_mb']
                })
        
        return {
            "status": "completed",
            "optimizations": optimizations,
            "analysis_period": len(self._stats_history)
        }
    
    def _get_system_stats(self) -> ResourceStats:
        """Get current system resource statistics."""
        try:
            # Get CPU usage
            cpu_percent = psutil.cpu_percent(interval=0.1)
            
            # Get memory usage
            memory = psutil.virtual_memory()
            memory_mb = (memory.total - memory.available) / (1024 * 1024)
            memory_percent = memory.percent
            
            # Get disk I/O
            disk_io = psutil.disk_io_counters()
            if disk_io:
                # Calculate I/O rate (simplified)
                disk_io_read_mb_per_sec = (disk_io.read_bytes / (1024 * 1024)) / max(time.time(), 1)
                disk_io_write_mb_per_sec = (disk_io.write_bytes / (1024 * 1024)) / max(time.time(), 1)
            else:
                disk_io_read_mb_per_sec = 0
                disk_io_write_mb_per_sec = 0
            
            return ResourceStats(
                cpu_percent=cpu_percent,
                memory_mb=memory_mb,
                memory_percent=memory_percent,
                disk_io_read_mb_per_sec=disk_io_read_mb_per_sec,
                disk_io_write_mb_per_sec=disk_io_write_mb_per_sec,
                available_memory_mb=memory.available / (1024 * 1024)
            )
            
        except Exception:
            # Return default stats if monitoring fails
            return ResourceStats(
                cpu_percent=0,
                memory_mb=0,
                memory_percent=0,
                disk_io_read_mb_per_sec=0,
                disk_io_write_mb_per_sec=0,
                available_memory_mb=0
            )
    
    def _get_current_usage(self) -> Dict[str, float]:
        """Get current resource usage including allocations."""
        base_usage = self._current_stats
        
        with self._allocation_lock:
            allocated_cpu = sum(a.cpu_percent for a in self._allocations.values())
            allocated_memory = sum(a.memory_mb for a in self._allocations.values())
            allocated_disk_io = sum(a.disk_io_mb_per_sec for a in self._allocations.values())
        
        return {
            'cpu_percent': base_usage.cpu_percent + allocated_cpu,
            'memory_mb': base_usage.memory_mb + allocated_memory,
            'memory_percent': base_usage.memory_percent,
            'disk_io_mb_per_sec': base_usage.disk_io_read_mb_per_sec + allocated_disk_io
        }
    
    def _get_max_cpu_limit(self) -> float:
        """Get maximum CPU usage limit based on policy."""
        if self.config.resource_policy == ResourcePolicy.CONSERVATIVE:
            return min(self.config.max_cpu_percent, 70.0)
        elif self.config.resource_policy == ResourcePolicy.AGGRESSIVE:
            return min(self.config.max_cpu_percent, 95.0)
        else:  # BALANCED or ADAPTIVE
            return self.config.max_cpu_percent
    
    def _get_max_memory_limit(self) -> float:
        """Get maximum memory limit based on policy."""
        if self.config.resource_policy == ResourcePolicy.CONSERVATIVE:
            return min(self.config.max_memory_mb, psutil.virtual_memory().total / (1024 * 1024) * 0.7)
        elif self.config.resource_policy == ResourcePolicy.AGGRESSIVE:
            return min(self.config.max_memory_mb, psutil.virtual_memory().total / (1024 * 1024) * 0.9)
        else:  # BALANCED or ADAPTIVE
            return self.config.max_memory_mb
    
    def _get_max_disk_io_limit(self) -> float:
        """Get maximum disk I/O limit based on policy."""
        if self.config.resource_policy == ResourcePolicy.CONSERVATIVE:
            return min(self.config.max_disk_io_mb_per_sec, 50.0)
        elif self.config.resource_policy == ResourcePolicy.AGGRESSIVE:
            return min(self.config.max_disk_io_mb_per_sec, 200.0)
        else:  # BALANCED or ADAPTIVE
            return self.config.max_disk_io_mb_per_sec
    
    def _get_default_cpu_requirement(self) -> float:
        """Get default CPU requirement for a task."""
        if self.config.resource_policy == ResourcePolicy.CONSERVATIVE:
            return 5.0
        elif self.config.resource_policy == ResourcePolicy.AGGRESSIVE:
            return 15.0
        else:  # BALANCED
            return 10.0
    
    def _get_default_memory_requirement(self) -> float:
        """Get default memory requirement for a task."""
        if self.config.resource_policy == ResourcePolicy.CONSERVATIVE:
            return 50.0
        elif self.config.resource_policy == ResourcePolicy.AGGRESSIVE:
            return 200.0
        else:  # BALANCED
            return 100.0
    
    def _get_default_disk_io_requirement(self) -> float:
        """Get default disk I/O requirement for a task."""
        if self.config.resource_policy == ResourcePolicy.CONSERVATIVE:
            return 1.0
        elif self.config.resource_policy == ResourcePolicy.AGGRESSIVE:
            return 10.0
        else:  # BALANCED
            return 5.0
    
    def _start_monitoring(self) -> None:
        """Start resource monitoring."""
        def monitoring_loop():
            while self._monitoring_active:
                try:
                    # Update current stats
                    self._current_stats = self._get_system_stats()
                    
                    # Add to history
                    self._stats_history.append(self._current_stats)
                    if len(self._stats_history) > self._max_history_size:
                        self._stats_history.pop(0)
                    
                    # Clean up old allocations
                    self._cleanup_stale_allocations()
                    
                    time.sleep(self.config.monitoring_interval_seconds)
                    
                except Exception as e:
                    if self.config.detailed_logging:
                        print(f"Resource monitoring error: {e}")
                    time.sleep(5)
        
        self._monitoring_active = True
        self._monitoring_thread = threading.Thread(target=monitoring_loop, daemon=True)
        self._monitoring_thread.start()
    
    def _cleanup_stale_allocations(self) -> None:
        """Clean up stale resource allocations."""
        current_time = time.time()
        stale_tasks = []
        
        with self._allocation_lock:
            for task_id, allocation in self._allocations.items():
                # Consider allocation stale if not used for 1 hour
                if current_time - allocation.last_used > 3600:
                    stale_tasks.append(task_id)
            
            for task_id in stale_tasks:
                del self._allocations[task_id]
    
    def update_usage_stats(self) -> None:
        """Update current resource usage statistics."""
        self._current_stats = self._get_system_stats()
    
    def stop_monitoring(self) -> None:
        """Stop resource monitoring."""
        self._monitoring_active = False
        if self._monitoring_thread and self._monitoring_thread.is_alive():
            self._monitoring_thread.join(timeout=5)
    
    def get_performance_history(self, hours: int = 1) -> List[ResourceStats]:
        """Get resource usage history for specified time period."""
        cutoff_time = time.time() - (hours * 3600)
        return [stat for stat in self._stats_history if stat.timestamp > cutoff_time]
