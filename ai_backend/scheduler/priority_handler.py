"""
Priority Handler for AI Backend

Handles task prioritization, starvation prevention, and priority-based scheduling.
"""

import time
import threading
from typing import Dict, Any, List, Optional, Set
from dataclasses import dataclass, field
from enum import Enum
from .config import SchedulerConfig, get_config
from .task_scheduler import Task, TaskPriority, TaskStatus


class StarvationLevel(Enum):
    """Starvation prevention levels."""
    NONE = "none"
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"


@dataclass
class StarvationInfo:
    """Information about task starvation."""
    task_id: str
    waiting_time: float
    starvation_level: StarvationLevel
    priority_boost: float
    last_checked: float = field(default_factory=time.time)


class PriorityHandler:
    """
    Handles task prioritization and starvation prevention.
    
    Responsibilities:
    - Dynamic priority adjustment
    - Starvation detection and prevention
    - Priority inheritance
    - Fair scheduling support
    - Priority boost mechanisms
    """
    
    def __init__(self, config: Optional[SchedulerConfig] = None):
        """Initialize the priority handler."""
        self.config = config or get_config()
        
        # Starvation tracking
        self._starvation_tracker: Dict[str, StarvationInfo] = {}
        self._starvation_lock = threading.Lock()
        
        # Priority adjustment history
        self._priority_history: List[Dict[str, Any]] = []
        
        # Fair scheduling metrics
        self._fairness_metrics = {
            "tasks_by_priority": {priority.value: 0 for priority in TaskPriority},
            "total_wait_time_by_priority": {priority.value: 0.0 for priority in TaskPriority},
            "average_wait_time_by_priority": {priority.value: 0.0 for priority in TaskPriority}
        }
        
        # Priority boost rules
        self._boost_rules = self._initialize_boost_rules()
        
        # Starvation thresholds
        self._starvation_thresholds = {
            StarvationLevel.LOW: self.config.starvation_threshold_seconds * 0.5,
            StarvationLevel.MEDIUM: self.config.starvation_threshold_seconds * 1.0,
            StarvationLevel.HIGH: self.config.starvation_threshold_seconds * 1.5,
            StarvationLevel.CRITICAL: self.config.starvation_threshold_seconds * 2.0
        }
        
        # Background thread for monitoring
        self._monitoring_active = False
        self._monitoring_thread = None
        
        # Start monitoring if enabled
        if self.config.enable_priority and self.config.starvation_prevention:
            self._start_monitoring()
    
    def _initialize_boost_rules(self) -> Dict[str, Any]:
        """Initialize priority boost rules."""
        return {
            "time_based": {
                "waiting_threshold": self.config.starvation_threshold_seconds,
                "boost_increment": 1,
                "max_boost": 3
            },
            "load_based": {
                "queue_size_threshold": self.config.max_queue_size * 0.8,
                "boost_increment": 1,
                "max_boost": 2
            },
            "fairness_based": {
                "waiting_threshold": self.config.starvation_threshold_seconds * 0.8,
                "boost_increment": 1,
                "max_boost": 2
            }
        }
    
    def adjust_task_priority(self, task: Task, current_time: float) -> TaskPriority:
        """
        Adjust task priority based on waiting time and other factors.
        
        Args:
            task: Task to adjust priority for
            current_time: Current timestamp
            
        Returns:
            Adjusted task priority
        """
        if not self.config.enable_priority:
            return task.priority
        
        original_priority = task.priority
        waiting_time = current_time - task.created_at
        
        # Check for starvation
        starvation_info = self._get_starvation_info(task.id, waiting_time)
        
        # Apply time-based priority boost
        boosted_priority = self._apply_time_based_boost(task, waiting_time)
        
        # Apply fairness-based adjustments
        boosted_priority = self._apply_fairness_adjustment(boosted_priority, waiting_time)
        
        # Apply load-based adjustments
        boosted_priority = self._apply_load_based_adjustment(boosted_priority)
        
        # Log priority adjustment
        if original_priority != boosted_priority:
            self._log_priority_adjustment(task, original_priority, boosted_priority, waiting_time)
        
        return boosted_priority
    
    def _get_starvation_info(self, task_id: str, waiting_time: float) -> Optional[StarvationInfo]:
        """Get or create starvation information for a task."""
        with self._starvation_lock:
            if task_id not in self._starvation_tracker:
                starvation_info = StarvationInfo(
                    task_id=task_id,
                    waiting_time=waiting_time,
                    starvation_level=StarvationLevel.NONE,
                    priority_boost=0.0
                )
                self._starvation_tracker[task_id] = starvation_info
            else:
                starvation_info = self._starvation_tracker[task_id]
                starvation_info.waiting_time = waiting_time
            
            # Update starvation level
            starvation_info.starvation_level = self._assess_starvation_level(waiting_time)
            starvation_info.last_checked = time.time()
            
            return starvation_info
    
    def _assess_starvation_level(self, waiting_time: float) -> StarvationLevel:
        """Assess the starvation level based on waiting time."""
        if waiting_time < self._starvation_thresholds[StarvationLevel.LOW]:
            return StarvationLevel.NONE
        elif waiting_time < self._starvation_thresholds[StarvationLevel.MEDIUM]:
            return StarvationLevel.LOW
        elif waiting_time < self._starvation_thresholds[StarvationLevel.HIGH]:
            return StarvationLevel.MEDIUM
        elif waiting_time < self._starvation_thresholds[StarvationLevel.CRITICAL]:
            return StarvationLevel.HIGH
        else:
            return StarvationLevel.CRITICAL
    
    def _apply_time_based_boost(self, task: Task, waiting_time: float) -> TaskPriority:
        """Apply time-based priority boost."""
        boost_rule = self._boost_rules["time_based"]
        
        if waiting_time > boost_rule["waiting_threshold"]:
            # Calculate boost amount
            boost_increment = boost_rule["boost_increment"]
            max_boost = boost_rule["max_boost"]
            
            # Calculate how many increments to apply
            boost_amount = min(
                int(waiting_time / boost_rule["waiting_threshold"]),
                max_boost
            )
            
            # Apply boost
            new_priority_value = max(1, task.priority.value - boost_amount)
            return TaskPriority(new_priority_value)
        
        return task.priority
    
    def _apply_fairness_adjustment(self, priority: TaskPriority, waiting_time: float) -> TaskPriority:
        """Apply fairness-based priority adjustment."""
        boost_rule = self._boost_rules["fairness_based"]
        
        if waiting_time > boost_rule["waiting_threshold"]:
            # Check if this priority level has been waiting too long
            avg_wait_time = self._fairness_metrics["average_wait_time_by_priority"].get(priority.value, 0)
            
            if waiting_time > avg_wait_time * 1.5:
                # Boost priority to improve fairness
                new_priority_value = max(1, priority.value - boost_rule["boost_increment"])
                return TaskPriority(new_priority_value)
        
        return priority
    
    def _apply_load_based_adjustment(self, priority: TaskPriority) -> TaskPriority:
        """Apply load-based priority adjustment."""
        boost_rule = self._boost_rules["load_based"]
        
        # This would need access to current queue size
        # For now, we'll implement a simplified version
        current_queue_size = self._get_current_queue_size()
        
        if current_queue_size > boost_rule["queue_size_threshold"]:
            # High load - boost lower priority tasks
            if priority.value >= 4:  # LOW or BACKGROUND priority
                new_priority_value = max(1, priority.value - boost_rule["boost_increment"])
                return TaskPriority(new_priority_value)
        
        return priority
    
    def check_starvation(self, tasks: Dict[str, Task]) -> Dict[str, Any]:
        """
        Check for starved tasks and apply corrective measures.
        
        Args:
            tasks: Dictionary of all tasks
            
        Returns:
            Dictionary with starvation analysis results
        """
        if not self.config.starvation_prevention:
            return {"status": "disabled", "starved_tasks": []}
        
        current_time = time.time()
        starved_tasks = []
        
        for task_id, task in tasks.items():
            if task.status != TaskStatus.PENDING:
                continue
            
            waiting_time = current_time - task.created_at
            starvation_info = self._get_starvation_info(task_id, waiting_time)
            
            if starvation_info.starvation_level in [StarvationLevel.HIGH, StarvationLevel.CRITICAL]:
                starved_tasks.append({
                    "task_id": task_id,
                    "waiting_time": waiting_time,
                    "starvation_level": starvation_info.starvation_level.value,
                    "original_priority": task.priority.value,
                    "corrective_action": self._apply_starvation_correction(task, waiting_time)
                })
        
        return {
            "status": "completed",
            "starved_tasks": starved_tasks,
            "total_checked": len(tasks),
            "starvation_rate": len(starved_tasks) / max(len(tasks), 1)
        }
    
    def _apply_starvation_correction(self, task: Task, waiting_time: float) -> str:
        """Apply corrective measures for starved tasks."""
        current_priority = task.priority
        corrected_priority = self.adjust_task_priority(task, time.time())
        
        if current_priority != corrected_priority:
            # Update task priority
            task.priority = corrected_priority
            
            # Update fairness metrics
            self._update_fairness_metrics(task, waiting_time)
            
            return f"Priority adjusted from {current_priority.value} to {corrected_priority.value}"
        else:
            return "No correction needed"
    
    def _update_fairness_metrics(self, task: Task, waiting_time: float) -> None:
        """Update fairness tracking metrics."""
        priority_value = task.priority.value
        
        # Update task count
        self._fairness_metrics["tasks_by_priority"][priority_value] += 1
        
        # Update wait times
        current_total = self._fairness_metrics["total_wait_time_by_priority"][priority_value]
        current_count = self._fairness_metrics["tasks_by_priority"][priority_value]
        
        new_total = current_total + waiting_time
        self._fairness_metrics["total_wait_time_by_priority"][priority_value] = new_total
        self._fairness_metrics["average_wait_time_by_priority"][priority_value] = new_total / current_count
    
    def _log_priority_adjustment(self, task: Task, original: TaskPriority, 
                                adjusted: TaskPriority, waiting_time: float) -> None:
        """Log priority adjustment for monitoring."""
        adjustment_record = {
            "timestamp": time.time(),
            "task_id": task.id,
            "task_name": task.name,
            "original_priority": original.value,
            "adjusted_priority": adjusted.value,
            "waiting_time": waiting_time,
            "adjustment_reason": self._get_adjustment_reason(original, adjusted, waiting_time)
        }
        
        self._priority_history.append(adjustment_record)
        
        # Keep history limited
        if len(self._priority_history) > 1000:
            self._priority_history.pop(0)
    
    def _get_adjustment_reason(self, original: TaskPriority, adjusted: TaskPriority, 
                              waiting_time: float) -> str:
        """Get reason for priority adjustment."""
        if adjusted.value < original.value:
            if waiting_time > self.config.starvation_threshold_seconds * 2:
                return "starvation_prevention"
            elif waiting_time > self.config.starvation_threshold_seconds:
                return "waiting_time_threshold"
            else:
                return "fairness_adjustment"
        elif adjusted.value > original.value:
            return "priority_inheritance"
        else:
            return "no_change"
    
    def _get_current_queue_size(self) -> int:
        """Get current queue size (simplified implementation)."""
        # This would need access to the actual queue
        # For now, return a placeholder
        return 0
    
    def get_fairness_report(self) -> Dict[str, Any]:
        """Get fairness metrics report."""
        with self._starvation_lock:
            current_starvation = list(self._starvation_tracker.values())
        
        # Calculate starvation statistics
        starvation_by_level = {}
        for level in StarvationLevel:
            starvation_by_level[level.value] = sum(
                1 for info in current_starvation 
                if info.starvation_level == level
            )
        
        return {
            "fairness_metrics": self._fairness_metrics,
            "starvation_statistics": starvation_by_level,
            "priority_adjustments": len(self._priority_history),
            "recent_adjustments": self._priority_history[-10:] if self._priority_history else []
        }
    
    def reset_fairness_metrics(self) -> None:
        """Reset fairness tracking metrics."""
        self._fairness_metrics = {
            "tasks_by_priority": {priority.value: 0 for priority in TaskPriority},
            "total_wait_time_by_priority": {priority.value: 0.0 for priority in TaskPriority},
            "average_wait_time_by_priority": {priority.value: 0.0 for priority in TaskPriority}
        }
        
        with self._starvation_lock:
            self._starvation_tracker.clear()
        
        self._priority_history.clear()
    
    def _start_monitoring(self) -> None:
        """Start monitoring for priority adjustments."""
        def monitoring_loop():
            while self._monitoring_active:
                try:
                    # Clean up old starvation tracking
                    self._cleanup_old_starvation_data()
                    
                    # Log periodic fairness report
                    if self.config.detailed_logging:
                        fairness_report = self.get_fairness_report()
                        print(f"Priority Handler Fairness Report: {fairness_report}")
                    
                    time.sleep(self.config.monitoring_interval_seconds)
                    
                except Exception as e:
                    if self.config.detailed_logging:
                        print(f"Priority monitoring error: {e}")
                    time.sleep(5)
        
        self._monitoring_active = True
        self._monitoring_thread = threading.Thread(target=monitoring_loop, daemon=True)
        self._monitoring_thread.start()
    
    def _cleanup_old_starvation_data(self) -> None:
        """Clean up old starvation tracking data."""
        current_time = time.time()
        cutoff_time = current_time - 3600  # 1 hour
        
        with self._starvation_lock:
            expired_tasks = [
                task_id for task_id, info in self._starvation_tracker.items()
                if info.last_checked < cutoff_time
            ]
            
            for task_id in expired_tasks:
                del self._starvation_tracker[task_id]
    
    def stop_monitoring(self) -> None:
        """Stop monitoring."""
        self._monitoring_active = False
        if self._monitoring_thread and self._monitoring_thread.is_alive():
            self._monitoring_thread.join(timeout=5)
    
    def get_priority_statistics(self) -> Dict[str, Any]:
        """Get priority handling statistics."""
        return {
            "starvation_tracker_size": len(self._starvation_tracker),
            "priority_history_size": len(self._priority_history),
            "fairness_metrics": self._fairness_metrics,
            "boost_rules": self._boost_rules,
            "starvation_thresholds": {
                level.value: threshold for level, threshold in self._starvation_thresholds.items()
            }
        }
