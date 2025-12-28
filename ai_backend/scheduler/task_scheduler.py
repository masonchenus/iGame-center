"""
Task Scheduler for AI Backend

Core task scheduling logic that manages task queues, execution, and monitoring.
"""

import time
import threading
import uuid
from typing import Dict, Any, List, Optional, Callable, Union
from dataclasses import dataclass, field
from enum import Enum
from concurrent.futures import ThreadPoolExecutor, Future, as_completed
from queue import PriorityQueue, Queue, Empty
import heapq
from .config import SchedulerConfig, get_config, SchedulingStrategy
from .priority_handler import PriorityHandler
from .resource_manager import ResourceManager


class TaskStatus(Enum):
    """Task execution status."""
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"
    TIMEOUT = "timeout"


class TaskPriority(Enum):
    """Task priority levels."""
    CRITICAL = 1
    HIGH = 2
    NORMAL = 3
    LOW = 4
    BACKGROUND = 5


@dataclass
class Task:
    """Represents a task in the scheduler."""
    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    name: str = ""
    function: Optional[Callable] = None
    args: tuple = field(default_factory=tuple)
    kwargs: dict = field(default_factory=dict)
    priority: TaskPriority = TaskPriority.NORMAL
    status: TaskStatus = TaskStatus.PENDING
    created_at: float = field(default_factory=time.time)
    started_at: Optional[float] = None
    completed_at: Optional[float] = None
    result: Any = None
    error: Optional[str] = None
    timeout_seconds: Optional[float] = None
    retry_count: int = 0
    max_retries: int = 0
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def __lt__(self, other):
        """Enable priority comparison for priority queue."""
        if not isinstance(other, Task):
            return NotImplemented
        return self.priority.value < other.priority.value
    
    @property
    def duration(self) -> Optional[float]:
        """Get task duration in seconds."""
        if self.started_at and self.completed_at:
            return self.completed_at - self.started_at
        return None
    
    @property
    def is_complete(self) -> bool:
        """Check if task is complete."""
        return self.status in [TaskStatus.COMPLETED, TaskStatus.FAILED, TaskStatus.CANCELLED, TaskStatus.TIMEOUT]


class TaskScheduler:
    """
    Advanced task scheduler with multiple scheduling strategies and resource management.
    
    Features:
    - Multiple scheduling strategies (FIFO, Priority, Round-robin, etc.)
    - Resource management and monitoring
    - Task prioritization and starvation prevention
    - Batch processing and caching
    - Performance monitoring and statistics
    """
    
    def __init__(self, config: Optional[SchedulerConfig] = None):
        """Initialize the task scheduler."""
        self.config = config or get_config()
        self.priority_handler = PriorityHandler(self.config)
        self.resource_manager = ResourceManager(self.config)
        
        # Task queues based on scheduling strategy
        self._queues = self._initialize_queues()
        
        # Thread pool for task execution
        self._executor = ThreadPoolExecutor(
            max_workers=self.config.worker_threads,
            thread_name_prefix="AITaskScheduler"
        )
        
        # Task tracking
        self._tasks: Dict[str, Task] = {}
        self._task_futures: Dict[str, Future] = {}
        self._running_tasks = 0
        self._scheduler_lock = threading.Lock()
        
        # Statistics
        self._stats = {
            "total_submitted": 0,
            "total_completed": 0,
            "total_failed": 0,
            "average_duration": 0.0,
            "queue_wait_time": 0.0,
            "start_time": time.time()
        }
        
        # Monitoring thread
        self._monitoring_active = False
        self._monitoring_thread = None
        
        # Start monitoring if enabled
        if self.config.enable_monitoring:
            self._start_monitoring()
    
    def _initialize_queues(self) -> Dict[str, Any]:
        """Initialize task queues based on scheduling strategy."""
        queues = {}
        
        if self.config.scheduling_strategy == SchedulingStrategy.PRIORITY:
            # Use priority queue
            queues["main"] = PriorityQueue(maxsize=self.config.max_queue_size)
        elif self.config.scheduling_strategy == SchedulingStrategy.FIFO:
            # Use regular queue with timestamp ordering
            queues["main"] = Queue(maxsize=self.config.max_queue_size)
        elif self.config.scheduling_strategy == SchedulingStrategy.ROUND_ROBIN:
            # Use multiple queues for round-robin
            queues["main"] = Queue(maxsize=self.config.max_queue_size)
            queues["rr_index"] = 0
        elif self.config.scheduling_strategy == SchedulingStrategy.FAIR_SHARE:
            # Use priority queue with fairness tracking
            queues["main"] = PriorityQueue(maxsize=self.config.max_queue_size)
            queues["fairness_tracker"] = {}
        else:
            # Default to priority queue
            queues["main"] = PriorityQueue(maxsize=self.config.max_queue_size)
        
        return queues
    
    def submit_task(self, 
                   func: Callable,
                   args: tuple = (),
                   kwargs: Optional[dict] = None,
                   name: str = "",
                   priority: TaskPriority = TaskPriority.NORMAL,
                   timeout_seconds: Optional[float] = None,
                   max_retries: int = 0,
                   metadata: Optional[Dict[str, Any]] = None) -> str:
        """
        Submit a task for execution.
        
        Args:
            func: Function to execute
            args: Function arguments
            kwargs: Function keyword arguments
            name: Task name for identification
            priority: Task priority
            timeout_seconds: Task timeout
            max_retries: Maximum retry attempts
            metadata: Additional task metadata
            
        Returns:
            Task ID
        """
        if kwargs is None:
            kwargs = {}
        if metadata is None:
            metadata = {}
        
        task = Task(
            function=func,
            args=args,
            kwargs=kwargs,
            name=name or f"Task_{int(time.time())}",
            priority=priority,
            timeout_seconds=timeout_seconds,
            max_retries=max_retries,
            metadata=metadata
        )
        
        with self._scheduler_lock:
            # Check if we can accept more tasks
            if len(self._tasks) >= self.config.max_queue_size:
                raise RuntimeError("Task queue is full")
            
            # Add task to tracking
            self._tasks[task.id] = task
            self._stats["total_submitted"] += 1
            
            # Add to appropriate queue
            self._add_to_queue(task)
        
        return task.id
    
    def _add_to_queue(self, task: Task) -> None:
        """Add task to the appropriate queue based on scheduling strategy."""
        try:
            if self.config.scheduling_strategy == SchedulingStrategy.PRIORITY:
                # Priority queue with (priority, timestamp, task)
                heapq.heappush(self._queues["main"].queue, (task.priority.value, time.time(), task))
            elif self.config.scheduling_strategy == SchedulingStrategy.FIFO:
                # Regular queue with timestamp
                self._queues["main"].put((task, time.time()))
            elif self.config.scheduling_strategy == SchedulingStrategy.ROUND_ROBIN:
                # Round-robin with task and timestamp
                self._queues["main"].put((task, time.time()))
            else:
                # Default to priority queue
                heapq.heappush(self._queues["main"].queue, (task.priority.value, time.time(), task))
                
        except Exception as e:
            # If queue is full, remove from tracking and raise error
            with self._scheduler_lock:
                del self._tasks[task.id]
            raise RuntimeError(f"Failed to add task to queue: {str(e)}")
    
    def get_next_task(self) -> Optional[Task]:
        """Get the next task from the queue based on scheduling strategy."""
        try:
            if self.config.scheduling_strategy == SchedulingStrategy.PRIORITY:
                # Get highest priority task
                _, _, task = self._queues["main"].get_nowait()
                return task
            elif self.config.scheduling_strategy == SchedulingStrategy.FIFO:
                # Get oldest task
                task, _ = self._queues["main"].get_nowait()
                return task
            elif self.config.scheduling_strategy == SchedulingStrategy.ROUND_ROBIN:
                # Get next task in round-robin fashion
                if not self._queues["main"].empty():
                    task, _ = self._queues["main"].get_nowait()
                    self._queues["rr_index"] = (self._queues["rr_index"] + 1) % self.config.worker_threads
                    return task
            else:
                # Default priority-based
                _, _, task = self._queues["main"].get_nowait()
                return task
                
        except Empty:
            return None
    
    def execute_task(self, task: Task) -> None:
        """Execute a single task."""
        try:
            # Check resource availability
            if not self.resource_manager.can_allocate_resources(task):
                # Re-queue for later execution
                self._add_to_queue(task)
                return
            
            # Mark task as running
            task.status = TaskStatus.RUNNING
            task.started_at = time.time()
            
            # Allocate resources
            self.resource_manager.allocate_resources(task)
            
            # Submit to thread pool
            future = self._executor.submit(self._run_task, task)
            self._task_futures[task.id] = future
            
        except Exception as e:
            self._handle_task_error(task, e)
    
    def _run_task(self, task: Task) -> None:
        """Run a single task with error handling and retries."""
        try:
            # Execute the task function
            if task.timeout_seconds:
                result = task.function(*task.args, **task.kwargs)
            else:
                result = task.function(*task.args, **task.kwargs)
            
            # Mark as completed
            task.status = TaskStatus.COMPLETED
            task.completed_at = time.time()
            task.result = result
            
            # Update statistics
            self._update_completion_stats(task)
            
        except Exception as e:
            # Handle errors and retries
            self._handle_task_error(task, e)
        
        finally:
            # Always release resources
            self.resource_manager.release_resources(task)
            
            # Clean up future reference
            if task.id in self._task_futures:
                del self._task_futures[task.id]
    
    def _handle_task_error(self, task: Task, error: Exception) -> None:
        """Handle task execution errors and retry logic."""
        task.retry_count += 1
        task.error = str(error)
        
        if task.retry_count <= task.max_retries:
            # Retry the task
            task.status = TaskStatus.PENDING
            task.started_at = None
            task.error = None
            time.sleep(min(2 ** task.retry_count, 10))  # Exponential backoff
            self._add_to_queue(task)
        else:
            # Mark as failed
            task.status = TaskStatus.FAILED
            task.completed_at = time.time()
            self._stats["total_failed"] += 1
    
    def _update_completion_stats(self, task: Task) -> None:
        """Update completion statistics."""
        self._stats["total_completed"] += 1
        
        if task.duration:
            # Update average duration
            current_avg = self._stats["average_duration"]
            completed = self._stats["total_completed"]
            self._stats["average_duration"] = (
                (current_avg * (completed - 1) + task.duration) / completed
            )
    
    def start_scheduler(self) -> None:
        """Start the main scheduling loop."""
        def scheduling_loop():
            while self._monitoring_active:
                try:
                    # Check if we can run more tasks
                    if (self._running_tasks < self.config.max_concurrent_tasks and 
                        not self._queues["main"].empty()):
                        
                        # Get next task
                        task = self.get_next_task()
                        if task:
                            self._running_tasks += 1
                            self.execute_task(task)
                    
                    # Process completed futures
                    self._process_completed_futures()
                    
                    # Check for starved tasks
                    if self.config.starvation_prevention:
                        self.priority_handler.check_starvation(self._tasks)
                    
                    time.sleep(0.1)  # Small delay to prevent busy waiting
                    
                except Exception as e:
                    # Log error but continue scheduling
                    if self.config.detailed_logging:
                        print(f"Scheduling loop error: {e}")
                    time.sleep(1)
        
        self._monitoring_active = True
        self._monitoring_thread = threading.Thread(target=scheduling_loop, daemon=True)
        self._monitoring_thread.start()
    
    def stop_scheduler(self) -> None:
        """Stop the scheduler."""
        self._monitoring_active = False
        if self._monitoring_thread and self._monitoring_thread.is_alive():
            self._monitoring_thread.join(timeout=5)
        self._executor.shutdown(wait=True)
    
    def _process_completed_futures(self) -> None:
        """Process completed task futures."""
        completed_futures = []
        
        for task_id, future in list(self._task_futures.items()):
            if future.done():
                completed_futures.append((task_id, future))
        
        for task_id, future in completed_futures:
            try:
                future.result()  # This will raise any exception that occurred
                self._running_tasks -= 1
            except Exception as e:
                # Task failed, but error is already handled in _run_task
                self._running_tasks -= 1
    
    def get_task_status(self, task_id: str) -> Optional[Dict[str, Any]]:
        """Get the status of a specific task."""
        task = self._tasks.get(task_id)
        if not task:
            return None
        
        return {
            "id": task.id,
            "name": task.name,
            "status": task.status.value,
            "priority": task.priority.value,
            "created_at": task.created_at,
            "started_at": task.started_at,
            "completed_at": task.completed_at,
            "duration": task.duration,
            "retry_count": task.retry_count,
            "error": task.error,
            "metadata": task.metadata
        }
    
    def cancel_task(self, task_id: str) -> bool:
        """Cancel a pending task."""
        with self._scheduler_lock:
            task = self._tasks.get(task_id)
            if not task or task.status != TaskStatus.PENDING:
                return False
            
            task.status = TaskStatus.CANCELLED
            return True
    
    def get_queue_status(self) -> Dict[str, Any]:
        """Get current queue status."""
        return {
            "queue_size": self._queues["main"].qsize(),
            "running_tasks": self._running_tasks,
            "total_tasks": len(self._tasks),
            "scheduling_strategy": self.config.scheduling_strategy.value,
            "resource_usage": self.resource_manager.get_usage_stats()
        }
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get scheduler statistics."""
        uptime = time.time() - self._stats["start_time"]
        
        return {
            **self._stats,
            "uptime_seconds": uptime,
            "tasks_per_second": self._stats["total_submitted"] / uptime if uptime > 0 else 0,
            "success_rate": (
                self._stats["total_completed"] / max(self._stats["total_submitted"], 1)
            ) * 100,
            "queue_status": self.get_queue_status()
        }
    
    def _start_monitoring(self) -> None:
        """Start the monitoring system."""
        def monitoring_loop():
            while self._monitoring_active:
                try:
                    # Health check
                    if self.config.health_check_enabled:
                        self._perform_health_check()
                    
                    # Performance monitoring
                    if self.config.log_performance_stats:
                        self._log_performance_stats()
                    
                    # Resource monitoring
                    self.resource_manager.update_usage_stats()
                    
                    time.sleep(self.config.monitoring_interval_seconds)
                    
                except Exception as e:
                    if self.config.detailed_logging:
                        print(f"Monitoring error: {e}")
                    time.sleep(5)
        
        self._monitoring_active = True
        self._monitoring_thread = threading.Thread(target=monitoring_loop, daemon=True)
        self._monitoring_thread.start()
    
    def _perform_health_check(self) -> None:
        """Perform system health check."""
        # Check if scheduler is responsive
        if not self._monitoring_thread.is_alive():
            if self.config.auto_recovery:
                self.start_scheduler()  # Restart scheduler
        else:
            # Check for dead tasks
            current_time = time.time()
            for task in self._tasks.values():
                if (task.status == TaskStatus.RUNNING and 
                    task.timeout_seconds and 
                    task.started_at and 
                    current_time - task.started_at > task.timeout_seconds):
                    
                    task.status = TaskStatus.TIMEOUT
                    task.completed_at = current_time
                    self._running_tasks -= 1
    
    def _log_performance_stats(self) -> None:
        """Log performance statistics."""
        stats = self.get_statistics()
        if self.config.detailed_logging:
            print(f"Scheduler Stats: {stats}")
    
    def batch_submit(self, 
                    tasks: List[Dict[str, Any]], 
                    batch_size: Optional[int] = None) -> List[str]:
        """Submit multiple tasks in batch."""
        if batch_size is None:
            batch_size = self.config.batch_size
        
        task_ids = []
        for i in range(0, len(tasks), batch_size):
            batch = tasks[i:i + batch_size]
            
            # Submit batch
            for task_config in batch:
                task_id = self.submit_task(**task_config)
                task_ids.append(task_id)
        
        return task_ids
