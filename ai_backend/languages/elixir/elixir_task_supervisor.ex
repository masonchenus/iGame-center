
# Elixir Task Supervisor - Task Management and Supervision
defmodule ElixirTaskSupervisor do
  # Simple Task Supervisor
  defmodule SimpleTaskSupervisor do
    use Task.Supervisor

    def start_link(opts \\ []) do
      Task.Supervisor.start_link(__MODULE__, :ok, opts)
    end

    def init(:ok) do
      children = [
        {Task, name: __MODULE__, restart: :transient}
      ]
      supervise(children, strategy: :one_for_one, max_restarts: 3, max_seconds: 5)
    end

    def async_task(task_name, fun) when is_function(fun) do
      Task.Supervisor.async_nolink(__MODULE__, fn ->
        try do
          result = fun.()
          %{task: task_name, result: result, status: :success, timestamp: System.system_time(:millisecond)}
        rescue
          error ->
            %{task: task_name, error: inspect(error), status: :error, timestamp: System.system_time(:millisecond)}
        end
      end)
    end

    def await_task(task) do
      Task.await(task)
    end

    def async_stream(tasks, fun, options \\ []) do
      concurrency = Keyword.get(options, :max_concurrency, 10)

      tasks
      |> Task.async_stream(fun, max_concurrency: concurrency, timeout: 30000)
      |> Enum.map(fn
        {:ok, result} -> result
        {:exit, reason} -> %{error: inspect(reason), status: :failed}
        {:throw, thrown} -> %{thrown: inspect(thrown), status: :thrown}
      end)
    end

    def supervise_function(task_name, fun, restart_strategy \\ :transient) do
      Task.Supervisor.start_child(__MODULE__, fn ->
        try do
          result = fun.()
          if restart_strategy == :temporary, do: :ok
          result
        rescue
          error ->
            if restart_strategy == :permanent or restart_strategy == :transient do
              reraise error, __STACKTRACE__
            end
            {:error, inspect(error)}
        end
      end)
    end
  end

  # Dynamic Task Supervisor for Runtime Task Creation
  defmodule DynamicTaskSupervisor do
    use DynamicSupervisor

    def start_link(opts \\ []) do
      name = Keyword.get(opts, :name, __MODULE__)
      DynamicSupervisor.start_link(__MODULE__, :ok, name: name)
    end

    def init(:ok) do
      DynamicSupervisor.init(strategy: :one_for_one)
    end

    def start_worker(supervisor, worker_id, fun) when is_function(fun) do
      spec = {Task, fn -> worker_loop(worker_id, fun) end}
      DynamicSupervisor.start_child(supervisor, spec)
    end

    def start_linked_worker(supervisor, worker_id, fun) when is_function(fun) do
      spec = {Task, fn -> worker_loop(worker_id, fun) end, restart: :temporary}
      DynamicSupervisor.start_child(supervisor, spec)
    end

    def count_children(supervisor) do
      DynamicSupervisor.count_children(supervisor)
    end

    def terminate_child(supervisor, pid) when is_pid(pid) do
      DynamicSupervisor.terminate_child(supervisor, pid)
    end

    def terminate_all(supervisor) do
      children = DynamicSupervisor.which_children(supervisor)
      Enum.each(children, fn
        {:undefined, pid, :worker, [Task]} ->
          DynamicSupervisor.terminate_child(supervisor, pid)
        _ -> :ok
      end)
    end

    defp worker_loop(worker_id, fun) do
      try do
        fun.(worker_id)
      rescue
        error ->
          IO.puts("Worker #{worker_id} failed: #{inspect(error)}")
          {:error, error}
      end
    end
  end

  # Task Registry for Worker Discovery
  defmodule TaskRegistry do
    use GenServer

    def start_link(opts \\ []) do
      GenServer.start_link(__MODULE__, :ok, opts)
    end

    def init(:ok) do
      {:ok, %{tasks: %{}, pids: %{}}}
    end

    def register_task(name, pid) when is_pid(pid) do
      GenServer.cast(__MODULE__, {:register, name, pid})
    end

    def unregister_task(name) do
      GenServer.cast(__MODULE__, {:unregister, name})
    end

    def get_pid(name) do
      GenServer.call(__MODULE__, {:get_pid, name})
    end

    def get_all_tasks do
      GenServer.call(__MODULE__, :get_all)
    end

    def handle_cast({:register, name, pid}, state) do
      Process.monitor(pid)
      {:noreply, %{state | tasks: Map.put(state.tasks, name, pid), pids: Map.put(state.pids, pid, name)}}
    end

    def handle_cast({:unregister, name}, state) do
      case Map.get(state.tasks, name) do
        nil -> {:noreply, state}
        pid ->
          Process.demonitor(pid, [:flush])
          {:noreply, %{state | tasks: Map.delete(state.tasks, name), pids: Map.delete(state.pids, pid)}}
      end
    end

    def handle_call({:get_pid, name}, _from, state) do
      {:reply, Map.get(state.tasks, name), state}
    end

    def handle_call(:get_all, _from, state) do
      {:reply, state.tasks, state}
    end

    def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
      case Map.get(state.pids, pid) do
        nil -> {:noreply, state}
        name ->
          {:noreply, %{state | tasks: Map.delete(state.tasks, name), pids: Map.delete(state.pids, pid)}}
      end
    end
  end

  # Batch Task Processor
  defmodule BatchTaskProcessor do
    use GenServer

    defstruct batch_size: 100, interval: 1000, queue: :queue.new(), processor: nil, workers: []

    def start_link(opts \\ []) do
      batch_size = Keyword.get(opts, :batch_size, 100)
      interval = Keyword.get(opts, :interval, 1000)
      processor = Keyword.get(opts, :processor, fn items -> items end)

      GenServer.start_link(__MODULE__, %{batch_size: batch_size, interval: interval, processor: processor, queue: :queue.new(), workers: []}, name: __MODULE__)
    end

    def enqueue(item) do
      GenServer.cast(__MODULE__, {:enqueue, item})
    end

    def enqueue_batch(items) when is_list(items) do
      GenServer.cast(__MODULE__, {:enqueue_batch, items})
    end

    def init(state) do
      {:ok, state, {:continue, :start_timer}}
    end

    def handle_continue(:start_timer, state) do
      Process.send_after(self(), :process_batch, state.interval)
      {:noreply, state}
    end

    def handle_cast({:enqueue, item}, state) do
      new_queue = :queue.in(item, state.queue)
      {:noreply, %{state | queue: new_queue}}
    end

    def handle_cast({:enqueue_batch, items}, state) do
      new_queue = Enum.reduce(items, state.queue, &:queue.in(&1, &2))
      {:noreply, %{state | queue: new_queue}}
    end

    def handle_info(:process_batch, state) do
      {items, new_queue} = :queue.out(state.batch_size, state.queue)

      if items != :empty do
        processed_items = state.processor.(items)
        IO.puts("Processed batch of #{length(items)} items")
      end

      Process.send_after(self(), :process_batch, state.interval)
      {:noreply, %{state | queue: new_queue}}
    end

    def queue_stats do
      GenServer.call(__MODULE__, :queue_stats)
    end

    def handle_call(:queue_stats, _from, state) do
      {:reply, %{queue_size: :queue.len(state.queue), batch_size: state.batch_size}, state}
    end
  end

  # Retry Task Handler
  defmodule RetryTaskHandler do
    defmodule TaskState do
      defstruct task_id: nil, fun: nil, max_retries: 3, current_retry: 0, backoff: 1000, last_error: nil, status: :pending
    end

    def start_retry_task(task_id, fun, opts \\ []) when is_function(fun) do
      max_retries = Keyword.get(opts, :max_retries, 3)
      backoff = Keyword.get(opts, :backoff, 1000)

      task_state = %TaskState{task_id: task_id, fun: fun, max_retries: max_retries, backoff: backoff, status: :pending}

      Task.Supervisor.start_child(ElixirTaskSupervisor.SimpleTaskSupervisor, fn ->
        execute_with_retry(task_state)
      end)
    end

    defp execute_with_retry(task_state) do
      try do
        result = task_state.fun.()
        %{task_id: task_state.task_id, result: result, status: :success, retries: task_state.current_retry}
      rescue
        error ->
          if task_state.current_retry < task_state.max_retries do
            new_retry_state = %{
              task_state |
              current_retry: task_state.current_retry + 1,
              last_error: inspect(error),
              status: :retrying
            }

            Process.sleep(task_state.backoff * (task_state.current_retry + 1))
            execute_with_retry(new_retry_state)
          else
            %{task_id: task_state.task_id, error: inspect(error), status: :failed, retries: task_state.current_retry}
          end
      end
    end
  end

  # Task Metrics Collector
  defmodule TaskMetrics do
    use Agent

    def start_link do
      Agent.start_link(fn -> %{tasks_completed: 0, tasks_failed: 0, tasks_retried: 0, total_duration: 0} end, name: __MODULE__)
    end

    def record_completion(duration \\ 0) do
      Agent.update(__MODULE__, fn state ->
        %{
          tasks_completed: state.tasks_completed + 1,
          tasks_failed: state.tasks_failed,
          tasks_retried: state.tasks_retried,
          total_duration: state.total_duration + duration
        }
      end)
    end

    def record_failure do
      Agent.update(__MODULE__, fn state ->
        %{
          tasks_completed: state.tasks_completed,
          tasks_failed: state.tasks_failed + 1,
          tasks_retried: state.tasks_retried,
          total_duration: state.total_duration
        }
      end)
    end

    def record_retry do
      Agent.update(__MODULE__, fn state ->
        %{
          tasks_completed: state.tasks_completed,
          tasks_failed: state.tasks_failed,
          tasks_retried: state.tasks_retried + 1,
          total_duration: state.total_duration
        }
      end)
    end

    def get_stats do
      Agent.get(__MODULE__, fn state ->
        total = state.tasks_completed + state.tasks_failed
        avg_duration = if total > 0, do: state.total_duration / total, else: 0

        Map.put(state, :average_duration_ms, avg_duration)
        |> Map.put(:success_rate, if(total > 0, do: state.tasks_completed / total * 100, else: 0))
      end)
    end
  end

  # Priority Task Queue
  defmodule PriorityTaskQueue do
    use GenServer

    defstruct high: :queue.new(), medium: :queue.new(), low: :queue.new(), processing: []

    def start_link(opts \\ []) do
      GenServer.start_link(__MODULE__, :ok, opts)
    end

    def init(:ok) do
      {:ok, %__MODULE__{}}
    end

    def add(task, priority \\ :medium) when is_map(task) do
      GenServer.cast(__MODULE__, {:add, task, priority})
    end

    def add_batch(tasks, priority \\ :medium) when is_list(tasks) do
      GenServer.cast(__MODULE__, {:add_batch, tasks, priority})
    end

    def get_next do
      GenServer.call(__MODULE__, :get_next)
    def get_queue_sizes do
      GenServer.call(__MODULE__, :get_queue_sizes)
    end

    def handle_cast({:add, task, priority}, state) do
      queue = Map.get(state, priority)
      new_queue = :queue.in(task, queue)
      {:noreply, Map.put(state, priority, new_queue)}
    end

    def handle_cast({:add_batch, tasks, priority}, state) do
      queue = Map.get(state, priority)
      new_queue = Enum.reduce(tasks, queue, &:queue.in(&1, &2))
      {:noreply, Map.put(state, priority, new_queue)}
    end

    def handle_call(:get_next, _from, state) do
      # Try high, then medium, then low priority
      case get_next_item(state, :high) do
        {:ok, item, new_state} -> {:reply, {:ok, item, :high}, new_state}
        :empty ->
          case get_next_item(state, :medium) do
            {:ok, item, new_state} -> {:reply, {:ok, item, :medium}, new_state}
            :empty ->
              case get_next_item(state, :low) do
                {:ok, item, new_state} -> {:reply, {:ok, item, :low}, new_state}
                :empty -> {:reply, :empty, state}
              end
          end
      end
    end

    def handle_call(:get_queue_sizes, _from, state) do
      sizes = %{
        high: :queue.len(state.high),
        medium: :queue.len(state.medium),
        low: :queue.len(state.low)
      }
      {:reply, sizes, state}
    end

    defp get_next_item(state, priority) do
      queue = Map.get(state, priority)
      case :queue.out(queue) do
        {{:value, item}, new_queue} -> {:ok, item, Map.put(state, priority, new_queue)}
        :empty -> :empty
      end
    end
  end

  # Task Timeout Handler
  defmodule TaskTimeoutHandler do
    def start_task_with_timeout(task_id, fun, timeout \\ 5000) when is_function(fun) do
      Task.Supervisor.start_child(ElixirTaskSupervisor.SimpleTaskSupervisor, fn ->
        task = Task.Supervisor.async_nolink(ElixirTaskSupervisor.SimpleTaskSupervisor, fun)

        ref = Process.monitor(task.pid)

        receive do
          {^ref, result} ->
            Process.demonitor(ref, [:flush])
            %{task_id: task_id, result: result, status: :completed, timed_out: false}

          {:DOWN, ^ref, :process, _pid, :normal} ->
            %{task_id: task_id, result: nil, status: :completed, timed_out: false}

          {:DOWN, ^ref, :process, _pid, reason} ->
            %{task_id: task_id, error: inspect(reason), status: :failed, timed_out: false}
        after
          timeout ->
            Process.demonitor(ref, [:flush])
            Task.Supervisor.terminate_child(ElixirTaskSupervisor.SimpleTaskSupervisor, task.pid)
            %{task_id: task_id, error: :timeout, status: :timed_out, timed_out: true}
        end
      end)
    end
  end

  # Distributed Task Supervisor
  defmodule DistributedTaskSupervisor do
    use Horde.Supervisor, shutdown: 30_000

    def start_link(opts \\ []) do
      name = Keyword.get(opts, :name, __MODULE__)
      Horde.Supervisor.start_link(__MODULE__, :ok, name: name)
    end

    def init(:ok) do
      supervisors = [
        {ElixirTaskSupervisor.SimpleTaskSupervisor, name: ElixirTaskSupervisor.SimpleTaskSupervisor}
      ]

      Horde.Supervisor.init(supervisors, strategy: :one_for_one, max_restarts: 3, max_seconds: 5)
    end

    def start_distributed_task(task_name, fun) when is_function(fun) do
      case Horde.Supervisor.start_child(__MODULE__, {Task, fn -> task_name, fun end}) do
        {:ok, pid} -> {:ok, pid}
        {:error, reason} -> {:error, reason}
      end
    end

    def get_local_supervisor do
      Horde.Supervisor.which_children(__MODULE__)
    end
  end
end

# Demo usage
defmodule ElixirTaskSupervisorDemo do
  def run do
    IO.puts("=== Elixir Task Supervisor Demo ===")

    # Start task supervisors
    IO.puts("\n--- Starting Task Supervisors ---")
    {:ok, simple_sup} = ElixirTaskSupervisor.SimpleTaskSupervisor.start_link(name: SimpleTaskSupervisor)
    {:ok, dynamic_sup} = ElixirTaskSupervisor.DynamicTaskSupervisor.start_link(name: DynamicTaskSupervisor)
    {:ok, _registry} = ElixirTaskSupervisor.TaskRegistry.start_link(name: TaskRegistry)
    {:ok, _batch} = ElixirTaskSupervisor.BatchTaskProcessor.start_link(name: BatchProcessor, batch_size: 10, interval: 2000)
    {:ok, _metrics} = ElixirTaskSupervisor.TaskMetrics.start_link()
    {:ok, _priority_queue} = ElixirTaskSupervisor.PriorityTaskQueue.start_link(name: PriorityQueue)

    IO.puts("Task supervisors started")

    # Demonstrate simple task supervision
    IO.puts("\n--- Simple Task Execution ---")
    task = ElixirTaskSupervisor.SimpleTaskSupervisor.async_task("compute_factorial", fn ->
      1..10 |> Enum.reduce(1, &(&1 * &2))
    end)

    result = ElixirTaskSupervisor.SimpleTaskSupervisor.await_task(task)
    IO.puts("Factorial of 10: #{result.result}")

    # Demonstrate batch processing
    IO.puts("\n--- Batch Task Processing ---")
    for i <- 1..15 do
      ElixirTaskSupervisor.BatchTaskProcessor.enqueue(%{id: i, data: "item_#{i}"})
    end

    :timer.sleep(2500)
    stats = ElixirTaskSupervisor.BatchTaskProcessor.queue_stats()
    IO.puts("Batch queue stats: #{inspect(stats)}")

    # Demonstrate priority queue
    IO.puts("\n--- Priority Task Queue ---")
    ElixirTaskSupervisor.PriorityTaskQueue.add(%{task: "low_priority_task", data: "low"}, :low)
    ElixirTaskSupervisor.PriorityTaskQueue.add(%{task: "high_priority_task", data: "high"}, :high)
    ElixirTaskSupervisor.PriorityTaskQueue.add(%{task: "medium_priority_task", data: "medium"}, :medium)

    IO.puts("Queue sizes: #{inspect(ElixirTaskSupervisor.PriorityTaskQueue.get_queue_sizes())}")

    case ElixirTaskSupervisor.PriorityTaskQueue.get_next() do
      {:ok, item, priority} -> IO.puts("Next task: #{item.task} (priority: #{priority})")
      :empty -> IO.puts("Queue is empty")
    end

    # Demonstrate task with timeout
    IO.puts("\n--- Task with Timeout ---")
    {:ok, timeout_task} = ElixirTaskSupervisor.TaskTimeoutHandler.start_task_with_timeout(
      "quick_task",
      fn -> :timer.sleep(100); "Completed" end,
      200
    )
    IO.puts("Quick task result: #{inspect(timeout_task)}")

    {:ok, slow_task} = ElixirTaskSupervisor.TaskTimeoutHandler.start_task_with_timeout(
      "slow_task",
      fn -> :timer.sleep(5000); "Completed" end,
      100
    )
    IO.puts("Slow task result: #{inspect(slow_task)}")

    # Demonstrate async stream
    IO.puts("\n--- Async Stream Processing ---")
    items = Enum.map(1..20, fn i -> %{id: i, value: i * i} end)

    results = ElixirTaskSupervisor.SimpleTaskSupervisor.async_stream(items, fn item ->
      :timer.sleep(50)
      %{id: item.id, processed: true}
    end, max_concurrency: 5)

    IO.puts("Processed #{length(results)} items")

    # Demonstrate task metrics
    IO.puts("\n--- Task Metrics ---")
    ElixirTaskSupervisor.TaskMetrics.record_completion(100)
    ElixirTaskSupervisor.TaskMetrics.record_completion(150)
    ElixirTaskSupervisor.TaskMetrics.record_failure()

    stats = ElixirTaskSupervisor.TaskMetrics.get_stats()
    IO.puts("Task metrics: #{inspect(stats)}")

    # Demonstrate dynamic task supervisor
    IO.puts("\n--- Dynamic Task Workers ---")
    for i <- 1..3 do
      {:ok, worker} = ElixirTaskSupervisor.DynamicTaskSupervisor.start_worker(
        DynamicTaskSupervisor,
        "worker_#{i}",
        fn id ->
          IO.puts("Worker #{id} processing...")
          :timer.sleep(200)
          "Worker #{id} done"
        end
      )
      IO.puts("Started worker #{i}: #{inspect(worker)}")
    end

    # Get metrics
    children = ElixirTaskSupervisor.DynamicTaskSupervisor.count_children(DynamicTaskSupervisor)
    IO.puts("Dynamic supervisor children: #{inspect(children)}")

    IO.puts("\n=== Task Supervisor Demo Complete ===")
  end
end

# Run the demo
if __ENV__.module == ElixirTaskSupervisorDemo do
  ElixirTaskSupervisorDemo.run()
end
