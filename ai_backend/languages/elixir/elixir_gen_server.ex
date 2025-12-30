# Elixir GenServer - Generic Server Behavior Implementation
defmodule ElixirGenServer do
  # Basic GenServer Counter
  defmodule BasicCounter do
    use GenServer

    # Client API
    def start_link(initial_value \\ 0) do
      GenServer.start_link(__MODULE__, initial_value, name: __MODULE__)
    end

    def increment do
      GenServer.cast(__MODULE__, :increment)
    end

    def decrement do
      GenServer.cast(__MODULE__, :decrement)
    end

    def get do
      GenServer.call(__MODULE__, :get)
    end

    def set(value) when is_integer(value) do
      GenServer.cast({__MODULE__, value})
    end

    def reset do
      GenServer.cast(__MODULE__, :reset)
    end

    # Server Callbacks
    def init(initial_value) do
      {:ok, initial_value}
    end

    def handle_call(:get, _from, state) do
      {:reply, state, state}
    end

    def handle_cast(:increment, state) do
      {:noreply, state + 1}
    end

    def handle_cast(:decrement, state) do
      {:noreply, state - 1}
    end

    def handle_cast(:reset, _state) do
      {:noreply, 0}
    end

    def handle_cast({:set, value}, _state) when is_integer(value) do
      {:noreply, value}
    end

    def handle_info(:timeout, state) do
      IO.puts("Timeout received with state: #{state}")
      {:noreply, state}
    end

    def handle_info({:increment_by, amount}, state) do
      {:noreply, state + amount}
    end

    def terminate(reason, state) do
      IO.puts("Terminating GenServer. Reason: #{inspect(reason)}, Final state: #{state}")
      :ok
    end

    def code_change(old_vsn, state, extra) do
      IO.puts("Code change: #{inspect(old_vsn)} -> #{inspect(extra)}")
      {:ok, state}
    end
  end

  # Key-Value Store GenServer
  defmodule KeyValueStore do
    use GenServer

    # Client API
    def start_link(initial_values \\ %{}) do
      GenServer.start_link(__MODULE__, initial_values)
    end

    def get(pid, key) when is_pid(pid) do
      GenServer.call(pid, {:get, key})
    end

    def put(pid, key, value) when is_pid(pid) do
      GenServer.cast(pid, {:put, key, value})
    end

    def delete(pid, key) when is_pid(pid) do
      GenServer.cast(pid, {:delete, key})
    end

    def keys(pid) when is_pid(pid) do
      GenServer.call(pid, :keys)
    end

    def values(pid) when is_pid(pid) do
      GenServer.call(pid, :values)
    end

    def clear(pid) when is_pid(pid) do
      GenServer.cast(pid, :clear)
    end

    def get_all(pid) when is_pid(pid) do
      GenServer.call(pid, :get_all)
    end

    def update(pid, key, updater) when is_pid(pid) and is_function(updater) do
      GenServer.cast(pid, {:update, key, updater})
    end

    # Server Callbacks
    def init(initial_values) when is_map(initial_values) do
      {:ok, initial_values}
    end

    def handle_call({:get, key}, _from, state) do
      value = Map.get(state, key)
      {:reply, value, state}
    end

    def handle_call(:keys, _from, state) do
      keys = Map.keys(state)
      {:reply, keys, state}
    end

    def handle_call(:values, _from, state) do
      values = Map.values(state)
      {:reply, values, state}
    end

    def handle_call(:get_all, _from, state) do
      {:reply, state, state}
    end

    def handle_cast({:put, key, value}, state) do
      new_state = Map.put(state, key, value)
      {:noreply, new_state}
    end

    def handle_cast({:delete, key}, state) do
      new_state = Map.delete(state, key)
      {:noreply, new_state}
    end

    def handle_cast(:clear, _state) do
      {:noreply, %{}}
    end

    def handle_cast({:update, key, updater}, state) do
      new_value = case Map.get(state, key) do
        nil -> updater.(nil)
        current_value -> updater.(current_value)
      end
      new_state = Map.put(state, key, new_value)
      {:noreply, new_state}
    end

    def handle_info({:store_value, key, value}, state) do
      new_state = Map.put(state, key, value)
      {:noreply, new_state}
    end

    def terminate(reason, state) do
      IO.puts("KeyValueStore terminated. Reason: #{inspect(reason)}")
      IO.puts("Final state: #{inspect(state)}")
      :ok
    end
  end

  # Task Worker Pool GenServer
  defmodule TaskWorkerPool do
    use GenServer

    defstruct workers: [], task_queue: [], max_workers: 5, worker_count: 0

    # Client API
    def start_link(max_workers \\ 5) do
      GenServer.start_link(__MODULE__, max_workers)
    end

    def submit_task(pid, task_fun) when is_pid(pid) and is_function(task_fun) do
      GenServer.cast(pid, {:submit_task, task_fun})
    end

    def get_worker_count(pid) when is_pid(pid) do
      GenServer.call(pid, :get_worker_count)
    end

    def get_queue_size(pid) when is_pid(pid) do
      GenServer.call(pid, :get_queue_size)
    end

    def get_status(pid) when is_pid(pid) do
      GenServer.call(pid, :get_status)
    end

    def stop(pid) when is_pid(pid) do
      GenServer.stop(pid, :normal)
    end

    # Server Callbacks
    def init(max_workers) do
      initial_state = %TaskWorkerPool{
        workers: [],
        task_queue: [],
        max_workers: max_workers,
        worker_count: 0
      }
      {:ok, initial_state}
    end

    def handle_cast({:submit_task, task_fun}, state) do
      %TaskWorkerPool{workers: workers, task_queue: queue, max_workers: max_workers, worker_count: count} = state

      if count < max_workers do
        # Start a new worker
        {:ok, worker_pid} = Task.start(fn -> execute_task(task_fun) end)
        new_workers = workers ++ [{worker_pid, :busy}]
        {:noreply, %{state | workers: new_workers, worker_count: count + 1}}
      else
        # Add to queue
        new_queue = queue ++ [{task_fun, self()}]
        {:noreply, %{state | task_queue: new_queue}}
      end
    end

    def handle_call(:get_worker_count, _from, state) do
      {:reply, state.worker_count, state}
    end

    def handle_call(:get_queue_size, _from, state) do
      {:reply, length(state.task_queue), state}
    end

    def handle_call(:get_status, _from, state) do
      %TaskWorkerPool{workers: workers, task_queue: queue, max_workers: max_workers, worker_count: count} = state
      status = %{
        active_workers: count,
        max_workers: max_workers,
        queue_size: length(queue),
        total_tasks_processed: "N/A"  # Would need to track this
      }
      {:reply, status, state}
    end

    def handle_info({:task_completed, worker_pid}, state) do
      %TaskWorkerPool{workers: workers, task_queue: queue, worker_count: count} = state

      # Mark worker as available
      new_workers = Enum.map(workers, fn
        {^worker_pid, :busy} -> {worker_pid, :available}
        other -> other
      end)

      # Process next task if available
      {new_state, updated_workers, updated_queue} = case queue do
        [] ->
          # No tasks in queue
          {state, new_workers, queue}

        [next_task | remaining_queue] ->
          # Start next task
          {task_fun, requester} = next_task
          {:ok, new_worker_pid} = Task.start(fn ->
            result = execute_task(task_fun)
            send(requester, {:task_result, result})
          end)

          updated_workers = Enum.map(new_workers, fn
            {^worker_pid, :available} -> {worker_pid, :busy}
            other -> other
          end)
          |> Enum.concat([{new_worker_pid, :busy}])

          new_state = %{state | task_queue: remaining_queue}
          {new_state, updated_workers, remaining_queue}
      end

      {:noreply, %{new_state | workers: updated_workers}}
    end

    def terminate(reason, state) do
      IO.puts("TaskWorkerPool terminated. Reason: #{inspect(reason)}")
      # Here you would typically clean up worker processes
      :ok
    end

    defp execute_task(task_fun) do
      try do
        result = task_fun.()
        send(self(), {:task_completed, self()})
        result
      rescue
        error ->
          IO.puts("Task failed with error: #{inspect(error)}")
          send(self(), {:task_completed, self()})
          {:error, error}
      end
    end
  end

  # Rate Limiter GenServer
  defmodule RateLimiter do
    use GenServer

    defstruct requests: %{}, limits: %{}, window_size: 60_000  # 60 seconds in milliseconds

    # Client API
    def start_link(window_size \\ 60_000) do
      GenServer.start_link(__MODULE__, window_size)
    end

    def check_rate(pid, key, limit) when is_pid(pid) and is_binary(key) and is_integer(limit) and limit > 0 do
      GenServer.call(pid, {:check_rate, key, limit})
    end

    def set_limit(pid, key, limit) when is_pid(pid) and is_binary(key) and is_integer(limit) and limit > 0 do
      GenServer.cast(pid, {:set_limit, key, limit})
    end

    def get_limits(pid) when is_pid(pid) do
      GenServer.call(pid, :get_limits)
    end

    def reset_limits(pid) when is_pid(pid) do
      GenServer.cast(pid, :reset_limits)
    end

    # Server Callbacks
    def init(window_size) do
      initial_state = %RateLimiter{
        requests: %{},
        limits: %{},
        window_size: window_size
      }
      {:ok, initial_state}
    end

    def handle_call({:check_rate, key, limit}, _from, state) do
      current_time = System.system_time(:millisecond)
      window_start = current_time - state.window_size

      # Get current request count for this key
      request_times = Map.get(state.requests, key, [])
      # Filter out old requests
      recent_requests = Enum.filter(request_times, &(&1 > window_start))

      if length(recent_requests) >= limit do
        {:reply, {:error, :rate_limited}, state}
      else
        # Add current request
        new_recent_requests = recent_requests ++ [current_time]
        new_requests = Map.put(state.requests, key, new_recent_requests)
        new_state = %{state | requests: new_requests}
        {:reply, {:ok, remaining_requests(limit, length(new_recent_requests))}, new_state}
      end
    end

    def handle_call(:get_limits, _from, state) do
      {:reply, state.limits, state}
    end

    def handle_cast({:set_limit, key, limit}, state) do
      new_limits = Map.put(state.limits, key, limit)
      new_state = %{state | limits: new_limits}
      {:noreply, new_state}
    end

    def handle_cast(:reset_limits, state) do
      {:noreply, %{state | requests: %{}}}
    end

    def handle_info(:cleanup, state) do
      current_time = System.system_time(:millisecond)
      window_start = current_time - state.window_size

      # Clean up old requests
      new_requests = Enum.map(state.requests, fn {key, times} ->
        {key, Enum.filter(times, &(&1 > window_start))}
      end)
      |> Enum.into(%{})

      new_state = %{state | requests: new_requests}
      # Schedule next cleanup
      Process.send_after(self(), :cleanup, state.window_size)
      {:noreply, new_state}
    end

    def terminate(reason, state) do
      IO.puts("RateLimiter terminated. Reason: #{inspect(reason)}")
      :ok
    end

    defp remaining_requests(limit, used) do
      max(0, limit - used)
    end
  end

  # Cache GenServer with TTL
  defmodule TTLCache do
    use GenServer

    defstruct cache: %{}, timers: %{}

    # Client API
    def start_link do
      GenServer.start_link(__MODULE__, nil)
    end

    def get(pid, key) when is_pid(pid) do
      GenServer.call(pid, {:get, key})
    end

    def put(pid, key, value, ttl \\ 300_000) when is_integer(ttl) and ttl > 0 do
      GenServer.cast(pid, {:put, key, value, ttl})
    end

    def delete(pid, key) when is_pid(pid) do
      GenServer.cast(pid, {:delete, key})
    end

    def clear(pid) when is_pid(pid) do
      GenServer.cast(pid, :clear)
    end

    def get_all(pid) when is_pid(pid) do
      GenServer.call(pid, :get_all)
    end

    def size(pid) when is_pid(pid) do
      GenServer.call(pid, :size)
    end

    # Server Callbacks
    def init(_) do
      {:ok, %TTLCache{cache: %{}, timers: %{}}}
    end

    def handle_call({:get, key}, _from, state) do
      case Map.get(state.cache, key) do
        nil ->
          {:reply, {:error, :not_found}, state}
        {value, _timestamp} ->
          {:reply, {:ok, value}, state}
      end
    end

    def handle_call(:get_all, _from, state) do
      cache_data = Map.map(state.cache, fn {key, {value, _timestamp}} -> {key, value} end)
      {:reply, cache_data, state}
    end

    def handle_call(:size, _from, state) do
      {:reply, map_size(state.cache), state}
    end

    def handle_cast({:put, key, value, ttl}, state) do
      # Cancel existing timer if any
      new_timers = case Map.get(state.timers, key) do
        nil -> state.timers
        timer ->
          Process.cancel_timer(timer)
          Map.delete(state.timers, key)
      end

      # Set new timer
      timer = Process.send_after(self(), {:expire, key}, ttl)
      new_timers_with_key = Map.put(new_timers, key, timer)

      # Store value with timestamp
      timestamp = System.system_time(:millisecond)
      new_cache = Map.put(state.cache, key, {value, timestamp})

      new_state = %{state | cache: new_cache, timers: new_timers_with_key}
      {:noreply, new_state}
    end

    def handle_cast({:delete, key}, state) do
      # Cancel timer if exists
      new_timers = case Map.get(state.timers, key) do
        nil -> state.timers
        timer ->
          Process.cancel_timer(timer)
          Map.delete(state.timers, key)
      end

      new_cache = Map.delete(state.cache, key)
      new_state = %{state | cache: new_cache, timers: new_timers}
      {:noreply, new_state}
    end

    def handle_cast(:clear, state) do
      # Cancel all timers
      Enum.each(state.timers, fn {_key, timer} ->
        Process.cancel_timer(timer)
      end)

      new_state = %{state | cache: %{}, timers: %{}}
      {:noreply, new_state}
    end

    def handle_info({:expire, key}, state) do
      new_cache = Map.delete(state.cache, key)
      new_timers = Map.delete(state.timers, key)
      new_state = %{state | cache: new_cache, timers: new_timers}
      {:noreply, new_state}
    end

    def terminate(reason, state) do
      IO.puts("TTLCache terminated. Reason: #{inspect(reason)}")
      # Cancel all timers
      Enum.each(state.timers, fn {_key, timer} ->
        Process.cancel_timer(timer)
      end)
      :ok
    end
  end

  # PubSub Manager GenServer
  defmodule PubSubManager do
    use GenServer

    defstruct subscribers: %{}, topics: %{}

    # Client API
    def start_link do
      GenServer.start_link(__MODULE__, nil)
    end

    def subscribe(pid, topic) when is_pid(pid) and is_binary(topic) do
      GenServer.cast(pid, {:subscribe, self(), topic})
    end

    def unsubscribe(pid, topic) when is_pid(pid) and is_binary(topic) do
      GenServer.cast(pid, {:unsubscribe, self(), topic})
    end

    def publish(pid, topic, message) when is_pid(pid) and is_binary(topic) do
      GenServer.cast(pid, {:publish, topic, message})
    end

    def get_topics(pid) when is_pid(pid) do
      GenServer.call(pid, :get_topics)
    end

    def get_subscribers(pid, topic) when is_pid(pid) and is_binary(topic) do
      GenServer.call(pid, {:get_subscribers, topic})
    end

    def get_topic_count(pid, topic) when is_pid(pid) and is_binary(topic) do
      GenServer.call(pid, {:get_topic_count, topic})
    end

    # Server Callbacks
    def init(_) do
      {:ok, %PubSubManager{subscribers: %{}, topics: %{}}}
    end

    def handle_cast({:subscribe, pid, topic}, state) do
      new_subscribers = Map.update(state.subscribers, pid, [topic], fn topics ->
        if topic in topics, do: topics, else: topics ++ [topic]
      end)

      new_topics = Map.update(state.topics, topic, [pid], fn subscribers ->
        if pid in subscribers, do: subscribers, else: subscribers ++ [pid]
      end)

      new_state = %{state | subscribers: new_subscribers, topics: new_topics}
      {:noreply, new_state}
    end

    def handle_cast({:unsubscribe, pid, topic}, state) do
      new_subscribers = Map.update(state.subscribers, pid, [], fn topics ->
        List.delete(topics, topic)
      end)

      new_topics = Map.update(state.topics, topic, [], fn subscribers ->
        List.delete(subscribers, pid)
      end)

      new_state = %{state | subscribers: new_subscribers, topics: new_topics}
      {:noreply, new_state}
    end

    def handle_cast({:publish, topic, message}, state) do
      subscribers = Map.get(state.topics, topic, [])
      Enum.each(subscribers, fn pid ->
        send(pid, {:published_message, topic, message})
      end)
      {:noreply, state}
    end

    def handle_call(:get_topics, _from, state) do
      topics = Map.keys(state.topics)
      {:reply, topics, state}
    end

    def handle_call({:get_subscribers, topic}, _from, state) do
      subscribers = Map.get(state.topics, topic, [])
      {:reply, subscribers, state}
    end

    def handle_call({:get_topic_count, topic}, _from, state) do
      count = length(Map.get(state.topics, topic, []))
      {:reply, count, state}
    end

    def handle_info({:subscriber_down, pid}, state) do
      # Clean up when a subscriber process dies
      topics = Map.get(state.subscribers, pid, [])

      new_subscribers = Map.delete(state.subscribers, pid)
      new_topics = Enum.reduce(topics, state.topics, fn topic, acc ->
        Map.update!(acc, topic, fn subscribers ->
          List.delete(subscribers, pid)
        end)
      end)

      new_state = %{state | subscribers: new_subscribers, topics: new_topics}
      {:noreply, new_state}
    end

    def terminate(reason, state) do
      IO.puts("PubSubManager terminated. Reason: #{inspect(reason)}")
      :ok
    end
  end

  # Configuration Manager GenServer
  defmodule ConfigManager do
    use GenServer

    defstruct config: %{}, defaults: %{}, watchers: %{}

    # Client API
    def start_link(initial_config \\ %{}) do
      GenServer.start_link(__MODULE__, initial_config)
    end

    def get(pid, key) when is_pid(pid) do
      GenServer.call(pid, {:get, key})
    end

    def set(pid, key, value) when is_pid(pid) do
      GenServer.cast(pid, {:set, key, value})
    end

    def get_all(pid) when is_pid(pid) do
      GenServer.call(pid, :get_all)
    end

    def set_defaults(pid, defaults) when is_map(defaults) do
      GenServer.cast(pid, {:set_defaults, defaults})
    end

    def get_default(pid, key) when is_pid(pid) do
      GenServer.call(pid, {:get_default, key})
    end

    def reset_to_defaults(pid) when is_pid(pid) do
      GenServer.cast(pid, :reset_to_defaults)
    end

    def watch_config(pid, key) when is_pid(pid) do
      GenServer.cast(pid, {:watch, self(), key})
    end

    def unwatch_config(pid, key) when is_pid(pid) do
      GenServer.cast(pid, {:unwatch, self(), key})
    end

    # Server Callbacks
    def init(initial_config) do
      {:ok, %ConfigManager{config: initial_config, defaults: %{}, watchers: %{}}}
    end

    def handle_call({:get, key}, _from, state) do
      value = Map.get(state.config, key, Map.get(state.defaults, key))
      {:reply, value, state}
    end

    def handle_call(:get_all, _from, state) do
      {:reply, state.config, state}
    end

    def handle_call({:get_default, key}, _from, state) do
      value = Map.get(state.defaults, key)
      {:reply, value, state}
    end

    def handle_cast({:set, key, value}, state) do
      new_config = Map.put(state.config, key, value)
      new_state = %{state | config: new_config}

      # Notify watchers
      watchers = Map.get(state.watchers, key, [])
      Enum.each(watchers, fn watcher_pid ->
        send(watcher_pid, {:config_changed, key, value})
      end)

      {:noreply, new_state}
    end

    def handle_cast({:set_defaults, defaults}, state) do
      new_state = %{state | defaults: defaults}
      {:noreply, new_state}
    end

    def handle_cast(:reset_to_defaults, state) do
      new_state = %{state | config: state.defaults}
      {:noreply, new_state}
    end

    def handle_cast({:watch, watcher_pid, key}, state) do
      new_watchers = Map.update(state.watchers, key, [watcher_pid], fn watchers ->
        if watcher_pid in watchers, do: watchers, else: watchers ++ [watcher_pid]
      end)
      new_state = %{state | watchers: new_watchers}
      {:noreply, new_state}
    end

    def handle_cast({:unwatch, watcher_pid, key}, state) do
      new_watchers = Map.update(state.watchers, key, [], fn watchers ->
        List.delete(watchers, watcher_pid)
      end)
      new_state = %{state | watchers: new_watchers}
      {:noreply, new_state}
    end

    def terminate(reason, state) do
      IO.puts("ConfigManager terminated. Reason: #{inspect(reason)}")
      :ok
    end
  end
end

# Demo usage
defmodule ElixirGenServerDemo do
  def run do
    IO.puts("=== Elixir GenServer Demo ===")

    # Basic Counter Demo
    IO.puts("\n--- Basic Counter ---")
    {:ok, counter} = ElixirGenServer.BasicCounter.start_link(10)
    IO.puts("Initial value: #{ElixirGenServer.BasicCounter.get()}")
    ElixirGenServer.BasicCounter.increment()
    ElixirGenServer.BasicCounter.increment()
    IO.puts("After increments: #{ElixirGenServer.BasicCounter.get()}")
    ElixirGenServer.BasicCounter.decrement()
    IO.puts("After decrement: #{ElixirGenServer.BasicCounter.get()}")
    ElixirGenServer.BasicCounter.reset()
    IO.puts("After reset: #{ElixirGenServer.BasicCounter.get()}")

    # Key-Value Store Demo
    IO.puts("\n--- Key-Value Store ---")
    {:ok, kv_store} = ElixirGenServer.KeyValueStore.start_link()
    ElixirGenServer.KeyValueStore.put(kv_store, "name", "Alice")
    ElixirGenServer.KeyValueStore.put(kv_store, "age", 30)
    ElixirGenServer.KeyValueStore.put(kv_store, "city", "New York")

    IO.puts("Get 'name': #{inspect(ElixirGenServer.KeyValueStore.get(kv_store, "name"))}")
    IO.puts("All keys: #{inspect(ElixirGenServer.KeyValueStore.keys(kv_store))}")
    IO.puts("All values: #{inspect(ElixirGenServer.KeyValueStore.values(kv_store))}")
    IO.puts("All data: #{inspect(ElixirGenServer.KeyValueStore.get_all(kv_store))}")

    # Task Worker Pool Demo
    IO.puts("\n--- Task Worker Pool ---")
    {:ok, pool} = ElixirGenServer.TaskWorkerPool.start_link(3)

    # Submit some tasks
    for i <- 1..5 do
      ElixirGenServer.TaskWorkerPool.submit_task(pool, fn ->
        :timer.sleep(1000)
        "Task #{i} completed at #{System.system_time(:millisecond)}"
      end)
    end

    # Get status
    status = ElixirGenServer.TaskWorkerPool.get_status(pool)
    IO.puts("Pool status: #{inspect(status)}")

    # Rate Limiter Demo
    IO.puts("\n--- Rate Limiter ---")
    {:ok, rate_limiter} = ElixirGenServer.RateLimiter.start_link()

    # Set limit
    ElixirGenServer.RateLimiter.set_limit(rate_limiter, "api_calls", 3)

    # Test rate limiting
    for i <- 1..5 do
      result = ElixirGenServer.RateLimiter.check_rate(rate_limiter, "api_calls", 3)
      IO.puts("Request #{i}: #{inspect(result)}")
      :timer.sleep(100)
    end

    # TTL Cache Demo
    IO.puts("\n--- TTL Cache ---")
    {:ok, cache} = ElixirGenServer.TTLCache.start_link()

    ElixirGenServer.TTLCache.put(cache, "user_1", "Alice", 2000)
    ElixirGenServer.TTLCache.put(cache, "user_2", "Bob", 1000)
    ElixirGenServer.TTLCache.put(cache, "user_3", "Charlie", 3000)

    IO.puts("Initial cache size: #{ElixirGenServer.TTLCache.size(cache)}")
    IO.puts("Get user_1: #{inspect(ElixirGenServer.TTLCache.get(cache, "user_1"))}")

    :timer.sleep(1500)  # Wait for user_2 to expire
    IO.puts("After 1.5s, cache size: #{ElixirGenServer.TTLCache.size(cache)}")

    # PubSub Manager Demo
    IO.puts("\n--- PubSub Manager ---")
    {:ok, pubsub} = ElixirGenServer.PubSubManager.start_link()

    ElixirGenServer.PubSubManager.subscribe(pubsub, "news")
    ElixirGenServer.PubSubManager.subscribe(pubsub, "updates")

    IO.puts("Topics: #{inspect(ElixirGenServer.PubSubManager.get_topics(pubsub))}")

    ElixirGenServer.PubSubManager.publish(pubsub, "news", "Breaking news!")
    ElixirGenServer.PubSubManager.publish(pubsub, "updates", "System update available")

    # Configuration Manager Demo
    IO.puts("\n--- Configuration Manager ---")
    {:ok, config} = ElixirGenServer.ConfigManager.start_link()

    ElixirGenServer.ConfigManager.set_defaults(config, %{
      "app_name" => "MyApp",
      "version" => "1.0.0",
      "debug" => false
    })

    IO.puts("Get default app_name: #{inspect(ElixirGenServer.ConfigManager.get_default(config, "app_name"))}")
    IO.puts("Get current app_name: #{inspect(ElixirGenServer.ConfigManager.get(config, "app_name"))}")

    ElixirGenServer.ConfigManager.set(config, "debug", true)
    IO.puts("After setting debug: #{inspect(ElixirGenServer.ConfigManager.get(config, "debug"))}")
    ElixirGenServer.ConfigManager.reset_to_defaults(config)
    IO.puts("After reset to defaults: #{inspect(ElixirGenServer.ConfigManager.get(config, "debug"))}")

    IO.puts("\n=== GenServer Demo Complete ===")
  end
end

# Run the demo
if __ENV__.module == ElixirGenServerDemo do
  ElixirGenServerDemo.run()
end
