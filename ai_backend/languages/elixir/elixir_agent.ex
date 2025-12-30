# Elixir Agent - Process State Management
defmodule ElixirAgent do
  # Simple Counter Agent
  defmodule CounterAgent do
    use Agent

    def start_link(initial_value \\ 0) do
      Agent.start_link(fn -> initial_value end, name: __MODULE__)
    end

    def increment do
      Agent.update(__MODULE__, &(&1 + 1))
    end

    def decrement do
      Agent.update(__MODULE__, &(&1 - 1))
    end

    def get do
      Agent.get(__MODULE__, &(&1))
    end

    def set(value) when is_integer(value) do
      Agent.update(__MODULE__, fn _ -> value end)
    end

    def reset do
      Agent.update(__MODULE__, fn _ -> 0 end)
    end

    def increment_by(amount) when is_integer(amount) do
      Agent.update(__MODULE__, &(&1 + amount))
    end

    def get_and_update(func) when is_function(func) do
      Agent.get_and_update(__MODULE__, func)
    end
  end

  # Configuration Agent
  defmodule ConfigAgent do
    use Agent

    def start_link(initial_config \\ %{}) do
      Agent.start_link(fn -> initial_config end, name: __MODULE__)
    end

    def get(key, default \\ nil) do
      Agent.get(__MODULE__, &Map.get(&1, key, default))
    end

    def set(key, value) do
      Agent.update(__MODULE__, &Map.put(&1, key, value))
    end

    def delete(key) do
      Agent.update(__MODULE__, &Map.delete(&1, key))
    end

    def get_all do
      Agent.get(__MODULE__, &(&1))
    end

    def update(key, updater) when is_function(updater) do
      Agent.update(__MODULE__, &Map.update!(&1, key, updater))
    end

    def merge(updates) when is_map(updates) do
      Agent.update(__MODULE__, &Map.merge(&1, updates))
    end

    def has_key?(key) do
      Agent.get(__MODULE__, &Map.has_key?(&1, key))
    end

    def keys do
      Agent.get(__MODULE__, &Map.keys(&1))
    end

    def values do
      Agent.get(__MODULE__, &Map.values(&1))
    end

    def size do
      Agent.get(__MODULE__, &map_size(&1))
    end

    def clear do
      Agent.update(__MODULE__, fn _ -> %{} end)
    end

    def atomize_keys do
      Agent.update(__MODULE__, fn config ->
        config
        |> Enum.map(fn {k, v} -> {String.to_atom(k), v} end)
        |> Enum.into(%{})
      end)
    end
  end

  # Cache Agent
  defmodule CacheAgent do
    use Agent

    def start_link do
      Agent.start_link(fn -> %{} end, name: __MODULE__)
    end

    def get(key) do
      Agent.get(__MODULE__, &Map.get(&1, key))
    end

    def put(key, value) do
      Agent.update(__MODULE__, &Map.put(&1, key, value))
    end

    def delete(key) do
      Agent.update(__MODULE__, &Map.delete(&1, key))
    end

    def has_key?(key) do
      Agent.get(__MODULE__, &Map.has_key?(&1, key))
    end

    def clear do
      Agent.update(__MODULE__, fn _ -> %{} end)
    end

    def size do
      Agent.get(__MODULE__, &map_size(&1))
    end

    def keys do
      Agent.get(__MODULE__, &Map.keys(&1))
    end

    def values do
      Agent.get(__MODULE__, &Map.values(&1))
    end

    def get_or_put(key, value_fun) when is_function(value_fun) do
      Agent.get_and_update(__MODULE__, fn cache ->
        case Map.get(cache, key) do
          nil ->
            value = value_fun.()
            {value, Map.put(cache, key, value)}
          existing_value ->
            {existing_value, cache}
        end
      end)
    end

    def update(key, updater) when is_function(updater) do
      Agent.update(__MODULE__, &Map.update(&1, key, updater.(), updater))
    end

    def merge_updates(updates) when is_map(updates) do
      Agent.update(__MODULE__, &Map.merge(&1, updates))
    end

    def get_all do
      Agent.get(__MODULE__, &(&1))
    end

    def get_all_matching(pattern) when is_binary(pattern) do
      Agent.get(__MODULE__, fn cache ->
        cache
        |> Enum.filter(fn {k, _} -> String.match?(to_string(k), ~r/#{pattern}/) end)
        |> Enum.into(%{})
      end)
    end

    def count_by(key_fn) when is_function(key_fn) do
      Agent.get(__MODULE__, fn cache ->
        cache
        |> Enum.map(key_fn)
        |> Enum.group_by(&(&1))
        |> Enum.map(fn {k, group} -> {k, length(group)} end)
        |> Enum.into(%{})
      end)
    end
  end

  # Session Manager Agent
  defmodule SessionAgent do
    use Agent

    def start_link do
      Agent.start_link(fn -> %{} end, name: __MODULE__)
    end

    def create_session(session_id, data) when is_binary(session_id) do
      Agent.update(__MODULE__, &Map.put(&1, session_id, %{data: data, created_at: System.system_time(:second), last_accessed: System.system_time(:second)}))
    end

    def get_session(session_id) when is_binary(session_id) do
      Agent.get(__MODULE__, fn sessions ->
        case Map.get(sessions, session_id) do
          nil -> nil
          session_data ->
            # Update last accessed time
            updated_session = Map.put(session_data, :last_accessed, System.system_time(:second))
            Map.put(sessions, session_id, updated_session)
            updated_session
        end
      end)
    end

    def update_session(session_id, updates) when is_binary(session_id) and is_map(updates) do
      Agent.get_and_update(__MODULE__, fn sessions ->
        case Map.get(sessions, session_id) do
          nil -> {nil, sessions}
          session_data ->
            updated_session = Map.merge(session_data, updates) |> Map.put(:last_accessed, System.system_time(:second))
            {updated_session, Map.put(sessions, session_id, updated_session)}
        end
      end)
    end

    def delete_session(session_id) when is_binary(session_id) do
      Agent.update(__MODULE__, &Map.delete(&1, session_id))
    end

    def session_count do
      Agent.get(__MODULE__, &map_size(&1))
    end

    def get_all_sessions do
      Agent.get(__MODULE__, &(&1))
    end

    def get_expired_sessions(max_age_seconds) when is_integer(max_age_seconds) do
      current_time = System.system_time(:second)
      max_age = max_age_seconds

      Agent.get(__MODULE__, fn sessions ->
        sessions
        |> Enum.filter(fn {_session_id, session_data} ->
          current_time - session_data.last_accessed > max_age
        end)
        |> Enum.into(%{})
      end)
    end

    def cleanup_expired_sessions(max_age_seconds) when is_integer(max_age_seconds) do
      expired_sessions = get_expired_sessions(max_age_seconds)
      Enum.each(expired_sessions, fn {session_id, _} ->
        delete_session(session_id)
      end)
      map_size(expired_sessions)
    end

    def cleanup_inactive_sessions(inactive_seconds) when is_integer(inactive_seconds) do
      current_time = System.system_time(:second)
      inactive_threshold = current_time - inactive_seconds

      Agent.update(__MODULE__, fn sessions ->
        sessions
        |> Enum.filter(fn {_session_id, session_data} ->
          session_data.last_accessed > inactive_threshold
        end)
        |> Enum.into(%{})
      end)
    end
  end

  # Event Logger Agent
  defmodule EventLoggerAgent do
    use Agent

    def start_link do
      Agent.start_link(fn -> %{events: [], max_events: 1000} end, name: __MODULE__)
    end

    def log_event(event_type, data) when is_atom(event_type) do
      timestamp = System.system_time(:millisecond)
      event = %{id: :crypto.hash(:sha, "#{timestamp}#{event_type}#{inspect(data)}") |> Base.encode16(), type: event_type, data: data, timestamp: timestamp}

      Agent.update(__MODULE__, fn state ->
        events = [event | state.events]
        truncated_events = if length(events) > state.max_events, do: Enum.slice(events, 0, state.max_events), else: events
        %{state | events: truncated_events}
      end)
    end

    def log_info(message, data \\ nil) do
      log_event(:info, %{message: message, data: data})
    end

    def log_warning(message, data \\ nil) do
      log_event(:warning, %{message: message, data: data})
    end

    def log_error(message, data \\ nil) do
      log_event(:error, %{message: message, data: data})
    end

    def log_debug(message, data \\ nil) do
      log_event(:debug, %{message: message, data: data})
    end

    def get_events(count \\ 10) when is_integer(count) and count > 0 do
      Agent.get(__MODULE__, &Enum.take(&1.events, count))
    end

    def get_events_by_type(event_type) when is_atom(event_type) do
      Agent.get(__MODULE__, fn state ->
        Enum.filter(state.events, &(&1.type == event_type))
      end)
    end

    def get_events_in_time_range(start_time, end_time) when is_integer(start_time) and is_integer(end_time) do
      Agent.get(__MODULE__, fn state ->
        Enum.filter(state.events, fn event ->
          event.timestamp >= start_time and event.timestamp <= end_time
        end)
      end)
    end

    def get_recent_events(minutes_ago) when is_integer(minutes_ago) do
      current_time = System.system_time(:millisecond)
      start_time = current_time - (minutes_ago * 60 * 1000)
      get_events_in_time_range(start_time, current_time)
    end

    def clear_events do
      Agent.update(__MODULE__, &Map.put(&1, :events, []))
    end

    def get_event_stats do
      Agent.get(__MODULE__, fn state ->
        events = state.events
        total_count = length(events)

        type_counts = events
        |> Enum.group_by(&(&1.type))
        |> Enum.map(fn {type, group} -> {type, length(group)} end)
        |> Enum.into(%{})

        %{total: total_count, types: type_counts}
      end)
    end

    def set_max_events(max_events) when is_integer(max_events) and max_events > 0 do
      Agent.update(__MODULE__, &Map.put(&1, :max_events, max_events))
    end
  end

  # Rate Limiter Agent
  defmodule RateLimiterAgent do
    use Agent

    def start_link do
      Agent.start_link(fn -> %{requests: %{}, limits: %{}} end, name: __MODULE__)
    end

    def allow_request(key, limit, window_size) when is_binary(key) and is_integer(limit) and limit > 0 do
      Agent.get_and_update(__MODULE__, fn state ->
        current_time = System.system_time(:millisecond)
        window_start = current_time - window_size

        # Get requests for this key
        requests = Map.get(state.requests, key, [])
        recent_requests = Enum.filter(requests, &(&1 > window_start))

        if length(recent_requests) >= limit do
          {false, state}
        else
          new_requests = recent_requests ++ [current_time]
          new_requests_state = Map.put(state.requests, key, new_requests)
          {true, %{state | requests: new_requests_state}}
        end
      end)
    end

    def set_limit(key, limit) when is_binary(key) and is_integer(limit) and limit > 0 do
      Agent.update(__MODULE__, &Map.put(&1, :limits, Map.put(&2.limits, key, limit)))
    end

    def get_request_count(key, window_size) when is_binary(key) and is_integer(window_size) do
      Agent.get(__MODULE__, fn state ->
        current_time = System.system_time(:millisecond)
        window_start = current_time - window_size

        requests = Map.get(state.requests, key, [])
        Enum.count(requests, &(&1 > window_start))
      end)
    end

    def reset_key(key) when is_binary(key) do
      Agent.update(__MODULE__, &Map.put(&1, :requests, Map.delete(&2.requests, key)))
    end

    def reset_all do
      Agent.update(__MODULE__, fn state -> %{state | requests: %{}} end)
    end

    def get_stats do
      Agent.get(__MODULE__, fn state ->
        total_keys = map_size(state.requests)
        total_requests = Enum.reduce(state.requests, 0, fn {_, requests}, acc -> acc + length(requests) end)

        %{total_keys: total_keys, total_requests: total_requests, limits: state.limits}
      end)
    end
  end

  # Task Tracker Agent
  defmodule TaskTrackerAgent do
    use Agent

    def start_link do
      Agent.start_link(fn -> %{tasks: %{}, completed: %{}, failed: %{}} end, name: __MODULE__)
    end

    def start_task(task_id, task_fun) when is_binary(task_id) and is_function(task_fun) do
      Agent.get_and_update(__MODULE__, fn state ->
        if Map.has_key?(state.tasks, task_id) do
          {:error, state}
        else
          # Start the task asynchronously
          task_pid = Task.Supervisor.async_nolink(ElixirAgent.TaskTrackerSupervisor, task_fun)

          new_state = Map.put(state.tasks, task_id, %{pid: task_pid.pid, started_at: System.system_time(:millisecond), status: :running})
          {{:ok, task_pid.pid}, new_state}
        end
      end)
    end

    def get_task_status(task_id) when is_binary(task_id) do
      Agent.get(__MODULE__, fn state ->
        case Map.get(state.tasks, task_id) do
          nil -> :not_found
          task_info -> task_info.status
        end
      end)
    end

    def complete_task(task_id, result) when is_binary(task_id) do
      Agent.get_and_update(__MODULE__, fn state ->
        case Map.get(state.tasks, task_id) do
          nil -> {:error, state}
          task_info ->
            completed_task = %{task_info | status: :completed, completed_at: System.system_time(:millisecond), result: result}
            new_state = Map.put(state, :tasks, Map.delete(state.tasks, task_id))
            new_state = Map.put(new_state, :completed, Map.put(state.completed, task_id, completed_task))
            {:ok, new_state}
        end
      end)
    end

    def fail_task(task_id, error) when is_binary(task_id) do
      Agent.get_and_update(__MODULE__, fn state ->
        case Map.get(state.tasks, task_id) do
          nil -> {:error, state}
          task_info ->
            failed_task = %{task_info | status: :failed, completed_at: System.system_time(:millisecond), error: error}
            new_state = Map.put(state, :tasks, Map.delete(state.tasks, task_id))
            new_state = Map.put(new_state, :failed, Map.put(state.failed, task_id, failed_task))
            {:ok, new_state}
        end
      end)
    end

    def get_all_tasks do
      Agent.get(__MODULE__, &(&1.tasks))
    end

    def get_completed_tasks do
      Agent.get(__MODULE__, &(&1.completed))
    end

    def get_failed_tasks do
      Agent.get(__MODULE__, &(&1.failed))
    end

    def get_task_count do
      Agent.get(__MODULE__, fn state ->
        %{running: map_size(state.tasks), completed: map_size(state.completed), failed: map_size(state.failed)}
      end)
    end

    def cleanup_completed do
      Agent.update(__MODULE__, fn state -> %{state | completed: %{}, failed: %{}} end)
    end

    def get_task_details(task_id) when is_binary(task_id) do
      Agent.get(__MODULE__, fn state ->
        Map.get(state.tasks, task_id) ||
        Map.get(state.completed, task_id) ||
        Map.get(state.failed, task_id)
      end)
    end
  end

  # User Preferences Agent
  defmodule UserPreferencesAgent do
    use Agent

    def start_link do
      Agent.start_link(fn -> %{} end, name: __MODULE__)
    end

    def set_preference(user_id, preferences) when is_binary(user_id) and is_map(preferences) do
      Agent.update(__MODULE__, &Map.put(&1, user_id, preferences))
    end

    def get_preference(user_id, key, default \\ nil) when is_binary(user_id) do
      Agent.get(__MODULE__, fn preferences ->
        user_prefs = Map.get(preferences, user_id, %{})
        Map.get(user_prefs, key, default)
      end)
    end

    def update_preference(user_id, key, value) when is_binary(user_id) do
      Agent.update(__MODULE__, &Map.update(&1, user_id, %{}, fn prefs -> Map.put(prefs, key, value) end))
    end

    def delete_preference(user_id, key) when is_binary(user_id) do
      Agent.update(__MODULE__, &Map.update(&1, user_id, %{}, fn prefs -> Map.delete(prefs, key) end))
    end

    def get_all_preferences(user_id) when is_binary(user_id) do
      Agent.get(__MODULE__, &Map.get(&1, user_id, %{}))
    end

    def has_preference?(user_id, key) when is_binary(user_id) do
      Agent.get(__MODULE__, fn preferences ->
        user_prefs = Map.get(preferences, user_id, %{})
        Map.has_key?(user_prefs, key)
      end)
    end

    def get_all_users do
      Agent.get(__MODULE__, &Map.keys(&1))
    end

    def get_user_count do
      Agent.get(__MODULE__, &map_size(&1))
    end

    def merge_preferences(user_id, updates) when is_binary(user_id) and is_map(updates) do
      Agent.update(__MODULE__, &Map.update(&1, user_id, updates, fn prefs -> Map.merge(prefs, updates) end))
    end

    def delete_user(user_id) when is_binary(user_id) do
      Agent.update(__MODULE__, &Map.delete(&1, user_id))
    end

    def get_users_with_key(key) when is_atom(key) or is_binary(key) do
      Agent.get(__MODULE__, fn preferences ->
        preferences
        |> Enum.filter(fn {_, prefs} -> Map.has_key?(prefs, key) end)
        |> Enum.map(fn {user_id, _} -> user_id end)
      end)
    end
  end

  # Simple Database Agent
  defmodule SimpleDatabaseAgent do
    use Agent

    def start_link do
      Agent.start_link(fn -> %{} end, name: __MODULE__)
    end

    def insert(table, record) when is_atom(table) and is_map(record) do
      Agent.get_and_update(__MODULE__, fn db ->
        table_data = Map.get(db, table, %{})
        id = :crypto.hash(:sha, "#{System.system_time(:millisecond)}#{inspect(record)}") |> Base.encode16()
        new_record = Map.put(record, :id, id) |> Map.put(:inserted_at, System.system_time(:millisecond))
        new_table_data = Map.put(table_data, id, new_record)
        {id, Map.put(db, table, new_table_data)}
      end)
    end

    def find_by_id(table, id) when is_atom(table) and is_binary(id) do
      Agent.get(__MODULE__, fn db ->
        table_data = Map.get(db, table, %{})
        Map.get(table_data, id)
      end)
    end

    def find_by(table, field, value) when is_atom(table) do
      Agent.get(__MODULE__, fn db ->
        table_data = Map.get(db, table, %{})
        table_data
        |> Enum.filter(fn {_, record} -> Map.get(record, field) == value end)
        |> Enum.map(fn {_, record} -> record end)
      end)
    end

    def find_all(table) when is_atom(table) do
      Agent.get(__MODULE__, fn db ->
        table_data = Map.get(db, table, %{})
        Map.values(table_data)
      end)
    end

    def update(table, id, updates) when is_atom(table) and is_binary(id) and is_map(updates) do
      Agent.get_and_update(__MODULE__, fn db ->
        table_data = Map.get(db, table, %{})
        case Map.get(table_data, id) do
          nil -> {nil, db}
          record ->
            updated_record = Map.merge(record, updates) |> Map.put(:updated_at, System.system_time(:millisecond))
            new_table_data = Map.put(table_data, id, updated_record)
            {updated_record, Map.put(db, table, new_table_data)}
        end
      end)
    end

    def delete(table, id) when is_atom(table) and is_binary(id) do
      Agent.get_and_update(__MODULE__, fn db ->
        table_data = Map.get(db, table, %{})
        case Map.get(table_data, id) do
          nil -> {nil, db}
          record ->
            new_table_data = Map.delete(table_data, id)
            {record, Map.put(db, table, new_table_data)}
        end
      end)
    end

    def count(table) when is_atom(table) do
      Agent.get(__MODULE__, fn db ->
        table_data = Map.get(db, table, %{})
        map_size(table_data)
      end)
    end

    def get_all_tables do
      Agent.get(__MODULE__, &Map.keys(&1))
    end

    def clear_table(table) when is_atom(table) do
      Agent.update(__MODULE__, &Map.put(&1, table, %{}))
    end

    def clear_all do
      Agent.update(__MODULE__, fn _ -> %{} end)
    end

    def get_database_stats do
      Agent.get(__MODULE__, fn db ->
        tables = Map.keys(db)
        stats = Enum.map(tables, fn table ->
          table_data = Map.get(db, table, %{})
          {table, map_size(table_data)}
        end)
        |> Enum.into(%{})

        %{tables: map_size(db), records: Enum.reduce(stats, 0, fn {_, count}, acc -> acc + count end), by_table: stats}
      end)
    end
  end

  # Task Supervisor for TaskTrackerAgent
  defmodule TaskTrackerSupervisor do
    use Task.Supervisor
  end
end

# Demo usage
defmodule ElixirAgentDemo do
  def run do
    IO.puts("=== Elixir Agent Demo ===")

    # Counter Agent Demo
    IO.puts("\n--- Counter Agent ---")
    {:ok, _counter} = ElixirAgent.CounterAgent.start_link(10)
    IO.puts("Initial value: #{ElixirAgent.CounterAgent.get()}")
    ElixirAgent.CounterAgent.increment()
    IO.puts("After increment: #{ElixirAgent.CounterAgent.get()}")
    ElixirAgent.CounterAgent.increment_by(5)
    IO.puts("After increment_by(5): #{ElixirAgent.CounterAgent.get()}")
    ElixirAgent.CounterAgent.decrement()
    IO.puts("After decrement: #{ElixirAgent.CounterAgent.get()}")
    ElixirAgent.CounterAgent.reset()
    IO.puts("After reset: #{ElixirAgent.CounterAgent.get()}")

    # Configuration Agent Demo
    IO.puts("\n--- Configuration Agent ---")
    {:ok, _config} = ElixirAgent.ConfigAgent.start_link(%{environment: "development"})
    ElixirAgent.ConfigAgent.set("app_name", "MyApp")
    ElixirAgent.ConfigAgent.set("version", "1.0.0")
    ElixirAgent.ConfigAgent.set("debug", true)

    IO.puts("App name: #{inspect(ElixirAgent.ConfigAgent.get("app_name"))}")
    IO.puts("Has version key: #{ElixirAgent.ConfigAgent.has_key?("version")}")
    IO.puts("All config: #{inspect(ElixirAgent.ConfigAgent.get_all())}")

    ElixirAgent.ConfigAgent.update("version", &"v" <> &1)
    IO.puts("Updated version: #{inspect(ElixirAgent.ConfigAgent.get("version"))}")

    # Cache Agent Demo
    IO.puts("\n--- Cache Agent ---")
    {:ok, _cache} = ElixirAgent.CacheAgent.start_link()
    ElixirAgent.CacheAgent.put("user_1", %{name: "Alice", age: 30})
    ElixirAgent.CacheAgent.put("user_2", %{name: "Bob", age: 25})
    ElixirAgent.CacheAgent.put("session_abc", "active")

    IO.puts("Get user_1: #{inspect(ElixirAgent.CacheAgent.get("user_1"))}")
    IO.puts("Cache size: #{ElixirAgent.CacheAgent.size()}")
    IO.puts("Has user_2: #{ElixirAgent.CacheAgent.has_key?("user_2")}")

    value = ElixirAgent.CacheAgent.get_or_put("user_3", fn -> %{name: "Charlie", age: 35} end)
    IO.puts("Get or put user_3: #{inspect(value)}")

    ElixirAgent.CacheAgent.delete("session_abc")
    IO.puts("After deleting session_abc, size: #{ElixirAgent.CacheAgent.size()}")

    # Session Manager Agent Demo
    IO.puts("\n--- Session Manager Agent ---")
    {:ok, _sessions} = ElixirAgent.SessionAgent.start_link()
    ElixirAgent.SessionAgent.create_session("session_123", %{user_id: "user_1", permissions: ["read", "write"]})
    ElixirAgent.SessionAgent.create_session("session_456", %{user_id: "user_2", permissions: ["read"]})

    session = ElixirAgent.SessionAgent.get_session("session_123")
    IO.puts("Session data: #{inspect(session)}")

    ElixirAgent.SessionAgent.update_session("session_123", %{last_activity: "viewing_dashboard"})
    updated_session = ElixirAgent.SessionAgent.get_session("session_123")
    IO.puts("Updated session: #{inspect(updated_session)}")

    IO.puts("Total sessions: #{ElixirAgent.SessionAgent.session_count()}")

    # Clean up expired sessions (none in this demo)
    cleaned = ElixirAgent.SessionAgent.cleanup_expired_sessions(3600)
    IO.puts("Cleaned expired sessions: #{cleaned}")

    # Event Logger Agent Demo
    IO.puts("\n--- Event Logger Agent ---")
    {:ok, _logger} = ElixirAgent.EventLoggerAgent.start_link()

    ElixirAgent.EventLoggerAgent.log_info("Application started", %{version: "1.0.0"})
    ElixirAgent.EventLoggerAgent.log_warning("High memory usage", %{usage: "85%"})
    ElixirAgent.EventLoggerAgent.log_error("Database connection failed", %{host: "db.example.com"})
    ElixirAgent.EventLoggerAgent.log_debug("Processing request", %{request_id: "req_123"})

    recent_events = ElixirAgent.EventLoggerAgent.get_events(3)
    IO.puts("Recent events: #{inspect(recent_events)}")

    error_events = ElixirAgent.EventLoggerAgent.get_events_by_type(:error)
    IO.puts("Error events: #{length(error_events)}")

    stats = ElixirAgent.EventLoggerAgent.get_event_stats()
    IO.puts("Event stats: #{inspect(stats)}")

    # Rate Limiter Agent Demo
    IO.puts("\n--- Rate Limiter Agent ---")
    {:ok, _rate_limiter} = ElixirAgent.RateLimiterAgent.start_link()
    ElixirAgent.RateLimiterAgent.set_limit("api_requests", 3)

    # Test rate limiting
    for i <- 1..5 do
      allowed = ElixirAgent.RateLimiterAgent.allow_request("api_requests", 3, 60_000)
      IO.puts("Request #{i}: #{if allowed, do: "Allowed", else: "Blocked"}")
    end

    IO.puts("Request count: #{ElixirAgent.RateLimiterAgent.get_request_count("api_requests", 60_000)}")

    stats = ElixirAgent.RateLimiterAgent.get_stats()
    IO.puts("Rate limiter stats: #{inspect(stats)}")

    # Task Tracker Agent Demo
    IO.puts("\n--- Task Tracker Agent ---")
    {:ok, _task_tracker} = ElixirAgent.TaskTrackerAgent.start_link()

    # Start some tasks
    {:ok, task_pid1} = ElixirAgent.TaskTrackerAgent.start_task("task_1", fn ->
      :timer.sleep(1000)
      "Task 1 completed"
    end)

    {:ok, task_pid2} = ElixirAgent.TaskTrackerAgent.start_task("task_2", fn ->
      :timer.sleep(500)
      "Task 2 completed"
    end)

    IO.puts("Started tasks: #{inspect(task_pid1)}, #{inspect(task_pid2)}")

    # Check task status (should be running)
    status1 = ElixirAgent.TaskTrackerAgent.get_task_status("task_1")
    IO.puts("Task 1 status: #{inspect(status1)}")

    # Wait a bit and check counts
    :timer.sleep(600)
    counts = ElixirAgent.TaskTrackerAgent.get_task_count()
    IO.puts("Task counts: #{inspect(counts)}")

    # User Preferences Agent Demo
    IO.puts("\n--- User Preferences Agent ---")
    {:ok, _preferences} = ElixirAgent.UserPreferencesAgent.start_link()

    ElixirAgent.UserPreferencesAgent.set_preference("user_1", %{
      theme: "dark",
      language: "en",
      notifications: true
    })

    ElixirAgent.UserPreferencesAgent.set_preference("user_2", %{
      theme: "light",
      language: "es",
      notifications: false
    })

    theme = ElixirAgent.UserPreferencesAgent.get_preference("user_1", "theme")
    IO.puts("User 1 theme: #{inspect(theme)}")

    ElixirAgent.UserPreferencesAgent.update_preference("user_1", "theme", "auto")
    updated_theme = ElixirAgent.UserPreferencesAgent.get_preference("user_1", "theme")
    IO.puts("Updated theme: #{inspect(updated_theme)}")

    all_prefs = ElixirAgent.UserPreferencesAgent.get_all_preferences("user_1")
    IO.puts("User 1 all preferences: #{inspect(all_prefs)}")

    users_with_dark_theme = ElixirAgent.UserPreferencesAgent.get_users_with_key(:theme)
    IO.puts("Users with theme set: #{inspect(users_with_dark_theme)}")

    IO.puts("Total users: #{ElixirAgent.UserPreferencesAgent.get_user_count()}")

    # Simple Database Agent Demo
    IO.puts("\n--- Simple Database Agent ---")
    {:ok, _database} = ElixirAgent.SimpleDatabaseAgent.start_link()

    # Insert some users
    user1_id = ElixirAgent.SimpleDatabaseAgent.insert(:users, %{name: "Alice", email: "alice@example.com", role: "admin"})
    user2_id = ElixirAgent.SimpleDatabaseAgent.insert(:users, %{name: "Bob", email: "bob@example.com", role: "user"})

    # Insert some posts
    post1_id = ElixirAgent.SimpleDatabaseAgent.insert(:posts, %{title: "First Post", content: "Hello World", user_id: user1_id})
    post2_id = ElixirAgent.SimpleDatabaseAgent.insert(:posts, %{title: "Second Post", content: "Elixir is great", user_id: user2_id})

    IO.puts("Inserted users: #{user1_id}, #{user2_id}")
    IO.puts("Inserted posts: #{post1_id}, #{post2_id}")

    # Find by ID
    user = ElixirAgent.SimpleDatabaseAgent.find_by_id(:users, user1_id)
    IO.puts("Found user: #{inspect(user)}")

    # Find by field
    admin_users = ElixirAgent.SimpleDatabaseAgent.find_by(:users, :role, "admin")
    IO.puts("Admin users: #{inspect(admin_users)}")

    # Find all
    all_users = ElixirAgent.SimpleDatabaseAgent.find_all(:users)
    IO.puts("All users: #{length(all_users)}")

    # Update
    ElixirAgent.SimpleDatabaseAgent.update(:users, user1_id, %{last_login: System.system_time(:millisecond)})
    updated_user = ElixirAgent.SimpleDatabaseAgent.find_by_id(:users, user1_id)
    IO.puts("Updated user: #{inspect(updated_user)}")

    # Database stats
    stats = ElixirAgent.SimpleDatabaseAgent.get_database_stats()
    IO.puts("Database stats: #{inspect(stats)}")

    IO.puts("\n=== Agent Demo Complete ===")
  end
end

# Run the demo
if __ENV__.module == ElixirAgentDemo do
  ElixirAgentDemo.run()
end
