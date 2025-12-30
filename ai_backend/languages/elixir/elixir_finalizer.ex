
# Elixir Finalizer - Complete Elixir Application with All Patterns
defmodule ElixirFinalizer do
  # Application Structure
  defmodule MyApp do
    use Application

    def start(_type, _args) do
      children = [
        # Supervisors
        MyApp.Supervisor,

        # GenServers
        MyApp.Repo,
        MyApp.Cache,
        MyApp.SessionManager,
        MyApp.RateLimiter,

        # Task Supervisors
        {Task.Supervisor, name: MyApp.TaskSupervisor},

        # Workers
        MyApp.BackgroundWorker,

        # PubSub
        {Phoenix.PubSub, name: MyApp.PubSub}
      ]

      Supervisor.start_link(children, strategy: :one_for_one, name: MyApp.Supervisor)
    end
  end

  # Database Repository
  defmodule MyApp.Repo do
    use GenServer

    def start_link do
      GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
    end

    def init(:ok) do
      # Initialize database connection
      {:ok, %{connection: nil, pool: []}}
    end

    def get(table, id) do
      GenServer.call(__MODULE__, {:get, table, id})
    end

    def insert(table, data) do
      GenServer.call(__MODULE__, {:insert, table, data})
    end

    def update(table, id, data) do
      GenServer.call(__MODULE__, {:update, table, id, data})
    end

    def delete(table, id) do
      GenServer.call(__MODULE__, {:delete, table, id})
    end

    def query(query_string, params \\ []) do
      GenServer.call(__MODULE__, {:query, query_string, params})
    end

    def handle_call({:get, table, id}, _from, state) do
      # Simulate database get
      result = %{id: id, table: table, data: %{}, inserted_at: System.system_time(:millisecond)}
      {:reply, {:ok, result}, state}
    end

    def handle_call({:insert, table, data}, _from, state) do
      id = :crypto.hash(:sha256, "#{System.system_time(:millisecond)}#{inspect(data)}") |> Base.encode16()
      record = Map.put(data, :id, id) |> Map.put(:inserted_at, System.system_time(:millisecond))
      {:reply, {:ok, id, record}, state}
    end

    def handle_call({:update, table, id, data}, _from, state) do
      updated = Map.put(data, :updated_at, System.system_time(:millisecond))
      {:reply, {:ok, %{id: id, table: table, data: updated}}, state}
    end

    def handle_call({:delete, table, id}, _from, state) do
      {:reply, {:ok, %{deleted: true, table: table, id: id}}, state}
    end

    def handle_call({:query, query, _params}, _from, state) do
      # Simulate query execution
      {:reply, {:ok, %{rows: [], count: 0}}, state}
    end
  end

  # Cache Server
  defmodule MyApp.Cache do
    use Agent

    def start_link do
      Agent.start_link(fn -> %{} end, name: __MODULE__)
    end

    def get(key) do
      Agent.get(__MODULE__, &Map.get(&1, key))
    end

    def set(key, value, ttl \\ 3600) do
      expiry = System.system_time(:millisecond) + ttl * 1000
      Agent.update(__MODULE__, &Map.put(&1, key, %{value: value, expires_at: expiry}))
    end

    def delete(key) do
      Agent.update(__MODULE__, &Map.delete(&1, key))
    end

    def clear do
      Agent.update(__MODULE__, fn _ -> %{} end)
    end

    def get_or_set(key, fun) when is_function(fun) do
      case get(key) do
        nil ->
          value = fun.()
          set(key, value)
          value
        cached -> cached
      end
    end
  end

  # Session Manager
  defmodule MyApp.SessionManager do
    use Agent

    def start_link do
      Agent.start_link(fn -> %{} end, name: __MODULE__)
    end

    def create_session(user_id, data \\ %{}) do
      session_id = generate_session_id()
      session = %{
        user_id: user_id,
        data: data,
        created_at: System.system_time(:millisecond),
        last_accessed: System.system_time(:millisecond)
      }
      Agent.update(__MODULE__, &Map.put(&1, session_id, session))
      {:ok, session_id}
    end

    def get_session(session_id) do
      Agent.get(__MODULE__, &Map.get(&1, session_id))
    end

    def update_session(session_id, data) do
      Agent.update(__MODULE__, fn sessions ->
        case Map.get(sessions, session_id) do
          nil -> sessions
          session ->
            updated = Map.merge(session, data) |> Map.put(:last_accessed, System.system_time(:millisecond))
            Map.put(sessions, session_id, updated)
        end
      end)
    end

    def delete_session(session_id) do
      Agent.update(__MODULE__, &Map.delete(&1, session_id))
    end

    def cleanup_expired(ttl \\ 3600000) do
      now = System.system_time(:millisecond)
      Agent.update(__MODULE__, fn sessions ->
        Enum.filter(sessions, fn {_, session} ->
          now - session.last_accessed < ttl
        end)
        |> Enum.into(%{})
      end)
    end

    defp generate_session_id do
      :crypto.strong_rand_bytes(32) |> Base.encode16()
    end
  end

  # Rate Limiter
  defmodule MyApp.RateLimiter do
    use Agent

    def start_link do
      Agent.start_link(fn -> %{} end, name: __MODULE__)
    end

    def allow_request(key, limit, window) do
      now = System.system_time(:millisecond)
      window_start = now - window

      Agent.get_and_update(__MODULE__, fn state ->
        requests = Map.get(state, key, [])
        recent_requests = Enum.filter(requests, &(&1 > window_start))

        if length(recent_requests) >= limit do
          {: exceeded, state}
        else
          new_requests = recent_requests ++ [now]
          {:allowed, Map.put(state, key, new_requests)}
        end
      end)
    end

    def get_remaining(key, limit, window) do
      now = System.system_time(:millisecond)
      window_start = now - window

      Agent.get(__MODULE__, fn state ->
        requests = Map.get(state, key, [])
        recent_requests = Enum.count(requests, &(&1 > window_start))
        max(0, limit - recent_requests)
      end)
    end

    def reset_key(key) do
      Agent.update(__MODULE__, &Map.delete(&1, key))
    end
  end

  # Background Worker
  defmodule MyApp.BackgroundWorker do
    use GenServer

    def start_link do
      GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
    end

    def init(:ok) do
      schedule_work()
      {:ok, %{jobs: [], running: false}}
    end

    def handle_info(:work, state) do
      process_jobs()
      schedule_work()
      {:noreply, state}
    end

    def schedule_work do
      Process.send_after(self(), :work, 60000)  # Every minute
    end

    defp process_jobs do
      # Process background jobs
      IO.puts("Processing background jobs...")
    end

    def enqueue(job) do
      GenServer.cast(__MODULE__, {:enqueue, job})
    end

    def handle_cast({:enqueue, job}, state) do
      {:noreply, %{state | jobs: [job | state.jobs]}}
    end
  end

  # HTTP Server using Plug
  defmodule MyApp.Router do
    use Plug.Router

    plug :match
    plug :dispatch

    get "/" do
      send_resp(conn, 200, Jason.encode!(%{message: "Welcome to MyApp API", version: "1.0.0"}))
    end

    get "/health" do
      send_resp(conn, 200, Jason.encode!(%{status: "healthy", timestamp: System.system_time(:millisecond)}))
    end

    get "/api/users" do
      users = [%{id: "1", name: "Alice"}, %{id: "2", name: "Bob"}]
      send_resp(conn, 200, Jason.encode!(%{users: users}))
    end

    post "/api/users" do
      {:ok, body, conn} = read_body(conn)
      data = Jason.decode!(body)

      case MyApp.Repo.insert(:users, data) do
        {:ok, id, record} -> send_resp(conn, 201, Jason.encode!(%{id: id, user: record}))
        {:error, reason} -> send_resp(conn, 400, Jason.encode!(%{error: reason}))
      end
    end

    get "/api/cache/:key" do
      case MyApp.Cache.get(conn.path_params["key"]) do
        nil -> send_resp(conn, 404, Jason.encode!(%{error: "Not found"}))
        value -> send_resp(conn, 200, Jason.encode!(%{value: value}))
      end
    end

    post "/api/cache" do
      {:ok, body, conn} = read_body(conn)
      data = Jason.decode!(body)
      key = data["key"]
      value = data["value"]
      ttl = data["ttl"] || 3600

      MyApp.Cache.set(key, value, ttl)
      send_resp(conn, 201, Jason.encode!(%{set: true}))
    end

    post "/api/sessions" do
      {:ok, body, conn} = read_body(conn)
      data = Jason.encode!(%{user_id: data["user_id"]})

      case MyApp.SessionManager.create_session(data["user_id"], data["data"] || %{}) do
        {:ok, session_id} -> send_resp(conn, 201, Jason.encode!(%{session_id: session_id}))
        {:error, reason} -> send_resp(conn, 400, Jason.encode!(%{error: reason}))
      end
    end

    match _ do
      send_resp(conn, 404, Jason.encode!(%{error: "Not found"}))
    end
  end

  # WebSocket Handler
  defmodule MyApp.WebSocketHandler do
    use Phoenix.Channel.Server

    def join("room:lobby", _params, socket) do
      {:ok, socket}
    end

    def join("room:" <> room_id, _params, socket) do
      {:ok, socket}
    end

    def handle_in("message", %{"content" => content}, socket) do
      broadcast!(socket, "new_message", %{
        content: content,
        user_id: socket.assigns.user_id,
        timestamp: System.system_time(:millisecond)
      })
      {:reply, {:ok, %{sent: true}}, socket}
    end
  end

  # Authentication Module
  defmodule MyApp.Auth do
    defmodule TokenManager do
      def generate_token(user_id) do
        :crypto.strong_rand_bytes(32) |> Base.encode16()
      end

      def validate_token(token) do
        if byte_size(token) > 20 do
          {:ok, %{user_id: "user_123", role: "user"}}
        else
          {:error, :invalid_token}
        end
      end
    end

    defmodule PasswordHasher do
      def hash(password) when is_binary(password) do
        :crypto.hash(:sha256, password) |> Base.encode16()
      end

      def verify(password, hash) do
        hash(password) == hash
      end
    end

    def login(email, password) do
      # Simulate login
      {:ok, %{token: TokenManager.generate_token(email), user_id: email}}
    end

    def logout(token) do
      :ok
    end
  end

  # Configuration Module
  defmodule MyApp.Config do
    def get(key, default \\ nil) do
      config = %{
        app_name: "MyApp",
        version: "1.0.0",
        environment: :development,
        database_url: "postgresql://localhost/myapp",
        cache_ttl: 3600,
        max_connections: 100,
        rate_limit: %{window: 60000, max_requests: 100},
        features: %{
          websocket: true,
          api: true,
          admin_panel: false
        }
      }

      case String.split(key, ".") do
        [single_key] -> Map.get(config, String.to_atom(single_key), default)
        [first | rest] ->
          case Map.get(config, String.to_atom(first)) do
            nil -> default
            nested -> Enum.reduce(rest, nested, fn k, acc -> Map.get(acc, String.to_atom(k)) end)
          end
      end
    end
  end

  # Logger Module
  defmodule MyApp.Logger do
    def info(message, metadata \\ %{}) do
      log(:info, message, metadata)
    end

    def warn(message, metadata \\ %{}) do
      log(:warn, message, metadata)
    end

    def error(message, metadata \\ %{}) do
      log(:error, message, metadata)
    end

    def debug(message, metadata \\ %{}) do
      log(:debug, message, metadata)
    end

    defp log(level, message, metadata) do
      timestamp = DateTime.utc_now() |> DateTime.to_iso8601()
      IO.puts("[#{timestamp}] #{level}: #{message} #{inspect(metadata)}")
    end
  end

  # Utils Module
  defmodule MyApp.Utils do
    def random_string(length) when is_integer(length) and length > 0 do
      :crypto.strong_rand_bytes(length) |> Base.encode16() |> String.slice(0, length)
    end

    def slugify(text) do
      text
      |> String.downcase()
      |> String.replace(~r/[^a-z0-9]+/, "-")
      |> String.replace(~r/^-+|-+$/, "")
    end

    def truncate(text, length) when is_binary(text) do
      if String.length(text) > length do
        String.slice(text, 0, length) <> "..."
      else
        text
      end
    end

    def delay(ms, fun) when is_integer(ms) and is_function(fun) do
      :timer.apply_after(ms, fun, [])
    end

    def retry(fun, attempts \\ 3, delay_time \\ 1000) when is_function(fun) do
      Enum.reduce(1..attempts, fn attempt, _ ->
        try do
          {:ok, fun.()}
        rescue
          error ->
            if attempt < attempts do
              :timer.sleep(delay_time)
              {:retry, attempt}
            else
              {:error, error}
            end
        end
      end)
    end
  end

  # Metrics Module
  defmodule MyApp.Metrics do
    use Agent

    def start_link do
      Agent.start_link(fn -> %{requests: 0, errors: 0, latency_sum: 0} end, name: __MODULE__)
    end

    def record_request(latency \\ 0) do
      Agent.update(__MODULE__, fn state ->
        %{
          requests: state.requests + 1,
          errors: state.errors,
          latency_sum: state.latency_sum + latency
        }
      end)
    end

    def record_error do
      Agent.update(__MODULE__, fn state ->
        %{state | errors: state.errors + 1}
      end)
    end

    def get_stats do
      Agent.get(__MODULE__, fn state ->
        total = state.requests
        avg_latency = if total > 0, do: state.latency_sum / total, else: 0
        error_rate = if total > 0, do: state.errors / total * 100, else: 0

        %{
          total_requests: total,
          total_errors: state.errors,
          average_latency_ms: avg_latency,
          error_rate_percent: error_rate
        }
      end)
    end
  end

  # Supervision Tree
  defmodule MyApp.Supervisor do
    use Supervisor

    def start_link(init_arg) do
      Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
    end

    def init(_init_arg) do
      children = [
        {MyApp.Repo, []},
        {MyApp.Cache, []},
        {MyApp.SessionManager, []},
        {MyApp.RateLimiter, []},
        {MyApp.BackgroundWorker, []},
        {MyApp.Metrics, []},
        {Task.Supervisor, name: MyApp.TaskSupervisor}
      ]

      Supervisor.init(children, strategy: :one_for_one)
    end
  end
end

# Demo application
defmodule MyAppDemo do
  def run do
    IO.puts("=" * 60)
    IO.puts("  Elixir Complete Application Demo")
    IO.puts("=" * 60)

    # Start the application
    IO.puts("\n--- Starting Application ---")
    {:ok, _supervisor} = ElixirFinalizer.MyApp.Supervisor.start_link([])
    IO.puts("Application supervisor started")

    # Demonstrate database operations
    IO.puts("\n--- Database Operations ---")
    {:ok, user_id, user} = ElixirFinalizer.MyApp.Repo.insert(:users, %{name: "Alice", email: "alice@example.com"})
    IO.puts("Inserted user: #{user_id}")

    {:ok, fetched_user} = ElixirFinalizer.MyApp.Repo.get(:users, user_id)
    IO.puts("Fetched user: #{inspect(fetched_user)}")

    {:ok, updated} = ElixirFinalizer.MyApp.Repo.update(:users, user_id, %{age: 30})
    IO.puts("Updated user")

    # Demonstrate caching
    IO.puts("\n--- Cache Operations ---")
    ElixirFinalizer.MyApp.Cache.set("user_#{user_id}", user, 300)
    cached = ElixirFinalizer.MyApp.Cache.get("user_#{user_id}")
    IO.puts("Cached user: #{inspect(cached)}")

    # Demonstrate sessions
    IO.puts("\n--- Session Management ---")
    {:ok, session_id} = ElixirFinalizer.MyApp.SessionManager.create_session(user_id, %{theme: "dark"})
    IO.puts("Created session: #{session_id}")

    session = ElixirFinalizer.MyApp.SessionManager.get_session(session_id)
    IO.puts("Session user_id: #{session.user_id}")

    # Demonstrate rate limiting
    IO.puts("\n--- Rate Limiting ---")
    for i <- 1..5 do
      case ElixirFinalizer.MyApp.RateLimiter.allow_request("api", 10, 60000) do
        :allowed -> IO.puts("Request #{i}: Allowed")
        :exceeded -> IO.puts("Request #{i}: Exceeded limit")
      end
    end

    # Demonstrate metrics
    IO.puts("\n--- Metrics ---")
    ElixirFinalizer.MyApp.Metrics.record_request(150)
    ElixirFinalizer.MyApp.Metrics.record_request(200)
    ElixirFinalizer.MyApp.Metrics.record_error()

    stats = ElixirFinalizer.MyApp.Metrics.get_stats()
    IO.puts("Metrics: #{inspect(stats)}")

    # Demonstrate utilities
    IO.puts("\n--- Utilities ---")
    random = ElixirFinalizer.MyApp.Utils.random_string(16)
    IO.puts("Random string: #{random}")

    slug = ElixirFinalizer.MyApp.Utils.slugify("Hello World! How Are You?")
    IO.puts("Slug: #{slug}")

    truncated = ElixirFinalizer.MyApp.Utils.truncate("This is a long text that should be truncated", 20)
    IO.puts("Truncated: #{truncated}")

    # Demonstrate configuration
    IO.puts("\n--- Configuration ---")
    app_name = ElixirFinalizer.MyApp.Config.get("app_name")
    db_url = ElixirFinalizer.MyApp.Config.get("database_url")
    features_websocket = ElixirFinalizer.MyApp.Config.get("features.websocket")

    IO.puts("App name: #{app_name}")
    IO.puts("Database URL: #{db_url}")
    IO.puts("WebSocket enabled: #{features_websocket}")

    # Demonstrate logging
    IO.puts("\n--- Logging ---")
    ElixirFinalizer.MyApp.Logger.info("Application started successfully")
    ElixirFinalizer.MyApp.Logger.warn("This is a warning")
    ElixirFinalizer.MyApp.Logger.error("This is an error")
    ElixirFinalizer.MyApp.Logger.debug("Debug information")

    # Demonstrate authentication
    IO.puts("\n--- Authentication ---")
    {:ok, login_result} = ElixirFinalizer.MyApp.Auth.login("alice@example.com", "password123")
    IO.puts("Login successful: #{inspect(login_result)}")

    # Show application structure
    IO.puts("\n--- Application Structure ---")
    IO.puts("""
    MyApp/
    ├── Supervisor (root supervisor)
    ├── Repo (database repository)
    ├── Cache (in-memory cache)
    ├── SessionManager (session management)
    ├── RateLimiter (rate limiting)
    ├── BackgroundWorker (background jobs)
    ├── Metrics (application metrics)
    ├── TaskSupervisor (task supervision)
    ├── Router (HTTP routing)
    ├── WebSocketHandler (real-time communication)
    ├── Auth (authentication module)
    ├── Config (configuration management)
    ├── Logger (application logging)
    └── Utils (utility functions)
    """)

    IO.puts("\n" * 2)
    IO.puts("=" * 60)
    IO.puts("  Complete Elixir Application Demo Finished!")
    IO.puts("=" * 60)
    IO.puts("\nFeatures demonstrated:")
    IO.puts("• GenServer-based services (Repo, Cache, SessionManager, RateLimiter)")
    IO.puts("• Supervision tree for fault tolerance")
    IO.puts("• Background worker for async processing")
    IO.puts("• HTTP routing with Plug")
    IO.puts("• WebSocket channel support")
    IO.puts("• Authentication and session management")
    IO.puts("• Rate limiting and metrics collection")
    IO.puts("• Configuration and logging utilities")
    IO.puts("• Complete OTP application structure")
  end
end

# Run the demo
if __ENV__.module == MyAppDemo do
  MyAppDemo.run()
end
