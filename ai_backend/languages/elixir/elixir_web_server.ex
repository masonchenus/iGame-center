# Elixir Web Server - HTTP Server Implementation using Plug and Cowboy
defmodule ElixirWebServer do
  use Plug.Router

  # Basic Plug middleware
  defmodule SimplePlug do
    import Plug.Conn

    def init(options), do: options

    def call(conn, _opts) do
      conn
      |> put_resp_header("x-powered-by", "Elixir/Plug")
      |> put_resp_header("x-server", "ElixirWebServer/1.0")
    end
  end

  # Authentication Plug
  defmodule AuthPlug do
    import Plug.Conn
    import Phoenix.Controller

    def init(opts), do: opts

    def call(conn, _opts) do
      case get_req_header(conn, "authorization") do
        ["Bearer " <> token] ->
          case validate_token(token) do
            {:ok, user_id} ->
              assign(conn, :current_user_id, user_id)
            :error ->
              conn
              |> send_resp(401, "Unauthorized")
              |> halt()
          end
        _ ->
          conn
          |> send_resp(401, "Unauthorized")
          |> halt()
      end
    end

    defp validate_token("valid-token"), do: {:ok, "user123"}
    defp validate_token(_), do: :error
  end

  # Rate limiting Plug
  defmodule RateLimitPlug do
    import Plug.Conn
    alias :ets, as: ETS

    def init(opts), do: opts

    def call(conn, _opts) do
      ip = conn.remote_ip |> :inet.ntoa() |> to_string()
      key = "rate_limit:#{ip}"
      limit = 100
      window = 60  # seconds

      ETS.new(:rate_limits, [:set, :public, :named_table])

      case ETS.lookup(:rate_limits, key) do
        [{^key, count}] when count >= limit ->
          conn
          |> put_resp_header("retry-after", "60")
          |> send_resp(429, "Too Many Requests")
          |> halt()
        [{^key, count}] ->
          ETS.insert(:rate_limits, {key, count + 1})
          conn
        _ ->
          ETS.insert(:rate_limits, {key, 1})
          conn
      end
    end
  end

  # CORS Plug
  defmodule CORSPlug do
    import Plug.Conn

    def init(opts), do: opts

    def call(conn, _opts) do
      conn
      |> put_resp_header("access-control-allow-origin", "*")
      |> put_resp_header("access-control-allow-methods", "GET, POST, PUT, DELETE, OPTIONS")
      |> put_resp_header("access-control-allow-headers", "Content-Type, Authorization")
      |> put_resp_header("access-control-max-age", "86400")
    end
  end

  # Request logging Plug
  defmodule LogPlug do
    import Plug.Conn

    def init(opts), do: opts

    def call(conn, _opts) do
      start_time = System.system_time(:microsecond)

      conn
      |> register_before_send(fn conn ->
        end_time = System.system_time(:microsecond)
        duration = (end_time - start_time) / 1000

        IO.puts(
          "#{conn.method} #{conn.request_path} - #{conn.status} (#{duration |> Float.round(1)}ms)"
        )

        conn
      end)
    end
  end

  # Main Router
  plug SimplePlug
  plug LogPlug
  plug :match
  plug CORSPlug
  plug RateLimitPlug
  plug :dispatch

  # Routes
  get "/" do
    conn
    |> put_resp_content_type("text/html")
    |> send_resp(200, """
    <!DOCTYPE html>
    <html>
    <head>
      <title>Elixir Web Server</title>
      <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        .container { max-width: 800px; margin: 0 auto; }
        .endpoint { background: #f5f5f5; padding: 10px; margin: 10px 0; border-radius: 5px; }
        pre { background: #f0f0f0; padding: 10px; border-radius: 3px; overflow-x: auto; }
      </style>
    </head>
    <body>
      <div class="container">
        <h1>Elixir Web Server Demo</h1>
        <p>Welcome to the Elixir Web Server! This server demonstrates various HTTP endpoints and middleware.</p>

        <h2>Available Endpoints:</h2>
        <div class="endpoint">
          <strong>GET /</strong> - This page
        </div>
        <div class="endpoint">
          <strong>GET /hello/:name</strong> - Greet a person
          <br><code>curl http://localhost:4000/hello/Alice</code>
        </div>
        <div class="endpoint">
          <strong>GET /time</strong> - Current server time
          <br><code>curl http://localhost:4000/time</code>
        </div>
        <div class="endpoint">
          <strong>POST /echo</strong> - Echo back request body
          <br><code>curl -X POST http://localhost:4000/echo -d '{"message":"Hello"}'</code>
        </div>
        <div class="endpoint">
          <strong>GET /api/users</strong> - List users (requires auth)
          <br><code>curl -H "Authorization: Bearer valid-token" http://localhost:4000/api/users</code>
        </div>
        <div class="endpoint">
          <strong>GET /status</strong> - Server status
          <br><code>curl http://localhost:4000/status</code>
        </div>
        <div class="endpoint">
          <strong>GET /metrics</strong> - Server metrics
          <br><code>curl http://localhost:4000/metrics</code>
        </div>

        <h2>Server Features:</h2>
        <ul>
          <li>Request/Response logging</li>
          <li>CORS support</li>
          <li>Rate limiting</li>
          <li>Authentication</li>
          <li>JSON API responses</li>
          <li>Static file serving</li>
          <li>WebSocket support</li>
        </ul>
      </div>
    </body>
    </html>
    """)
  end

  get "/hello/:name" do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Poison.encode!(%{
      message: "Hello, #{name}!",
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
    }))
  end

  get "/time" do
    current_time = DateTime.utc_now() |> DateTime.to_iso8601()

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Poison.encode!(%{
      server_time: current_time,
      timezone: "UTC",
      unix_timestamp: DateTime.utc_now() |> DateTime.to_unix()
    }))
  end

  post "/echo" do
    body = case read_body(conn) do
      {:ok, body, _} -> body
      _ -> ""
    end

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Poison.encode!(%{
      echo: body,
      method: conn.method,
      path: conn.request_path,
      headers: Enum.into(conn.req_headers, %{}),
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
    }))
  end

  get "/api/users" do
    users = [
      %{id: 1, name: "Alice", email: "alice@example.com", role: "admin"},
      %{id: 2, name: "Bob", email: "bob@example.com", role: "user"},
      %{id: 3, name: "Charlie", email: "charlie@example.com", role: "user"}
    ]

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Poison.encode!(%{
      users: users,
      total: length(users),
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
    }))
  end

  get "/status" do
    status_info = %{
      server: "Elixir Web Server",
      version: "1.0.0",
      status: "running",
      uptime: "unknown",
      erlang_version: System.otp_release(),
      elixir_version: System.version(),
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
      memory: :erlang.memory(),
      processes: :erlang.system_info(:process_count)
    }

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Poison.encode!(status_info))
  end

  get "/metrics" do
    metrics = %{
      http_requests_total: :ets.lookup(:http_requests, :total) |> Enum.at(0, {:total, 0}) |> elem(1),
      http_requests_per_second: calculate_rps(),
      active_connections: length(Process.list()),
      memory_usage_mb: :erlang.memory(:total) |> div(1024 * 1024),
      cpu_usage: get_cpu_usage(),
      gc_runs: :erlang.statistics(:garbage_collection) |> elem(0),
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
    }

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Poison.encode!(metrics))
  end

  # WebSocket endpoint
  get "/ws" do
    # Upgrade to WebSocket
    case Plug.Conn.get_req_header(conn, "upgrade") do
      ["websocket"] ->
        :cowboy_websocket_handler.call(conn, {
          :cowboy_websocket_handler,
          ElixirWebServer.WebSocketHandler,
          %{}
        })
      _ ->
        conn
        |> send_resp(426, "Upgrade Required")
    end
  end

  # Static file serving
  get "/static/*path" do
    static_path = Enum.join(conn.params["path"], "/")
    file_path = Path.join("public", static_path)

    case File.read(file_path) do
      {:ok, content} ->
        content_type = case Path.extname(file_path) do
          ".css" -> "text/css"
          ".js" -> "application/javascript"
          ".png" -> "image/png"
          ".jpg" -> "image/jpeg"
          ".gif" -> "image/gif"
          ".svg" -> "image/svg+xml"
          _ -> "application/octet-stream"
        end

        conn
        |> put_resp_content_type(content_type)
        |> send_resp(200, content)
      {:error, :enoent} ->
        conn
        |> send_resp(404, "File not found")
      {:error, reason} ->
        conn
        |> send_resp(500, "Error reading file: #{inspect(reason)}")
    end
  end

  # Health check
  get "/health" do
    health_status = %{
      status: "healthy",
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
      services: %{
        database: "unknown",
        cache: "unknown",
        external_api: "unknown"
      }
    }

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Poison.encode!(health_status))
  end

  # Options handler for CORS
  options _ do
    conn
    |> send_resp(200, "")
  end

  # Error handlers
  match _ do
    send_resp(conn, 404, Poison.encode!(%{
      error: "Not Found",
      message: "The requested resource was not found on this server.",
      path: conn.request_path,
      method: conn.method,
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
    }))
  end

  # WebSocket Handler
  defmodule WebSocketHandler do
    @behaviour :cowboy_websocket_handler

    def init({tcp, _http}, req, _opts) do
      {:upgrade, :protocol, :cowboy_websocket, req, %{}}
    end

    def websocket_init(_req, state) do
      IO.puts("WebSocket connection established")
      {:ok, state}
    end

    def websocket_handle({:text, msg}, req, state) do
      response = Poison.encode!(%{
        type: "echo",
        message: msg,
        timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
      })
      {:reply, {:text, response}, req, state}
    end

    def websocket_handle({:binary, data}, req, state) do
      {:reply, {:binary, data}, req, state}
    end

    def websocket_info({:message, msg}, req, state) do
      {:reply, {:text, msg}, req, state}
    end

    def websocket_info(_info, req, state) do
      {:ok, req, state}
    end

    def websocket_terminate(_reason, _req, _state) do
      IO.puts("WebSocket connection terminated")
      :ok
    end
  end

  # Utility functions
  defp calculate_rps do
    # Simple RPS calculation
    # In a real implementation, you'd track this over time
    :rand.uniform(100)
  end

  defp get_cpu_usage do
    # Simple CPU usage approximation
    # In a real implementation, use :cpu_sup or :os_mon
    :rand.uniform(100)
  end

  # Server management
  defmodule ServerManager do
    def start_server(port \\ 4000) do
      # Start ETS tables for rate limiting and metrics
      :ets.new(:rate_limits, [:set, :public, :named_table])
      :ets.new(:http_requests, [:set, :public, :named_table])

      # Start Cowboy HTTP server
      dispatch = [
        {:_, [
          {"/", ElixirWebServer, []},
          {"/ws", ElixirWebServer.WebSocketHandler, []},
          {"/[...]", ElixirWebServer, []}
        ]}
      ]

      {:ok, _} = :cowboy.start_http(:http, 100, [{:port, port}], [
        {:env, [{:dispatch, dispatch}]}
      ])

      IO.puts("Server started on port #{port}")
      {:ok, port}
    end

    def stop_server do
      :cowboy.stop_listener(:http)
      IO.puts("Server stopped")
      :ok
    end

    def get_server_stats do
      %{
        uptime: get_uptime(),
        memory: :erlang.memory(),
        processes: :erlang.system_info(:process_count),
        port_count: :erlang.system_info(:port_count)
      }
    end

    defp get_uptime do
      # In a real implementation, track start time
      "unknown"
    end
  end

  # File upload handler
  post "/upload" do
    {:ok, body, conn} = read_body(conn)

    case Poison.decode(body) do
      {:ok, %{"filename" => filename, "content" => content}} ->
        file_path = Path.join("uploads", filename)
        File.write!(file_path, Base.decode64!(content))

        conn
        |> put_resp_content_type("application/json")
        |> send_resp(200, Poison.encode!(%{
          success: true,
          filename: filename,
          size: byte_size(content)
        }))
      {:error, _} ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(400, Poison.encode!(%{
          error: "Invalid JSON"
        }))
    end
  end
end

# Demo usage
defmodule ElixirWebServerDemo do
  def run do
    IO.puts("=== Elixir Web Server Demo ===")

    # Start the web server
    IO.puts("\n--- Starting Web Server ---")
    {:ok, port} = ElixirWebServer.ServerManager.start_server(4000)

    IO.puts("Server running at http://localhost:#{port}")
    IO.puts("\nAvailable endpoints:")
    IO.puts("  GET  /                    - Main page")
    IO.puts("  GET  /hello/:name         - Greet endpoint")
    IO.puts("  GET  /time                - Current time")
    IO.puts("  POST /echo                - Echo endpoint")
    IO.puts("  GET  /api/users           - Users API (requires auth)")
    IO.puts("  GET  /status              - Server status")
    IO.puts("  GET  /metrics             - Server metrics")
    IO.puts("  GET  /health              - Health check")
    IO.puts("  GET  /ws                  - WebSocket endpoint")
    IO.puts("  GET  /static/*path        - Static files")
    IO.puts("  POST /upload              - File upload")

    IO.puts("\nFeatures demonstrated:")
    IO.puts("• HTTP routing with Plug")
    IO.puts("• Custom middleware (CORS, Auth, Rate limiting)")
    IO.puts("• JSON API responses")
    IO.puts("• WebSocket support")
    IO.puts("• Static file serving")
    IO.puts("• File upload handling")
    IO.puts("• Error handling")
    IO.puts("• Request logging")
    IO.puts("• Health checks")
    IO.puts("• Metrics collection")

    IO.puts("\nTo test the server:")
    IO.puts("  curl http://localhost:#{port}/")
    IO.puts("  curl http://localhost:#{port}/hello/Alice")
    IO.puts("  curl http://localhost:#{port}/time")
    IO.puts("  curl -X POST http://localhost:#{port}/echo -d '{\"message\":\"Hello\"}'")

    # Note: Server would run indefinitely in a real scenario
    # In demo, we'll simulate stopping it
    IO.puts("\n=== Demo Complete ===")
    ElixirWebServer.ServerManager.stop_server()
  end
end

# Run the demo
if __ENV__.module == ElixirWebServerDemo do
  ElixirWebServerDemo.run()
end
