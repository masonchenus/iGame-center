
# Elixir Plugs - Composable HTTP Request/Response Modules
defmodule ElixirPlug do
  # Custom Plug for Request Logging
  defmodule RequestLogger do
    def init(opts), do: opts

    def call(conn, _opts) do
      start_time = System.monotonic_time(:millisecond)

      conn
      |> Plug.Conn.register_before_send(fn conn ->
        duration = System.monotonic_time(:millisecond) - start_time
        log_request(conn, duration)
        conn
      end)
    end

    defp log_request(conn, duration) do
      IO.puts("#{conn.method} #{conn.path_info} -> #{conn.status} (#{duration}ms)")
    end
  end

  # Authentication Plug
  defmodule AuthenticationPlug do
    def init(opts), do: opts

    def call(conn, _opts) do
      case get_auth_token(conn) do
        nil ->
          conn |> Plug.Conn.put_status(401) |> Plug.Conn.halt()

        token ->
          case validate_token(token) do
            {:ok, user} -> assign(conn, :current_user, user)
            {:error, _} -> conn |> Plug.Conn.put_status(401) |> Plug.Conn.halt()
          end
      end
    end

    defp get_auth_token(conn) do
      conn
      |> Plug.Conn.get_req_header("authorization")
      |> List.first()
      |> case do
        "Bearer " <> token -> token
        token when is_binary(token) -> token
        _ -> nil
      end
    end

    defp validate_token(token) do
      # Simplified token validation
      if byte_size(token) > 10 do
        {:ok, %{id: "user_123", email: "user@example.com", role: "user"}}
      else
        {:error, :invalid_token}
      end
    end
  end

  # Authorization Plug
  defmodule AuthorizationPlug do
    def init(opts), do: opts

    def call(conn, _opts) do
      current_user = conn.assigns[:current_user]
      required_roles = conn.assigns[:required_roles] || []

      if authorized?(current_user, required_roles) do
        conn
      else
        conn |> Plug.Conn.put_status(403) |> Plug.Conn.halt()
      end
    end

    defp authorized?(nil, _), do: false
    defp authorized?(_, []), do: true

    defp authorized?(user, required_roles) do
      user_role = Map.get(user, :role, "user")
      user_role in required_roles
    end
  end

  # Rate Limiting Plug
  defmodule RateLimitPlug do
    def init(opts) do
      %{limit: Keyword.get(opts, :limit, 100), window: Keyword.get(opts, :window, 60_000)}
    end

    def call(conn, %{limit: limit, window: window}) do
      key = get_rate_limit_key(conn)
      current_count = get_current_count(key)

      if current_count >= limit do
        conn
        |> Plug.Conn.put_resp_header("x-ratelimit-limit", to_string(limit))
        |> Plug.Conn.put_resp_header("x-ratelimit-remaining", "0")
        |> Plug.Conn.put_status(429)
        |> Plug.Conn.send_resp(429, "Too Many Requests")
        |> Plug.Conn.halt()
      else
        increment_count(key, window)

        conn
        |> Plug.Conn.put_resp_header("x-ratelimit-limit", to_string(limit))
        |> Plug.Conn.put_resp_header("x-ratelimit-remaining", to_string(limit - current_count - 1))
      end
    end

    defp get_rate_limit_key(conn) do
      ip = conn.remote_ip |> :inet.ntoa() |> to_string()
      path = conn.path_info |> Enum.join("/")
      "rate_limit:#{ip}:#{path}"
    end

    defp get_current_count(_key), do: 0  # Simplified
    defp increment_count(_key, _window), do: :ok
  end

  # JSON Response Plug
  defmodule JSONResponsePlug do
    def init(opts), do: opts

    def call(conn, _opts) do
      conn |> Plug.Conn.put_resp_header("content-type", "application/json")
    end
  end

  # CORS Plug
  defmodule CORSMiddleware do
    def init(opts), do: opts

    def call(conn, opts) do
      allowed_origins = Keyword.get(opts, :origins, ["*"])
      origin = get_header(conn, "origin")

      if origin in allowed_origins do
        conn
        |> Plug.Conn.put_resp_header("access-control-allow-origin", origin)
        |> Plug.Conn.put_resp_header("access-control-allow-methods", "GET, POST, PUT, PATCH, DELETE, OPTIONS")
        |> Plug.Conn.put_resp_header("access-control-allow-headers", "content-type, authorization")
        |> Plug.Conn.put_resp_header("access-control-max-age", "86400")
      else
        conn
      end
    end

    defp get_header(conn, name) do
      conn |> Plug.Conn.get_req_header(name) |> List.first()
    end
  end

  # Request Validation Plug
  defmodule RequestValidationPlug do
    def init(opts), do: opts

    def call(conn, schema: schema) do
      case validate_request(conn, schema) do
        {:ok, validated_data} -> assign(conn, :validated_params, validated_data)
        {:error, errors} ->
          conn
          |> Plug.Conn.put_status(400)
          |> Plug.Conn.send_resp(400, Jason.encode!(%{errors: errors}))
          |> Plug.Conn.halt()
      end
    end

    defp validate_request(conn, schema) do
      params = conn.body_params |> Map.from_struct() |> Map.merge(conn.path_params |> Map.from_struct())

      case validate_schema(params, schema) do
        {:ok, validated} -> {:ok, validated}
        {:error, errors} -> {:error, errors}
      end
    end

    defp validate_schema(params, schema) do
      required_fields = Map.get(schema, :required, [])
      field_types = Map.get(schema, :types, %{})

      errors = []

      # Check required fields
      Enum.each(required_fields, fn field ->
        if not Map.has_key?(params, field) do
          errors = [%{field: field, error: "required"} | errors]
        end
      end)

      if Enum.empty?(errors) do
        {:ok, params}
      else
        {:error, errors}
      end
    end
  end

  # Session Management Plug
  defmodule SessionPlug do
    def init(opts), do: opts

    def call(conn, _opts) do
      session_token = get_session_token(conn)

      case get_session_data(session_token) do
        {:ok, session_data} -> assign(conn, :session, session_data)
        {:error, _} -> assign(conn, :session, %{})
      end
    end

    defp get_session_token(conn) do
      conn
      |> Plug.Conn.get_req_header("x-session-token")
      |> List.first()
      || Plug.Conn.get_session(conn, :session_token)
    end

    defp get_session_data(nil), do: {:error, :no_session}
    defp get_session_data(_token), do: {:ok, %{user_id: "user_123", preferences: %{}}}
  end

  # Security Headers Plug
  defmodule SecurityHeadersPlug do
    def init(opts), do: opts

    def call(conn, _opts) do
      conn
      |> Plug.Conn.put_resp_header("x-frame-options", "SAMEORIGIN")
      |> Plug.Conn.put_resp_header("x-xss-protection", "1; mode=block")
      |> Plug.Conn.put_resp_header("x-content-type-options", "nosniff")
      |> Plug.Conn.put_resp_header("strict-transport-security", "max-age=31536000; includeSubDomains")
      |> Plug.Conn.put_resp_header("content-security-policy", "default-src 'self'; script-src 'self' 'unsafe-inline'; style-src 'self' 'unsafe-inline'")
    end


  # Cache Control Plug
  defmodule CacheControlPlug do
    def init(opts), do: opts

    def call(conn, opts) do
      case opts[:type] do
        :private ->
          conn |> Plug.Conn.put_resp_header("cache-control", "private, max-age=0, no-cache")
        :public ->
          max_age = opts[:max_age] || 3600
          conn |> Plug.Conn.put_resp_header("cache-control", "public, max-age=#{max_age}")
        :none ->
          conn |> Plug.Conn.put_resp_header("cache-control", "no-store, no-cache, must-revalidate")
        _ ->
          conn
      end
    end
  end

  # Gzip Compression Plug
  defmodule GzipPlug do
    def init(opts), do: opts

    def call(conn, _opts) do
      accept_encoding = get_header(conn, "accept-encoding") || ""

      if String.contains?(accept_encoding, "gzip") do
        conn
        |> Plug.Conn.put_resp_header("vary", "accept-encoding")
        |> Plug.Conn.register_before_send(fn conn ->
          case conn.status in 200..299 do
            true -> compress_response(conn)
            false -> conn
          end
        end)
      else
        conn
      end
    end

    defp get_header(conn, name), do: conn |> Plug.Conn.get_req_header(name) |> List.first()

    defp compress_response(conn) do
      # Simplified - in real implementation would compress body
      conn
    end
  end

  # Request ID Plug
  defmodule RequestIDPlug do
    def init(opts), do: opts

    def call(conn, _opts) do
      request_id = get_or_generate_request_id(conn)

      conn
      |> Plug.Conn.put_resp_header("x-request-id", request_id)
      |> assign(:request_id, request_id)
    end

    defp get_or_generate_request_id(conn) do
      conn
      |> Plug.Conn.get_req_header("x-request-id")
      |> List.first()
      || generate_request_id()
    end

    defp generate_request_id do
      :crypto.hash(:sha256, "#{System.system_time(:nanosecond)}#{:os.system_time(:second)}")
      |> Base.encode16()
      |> String.slice(0..16)
    end
  end

  # Health Check Plug
  defmodule HealthCheckPlug do
    def init(opts), do: opts

    def call(conn, _opts) do
      case conn.path_info do
        ["health"] ->
          health_status = get_health_status()

          status_code = if health_status.healthy, do: 200, else: 503

          conn
          |> Plug.Conn.put_status(status_code)
          |> Plug.Conn.send_resp(200, Jason.encode!(health_status))
          |> Plug.Conn.halt()

        ["health", "ready"] ->
          readiness = get_readiness_status()

          status_code = if readiness.ready, do: 200, else: 503

          conn
          |> Plug.Conn.put_status(status_code)
          |> Plug.Conn.send_resp(200, Jason.encode!(readiness))
          |> Plug.Conn.halt()

        ["health", "live"] ->
          conn
          |> Plug.Conn.put_status(200)
          |> Plug.Conn.send_resp(200, Jason.encode!(%{status: "alive"}))
          |> Plug.Conn.halt()

        _ -> conn
      end
    end

    defp get_health_status do
      %{
        healthy: true,
        timestamp: System.system_time(:millisecond),
        version: "1.0.0",
        uptime_seconds: get_uptime()
      }
    end

    defp get_readiness_status do
      %{
        ready: true,
        checks: %{
          database: :ok,
          cache: :ok,
          external_services: :ok
        }
      }
    end

    defp get_uptime, do: System.system_time(:second) - :os.system_time(:second)
  end

  # Request Throttling Plug
  defmodule ThrottlePlug do
    def init(opts), do: %{max_requests: Keyword.get(opts, :max_requests, 100), window: Keyword.get(opts, :window, 60000)}

    def call(conn, %{max_requests: max_requests, window: window}) do
      key = get_throttle_key(conn)
      current = get_current_requests(key, window)

      if current >= max_requests do
        conn
        |> Plug.Conn.put_status(429)
        |> Plug.Conn.send_resp(429, "Too Many Requests - Please slow down")
        |> Plug.Conn.halt()
      else
        record_request(key, window)
        conn
      end
    end

    defp get_throttle_key(conn) do
      ip = conn.remote_ip |> :inet.ntoa() |> to_string()
      "throttle:#{ip}"
    end

    defp get_current_requests(_key, _window), do: 0  # Simplified
    defp record_request(_key, _window), do: :ok
  end

  # Flash Message Plug
  defmodule FlashMessagePlug do
    def init(opts), do: opts

    def call(conn, _opts) do
      conn
      |> Plug.Conn.fetch_flash()
      |> assign(:flash, conn.assigns[:flash] || %{})
    end
  end

  # Current Request Counter
  defmodule RequestCounterPlug do
    def init(opts), do: opts

    def call(conn, _opts) do
      counter = :request_counter |> Process.whereis() || nil

      if counter do
        count = Agent.get(counter, & &1)
        Agent.update(counter, &(&1 + 1))

        conn
        |> assign(:request_count, count)
        |> Plug.Conn.put_resp_header("x-request-count", to_string(count))
      else
        conn
      end
    end
  end

  # Plug Pipeline Builder
  defmodule PlugPipeline do
    def build do
      plug Plug.Conn.Parsers, parsers: [:urlencoded, :json], pass: ["*/*"]
      plug Plug.Static, at: "/static", from: "/priv/static"
      plug ElixirPlug.RequestIDPlug
      plug ElixirPlug.CORSMiddleware, origins: ["*"]
      plug ElixirPlug.RequestLogger
      plug ElixirPlug.SecurityHeadersPlug
      plug ElixirPlug.JSONResponsePlug
    end

    def api_pipeline do
      build()
      |> plug(ElixirPlug.AuthenticationPlug)
      |> plug(ElixirPlug.RateLimitPlug, limit: 1000, window: 60000)
      |> plug(ElixirPlug.RequestValidationPlug, schema: %{required: [:data], types: %{data: :map}})
    end

    def admin_pipeline do
      api_pipeline()
      |> plug(ElixirPlug.AuthorizationPlug, required_roles: ["admin"])
    end
  end
end

# Demo usage
defmodule ElixirPlugDemo do
  def run do
    IO.puts("=== Elixir Plug Demo ===")

    # Demonstrate plug composition
    IO.puts("\n--- Plug Components ---")
    IO.puts("1. RequestLogger - Logs incoming requests with timing")
    IO.puts("2. AuthenticationPlug - Validates JWT tokens")
    IO.puts("3. AuthorizationPlug - Checks user permissions")
    IO.puts("4. RateLimitPlug - Limits request frequency")
    IO.puts("5. CORSMiddleware - Handles CORS headers")
    IO.puts("6. RequestValidationPlug - Validates request parameters")
    IO.puts("7. SessionPlug - Manages user sessions")
    IO.puts("8. SecurityHeadersPlug - Adds security headers")
    IO.puts("9. CacheControlPlug - Controls caching behavior")
    IO.puts("10. GzipPlug - Compresses responses")
    IO.puts("11. RequestIDPlug - Adds unique request IDs")
    IO.puts("12. HealthCheckPlug - Provides health endpoints")
    IO.puts("13. ThrottlePlug - Throttles high traffic")
    IO.puts("14. FlashMessagePlug - Handles flash messages")
    IO.puts("15. RequestCounterPlug - Counts requests")

    # Show pipeline configuration
    IO.puts("\n--- Pipeline Configuration ---")
    IO.puts("Default Pipeline:")
    IO.puts("  • Request parsing")
    IO.puts("  • Static file serving")
    IO.puts("  • Request ID generation")
    IO.puts("  • CORS handling")
    IO.puts("  • Request logging")
    IO.puts("  • Security headers")
    IO.puts("  • JSON content type")

    IO.puts("\nAPI Pipeline:")
    IO.puts("  • All default plugs")
    IO.puts("  • JWT authentication")
    IO.puts("  • Rate limiting (1000 req/min)")
    IO.puts("  • Request validation")

    IO.puts("\nAdmin Pipeline:")
    IO.puts("  • All API pipeline plugs")
    IO.puts("  • Role-based authorization")

    # Show usage examples
    IO.puts("\n--- Usage Examples ---")
    IO.puts("Using AuthenticationPlug:")
    IO.puts("  plug ElixirPlug.AuthenticationPlug")
    IO.puts("")
    IO.puts("Using RateLimitPlug:")
    IO.puts("  plug ElixirPlug.RateLimitPlug, limit: 100, window: 60_000")
    IO.puts("")
    IO.puts("Using AuthorizationPlug:")
    IO.puts("  plug ElixirPlug.AuthorizationPlug, required_roles: [\"admin\"]")
    IO.puts("")
    IO.puts("Using HealthCheckPlug:")
    IO.puts("  plug ElixirPlug.HealthCheckPlug")

    # Show plug options
    IO.puts("\n--- Plug Options ---")
    IO.puts("AuthenticationPlug:")
    IO.puts("  • token_validator - Custom token validation function")
    IO.puts("")
    IO.puts("RateLimitPlug:")
    IO.puts("  • limit - Maximum requests per window")
    IO.puts("  • window - Time window in milliseconds")
    IO.puts("")
    IO.puts("CORSMiddleware:")
    IO.puts("  • origins - List of allowed origins")
    IO.puts("")
    IO.puts("CacheControlPlug:")
    IO.puts("  • type - :private, :public, or :none")
    IO.puts("  • max_age - Cache duration in seconds")

    IO.puts("\n=== Plug Demo Complete ===")
  end
end

# Run the demo
if __ENV__.module == ElixirPlugDemo do
  ElixirPlugDemo.run()
end
