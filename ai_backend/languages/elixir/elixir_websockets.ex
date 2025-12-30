
# Elixir WebSockets - Real-time WebSocket Communication
defmodule ElixirWebsockets do
  # WebSocket Server using Plug and WebSock
  defmodule WebSocketServer do
    use Plug.WebSocket

    defmodule State do
      defstruct socket: nil, connected_at: nil, message_count: 0
    end

    def init(opts) do
      %{opts: opts}
    end

    def connect(remote_ip, state) do
      IO.puts("New WebSocket connection from #{inspect(remote_ip)}")
      {:ok, %State{socket: nil, connected_at: System.system_time(:millisecond), message_count: 0}}
    end

    def handle_in({text, [opcode: :text]}, state) do
      IO.puts("Received: #{text}")

      # Echo back with timestamp
      response = %{type: :message, text: text, timestamp: System.system_time(:millisecond), count: state.message_count + 1}

      {:reply, :text, Jason.encode!(response), %{state | message_count: state.message_count + 1}}
    end

    def handle_in({data, [opcode: :binary]}, state) do
      IO.puts("Received binary data: #{inspect(data)}")
      {:reply, :binary, data, state}
    end

    def handle_info(:ping, state) do
      {:reply, :ping, "", state}
    end

    def handle_info(:close, state) do
      {:close, "", state}
    end

    def terminate(_reason, _state) do
      IO.puts("WebSocket connection closed")
      :ok
    end
  end

  # Phoenix Channels for Real-time Communication
  defmodule MyAppWeb.UserSocket do
    use Phoenix.Socket

    channel "user:*", ElixirWebsockets.UserChannel
    channel "room:*", ElixirWebsockets.RoomChannel
    channel "notifications", ElixirWebsockets.NotificationChannel

    def connect(params, socket, connect_info) do
      # Authenticate user from params
      user_id = Map.get(params, "user_id")
      token = Map.get(params, "token")

      if validate_token(user_id, token) do
        {:ok, assign(socket, :user_id, user_id)}
      else
        {:error, "Authentication failed"}
      end
    end

    def id(socket), do: "user:#{socket.assigns.user_id}"

    defp validate_token(_user_id, _token) do
      # Implement actual token validation
      true
    end
  end

  defmodule UserChannel do
    use Phoenix.Channel

    def join("user:" <> user_id, _params, socket) do
      if socket.assigns.user_id == user_id do
        {:ok, %{message: "Welcome to your private channel, user #{user_id}"}, socket}
      else
        {:error, %{reason: "Unauthorized"}}
      end
    end

    def handle_in("get_profile", _params, socket) do
      user_id = socket.assigns.user_id
      profile = get_user_profile(user_id)
      {:reply, {:ok, profile}, socket}
    end

    def handle_in("update_settings", settings, socket) do
      user_id = socket.assigns.user_id
      :ok = update_user_settings(user_id, settings)
      {:reply, {:ok, %{message: "Settings updated"}}, socket}
    end

    def handle_in("typing", _params, socket) do
      broadcast!(socket, "user_typing", %{user_id: socket.assigns.user_id})
      {:noreply, socket}
    end

    def handle_in("private_message", %{"to" => to_user_id, "content" => content}, socket) do
      message = %{from: socket.assigns.user_id, content: content, timestamp: System.system_time(:millisecond)}
      Phoenix.Channel.push(socket, "new_message", message)
      {:noreply, socket}
    end

    def handle_info(:ping, socket) do
      push(socket, "ping", %{timestamp: System.system_time(:millisecond)})
      {:noreply, socket}
    end

    defp get_user_profile(user_id) do
      %{id: user_id, name: "User #{user_id}", status: :online, last_active: System.system_time(:millisecond)}
    end

    defp update_user_settings(_user_id, _settings), do: :ok
  end

  defmodule RoomChannel do
    use Phoenix.Channel

    def join("room:" <> room_id, %{"nickname" => nickname}, socket) do
      # Add user to room
      :ok = RoomManager.add_user(room_id, socket.assigns.user_id, nickname)

      # Notify others
      broadcast!(socket, "user_joined", %{user_id: socket.assigns.user_id, nickname: nickname})

      # Send current room state
      room_state = RoomManager.get_room_state(room_id)
      {:ok, %{room: room_state}, socket}
    end

    def handle_in("send_message", %{"content" => content}, socket) do
      room_id = socket.topic |> String.replace("room:", "")
      message = %{
        user_id: socket.assigns.user_id,
        content: content,
        timestamp: System.system_time(:millisecond),
        message_id: generate_message_id()
      }

      broadcast!(socket, "new_message", message)
      {:reply, {:ok, %{message_id: message.message_id}}, socket}
    end

    def handle_in("leave_room", _params, socket) do
      room_id = socket.topic |> String.replace("room:", "")
      :ok = RoomManager.remove_user(room_id, socket.assigns.user_id)
      {:error, %{reason: "Left room"}, socket}
    end

    def handle_in("get_history", _params, socket) do
      room_id = socket.topic |> String.replace("room:", "")
      messages = RoomManager.get_message_history(room_id)
      {:reply, {:ok, %{messages: messages}}, socket}
    end

    def handle_in("typing", _params, socket) do
      broadcast!(socket, "user_typing", %{user_id: socket.assigns.user_id})
      {:noreply, socket}
    end

    def handle_in("react", %{"message_id" => message_id, "emoji" => emoji}, socket) do
      broadcast!(socket, "message_reaction", %{message_id: message_id, emoji: emoji, user_id: socket.assigns.user_id})
      {:noreply, socket}
    end

    def terminate(_reason, socket) do
      room_id = socket.topic |> String.replace("room:", "")
      RoomManager.remove_user(room_id, socket.assigns.user_id)
      :ok
    end

    defp generate_message_id do
      :crypto.hash(:sha256, "#{System.system_time(:millisecond)}#{:os.system_time(:second)}") |> Base.encode16()
    end
  end

  defmodule NotificationChannel do
    use Phoenix.Channel

    def join("notifications", _params, socket) do
      {:ok, socket}
    end

    def handle_in("subscribe", %{"channels" => channels}, socket) do
      Enum.each(channels, fn channel ->
        :ok = NotificationManager.subscribe(socket.assigns.user_id, channel)
      end)
      {:reply, {:ok, %{subscribed: channels}}, socket}
    end

    def handle_in("unsubscribe", %{"channels" => channels}, socket) do
      Enum.each(channels, fn channel ->
        :ok = NotificationManager.unsubscribe(socket.assigns.user_id, channel)
      end)
      {:reply, {:ok, %{unsubscribed: channels}}, socket}
    end

    def handle_info({:notification, notification}, socket) do
      push(socket, "new_notification", notification)
      {:noreply, socket}
    end
  end

  # Room Manager for Multi-user Rooms
  defmodule RoomManager do
    use Agent

    def start_link do
      Agent.start_link(fn -> %{} end, name: __MODULE__)
    end

    def add_user(room_id, user_id, nickname) do
      Agent.update(__MODULE__, fn rooms ->
        room = Map.get(rooms, room_id, %{users: %{}, messages: []})
        updated_users = Map.put(room.users, user_id, %{nickname: nickname, joined_at: System.system_time(:millisecond)})
        Map.put(rooms, room_id, %{room | users: updated_users})
      end)
      :ok
    end

    def remove_user(room_id, user_id) do
      Agent.update(__MODULE__, fn rooms ->
        case Map.get(rooms, room_id) do
          nil -> rooms
          room ->
            updated_users = Map.delete(room.users, user_id)
            Map.put(rooms, room_id, %{room | users: updated_users})
        end
      end)
      :ok
    end

    def get_room_state(room_id) do
      Agent.get(__MODULE__, fn rooms ->
        Map.get(rooms, room_id, %{users: %{}, messages: []})
      end)
    end

    def add_message(room_id, message) do
      Agent.update(__MODULE__, fn rooms ->
        case Map.get(rooms, room_id) do
          nil -> rooms
          room ->
            messages = [message | room.messages] |> Enum.take(100)  # Keep last 100 messages
            Map.put(rooms, room_id, %{room | messages: messages})
        end
      end)
    end

    def get_message_history(room_id) do
      Agent.get(__MODULE__, fn rooms ->
        case Map.get(rooms, room_id) do
          nil -> []
          room -> Enum.reverse(room.messages)
        end
      end)
    end
  end

  # Notification Manager
  defmodule NotificationManager do
    use Agent

    def start_link do
      Agent.start_link(fn -> %{subscriptions: %{}, notifications: []} end, name: __MODULE__)
    end

    def subscribe(user_id, channel) do
      Agent.update(__MODULE__, fn state ->
        subscriptions = Map.get(state, :subscriptions, %{})
        channel_subscribers = Map.get(subscriptions, channel, MapSet.new())
        new_subscriptions = Map.put(subscriptions, channel, MapSet.put(channel_subscribers, user_id))
        Map.put(state, :subscriptions, new_subscriptions)
      end)
      :ok
    end

    def unsubscribe(user_id, channel) do
      Agent.update(__MODULE__, fn state ->
        subscriptions = Map.get(state, :subscriptions, %{})
        case Map.get(subscriptions, channel) do
          nil -> state
          channel_subscribers ->
            new_subscribers = MapSet.delete(channel_subscribers, user_id)
            Map.put(state, :subscriptions, Map.put(subscriptions, channel, new_subscribers))
        end
      end)
      :ok
    end

    def broadcast(channel, notification) do
      Agent.update(__MODULE__, fn state ->
        notifications = [notification | Map.get(state, :notifications, [])] |> Enum.take(1000)
        Map.put(state, :notifications, notifications)
      end)

      # In real implementation, would push to all subscribed users via their channels
      IO.puts("Broadcasting to channel #{channel}: #{inspect(notification)}")
      :ok
    end
  end

  # WebSocket Client for Testing
  defmodule WebSocketClient do
    def connect(url) do
      case WebSockex.start(url, __MODULE__, %{}) do
        {:ok, pid} -> {:ok, pid}
        {:error, reason} -> {:error, reason}
      end
    end

    def send_message(pid, message) do
      WebSockex.send_frame(pid, {:text, Jason.encode!(message)})
    end

    def close(pid) do
      WebSockex.close(pid)
    end

    def init(state) do
      {:ok, state}
    end

    def handle_frame({:text, data}, state) do
      IO.puts("Received: #{data}")
      {:ok, state}
    end

    def handle_frame(_frame, state) do
      {:ok, state}
    end

    def handle_info(:connect, state) do
      {:connect, state}
    end

    def terminate(_reason, _state) do
      :ok
    end
  end

  # LiveView for Real-time Updates
  defmodule LiveDashboard do
    use Phoenix.LiveView

    def mount(_params, _session, socket) do
      if connected?(socket) do
        Phoenix.PubSub.subscribe(ElixirWebsockets.PubSub, "dashboard_updates")
      end

      {:ok, assign(socket, metrics: get_initial_metrics())}
    end

    def render(assigns) do
      ~H"""
      <div class="dashboard">
        <h1>Real-time Dashboard</h1>

        <div class="metrics-grid">
          <div class="metric-card">
            <h3>Active Users</h3>
            <p class="metric-value"><%= @metrics.active_users %></p>
          </div>

          <div class="metric-card">
            <h3>Messages/min</h3>
            <p class="metric-value"><%= @metrics.messages_per_minute %></p>
          </div>

          <div class="metric-card">
            <h3>Connections</h3>
            <p class="metric-value"><%= @metrics.connection_count %></p>
          </div>

          <div class="metric-card">
            <h3>Uptime</h3>
            <p class="metric-value"><%= @metrics.uptime_seconds %>s</p>
          </div>
        </div>

        <div class="live-log">
          <h3>Live Activity Log</h3>
          <div class="log-entries">
            <%= for log <- @metrics.recent_logs do %>
              <div class="log-entry">
                <span class="timestamp"><%= log.timestamp %></span>
                <span class="message"><%= log.message %></span>
              </div>
            <% end %>
          </div>
        </div>
      </div>
      """
    end

    def handle_info({:update, metrics}, socket) do
      {:noreply, assign(socket, metrics: metrics)}
    end

    defp get_initial_metrics do
      %{
        active_users: 0,
        messages_per_minute: 0,
        connection_count: 0,
        uptime_seconds: 0,
        recent_logs: []
      }
    end
  end

  # Presence Tracking
  defmodule Presence do
    use Phoenix.Presence, otp_app: :my_app

    def fetch(_topic, presences) do
      Enum.into(presences, %{}, fn {key, %{metas: metas}} ->
        {key, %{metas: metas, user_id: key}}
      end)
    end

    def track_user(socket, user_id, meta \\ %{}) do
      Presence.track(socket, user_id, meta)
    end

    def get_present_users(topic) do
      case list(topic) do
        %{users: users} -> users
        _ -> %{}
      end
    end
  end

  # WebSocket Connection Manager
  defmodule ConnectionManager do
    use GenServer

    defstruct connections: %{}, stats: %{total_connections: 0, messages_processed: 0}

    def start_link do
      GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
    end

    def register_connection(pid, metadata \\ %{}) do
      GenServer.cast(__MODULE__, {:register, pid, metadata})
    end

    def unregister_connection(pid) do
      GenServer.cast(__MODULE__, {:unregister, pid})
    end

    def record_message(pid) do
      GenServer.cast(__MODULE__, {:record_message, pid})
    end

    def get_stats do
      GenServer.call(__MODULE__, :get_stats)
    end

    def init(:ok) do
      {:ok, %__MODULE__{}}
    end

    def handle_cast({:register, pid, metadata}, state) do
      Process.monitor(pid)
      connections = Map.put(state.connections, pid, Map.put(metadata, :connected_at, System.system_time(:millisecond)))
      stats = %{state.stats | total_connections: state.stats.total_connections + 1}
      {:noreply, %{state | connections: connections, stats: stats}}
    end

    def handle_cast({:unregister, pid}, state) do
      connections = Map.delete(state.connections, pid)
      {:noreply, %{state | connections: connections}}
    end

    def handle_cast({:record_message, _pid}, state) do
      stats = %{state.stats | messages_processed: state.stats.messages_processed + 1}
      {:noreply, %{state | stats: stats}}
    end

    def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
      connections = Map.delete(state.connections, pid)
      {:noreply, %{state | connections: connections}}
    end

    def handle_call(:get_stats, _from, state) do
      {:reply, %{connections: map_size(state.connections), stats: state.stats}, state}
    end
  end

  # WebSocket Router
  defmodule WebSocketRouter do
    use Plug.Router

    plug :match
    plug :dispatch

    get "/ws" do
      conn
      |> put_resp_header("upgrade", "websocket")
      |> put_resp_header("connection", "upgrade")
      |> send_file(101, "priv/static/ws.html")
    end

    post "/ws/connect" do
      {:ok, body, conn} = read_body(conn)
      params = Jason.decode!(body)

      ws_url = Map.get(params, "url", "ws://localhost:4000/ws")

      {:ok, pid} = WebSocketClient.connect(ws_url)

      ConnectionManager.register_connection(pid, Map.get(params, "metadata", %{}))

      json(conn, %{status: :connected, pid: inspect(pid)})
    end

    post "/ws/send" do
      {:ok, body, conn} = read_body(conn)
      params = Jason.decode!(body)

      pid = params["pid"] |> String.to_atom() |> Process.whereis()

      if pid do
        WebSocketClient.send_message(pid, Map.get(params, "message", %{}))
        json(conn, %{status: :sent})
      else
        json(conn, %{status: :error, reason: "Connection not found"})
      end
    end

    post "/ws/stats" do
      stats = ConnectionManager.get_stats()
      json(conn, stats)
    end
  end

  # PubSub Module
  defmodule PubSub do
    @moduledoc """
    Simple PubSub implementation for demonstration.
    In production, use Phoenix.PubSub.
    """

    def start_link do
      Agent.start_link(fn -> %{topics: %{}, subscribers: %{}} end, name: __MODULE__)
    end

    def subscribe(topic, pid) when is_pid(pid) do
      Agent.update(__MODULE__, fn state ->
        topics = Map.get(state, :topics, %{})
        subscribers = Map.get(state, :subscribers, %{})

        topic_subscribers = Map.get(subscribers, topic, MapSet.new())
        new_subscribers = Map.put(subscribers, topic, MapSet.put(topic_subscribers, pid))

        %{state | subscribers: new_subscribers}
      end)
      :ok
    end

    def broadcast(topic, message) do
      Agent.get(__MODULE__, fn state ->
        subscribers = Map.get(state, :subscribers, %{})
        Map.get(subscribers, topic, MapSet.new())
      end)
      |> Enum.each(fn pid ->
        send(pid, {:broadcast, topic, message})
      end)

      :ok
    end
  end
end

# Demo usage
defmodule ElixirWebsocketsDemo do
  def run do
    IO.puts("=== Elixir WebSockets Demo ===")

    # Start supporting services
    IO.puts("\n--- Starting Support Services ---")
    {:ok, _room_manager} = ElixirWebsockets.RoomManager.start_link()
    {:ok, _notification_manager} = ElixirWebsockets.NotificationManager.start_link()
    {:ok, _pubsub} = ElixirWebsockets.PubSub.start_link()
    {:ok, _connection_manager} = ElixirWebsockets.ConnectionManager.start_link()

    IO.puts("Support services started")

    # Demonstrate room management
    IO.puts("\n--- Room Management ---")
    ElixirWebsockets.RoomManager.add_user("room_1", "user_1", "Alice")
    ElixirWebsockets.RoomManager.add_user("room_1", "user_2", "Bob")
    ElixirWebsockets.RoomManager.add_user("room_1", "user_3", "Charlie")

    room_state = ElixirWebsockets.RoomManager.get_room_state("room_1")
    IO.puts("Room 1 state: #{map_size(room_state.users)} users")

    # Add a message
    message = %{user_id: "user_1", content: "Hello everyone!", timestamp: System.system_time(:millisecond)}
    ElixirWebsockets.RoomManager.add_message("room_1", message)

    # Get message history
    history = ElixirWebsockets.RoomManager.get_message_history("room_1")
    IO.puts("Message history: #{length(history)} messages")

    # Demonstrate notification system
    IO.puts("\n--- Notification System ---")
    ElixirWebsockets.NotificationManager.subscribe("user_1", "updates")
    ElixirWebsockets.NotificationManager.subscribe("user_1", "alerts")
    ElixirWebsockets.NotificationManager.subscribe("user_2", "updates")

    notification = %{type: :update, title: "New version available", timestamp: System.system_time(:millisecond)}
    ElixirWebsockets.NotificationManager.broadcast("updates", notification)

    IO.puts("Notifications broadcast to subscribers")

    # Demonstrate connection manager
    IO.puts("\n--- Connection Management ---")
    stats = ElixirWebsockets.ConnectionManager.get_stats()
    IO.puts("Initial connection stats: #{inspect(stats)}")

    # Record some connections and messages
    ElixirWebsockets.ConnectionManager.register_connection(self(), %{user_id: "test_user"})
    ElixirWebsockets.ConnectionManager.record_message(self())
    ElixirWebsockets.ConnectionManager.record_message(self())

    updated_stats = ElixirWebsockets.ConnectionManager.get_stats()
    IO.puts("Updated connection stats: #{inspect(updated_stats)}")

    # Demonstrate PubSub
    IO.puts("\n--- PubSub System ---")
    ElixirWebsockets.PubSub.subscribe("chat:room_1", self())

    # This would normally send to subscribers
    # ElixirWebsockets.PubSub.broadcast("chat:room_1", %{message: "Hello subscribers!"})

    IO.puts("Subscribed to chat:room_1 topic")

    IO.puts("\n=== WebSockets Demo Complete ===")
  end
end

# Run the demo
if __ENV__.module == ElixirWebsocketsDemo do
  ElixirWebsocketsDemo.run()
end
