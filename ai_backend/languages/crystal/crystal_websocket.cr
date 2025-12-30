# Crystal WebSocket - Real-time Communication Implementation
require "http/server"
require "websocket"

class WebSocketServer
  def initialize(port : Int32 = 3000)
    @port = port
    @connections = [] of HTTP::WebSocket
    @server = HTTP::Server.new do |context|
      handle_connection(context)
    end
  end
  
  def start
    puts "WebSocket Server starting on port #{@port}..."
    address = @server.bind_tcp(@port)
    puts "WebSocket server available at: ws://#{address}"
    @server.listen
  end
  
  def handle_connection(context : HTTP::Server::Context)
    request = context.request
    
    if request.path == "/ws"
      # Upgrade to WebSocket
      ws = HTTP::WebSocket.new(context)
      
      # Add connection to our list
      @connections << ws
      puts "New WebSocket connection established. Total connections: #{@connections.size}"
      
      # Handle WebSocket events
      ws.on_message do |message|
        handle_message(ws, message)
      end
      
      ws.on_close do
        @connections.delete(ws)
        puts "WebSocket connection closed. Total connections: #{@connections.size}"
      end
      
      ws.on_ping do |data|
        ws.pong(data)
      end
      
      # Start listening for this WebSocket
      ws.listen
    else
      # Regular HTTP endpoints
      handle_http_request(context)
    end
  end
  
  def handle_http_request(context : HTTP::Server::Context)
    response = context.response
    
    case context.request.path
    when "/"
      response.status = HTTP::Status::OK
      response.content_type = "text/html"
      response.puts <<-HTML
      <!DOCTYPE html>
      <html>
      <head>
        <title>Crystal WebSocket Demo</title>
        <style>
          body { font-family: Arial, sans-serif; margin: 40px; }
          .container { max-width: 600px; margin: 0 auto; }
          .status { padding: 10px; margin: 10px 0; border-radius: 5px; }
          .connected { background-color: #d4edda; color: #155724; }
          .disconnected { background-color: #f8d7da; color: #721c24; }
          #messages { border: 1px solid #ccc; padding: 10px; height: 300px; overflow-y: scroll; }
          input, button { padding: 10px; margin: 5px; }
        </style>
      </head>
      <body>
        <div class="container">
          <h1>Crystal WebSocket Demo</h1>
          <div id="status" class="status disconnected">Disconnected</div>
          <div>
            <input type="text" id="messageInput" placeholder="Enter your message...">
            <button onclick="sendMessage()">Send</button>
            <button onclick="clearMessages()">Clear</button>
          </div>
          <div id="messages"></div>
        </div>
        
        <script>
          let ws;
          const status = document.getElementById('status');
          const messages = document.getElementById('messages');
          const messageInput = document.getElementById('messageInput');
          
          function connect() {
            ws = new WebSocket('ws://' + window.location.host + '/ws');
            
            ws.onopen = function(event) {
              status.textContent = 'Connected';
              status.className = 'status connected';
              addMessage('Connected to WebSocket server');
            };
            
            ws.onmessage = function(event) {
              addMessage('Server: ' + event.data);
            };
            
            ws.onclose = function(event) {
              status.textContent = 'Disconnected';
              status.className = 'status disconnected';
              addMessage('Disconnected from server');
              // Attempt to reconnect after 2 seconds
              setTimeout(connect, 2000);
            };
            
            ws.onerror = function(error) {
              addMessage('WebSocket error: ' + error);
            };
          }
          
          function sendMessage() {
            const message = messageInput.value.trim();
            if (message && ws && ws.readyState === WebSocket.OPEN) {
              ws.send(message);
              addMessage('You: ' + message);
              messageInput.value = '';
            }
          }
          
          function addMessage(message) {
            const timestamp = new Date().toLocaleTimeString();
            messages.innerHTML += '<div>[' + timestamp + '] ' + message + '</div>';
            messages.scrollTop = messages.scrollHeight;
          }
          
          function clearMessages() {
            messages.innerHTML = '';
          }
          
          // Handle Enter key
          messageInput.addEventListener('keypress', function(e) {
            if (e.key === 'Enter') {
              sendMessage();
            }
          });
          
          // Connect when page loads
          connect();
        </script>
      </body>
      </html>
      HTML
    when "/stats"
      response.status = HTTP::Status::OK
      response.content_type = "application/json"
      
      stats = {
        "active_connections" => @connections.size,
        "server_time" => Time.utc.to_s,
        "uptime" => "running"
      }
      response.puts stats.to_json
    else
      response.status = HTTP::Status::NOT_FOUND
      response.puts "Not found"
    end
  end
  
  def handle_message(ws : HTTP::WebSocket, message : String)
    puts "Received message: #{message}"
    
    # Echo the message back with timestamp
    timestamp = Time.utc.to_s
    response = "[#{timestamp}] Echo: #{message}"
    ws.send(response)
    
    # Broadcast to all connected clients (except sender)
    @connections.each do |connection|
      next if connection == ws
      broadcast_message = "[#{timestamp}] Broadcast from #{connection.object_id}: #{message}"
      connection.send(broadcast_message)
    end
    
    # Handle special commands
    case message
    when "ping"
      ws.send("pong")
    when "stats"
      stats_message = "Active connections: #{@connections.size}"
      ws.send(stats_message)
    when "broadcast"
      @connections.each do |connection|
        connection.send("System broadcast: Hello everyone!")
      end
    end
  end
  
  def broadcast_message(message : String)
    timestamp = Time.utc.to_s
    @connections.each do |connection|
      connection.send("[#{timestamp}] System: #{message}")
    end
  end
end

# WebSocket Client for testing
class WebSocketClient
  def initialize(url : String)
    @url = url
    @connected = false
  end
  
  def connect
    puts "Connecting to WebSocket server..."
    # This would require a WebSocket client library
    # For demo purposes, showing the structure
    @connected = true
    puts "Connected to #{@url}"
  end
  
  def send(message : String)
    if @connected
      puts "Sending: #{message}"
      # WebSocket client send implementation
    else
      puts "Not connected"
    end
  end
  
  def disconnect
    if @connected
      puts "Disconnecting..."
      @connected = false
    end
  end
end

# Chat Room Implementation
class ChatRoom
  def initialize
    @users = {} of HTTP::WebSocket => String
    @server = WebSocketServer.new(3001)
  end
  
  def start
    puts "Chat Room Server starting on port 3001..."
    @server.start
  end
  
  def handle_user_join(ws : HTTP::WebSocket, username : String)
    @users[ws] = username
    broadcast_system_message("#{username} joined the chat")
  end
  
  def handle_user_leave(ws : HTTP::WebSocket)
    username = @users[ws]?
    @users.delete(ws)
    broadcast_system_message("#{username} left the chat") if username
  end
  
  def broadcast_message(username : String, message : String)
    timestamp = Time.utc.to_s
    @users.each_key do |ws|
      ws.send("[#{timestamp}] #{username}: #{message}")
    end
  end
  
  def broadcast_system_message(message : String)
    timestamp = Time.utc.to_s
    @users.each_key do |ws|
      ws.send("[#{timestamp}] SYSTEM: #{message}")
    end
  end
  
  def get_user_list : Array(String)
    @users.values
  end
end

# Demo usage
if __FILE__ == $0
  puts "=== Crystal WebSocket Demo ==="
  
  # Create WebSocket server
  ws_server = WebSocketServer.new(3000)
  
  # Create chat room
  chat_room = ChatRoom.new
  
  # Create client for testing
  client = WebSocketClient.new("ws://localhost:3000/ws")
  client.connect
  
  puts "\nWebSocket server configured!"
  puts "Available endpoints:"
  puts "  WebSocket: ws://localhost:3000/ws"
  puts "  HTTP: http://localhost:3000/ (WebSocket demo page)"
  puts "  Stats: http://localhost:3000/stats"
  
  puts "\nSpecial commands:"
  puts "  'ping' - Server responds with 'pong'"
  puts "  'stats' - Shows connection statistics"
  puts "  'broadcast' - Sends system broadcast to all clients"
  
  # Note: Server not started for safety in demo
  # To start: ws_server.start
end
