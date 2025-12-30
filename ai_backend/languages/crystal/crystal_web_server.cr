# Crystal Web Server - HTTP Server Implementation
require "http/server"
require "json"

class WebServerDemo
  def initialize(port : Int32 = 8080)
    @port = port
    @server = HTTP::Server.new do |context|
      handle_request(context)
    end
  end
  
  def start
    address = @server.bind_tcp(@port)
    puts "Crystal Web Server started on http://#{address}"
    
    # Start the server
    @server.listen
  end
  
  def handle_request(context : HTTP::Server::Context)
    request = context.request
    response = context.response
    
    case request.path
    when "/"
      handle_root(response)
    when "/api/status"
      handle_status(response)
    when "/api/echo"
      handle_echo(request, response)
    when "/api/users"
      handle_users(response)
    else
      response.status = HTTP::Status::NOT_FOUND
      response.puts "404 - Route not found"
    end
  end
  
  def handle_root(response : HTTP::Server::Response)
    response.status = HTTP::Status::OK
    response.content_type = "text/html"
    response.puts <<-HTML
    <!DOCTYPE html>
    <html>
    <head><title>Crystal Web Server</title></head>
    <body>
      <h1>Crystal Web Server Demo</h1>
      <p>Available endpoints:</p>
      <ul>
        <li><a href="/api/status">/api/status</a></li>
        <li><a href="/api/users">/api/users</a></li>
        <li>POST /api/echo</li>
      </ul>
    </body>
    </html>
    HTML
  end
  
  def handle_status(response : HTTP::Server::Response)
    response.status = HTTP::Status::OK
    response.content_type = "application/json"
    
    status_data = {
      "server" => "Crystal",
      "version" => "1.0.0",
      "status" => "running",
      "timestamp" => Time.utc.to_s,
      "uptime" => "running"
    }
    
    response.puts status_data.to_json
  end
  
  def handle_echo(request : HTTP::Request, response : HTTP::Server::Response)
    response.status = HTTP::Status::OK
    response.content_type = "application/json"
    
    echo_data = {
      "method" => request.method,
      "path" => request.path,
      "headers" => request.headers.to_h,
      "body" => request.body.try(&.gets_to_end) || ""
    }
    
    response.puts echo_data.to_json
  end
  
  def handle_users(response : HTTP::Server::Response)
    response.status = HTTP::Status::OK
    response.content_type = "application/json"
    
    users = [
      {"id" => 1, "name" => "Alice", "email" => "alice@example.com"},
      {"id" => 2, "name" => "Bob", "email" => "bob@example.com"},
      {"id" => 3, "name" => "Charlie", "email" => "charlie@example.com"}
    ]
    
    response.puts users.to_json
  end
end

# Start the server (commented out for demo)
# server = WebServerDemo.new(8080)
# server.start

puts "Crystal Web Server demo created (server not started for safety)"
