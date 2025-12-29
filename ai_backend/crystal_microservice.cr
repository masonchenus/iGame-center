# Crystal Microservice - Service Architecture with HTTP Server
require "http/server"
require "json"
require "uri"

class Microservice
  property service_name : String
  property version : String
  property port : Int32
  property server : HTTP::Server
  property routes : Hash(String, String)
  
  def initialize(@service_name : String, @version : String, @port : Int32)
    @routes = {} of String => String
    @server = HTTP::Server.new do |context|
      handle_request(context)
    end
  end
  
  def add_route(path : String, handler : String)
    @routes[path] = handler
  end
  
  def start
    puts "#{@service_name} v#{@version} starting on port #{@port}..."
    address = @server.bind_tcp(@port)
    puts "Service available at: http://#{address}"
    @server.listen
  end
  
  def handle_request(context : HTTP::Server::Context)
    request = context.request
    response = context.response
    
    # Add CORS headers
    response.headers["Access-Control-Allow-Origin"] = "*"
    response.headers["Access-Control-Allow-Methods"] = "GET, POST, PUT, DELETE, OPTIONS"
    response.headers["Access-Control-Allow-Headers"] = "Content-Type, Authorization"
    
    case request.method
    when "OPTIONS"
      response.status = HTTP::Status::OK
    when "GET"
      handle_get(request, response)
    when "POST"
      handle_post(request, response)
    when "PUT"
      handle_put(request, response)
    when "DELETE"
      handle_delete(request, response)
    else
      response.status = HTTP::Status::METHOD_NOT_ALLOWED
      response.puts "Method not allowed"
    end
  end
  
  def handle_get(request : HTTP::Request, response : HTTP::Server::Response)
    case request.path
    when "/health"
      health_check(response)
    when "/info"
      service_info(response)
    when "/metrics"
      metrics(response)
    when "/routes"
      list_routes(response)
    else
      response.status = HTTP::Status::NOT_FOUND
      response.content_type = "application/json"
      response.puts({"error" => "Route not found"}.to_json)
    end
  end
  
  def handle_post(request : HTTP::Request, response : HTTP::Server::Response)
    body = request.body.try(&.gets_to_end) || ""
    
    case request.path
    when "/process"
      process_data(body, response)
    when "/validate"
      validate_data(body, response)
    else
      response.status = HTTP::Status::NOT_FOUND
      response.content_type = "application/json"
      response.puts({"error" => "Route not found"}.to_json)
    end
  end
  
  def handle_put(request : HTTP::Request, response : HTTP::Server::Response)
    body = request.body.try(&.gets_to_end) || ""
    
    case request.path
    when "/config"
      update_config(body, response)
    else
      response.status = HTTP::Status::NOT_FOUND
      response.content_type = "application/json"
      response.puts({"error" => "Route not found"}.to_json)
    end
  end
  
  def handle_delete(request : HTTP::Request, response : HTTP::Server::Response)
    response.status = HTTP::Status::NOT_IM    response.contentPLEMENTED
_type = "application/json"
    response.puts({"error" => "Delete not implemented"}.to_json)
  end
  
  def health_check(response : HTTP::Server::Response)
    response.status = HTTP::Status::OK
    response.content_type = "application/json"
    
    health_data = {
      "service" => @service_name,
      "version" => @version,
      "status" => "healthy",
      "timestamp" => Time.utc.to_s,
      "uptime" => "running"
    }
    
    response.puts health_data.to_json
  end
  
  def service_info(response : HTTP::Server::Response)
    response.status = HTTP::Status::OK
    response.content_type = "application/json"
    
    info_data = {
      "service_name" => @service_name,
      "version" => @version,
      "port" => @port,
      "language" => "Crystal",
      "routes" => @routes.keys
    }
    
    response.puts info_data.to_json
  end
  
  def metrics(response : HTTP::Server::Response)
    response.status = HTTP::Status::OK
    response.content_type = "application/json"
    
    metrics_data = {
      "requests_total" => rand(1000..5000),
      "requests_per_second" => rand(10..100),
      "memory_usage_mb" => rand(50..200),
      "cpu_usage_percent" => rand(10..80),
      "active_connections" => rand(0..50)
    }
    
    response.puts metrics_data.to_json
  end
  
  def list_routes(response : HTTP::Server::Response)
    response.status = HTTP::Status::OK
    response.content_type = "application/json"
    
    routes_data = {
      "available_routes" => @routes
    }
    
    response.puts routes_data.to_json
  end
  
  def process_data(body : String, response : HTTP::Server::Response)
    begin
      data = JSON.parse(body)
      # Simulate data processing
      processed_data = {
        "original" => data,
        "processed" => true,
        "timestamp" => Time.utc.to_s,
        "result" => "Data processed successfully"
      }
      
      response.status = HTTP::Status::OK
      response.content_type = "application/json"
      response.puts processed_data.to_json
    rescue ex
      response.status = HTTP::Status::BAD_REQUEST
      response.content_type = "application/json"
      response.puts({"error" => "Invalid JSON: #{ex.message}"}.to_json)
    end
  end
  
  def validate_data(body : String, response : HTTP::Server::Response)
    begin
      data = JSON.parse(body)
      # Simple validation rules
      errors = [] of String
      
      if data["email"]?.nil? || data["email"].as_s =~ /@/
        # valid email
      else
        errors << "Invalid email format"
      end
      
      if data["name"]?.nil? || data["name"].as_s.size > 0
        # valid name
      else
        errors << "Name is required"
      end
      
      if errors.empty?
        response.status = HTTP::Status::OK
        response.content_type = "application/json"
        response.puts({"valid" => true, "message" => "Data is valid"}.to_json)
      else
        response.status = HTTP::Status::BAD_REQUEST
        response.content_type = "application/json"
        response.puts({"valid" => false, "errors" => errors}.to_json)
      end
    rescue ex
      response.status = HTTP::Status::BAD_REQUEST
      response.content_type = "application/json"
      response.puts({"error" => "Invalid JSON: #{ex.message}"}.to_json)
    end
  end
  
  def update_config(body : String, response : HTTP::Server::Response)
    begin
      config = JSON.parse(body)
      # Simulate config update
      response.status = HTTP::Status::OK
      response.content_type = "application/json"
      response.puts({
        "message" => "Configuration updated",
        "config" => config,
        "timestamp" => Time.utc.to_s
      }.to_json)
    rescue ex
      response.status = HTTP::Status::BAD_REQUEST
      response.content_type = "application/json"
      response.puts({"error" => "Invalid JSON: #{ex.message}"}.to_json)
    end
  end
end

# Service Discovery Component
class ServiceDiscovery
  def initialize
    @services = {} of String => Hash(String, String | Int32)
  end
  
  def register(service_name : String, host : String, port : Int32, health_url : String)
    @services[service_name] = {
      "host" => host,
      "port" => port,
      "health_url" => health_url,
      "status" => "unknown"
    }
    puts "Service registered: #{service_name} at #{host}:#{port}"
  end
  
  def discover(service_name : String) : Hash(String, String | Int32)?
    @services[service_name]?
  end
  
  def list_services : Hash(String, Hash(String, String | Int32))
    @services
  end
  
  def health_check(service_name : String) : Bool
    # Simulate health check
    puts "Performing health check for #{service_name}..."
    rand(0..1) == 1
  end
end

# Demo usage
if __FILE__ == $0
  puts "=== Crystal Microservice Demo ==="
  
  # Create and configure the microservice
  service = Microservice.new("DataProcessor", "1.0.0", 8081)
  
  # Add custom routes
  service.add_route("/health", "Health check endpoint")
  service.add_route("/process", "Process incoming data")
  service.add_route("/validate", "Validate data")
  service.add_route("/metrics", "Get service metrics")
  
  # Service discovery
  discovery = ServiceDiscovery.new
  discovery.register("UserService", "localhost", 8082, "/health")
  discovery.register("AuthService", "localhost", 8083, "/health")
  
  puts "Service discovery registered services: #{discovery.list_services.keys.join(", ")}"
  
  puts "\nMicroservice configured and ready to start!"
  puts "Available endpoints:"
  puts "  GET  /health     - Health check"
  puts "  GET  /info       - Service information"
  puts "  GET  /metrics    - Service metrics"
  puts "  GET  /routes     - List available routes"
  puts "  POST /process    - Process data"
  puts "  POST /validate   - Validate data"
  puts "  PUT  /config     - Update configuration"
  
  # Note: Server not started for safety in demo
  # To start: service.start
end
