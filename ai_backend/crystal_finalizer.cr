# Crystal Finalizer - Comprehensive Application Example
require "http/server"
require "json"
require "sqlite3"

# This is a comprehensive Crystal application that demonstrates
# integration of all the concepts from the previous Crystal files

class ApplicationConfig
  property database_path : String
  property server_port : Int32
  property cache_size : Int32
  property worker_count : Int32
  property api_rate_limit : Int32
  property jwt_secret : String
  
  def initialize
    @database_path = "app.db"
    @server_port = 8080
    @cache_size = 1000
    @worker_count = 4
    @api_rate_limit = 100
    @jwt_secret = "crystal-app-secret-key-2024"
  end
  
  def self.from_env : ApplicationConfig
    config = ApplicationConfig.new
    # In a real app, load from environment variables
    config
  end
end

class User
  property id : String
  property username : String
  property email : String
  property password_hash : String
  property roles : Array(String)
  property created_at : Time
  property active : Bool
  
  def initialize(@id : String, @username : String, @email : String, @password_hash : String)
    @roles = ["user"]
    @created_at = Time.utc
    @active = true
  end
end

class ApplicationCache
  def initialize(size : Int32 = 1000)
    @store = {} of String => {value: JSON::Any, expires_at: Time}
    @max_size = size
    @mutex = Mutex.new
  end
  
  def get(key : String) : JSON::Any?
    @mutex.synchronize do
      entry = @store[key]?
      return nil unless entry
      
      if Time.utc > entry[:expires_at]
        @store.delete(key)
        return nil
      end
      
      entry[:value]
    end
  end
  
  def set(key : String, value : JSON::Any, ttl : Time::Span = 1.hour)
    @mutex.synchronize do
      @store[key] = {value: value, expires_at: Time.utc + ttl}
      cleanup if @store.size > @max_size
    end
  end
  
  private def cleanup
    expired_keys = @store.select { |_, entry| Time.utc > entry[:expires_at] }.keys
    expired_keys.each { |key| @store.delete(key) }
  end
end

class DatabaseManager
  def initialize(db_path : String)
    @db = SQLite3::Database.new(db_path)
    setup_tables
  end
  
  def setup_tables
    @db.execute <<-SQL
      CREATE TABLE IF NOT EXISTS users (
        id TEXT PRIMARY KEY,
        username TEXT UNIQUE NOT NULL,
        email TEXT UNIQUE NOT NULL,
        password_hash TEXT NOT NULL,
        roles TEXT NOT NULL,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        active BOOLEAN DEFAULT 1
      );
    SQL
    
    @db.execute <<-SQL
      CREATE TABLE IF NOT EXISTS sessions (
        token TEXT PRIMARY KEY,
        user_id TEXT NOT NULL,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        expires_at TIMESTAMP NOT NULL,
        FOREIGN KEY (user_id) REFERENCES users (id)
      );
    SQL
    
    @db.execute <<-SQL
      CREATE TABLE IF NOT EXISTS audit_logs (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        user_id TEXT,
        action TEXT NOT NULL,
        details TEXT,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (user_id) REFERENCES users (id)
      );
    SQL
  end
  
  def create_user(username : String, email : String, password_hash : String) : String
    user_id = "user_#{Time.utc.to_unix}_#{rand(1000..9999)}"
    
    @db.execute(
      "INSERT INTO users (id, username, email, password_hash, roles) VALUES (?, ?, ?, ?, ?)",
      [user_id, username, email, password_hash, "user"]
    )
    
    user_id
  end
  
  def get_user_by_username(username : String) : User?
    @db.query("SELECT * FROM users WHERE username = ? AND active = 1", [username]) do |rs|
      rs.each do
        id = rs.read(String)
        username = rs.read(String)
        email = rs.read(String)
        password_hash = rs.read(String)
        roles_str = rs.read(String)
        created_at = rs.read(String)
        active = rs.read(Bool)
        
        user = User.new(id, username, email, password_hash)
        user.roles = roles_str.split(",")
        return user
      end
    end
    
    nil
  end
  
  def create_session(user_id : String, ttl : Time::Span = 24.hours) : String
    token = Random::Secure.random_bytes(32).hexstring
    expires_at = Time.utc + ttl
    
    @db.execute(
      "INSERT INTO sessions (token, user_id, expires_at) VALUES (?, ?, ?)",
      [token, user_id, expires_at.to_s]
    )
    
    token
  end
  
  def validate_session(token : String) : User?
    @db.query("SELECT * FROM sessions WHERE token = ? AND expires_at > datetime('now')", [token]) do |rs|
      rs.each do
        token = rs.read(String)
        user_id = rs.read(String)
        
        user = get_user_by_id(user_id)
        return user if user
      end
    end
    
    nil
  end
  
  def log_action(user_id : String?, action : String, details : String? = nil)
    @db.execute(
      "INSERT INTO audit_logs (user_id, action, details) VALUES (?, ?, ?)",
      [user_id, action, details]
    )
  end
  
  def get_user_by_id(user_id : String) : User?
    @db.query("SELECT * FROM users WHERE id = ? AND active = 1", [user_id]) do |rs|
      rs.each do
        id = rs.read(String)
        username = rs.read(String)
        email = rs.read(String)
        password_hash = rs.read(String)
        roles_str = rs.read(String)
        created_at = rs.read(String)
        active = rs.read(Bool)
        
        user = User.new(id, username, email, password_hash)
        user.roles = roles_str.split(",")
        return user
      end
    end
    
    nil
  end
  
  def close
    @db.close
  end
end

class JobQueue
  def initialize
    @jobs = [] of {id: String, type: String, payload: JSON::Any, status: String, created_at: Time}
    @mutex = Mutex.new
  end
  
  def enqueue(job_type : String, payload : JSON::Any) : String
    job_id = "job_#{Time.utc.to_unix}_#{rand(1000..9999)}"
    
    @mutex.synchronize do
      @jobs << {
        id: job_id,
        type: job_type,
        payload: payload,
        status: "pending",
        created_at: Time.utc
      }
    end
    
    job_id
  end
  
  def get_next_job
    @mutex.synchronize do
      pending_job = @jobs.find { |job| job[:status] == "pending" }
      return nil unless pending_job
      
      # Mark as processing
      pending_job[:status] = "processing"
      pending_job
    end
  end
  
  def complete_job(job_id : String, result : String)
    @mutex.synchronize do
      job = @jobs.find { |j| j[:id] == job_id }
      return unless job
      
      job[:status] = "completed"
    end
  end
  
  def get_stats
    @mutex.synchronize do
      {
        total: @jobs.size,
        pending: @jobs.count { |j| j[:status] == "pending" },
        processing: @jobs.count { |j| j[:status] == "processing" },
        completed: @jobs.count { |j| j[:status] == "completed" }
      }
    end
  end
end

class DataProcessor
  def initialize(@cache : ApplicationCache, @db : DatabaseManager, @job_queue : JobQueue)
  end
  
  def process_user_registration(username : String, email : String, password : String) : Tuple(Bool, String?, String?)
    # Check cache first
    cache_key = "user_exists:#{username}"
    if @cache.get(cache_key)
      return {false, "User already exists", nil}
    end
    
    # Check database
    existing_user = @db.get_user_by_username(username)
    if existing_user
      @cache.set(cache_key, {"exists" => true}, 30.minutes)
      return {false, "User already exists", nil}
    end
    
    # Hash password (simplified for demo)
    password_hash = "hashed_#{password}"
    
    # Create user
    user_id = @db.create_user(username, email, password_hash)
    
    # Log the action
    @db.log_action(user_id, "user_registration", "User #{username} registered")
    
    # Create initial session
    token = @db.create_session(user_id)
    
    {true, user_id, token}
  end
  
  def get_user_dashboard(user_id : String) : JSON::Any
    cache_key = "dashboard:#{user_id}"
    
    # Try cache first
    cached_data = @cache.get(cache_key)
    return cached_data if cached_data
    
    # Get user data
    user = @db.get_user_by_id(user_id)
    return JSON::Any.new(nil) unless user
    
    # Get job stats
    job_stats = @job_queue.get_stats
    
    # Build dashboard data
    dashboard_data = {
      "user" => {
        "id" => user.id,
        "username" => user.username,
        "email" => user.email,
        "roles" => user.roles,
        "created_at" => user.created_at.to_s
      },
      "stats" => job_stats,
      "features" => [
        "Real-time data processing",
        "Secure authentication",
        "Background job processing",
        "Caching layer",
        "Audit logging"
      ],
      "crystal_version" => "1.0.0"
    }
    
    # Cache for 5 minutes
    @cache.set(cache_key, dashboard_data, 5.minutes)
    
    dashboard_data
  end
  
  def enqueue_data_processing_job(data : JSON::Any) : String
    job_id = @job_queue.enqueue("data_processing", data)
    
    # Log the job creation
    @db.log_action(nil, "job_enqueued", "Job #{job_id} enqueued for data processing")
    
    job_id
  end
end

class APIServer
  property config : ApplicationConfig
  property cache : ApplicationCache
  property db : DatabaseManager
  property processor : DataProcessor
  property job_queue : JobQueue
  property server : HTTP::Server
  
  def initialize(@config : ApplicationConfig)
    @cache = ApplicationCache.new(config.cache_size)
    @db = DatabaseManager.new(config.database_path)
    @job_queue = JobQueue.new
    @processor = DataProcessor.new(@cache, @db, @job_queue)
    
    @server = HTTP::Server.new do |context|
      handle_request(context)
    end
    @server.bind_tcp(config.server_port)
  end
  
  def start
    puts "=== Crystal Finalizer Application ==="
    puts "Starting Crystal application server on port #{@config.server_port}..."
    
    # Create demo data
    create_demo_data
    
    puts "Server listening at http://localhost:#{@config.server_port}"
    puts "Available endpoints:"
    puts "  POST /api/register - Register new user"
    puts "  POST /api/login    - User login"
    puts "  GET  /api/dashboard - User dashboard (requires auth)"
    puts "  POST /api/jobs     - Enqueue processing job"
    puts "  GET  /api/stats    - Application statistics"
    puts "  GET  /health       - Health check"
    
    @server.listen
  end
  
  def stop
    @db.close
    puts "Server stopped"
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
    when "POST"
      handle_post(request, response)
    when "GET"
      handle_get(request, response)
    else
      response.status = HTTP::Status::METHOD_NOT_ALLOWED
      response.puts "Method not allowed"
    end
  end
  
  def handle_post(request : HTTP::Request, response : HTTP::Server::Response)
    case request.path
    when "/api/register"
      handle_register(request, response)
    when "/api/login"
      handle_login(request, response)
    when "/api/jobs"
      handle_enqueue_job(request, response)
    else
      response.status = HTTP::Status::NOT_FOUND
      response.puts "Endpoint not found"
    end
  end
  
  def handle_get(request : HTTP::Request, response : HTTP::Server::Response)
    case request.path
    when "/api/dashboard"
      handle_dashboard(request, response)
    when "/api/stats"
      handle_stats(request, response)
    when "/health"
      handle_health(response)
    else
      response.status = HTTP::Status::NOT_FOUND
      response.puts "Endpoint not found"
    end
  end
  
  def handle_register(request : HTTP::Request, response : HTTP::Server::Response)
    body = request.body.try(&.gets_to_end) || "{}"
    
    begin
      data = JSON.parse(body)
      username = data["username"]?.to_s
      email = data["email"]?.to_s
      password = data["password"]?.to_s
      
      unless username && email && password
        response.status = HTTP::Status::BAD_REQUEST
        response.content_type = "application/json"
        response.puts({"error" => "Missing required fields"}.to_json)
        return
      end
      
      success, user_id, token = @processor.process_user_registration(username, email, password)
      
      if success
        response.status = HTTP::Status::CREATED
        response.content_type = "application/json"
        response.puts({
          "message" => "User registered successfully",
          "user_id" => user_id,
          "token" => token
        }.to_json)
      else
        response.status = HTTP::Status::BAD_REQUEST
        response.content_type = "application/json"
        response.puts({"error" => user_id}.to_json)
      end
    rescue ex
      response.status = HTTP::Status::BAD_REQUEST
      response.content_type = "application/json"
      response.puts({"error" => "Invalid JSON"}.to_json)
    end
  end
  
  def handle_login(request : HTTP::Request, response : HTTP::Server::Response)
    body = request.body.try(&.gets_to_end) || "{}"
    
    begin
      data = JSON.parse(body)
      username = data["username"]?.to_s
      password = data["password"]?.to_s
      
      unless username && password
        response.status = HTTP::Status::BAD_REQUEST
        response.puts "Missing credentials"
        return
      end
      
      # Simplified authentication for demo
      user = @db.get_user_by_username(username)
      
      if user && password == "demo123" # Simplified password check
        token = @db.create_session(user.id)
        
        response.status = HTTP::Status::OK
        response.content_type = "application/json"
        response.puts({
          "message" => "Login successful",
          "token" => token,
          "user_id" => user.id
        }.to_json)
      else
        response.status = HTTP::Status::UNAUTHORIZED
        response.content_type = "application/json"
        response.puts({"error" => "Invalid credentials"}.to_json)
      end
    rescue ex
      response.status = HTTP::Status::BAD_REQUEST
      response.puts "Invalid JSON"
    end
  end
  
  def handle_dashboard(request : HTTP::Request, response : HTTP::Server::Response)
    token = get_auth_token(request)
    
    unless token
      response.status = HTTP::Status::UNAUTHORIZED
      response.puts "Missing auth token"
      return
    end
    
    user = @db.validate_session(token)
    
    unless user
      response.status = HTTP::Status::UNAUTHORIZED
      response.puts "Invalid or expired token"
      return
    end
    
    dashboard_data = @processor.get_user_dashboard(user.id)
    
    response.status = HTTP::Status::OK
    response.content_type = "application/json"
    response.puts dashboard_data.to_json
  end
  
  def handle_enqueue_job(request : HTTP::Request, response : HTTP::Server::Response)
    body = request.body.try(&.gets_to_end) || "{}"
    
    begin
      data = JSON.parse(body)
      job_data = data["data"]? || JSON::Any.new(nil)
      
      job_id = @processor.enqueue_data_processing_job(job_data)
      
      response.status = HTTP::Status::OK
      response.content_type = "application/json"
      response.puts({
        "message" => "Job enqueued successfully",
        "job_id" => job_id
      }.to_json)
    rescue ex
      response.status = HTTP::Status::BAD_REQUEST
      response.puts "Invalid JSON"
    end
  end
  
  def handle_stats(request : HTTP::Request, response : HTTP::Server::Response)
    job_stats = @job_queue.get_stats
    
    stats = {
      "server" => "Crystal Finalizer",
      "version" => "1.0.0",
      "uptime" => "running",
      "jobs" => job_stats,
      "features" => [
        "Web server with HTTP endpoints",
        "Database integration with SQLite",
        "In-memory caching",
        "Background job processing",
        "User authentication",
        "Session management",
        "Audit logging"
      ]
    }
    
    response.status = HTTP::Status::OK
    response.content_type = "application/json"
    response.puts stats.to_json
  end
  
  def handle_health(response : HTTP::Server::Response)
    health = {
      "status" => "healthy",
      "timestamp" => Time.utc.to_s,
      "version" => "1.0.0",
      "database" => "connected",
      "cache" => "operational"
    }
    
    response.status = HTTP::Status::OK
    response.content_type = "application/json"
    response.puts health.to_json
  end
  
  private def get_auth_token(request : HTTP::Request) : String?
    auth_header = request.headers["Authorization"]?
    
    if auth_header && auth_header.starts_with?("Bearer ")
      auth_header[7..-1]
    else
      request.query_params["token"]?
    end
  end
  
  private def create_demo_data
    # Create a demo user
    @db.create_user("demo", "demo@example.com", "hashed_demo123")
    
    puts "Demo data created:"
    puts "  User: demo/demo123"
  end
end

# Main Application Class
class CrystalFinalizerApp
  property config : ApplicationConfig
  property server : APIServer?
  
  def initialize
    @config = ApplicationConfig.from_env
  end
  
  def start
    puts "Starting Crystal Finalizer Application..."
    
    @server = APIServer.new(@config)
    @server.not_nil!.start
  end
  
  def stop
    @server.try(&.stop)
  end
end

# Demo usage and testing
if __FILE__ == $0
  puts "=== Crystal Finalizer - Comprehensive Application Demo ==="
  
  app = CrystalFinalizerApp.new
  
  puts "\nThis comprehensive Crystal application demonstrates:"
  puts "✓ Web server with HTTP endpoints"
  puts "✓ SQLite database integration"
  puts "✓ In-memory caching system"
  puts "✓ Background job processing"
  puts "✓ User authentication & sessions"
  puts "✓ JSON API responses"
  puts "✓ CORS support"
  puts "✓ Health monitoring"
  puts "✓ Audit logging"
  puts "✓ Configuration management"
  
  puts "\nArchitecture Components:"
  puts "• ApplicationConfig - Configuration management"
  puts "• DatabaseManager - SQLite database operations"
  puts "• ApplicationCache - In-memory caching"
  puts "• JobQueue - Background job processing"
  puts "• DataProcessor - Business logic processing"
  puts "• APIServer - HTTP API endpoints"
  puts "• User - User model and management"
  
  puts "\nTo start the application:"
  puts "  app = CrystalFinalizerApp.new"
  puts "  app.start"
  
  puts "\nDemo API calls:"
  puts "  curl -X POST http://localhost:8080/api/register \\"
  puts "    -H 'Content-Type: application/json' \\"
  puts "    -d '{\"username\":\"alice\",\"email\":\"alice@example.com\",\"password\":\"alice123\"}'"
  puts ""
  puts "  curl -X POST http://localhost:8080/api/login \\"
  puts "    -H 'Content-Type: application/json' \\"
  puts "    -d '{\"username\":\"demo\",\"password\":\"demo123\"}'"
  puts ""
  puts "  curl -X GET http://localhost:8080/api/dashboard \\"
  puts "    -H 'Authorization: Bearer YOUR_TOKEN'"
  puts ""
  puts "  curl -X GET http://localhost:8080/health"
  
  # Note: Server not started for safety in demo
  # To start: app.start
end
