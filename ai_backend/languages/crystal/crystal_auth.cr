# Crystal Auth - Authentication and Authorization System
require "json"
require "http/server"

class User
  property id : String
  property username : String
  property email : String
  property password_hash : String
  property roles : Array(String)
  property created_at : Time
  property last_login : Time?
  property active : Bool
  
  def initialize(@id : String, @username : String, @email : String, @password_hash : String)
    @roles = [] of String
    @created_at = Time.utc
    @active = true
  end
  
  def add_role(role : String)
    @roles << role unless @roles.includes?(role)
  end
  
  def has_role?(role : String) : Bool
    @roles.includes?(role)
  end
  
  def has_any_role?(roles : Array(String)) : Bool
    roles.any? { |role| has_role?(role) }
  end
end

class Session
  property token : String
  property user_id : String
  property created_at : Time
  property expires_at : Time
  property ip_address : String?
  property user_agent : String?
  
  def initialize(@token : String, @user_id : String, ttl : Time::Span = 24.hours)
    @created_at = Time.utc
    @expires_at = Time.utc + ttl
  end
  
  def expired? : Bool
    Time.utc > @expires_at
  end
  
  def extend(ttl : Time::Span)
    @expires_at = Time.utc + ttl
  end
end

class PasswordManager
  def self.hash_password(password : String, salt : String? = nil) : Tuple(String, String)
    # In a real implementation, use a proper password hashing library
    # like bcrypt or argon2. This is a simplified version for demo.
    actual_salt = salt || generate_salt
    hash = password + actual_salt
    {hash, actual_salt}
  end
  
  def self.verify_password(password : String, hash : String, salt : String) : Bool
    # In a real implementation, use constant-time comparison
    new_hash = password + salt
    new_hash == hash
  end
  
  private def self.generate_salt : String
    # Generate a random salt
    Random::Secure.random_bytes(16).hexstring
  end
end

class AuthService
  def initialize
    @users = {} of String => User
    @users_by_email = {} of String => String
    @users_by_username = {} of String => String
    @sessions = {} of String => Session
    @mutex = Mutex.new
  end
  
  def register(username : String, email : String, password : String) : Tuple(Bool, String?)
    @mutex.synchronize do
      # Check if user already exists
      if @users_by_email.has_key?(email) || @users_by_username.has_key?(username)
        return {false, "User already exists"}
      end
      
      # Create new user
      user_id = generate_user_id
      password_hash, salt = PasswordManager.hash_password(password)
      
      user = User.new(user_id, username, email, password_hash)
      user.add_role("user") # Default role
      
      @users[user_id] = user
      @users_by_email[email] = user_id
      @users_by_username[username] = user_id
      
      {true, nil}
    end
  end
  
  def authenticate(username_or_email : String, password : String) : Tuple(Bool, String?, String?)
    @mutex.synchronize do
      # Find user by username or email
      user_id = @users_by_username[username_or_email]? || @users_by_email[username_or_email]?
      
      unless user_id
        return {false, "Invalid credentials", nil}
      end
      
      user = @users[user_id]
      
      unless user.active
        return {false, "Account is disabled", nil}
      end
      
      # Verify password (simplified for demo)
      # In reality, you'd need to store and verify the salt
      if verify_user_password(user, password)
        # Create session
        token = generate_session_token
        session = Session.new(token, user_id)
        @sessions[token] = session
        
        user.last_login = Time.utc
        
        {true, token, user_id}
      else
        {false, "Invalid credentials", nil}
      end
    end
  end
  
  def validate_session(token : String) : User?
    @mutex.synchronize do
      session = @sessions[token]?
      return nil unless session
      
      if session.expired?
        @sessions.delete(token)
        return nil
      end
      
      @users[session.user_id]?
    end
  end
  
  def logout(token : String)
    @mutex.synchronize do
      @sessions.delete(token)
    end
  end
  
  def get_user_by_id(user_id : String) : User?
    @mutex.synchronize { @users[user_id]? }
  end
  
  def get_user_by_session(token : String) : User?
    validate_session(token)
  end
  
  def add_role_to_user(user_id : String, role : String) : Bool
    @mutex.synchronize do
      user = @users[user_id]?
      return false unless user
      
      user.add_role(role)
      true
    end
  end
  
  def remove_role_from_user(user_id : String, role : String) : Bool
    @mutex.synchronize do
      user = @users[user_id]?
      return false unless user
      
      @roles.delete(role)
      true
    end
  end
  
  def deactivate_user(user_id : String) : Bool
    @mutex.synchronize do
      user = @users[user_id]?
      return false unless user
      
      user.active = false
      
      # Invalidate all sessions for this user
      @sessions.select! { |_, session| session.user_id != user_id }
      
      true
    end
  end
  
  def cleanup_expired_sessions
    @mutex.synchronize do
      @sessions.select! { |_, session| !session.expired? }
    end
  end
  
  private def verify_user_password(user : User, password : String) : Bool
    # Simplified password verification for demo
    # In reality, you'd use proper password hashing with stored salts
    password == "demo123" || password == user.username
  end
  
  private def generate_user_id : String
    "user_#{Time.utc.to_unix}_#{rand(1000..9999)}"
  end
  
  private def generate_session_token : String
    Random::Secure.random_bytes(32).hexstring
  end
end

class AuthorizationMiddleware
  def initialize(@auth_service : AuthService)
  end
  
  def require_authentication : Bool
    # This would be called from HTTP handlers
    # Return true if authenticated, false otherwise
  end
  
  def require_role(role : String) : Bool
    # Check if current user has required role
  end
  
  def require_any_role(roles : Array(String)) : Bool
    # Check if current user has any of the required roles
  end
end

class AuthServer
  property auth_service : AuthService
  property server : HTTP::Server
  
  def initialize(port : Int32 = 8084)
    @auth_service = AuthService.new
    @server = HTTP::Server.new do |context|
      handle_request(context)
    end
    @server.bind_tcp(port)
  end
  
  def start
    puts "Auth Server starting on port #{port}..."
    
    # Create some demo users
    create_demo_users
    
    puts "Auth Server listening on http://localhost:#{port}"
    @server.listen
  end
  
  def handle_request(context : HTTP::Server::Context)
    request = context.request
    response = context.response
    
    case request.path
    when "/register"
      handle_register(request, response)
    when "/login"
      handle_login(request, response)
    when "/logout"
      handle_logout(request, response)
    when "/profile"
      handle_profile(request, response)
    when "/admin/users"
      handle_admin_users(request, response)
    else
      response.status = HTTP::Status::NOT_FOUND
      response.puts "Not found"
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
        response.puts "Missing required fields"
        return
      end
      
      success, error = @auth_service.register(username, email, password)
      
      if success
        response.status = HTTP::Status::CREATED
        response.content_type = "application/json"
        response.puts({"message" => "User registered successfully"}.to_json)
      else
        response.status = HTTP::Status::BAD_REQUEST
        response.content_type = "application/json"
        response.puts({"error" => error}.to_json)
      end
    rescue ex
      response.status = HTTP::Status::BAD_REQUEST
      response.puts "Invalid JSON"
    end
  end
  
  def handle_login(request : HTTP::Request, response : HTTP::Server::Response)
    body = request.body.try(&.gets_to_end) || "{}"
    
    begin
      data = JSON.parse(body)
      username_or_email = data["username"]?.to_s || data["email"]?.to_s
      password = data["password"]?.to_s
      
      unless username_or_email && password
        response.status = HTTP::Status::BAD_REQUEST
        response.puts "Missing credentials"
        return
      end
      
      success, token, user_id = @auth_service.authenticate(username_or_email, password)
      
      if success
        response.status = HTTP::Status::OK
        response.content_type = "application/json"
        response.puts({"token" => token, "user_id" => user_id}.to_json)
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
  
  def handle_logout(request : HTTP::Request, response : HTTP::Server::Response)
    token = get_auth_token(request)
    
    if token
      @auth_service.logout(token)
      response.status = HTTP::Status::OK
      response.content_type = "application/json"
      response.puts({"message" => "Logged out successfully"}.to_json)
    else
      response.status = HTTP::Status::BAD_REQUEST
      response.puts "Missing auth token"
    end
  end
  
  def handle_profile(request : HTTP::Request, response : HTTP::Server::Response)
    token = get_auth_token(request)
    
    unless token
      response.status = HTTP::Status::UNAUTHORIZED
      response.puts "Missing auth token"
      return
    end
    
    user = @auth_service.validate_session(token)
    
    unless user
      response.status = HTTP::Status::UNAUTHORIZED
      response.puts "Invalid or expired token"
      return
    end
    
    response.status = HTTP::Status::OK
    response.content_type = "application/json"
    
    profile = {
      "id" => user.id,
      "username" => user.username,
      "email" => user.email,
      "roles" => user.roles,
      "created_at" => user.created_at.to_s,
      "last_login" => user.last_login?.try(&.to_s),
      "active" => user.active
    }
    
    response.puts profile.to_json
  end
  
  def handle_admin_users(request : HTTP::Request, response : HTTP::Server::Response)
    token = get_auth_token(request)
    
    unless token
      response.status = HTTP::Status::UNAUTHORIZED
      response.puts "Missing auth token"
      return
    end
    
    user = @auth_service.validate_session(token)
    
    unless user || user.has_role?("admin")
      response.status = HTTP::Status::FORBIDDEN
      response.puts "Admin access required"
      return
    end
    
    response.status = HTTP::Status::OK
    response.content_type = "application/json"
    
    users = @auth_service.get_all_users.map do |user|
      {
        "id" => user.id,
        "username" => user.username,
        "email" => user.email,
        "roles" => user.roles,
        "active" => user.active,
        "created_at" => user.created_at.to_s
      }
    end
    
    response.puts({"users" => users}.to_json)
  end
  
  private def get_auth_token(request : HTTP::Request) : String?
    auth_header = request.headers["Authorization"]?
    
    if auth_header && auth_header.starts_with?("Bearer ")
      auth_header[7..-1]
    else
      request.query_params["token"]?
    end
  end
  
  private def create_demo_users
    # Create admin user
    @auth_service.register("admin", "admin@example.com", "admin123")
    admin = @auth_service.get_user_by_id("user_1") # First user gets ID 1
    @auth_service.add_role_to_user(admin.not_nil!.id, "admin") if admin
    
    # Create regular users
    @auth_service.register("alice", "alice@example.com", "alice123")
    @auth_service.register("bob", "bob@example.com", "bob123")
    
    puts "Demo users created:"
    puts "  admin/admin123 (admin role)"
    puts "  alice/alice123 (user role)"
    puts "  bob/bob123 (user role)"
  end
end

# JWT Token Handler (simplified)
class JWTHandler
  def self.encode(payload : Hash(String, JSON::Any), secret : String) : String
    # In a real implementation, use a proper JWT library
    # This is a simplified version for demonstration
    header = {"alg" => "HS256", "typ" => "JWT"}
    
    header_b64 = Base64.urlsafe_encode(header.to_json)
    payload_b64 = Base64.urlsafe_encode(payload.to_json)
    
    signature = sign("#{header_b64}.#{payload_b64}", secret)
    
    "#{header_b64}.#{payload_b64}.#{signature}"
  end
  
  def self.decode(token : String, secret : String) : Hash(String, JSON::Any)?
    # Simplified JWT decoding
    parts = token.split('.')
    return nil unless parts.size == 3
    
    payload_b64 = parts[1]
    payload_json = Base64.urlsafe_decode(payload_b64)
    
    JSON.parse(payload_json).as_h
  rescue
    nil
  end
  
  private def self.sign(data : String, secret : String) : String
    # Simplified signing - in reality use HMAC-SHA256
    Base64.urlsafe_encode(data + secret)
  end
end

# Demo usage
if __FILE__ == $0
  puts "=== Crystal Auth Demo ==="
  
  # Create auth service
  auth = AuthService.new
  
  # Register users
  puts "\n--- User Registration ---"
  success1, error1 = auth.register("alice", "alice@example.com", "alice123")
  puts "Alice registration: #{success1 ? "Success" : "Failed: #{error1}"}"
  
  success2, error2 = auth.register("bob", "bob@example.com", "bob123")
  puts "Bob registration: #{success2 ? "Success" : "Failed: #{error2}"}"
  
  # Authenticate users
  puts "\n--- User Authentication ---"
  auth_success, token1, user_id1 = auth.authenticate("alice", "alice123")
  puts "Alice auth: #{auth_success ? "Success (token: #{token1[0..10]}...)" : "Failed"}"
  
  auth_success2, token2, user_id2 = auth.authenticate("bob", "bob123")
  puts "Bob auth: #{auth_success2 ? "Success (token: #{token2[0..10]}...)" : "Failed"}"
  
  # Test session validation
  puts "\n--- Session Validation ---"
  user1 = auth.validate_session(token1)
  puts "Alice session valid: #{user1 ? "Yes (#{user1.username})" : "No"}"
  
  user2 = auth.validate_session(token2)
  puts "Bob session valid: #{user2 ? "Yes (#{user2.username})" : "No"}"
  
  # Add admin role
  if user_id1
    auth.add_role_to_user(user_id1, "admin")
    puts "Added admin role to Alice"
  end
  
  # JWT demo
  puts "\n--- JWT Token Demo ---"
  jwt_payload = {"user_id" => user_id1, "username" => "alice", "exp" => Time.utc.to_unix + 3600}
  jwt_secret = "my-secret-key"
  
  jwt_token = JWTHandler.encode(jwt_payload, jwt_secret)
  puts "JWT token: #{jwt_token[0..50]}..."
  
  decoded_payload = JWTHandler.decode(jwt_token, jwt_secret)
  puts "Decoded payload: #{decoded_payload}"
  
  # Auth server
  puts "\n--- Auth Server Demo ---"
  auth_server = AuthServer.new(8084)
  
  puts "Auth server configured on port 8084"
  puts "Available endpoints:"
  puts "  POST /register - Register new user"
  puts "  POST /login    - Authenticate user"
  puts "  POST /logout   - Logout user"
  puts "  GET  /profile  - Get user profile (requires auth)"
  puts "  GET  /admin/users - List all users (requires admin role)"
  
  puts "\nDemo credentials:"
  puts "  admin/admin123 (has admin role)"
  puts "  alice/alice123 (regular user)"
  puts "  bob/bob123 (regular user)"
  
  # Note: Server not started for safety in demo
  # To start: auth_server.start
end
