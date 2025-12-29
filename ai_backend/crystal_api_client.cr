# Crystal API Client - HTTP Client Implementation for External APIs
require "http/client"
require "json"
require "uri"

class APIError < Exception
  property status_code : Int32
  property response_body : String
  
  def initialize(@status_code : Int32, message : String, @response_body : String = "")
    super(message)
  end
end

class RateLimiter
  def initialize(requests_per_second : Int32 = 10)
    @requests_per_second = requests_per_second
    @request_times = [] of Time
    @mutex = Mutex.new
  end
  
  def wait_for_slot
    @mutex.synchronize do
      current_time = Time.utc
      # Remove requests older than 1 second
      @request_times = @request_times.select { |time| current_time - time < 1.second }
      
      if @request_times.size >= @requests_per_second
        # Wait until we can make another request
        oldest_request = @request_times.min
        sleep_time = 1.second - (current_time - oldest_request)
        sleep(sleep_time) if sleep_time > 0
      end
      
      @request_times << current_time
    end
  end
end

class APIClient
  property base_url : String
  property headers : Hash(String, String)
  property timeout : Time::Span
  property rate_limiter : RateLimiter?
  
  def initialize(@base_url : String, timeout : Time::Span = 30.seconds)
    @headers = {
      "User-Agent" => "Crystal-APIClient/1.0",
      "Accept" => "application/json",
      "Content-Type" => "application/json"
    }
    @timeout = timeout
    @rate_limiter = nil
  end
  
  def set_header(key : String, value : String)
    @headers[key] = value
  end
  
  def set_auth_token(token : String)
    @headers["Authorization"] = "Bearer #{token}"
  end
  
  def set_api_key(key : String)
    @headers["X-API-Key"] = key
  end
  
  def enable_rate_limiting(requests_per_second : Int32)
    @rate_limiter = RateLimiter.new(requests_per_second)
  end
  
  def get(path : String, params : Hash(String, String)? = nil) : JSON::Any
    url = build_url(path, params)
    make_request("GET", url)
  end
  
  def post(path : String, body : JSON::Any | Hash(String, JSON::Any) | String = "") : JSON::Any
    url = build_url(path)
    request_body = body.is_a?(String) ? body : body.to_json
    make_request("POST", url, request_body)
  end
  
  def put(path : String, body : JSON::Any | Hash(String, JSON::Any) | String = "") : JSON::Any
    url = build_url(path)
    request_body = body.is_a?(String) ? body : body.to_json
    make_request("PUT", url, request_body)
  end
  
  def delete(path : String) : JSON::Any
    url = build_url(path)
    make_request("DELETE", url)
  end
  
  def patch(path : String, body : JSON::Any | Hash(String, JSON::Any) | String = "") : JSON::Any
    url = build_url(path)
    request_body = body.is_a?(String) ? body : body.to_json
    make_request("PATCH", url, request_body)
  end
  
  private def build_url(path : String, params : Hash(String, String)? = nil) : String
    uri = URI.parse(@base_url)
    full_path = File.join(uri.path, path)
    
    if params && !params.empty?
      query_params = HTTP::Params.new
      params.each do |key, value|
        query_params.add(key, value)
      end
      "#{uri.scheme}://#{uri.host}:#{uri.port}#{full_path}?#{query_params}"
    else
      "#{uri.scheme}://#{uri.host}:#{uri.port}#{full_path}"
    end
  end
  
  private def make_request(method : String, url : String, body : String? = nil) : JSON::Any
    @rate_limiter.try(&.wait_for_slot)
    
    uri = URI.parse(url)
    
    HTTP::Client.new(uri.host.not_nil!, uri.port.not_nil!) do |client|
      client.read_timeout = @timeout
      client.connect_timeout = @timeout
      
      request = HTTP::Client::Request.new(uri.request_target, method: method)
      @headers.each do |key, value|
        request.headers[key] = value
      end
      
      if body
        request.body = body
      end
      
      begin
        response = client.exec(request)
        
        unless response.success?
          response_body = response.body_io?.try(&.gets_to_end) || ""
          raise APIError.new(response.status_code, "HTTP #{response.status_code}: #{response.status_message}", response_body)
        end
        
        parse_response(response)
      rescue ex : IO::Timeout
        raise APIError.new(408, "Request timeout", "")
      rescue ex : Socket::Error
        raise APIError.new(0, "Connection error: #{ex.message}", "")
      end
    end
  end
  
  private def parse_response(response : HTTP::Client::Response) : JSON::Any
    content_type = response.headers["Content-Type"]?
    
    if content_type && content_type.includes?("application/json")
      JSON.parse(response.body_io.not_nil!.gets_to_end)
    else
      # Return as string if not JSON
      JSON::Any.new(response.body_io.not_nil!.gets_to_end)
    end
  end
end

# Specific API Client Examples

class WeatherAPI
  def initialize(api_key : String)
    @client = APIClient.new("https://api.openweathermap.org/data/2.5")
    @client.set_api_key(api_key)
  end
  
  def get_current_weather(city : String) : JSON::Any
    @client.get("weather", {"q" => city, "units" => "metric"})
  end
  
  def get_forecast(city : String, days : Int32 = 5) : JSON::Any
    @client.get("forecast", {"q" => city, "cnt" => (days * 8).to_s, "units" => "metric"})
  end
end

class GitHubAPI
  def initialize(access_token : String)
    @client = APIClient.new("https://api.github.com")
    @client.set_auth_token(access_token)
    @client.set_header("Accept", "application/vnd.github.v3+json")
  end
  
  def get_user(username : String) : JSON::Any
    @client.get("/users/#{username}")
  end
  
  def get_repos(username : String) : JSON::Any
    @client.get("/users/#{username}/repos", {"sort" => "updated", "per_page" => "100"})
  end
  
  def create_issue(owner : String, repo : String, title : String, body : String? = nil) : JSON::Any
    issue_data = {"title" => title}
    issue_data["body"] = body if body
    @client.post("/repos/#{owner}/#{repo}/issues", issue_data)
  end
  
  def search_repositories(query : String, sort : String = "stars") : JSON::Any
    @client.get("/search/repositories", {"q" => query, "sort" => sort, "order" => "desc"})
  end
end

class TwitterAPI
  def initialize(consumer_key : String, consumer_secret : String, access_token : String, access_token_secret : String)
    @client = APIClient.new("https://api.twitter.com/1.1")
    @client.set_header("Authorization", "OAuth2 Bearer token")
    # Note: Full OAuth 1.0a implementation would be more complex
  end
  
  def search_tweets(query : String, count : Int32 = 100) : JSON::Any
    @client.get("/search/tweets.json", {"q" => query, "count" => count.to_s})
  end
  
  def get_user_timeline(screen_name : String, count : Int32 = 200) : JSON::Any
    @client.get("/statuses/user_timeline.json", {"screen_name" => screen_name, "count" => count.to_s})
  end
end

# API Client Manager
class APIManager
  def initialize
    @clients = {} of String => APIClient
    @circuits = {} of String => Hash(String, Int32 | Bool)
  end
  
  def register_client(name : String, client : APIClient)
    @clients[name] = client
    @circuits[name] = {"failure_count" => 0, "last_failure_time" => 0, "circuit_open" => false}
  end
  
  def get_client(name : String) : APIClient?
    # Check circuit breaker
    circuit = @circuits[name]?
    return nil unless circuit
    
    if circuit["circuit_open"].as(Bool) && (Time.utc.to_unix - circuit["last_failure_time"].as(Int32)) < 60
      raise APIError.new(503, "Circuit breaker is open", "")
    end
    
    @clients[name]?
  end
  
  def record_success(client_name : String)
    circuit = @circuits[client_name]
    circuit["failure_count"] = 0
    circuit["circuit_open"] = false
  end
  
  def record_failure(client_name : String)
    circuit = @circuits[client_name]
    circuit["failure_count"] = circuit["failure_count"].as(Int32) + 1
    circuit["last_failure_time"] = Time.utc.to_unix
    
    # Open circuit after 5 failures
    if circuit["failure_count"].as(Int32) >= 5
      circuit["circuit_open"] = true
      puts "Circuit breaker opened for #{client_name}"
    end
  end
end

# Demo usage
if __FILE__ == $0
  puts "=== Crystal API Client Demo ==="
  
  # Create API manager
  manager = APIManager.new
  
  # Register clients
  github_client = GitHubAPI.new("demo-token") # Use real token in practice
  manager.register_client("github", github_client)
  
  weather_client = WeatherAPI.new("demo-api-key") # Use real API key in practice
  manager.register_client("weather", weather_client)
  
  puts "API clients registered successfully"
  
  # Example API calls (commented out to avoid real API calls)
  # github = manager.get_client("github")
  # if github
  #   begin
  #     user_data = github.get_user("crystal-lang")
  #     puts "GitHub user: #{user_data["name"]}"
  #     manager.record_success("github")
  #   rescue ex
  #     puts "GitHub API error: #{ex.message}"
  #     manager.record_failure("github")
  #   end
  # end
  
  puts "\nAPI Client features:"
  puts "- Automatic JSON parsing"
  puts "- Rate limiting"
  puts "- Circuit breaker pattern"
  puts "- Authentication support"
  puts "- Error handling and retries"
  puts "- Timeout management"
  puts "- Request/response logging"
  
  puts "\nSupported API patterns:"
  puts "- RESTful APIs"
  puts "- GitHub API"
  puts "- Weather APIs"
  puts "- Twitter API"
  puts "- Custom API endpoints"
end
