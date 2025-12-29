# Crystal Cache - In-Memory and Distributed Cache Implementation
require "json"

class CacheEntry(T)
  property key : String
  property value : T
  property created_at : Time
  property expires_at : Time?
  property access_count : Int32
  property last_accessed : Time
  
  def initialize(@key : String, @value : T, ttl : Time::Span? = nil)
    @created_at = Time.utc
    @expires_at = ttl ? Time.utc + ttl : nil
    @access_count = 0
    @last_accessed = Time.utc
  end
  
  def expired? : Bool
    return false unless @expires_at
    Time.utc > @expires_at
  end
  
  def access
    @access_count += 1
    @last_accessed = Time.utc
  end
end

class MemoryCache
  def initialize(max_size : Int32 = 1000, default_ttl : Time::Span? = nil)
    @max_size = max_size
    @default_ttl = default_ttl
    @store = {} of String => CacheEntry(JSON::Any)
    @mutex = Mutex.new
    @hits = 0
    @misses = 0
  end
  
  def set(key : String, value : JSON::Any, ttl : Time::Span? = nil)
    @mutex.synchronize do
      entry = CacheEntry(JSON::Any).new(key, value, ttl || @default_ttl)
      @store[key] = entry
      evict_if_needed
    end
  end
  
  def get(key : String) : JSON::Any?
    @mutex.synchronize do
      entry = @store[key]?
      return nil unless entry
      
      if entry.expired?
        @store.delete(key)
        @misses += 1
        return nil
      end
      
      entry.access
      @hits += 1
      entry.value
    end
  end
  
  def has_key?(key : String) : Bool
    @mutex.synchronize do
      entry = @store[key]?
      return false unless entry
      
      if entry.expired?
        @store.delete(key)
        return false
      end
      
      true
    end
  end
  
  def delete(key : String) : Bool
    @mutex.synchronize do
      @store.delete(key) != nil
    end
  end
  
  def clear
    @mutex.synchronize do
      @store.clear
    end
  end
  
  def size : Int32
    @mutex.synchronize { @store.size }
  end
  
  def hit_rate : Float64
    total = @hits + @misses
    total > 0 ? @hits.to_f64 / total : 0.0
  end
  
  def keys : Array(String)
    @mutex.synchronize { @store.keys }
  end
  
  def values : Array(JSON::Any)
    @mutex.synchronize { @store.values.map(&.value) }
  end
  
  def cleanup_expired
    @mutex.synchronize do
      expired_keys = @store.select { |_, entry| entry.expired? }.keys
      expired_keys.each { |key| @store.delete(key) }
      expired_keys.size
    end
  end
  
  def get_stats : Hash(String, String | Int32 | Float64)
    {
      "hits" => @hits,
      "misses" => @misses,
      "hit_rate" => hit_rate,
      "size" => size,
      "max_size" => @max_size
    }
  end
  
  private def evict_if_needed
    return if @store.size <= @max_size
    
    # LRU eviction
    lru_entry = @store.min_by { |_, entry| entry.last_accessed }
    @store.delete(lru_entry[0])
  end
end

class DistributedCache
  def initialize(nodes : Array(String), replication_factor : Int32 = 1)
    @nodes = nodes
    @replication_factor = replication_factor
    @local_cache = MemoryCache.new
    @mutex = Mutex.new
  end
  
  def set(key : String, value : JSON::Any, ttl : Time::Span? = nil)
    # Store locally for fast access
    @local_cache.set(key, value, ttl)
    
    # Determine which nodes should store this key
    target_nodes = get_target_nodes(key)
    
    # In a real implementation, you would send to remote nodes
    puts "Would store #{key} on nodes: #{target_nodes.join(", ")}"
  end
  
  def get(key : String) : JSON::Any?
    # Check local cache first
    value = @local_cache.get(key)
    return value if value
    
    # In a real implementation, query remote nodes
    puts "Would query remote nodes for key: #{key}"
    nil
  end
  
  def delete(key : String)
    @local_cache.delete(key)
    target_nodes = get_target_nodes(key)
    puts "Would delete #{key} from nodes: #{target_nodes.join(", ")}"
  end
  
  def get_target_nodes(key : String) : Array(String)
    hash = key.hash.abs
    target_nodes = [] of String
    
    @replication_factor.times do |i|
      node_index = (hash + i) % @nodes.size
      target_nodes << @nodes[node_index]
    end
    
    target_nodes
  end
end

class CacheDecorator
  def initialize(cache : MemoryCache)
    @cache = cache
  end
  
  def memoize(key_prefix : String = "", ttl : Time::Span? = nil)
    ->(key : String, &block : -> JSON::Any) do
      cache_key = "#{key_prefix}#{key}"
      
      # Try to get from cache
      cached_result = @cache.get(cache_key)
      return cached_result if cached_result
      
      # Compute and store
      result = block.call
      @cache.set(cache_key, result, ttl)
      result
    end
  end
  
  def cache_key(*args) : String
    args.join("_")
  end
end

class CacheServer
  property cache : MemoryCache
  property server : HTTP::Server
  
  def initialize(port : Int32 = 8083)
    @cache = MemoryCache.new(10000, 1.hour)
    @server = HTTP::Server.new do |context|
      handle_request(context)
    end
    @server.bind_tcp(port)
  end
  
  def start
    puts "Cache Server starting on port #{port}..."
    @server.listen
  end
  
  def handle_request(context : HTTP::Server::Context)
    request = context.request
    response = context.response
    
    case request.path
    when "/get"
      handle_get(request, response)
    when "/set"
      handle_set(request, response)
    when "/delete"
      handle_delete(request, response)
    when "/clear"
      handle_clear(response)
    when "/stats"
      handle_stats(response)
    when "/keys"
      handle_keys(response)
    when "/cleanup"
      handle_cleanup(response)
    else
      response.status = HTTP::Status::NOT_FOUND
      response.puts "Not found"
    end
  end
  
  def handle_get(request : HTTP::Request, response : HTTP::Server::Response)
    key = request.query_params["key"]?
    
    unless key
      response.status = HTTP::Status::BAD_REQUEST
      response.puts "Missing key parameter"
      return
    end
    
    value = @cache.get(key)
    
    if value
      response.status = HTTP::Status::OK
      response.content_type = "application/json"
      response.puts({"key" => key, "value" => value}.to_json)
    else
      response.status = HTTP::Status::NOT_FOUND
      response.content_type = "application/json"
      response.puts({"error" => "Key not found", "key" => key}.to_json)
    end
  end
  
  def handle_set(request : HTTP::Request, response : HTTP::Server::Response)
    body = request.body.try(&.gets_to_end) || "{}"
    
    begin
      data = JSON.parse(body)
      key = data["key"]?.to_s
      value = data["value"]?
      ttl = data["ttl"]?.to_s
      
      unless key && value
        response.status = HTTP::Status::BAD_REQUEST
        response.puts "Missing key or value"
        return
      end
      
      ttl_span = parse_ttl(ttl) if ttl
      @cache.set(key, value, ttl_span)
      
      response.status = HTTP::Status::OK
      response.content_type = "application/json"
      response.puts({"status" => "stored", "key" => key}.to_json)
    rescue ex
      response.status = HTTP::Status::BAD_REQUEST
      response.content_type = "application/json"
      response.puts({"error" => ex.message}.to_json)
    end
  end
  
  def handle_delete(request : HTTP::Request, response : HTTP::Server::Response)
    key = request.query_params["key"]?
    
    unless key
      response.status = HTTP::Status::BAD_REQUEST
      response.puts "Missing key parameter"
      return
    end
    
    deleted = @cache.delete(key)
    
    response.status = HTTP::Status::OK
    response.content_type = "application/json"
    response.puts({"deleted" => deleted, "key" => key}.to_json)
  end
  
  def handle_clear(response : HTTP::Server::Response)
    @cache.clear
    
    response.status = HTTP::Status::OK
    response.content_type = "application/json"
    response.puts({"status" => "cleared"}.to_json)
  end
  
  def handle_stats(response : HTTP::Server::Response)
    stats = @cache.get_stats
    
    response.status = HTTP::Status::OK
    response.content_type = "application/json"
    response.puts(stats.to_json)
  end
  
  def handle_keys(response : HTTP::Server::Response)
    keys = @cache.keys
    
    response.status = HTTP::Status::OK
    response.content_type = "application/json"
    response.puts({"keys" => keys}.to_json)
  end
  
  def handle_cleanup(response : HTTP::Server::Response)
    cleaned = @cache.cleanup_expired
    
    response.status = HTTP::Status::OK
    response.content_type = "application/json"
    response.puts({"cleaned" => cleaned, "message" => "Expired entries removed"}.to_json)
  end
  
  private def parse_ttl(ttl_string : String?) : Time::Span?
    return nil unless ttl_string
    
    case ttl_string
    when /(\d+)s/
      $1.to_i.seconds
    when /(\d+)m/
      $1.to_i.minutes
    when /(\d+)h/
      $1.to_i.hours
    when /(\d+)d/
      $1.to_i.days
    else
      nil
    end
  end
end

# Cache Strategies
module CacheStrategy
  def self.lru(cache : MemoryCache, limit : Int32)
    cache.cleanup_expired
    current_size = cache.size
    excess = current_size - limit
    
    if excess > 0
      # Remove least recently used entries
      entries = cache.keys.map { |key| {key: key, cache: cache} }
      # In a real implementation, you'd sort by last_accessed and remove excess
    end
  end
  
  def self.ttl_cleanup(cache : MemoryCache)
    cache.cleanup_expired
  end
  
  def self.fifo(cache : MemoryCache)
    # First In, First Out eviction
    # This would require tracking insertion order
  end
end

# Demo usage
if __FILE__ == $0
  puts "=== Crystal Cache Demo ==="
  
  # Create memory cache
  cache = MemoryCache.new(100, 5.minutes)
  
  # Store some data
  cache.set("user:1", {"name" => "Alice", "age" => 30})
  cache.set("user:2", {"name" => "Bob", "age" => 25})
  cache.set("session:abc123", {"user_id" => 1, "login_time" => Time.utc.to_s})
  
  # Retrieve data
  user1 = cache.get("user:1")
  puts "User 1: #{user1}"
  
  # Check if key exists
  puts "Has user:3? #{cache.has_key?("user:3")}"
  
  # Get cache statistics
  stats = cache.get_stats
  puts "Cache stats: #{stats}"
  
  # Test cache hit/miss
  cache.get("user:1") # Hit
  cache.get("user:3") # Miss
  
  puts "Hit rate: #{(cache.hit_rate * 100).round(1)}%"
  
  # Distributed cache demo
  puts "\n--- Distributed Cache Demo ---"
  nodes = ["cache-node-1", "cache-node-2", "cache-node-3"]
  dist_cache = DistributedCache.new(nodes, 2)
  
  dist_cache.set("global:counter", {"value" => 42})
  puts "Set global counter in distributed cache"
  
  # Cache decorator demo
  puts "\n--- Cache Decorator Demo ---"
  decorator = CacheDecorator.new(cache)
  expensive_operation = decorator.memoize("calc:") do
    puts "Performing expensive calculation..."
    sleep 0.1
    JSON::Any.new(Math.sqrt(rand(1..100)))
  end
  
  # First call - will compute
  result1 = expensive_operation.call("sqrt_25")
  puts "First call result: #{result1}"
  
  # Second call - will use cache
  result2 = expensive_operation.call("sqrt_25")
  puts "Second call result: #{result2}"
  
  # Cache server
  puts "\n--- Cache Server Demo ---"
  cache_server = CacheServer.new(8083)
  
  puts "Cache server configured on port 8083"
  puts "Available endpoints:"
  puts "  GET  /get?key=KEY    - Get cached value"
  puts "  POST /set            - Set cache value"
  puts "  GET  /delete?key=KEY - Delete cache entry"
  puts "  GET  /clear          - Clear all cache"
  puts "  GET  /stats          - Get cache statistics"
  puts "  GET  /keys           - List all cache keys"
  puts "  GET  /cleanup        - Remove expired entries"
  
  # Note: Server not started for safety in demo
  # To start: cache_server.start
end
