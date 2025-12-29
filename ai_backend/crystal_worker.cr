# Crystal Worker - Background Job Processing System
require "http/server"
require "json"
require "uri"

class Job
  property id : String
  property type : String
  property payload : JSON::Any
  property status : String
  property created_at : Time
  property started_at : Time?
  property completed_at : Time?
  property retry_count : Int32
  property max_retries : Int32
  property result : String?
  property error : String?
  
  def initialize(@id : String, @type : String, @payload : JSON::Any)
    @status = "pending"
    @created_at = Time.utc
    @retry_count = 0
    @max_retries = 3
  end
  
  def mark_started
    @status = "running"
    @started_at = Time.utc
  end
  
  def mark_completed(result : String)
    @status = "completed"
    @completed_at = Time.utc
    @result = result
  end
  
  def mark_failed(error : String)
    @status = "failed"
    @completed_at = Time.utc
    @error = error
  end
  
  def can_retry? : Bool
    @retry_count < @max_retries
  end
  
  def increment_retry
    @retry_count += 1
    @status = "pending"
    @started_at = nil
    @completed_at = nil
    @error = nil
  end
end

class JobProcessor
  def initialize
    @jobs = {} of String => Job
    @queue = Array(String).new
    @processing = Set(String).new
    @mutex = Mutex.new
    @worker_count = 0
  end
  
  def enqueue(job_type : String, payload : JSON::Any) : String
    job_id = generate_job_id
    job = Job.new(job_id, job_type, payload)
    
    @mutex.synchronize do
      @jobs[job_id] = job
      @queue << job_id
    end
    
    puts "Job enqueued: #{job_id} (#{job_type})"
    job_id
  end
  
  def dequeue : Job?
    job_id = nil
    
    @mutex.synchronize do
      return nil if @queue.empty?
      job_id = @queue.shift
    end
    
    return nil unless job_id
    
    job = @jobs[job_id]?
    return nil unless job
    
    @mutex.synchronize do
      if @processing.includes?(job_id)
        return nil
      end
      @processing.add(job_id)
    end
    
    job.mark_started
    job
  end
  
  def complete_job(job_id : String, result : String)
    @mutex.synchronize do
      job = @jobs[job_id]?
      return unless job
      
      job.mark_completed(result)
      @processing.delete(job_id)
    end
    
    puts "Job completed: #{job_id}"
  end
  
  def fail_job(job_id : String, error : String)
    @mutex.synchronize do
      job = @jobs[job_id]?
      return unless job
      
      if job.can_retry?
        job.increment_retry
        @queue << job_id
        puts "Job retry scheduled: #{job_id} (attempt #{job.retry_count})"
      else
        job.mark_failed(error)
        @processing.delete(job_id)
        puts "Job failed permanently: #{job_id} - #{error}"
      end
    end
  end
  
  def get_job(job_id : String) : Job?
    @jobs[job_id]?
  end
  
  def get_queue_size : Int32
    @mutex.synchronize { @queue.size }
  end
  
  def get_processing_count : Int32
    @mutex.synchronize { @processing.size }
  end
  
  def get_all_jobs : Array(Job)
    @jobs.values.to_a
  end
  
  def get_jobs_by_status(status : String) : Array(Job)
    @jobs.values.select { |job| job.status == status }
  end
  
  private def generate_job_id : String
    "job_#{Time.utc.to_unix}_#{rand(1000..9999)}"
  end
end

class JobWorker
  property processor : JobProcessor
  property worker_id : String
  property running : Bool
  
  def initialize(@processor : JobProcessor)
    @worker_id = "worker_#{rand(1000..9999)}"
    @running = false
  end
  
  def start
    @running = true
    puts "#{@worker_id} starting..."
    
    spawn do
      loop do
        break unless @running
        
        job = @processor.dequeue
        if job
          process_job(job)
        else
          sleep 0.1
        end
      end
    end
  end
  
  def stop
    @running = false
    puts "#{@worker_id} stopping..."
  end
  
  private def process_job(job : Job)
    puts "#{@worker_id} processing job #{job.id} (#{job.type})"
    
    begin
      result = execute_job(job)
      @processor.complete_job(job.id, result)
    rescue ex
      error_message = "Error in #{job.type}: #{ex.message}"
      @processor.fail_job(job.id, error_message)
    end
  end
  
  private def execute_job(job : Job) : String
    case job.type
    when "send_email"
      execute_email_job(job.payload)
    when "generate_report"
      execute_report_job(job.payload)
    when "data_processing"
      execute_data_job(job.payload)
    when "image_processing"
      execute_image_job(job.payload)
    when "api_call"
      execute_api_job(job.payload)
    when "cleanup"
      execute_cleanup_job(job.payload)
    else
      raise "Unknown job type: #{job.type}"
    end
  end
  
  private def execute_email_job(payload : JSON::Any) : String
    to = payload["to"]?.to_s
    subject = payload["subject"]?.to_s
    body = payload["body"]?.to_s
    
    # Simulate email sending
    sleep rand(0.1..0.5)
    
    if rand(0..1) == 0 # 50% success rate for demo
      raise "SMTP connection failed"
    end
    
    "Email sent successfully to #{to} with subject '#{subject}'"
  end
  
  private def execute_report_job(payload : JSON::Any) : String
    report_type = payload["type"]?.to_s || "summary"
    data_source = payload["data_source"]?.to_s || "database"
    
    # Simulate report generation
    sleep rand(0.5..2.0)
    
    "Report '#{report_type}' generated from #{data_source} in #{rand(100..999)}ms"
  end
  
  private def execute_data_job(payload : JSON::Any) : String
    operation = payload["operation"]?.to_s
    input_data = payload["data"]
    
    # Simulate data processing
    sleep rand(0.2..1.0)
    
    case operation
    when "transform"
      "Data transformation completed: #{input_data.as_a?.try(&.size) || 0} records processed"
    when "validate"
      "Data validation completed: all #{input_data.as_a?.try(&.size) || 0} records valid"
    when "aggregate"
      "Data aggregation completed: summary statistics calculated"
    else
      "Data operation '#{operation}' completed"
    end
  end
  
  private def execute_image_job(payload : JSON::Any) : String
    image_path = payload["path"]?.to_s
    operation = payload["operation"]?.to_s || "resize"
    
    # Simulate image processing
    sleep rand(0.5..3.0)
    
    "Image #{operation} completed for #{image_path}"
  end
  
  private def execute_api_job(payload : JSON::Any) : String
    endpoint = payload["endpoint"]?.to_s
    method = payload["method"]?.to_s || "GET"
    
    # Simulate API call
    sleep rand(0.1..1.0)
    
    "API #{method} call to #{endpoint} completed successfully"
  end
  
  private def execute_cleanup_job(payload : JSON::Any) : String
    target = payload["target"]?.to_s
    
    # Simulate cleanup operation
    sleep rand(0.2..0.8)
    
    "Cleanup operation completed for #{target}"
  end
end

class JobServer
  property processor : JobProcessor
  property workers : Array(JobWorker)
  property server : HTTP::Server
  
  def initialize(port : Int32 = 8082)
    @processor = JobProcessor.new
    @workers = [] of JobWorker
    @server = HTTP::Server.new do |context|
      handle_request(context)
    end
    @server.bind_tcp(port)
  end
  
  def start(worker_count : Int32 = 3)
    puts "Job Server starting with #{worker_count} workers..."
    
    # Start workers
    worker_count.times do |i|
      worker = JobWorker.new(@processor)
      worker.start
      @workers << worker
    end
    
    puts "Job Server listening on http://localhost:8082"
    @server.listen
  end
  
  def stop
    @workers.each(&.stop)
    puts "Job Server stopped"
  end
  
  def handle_request(context : HTTP::Server::Context)
    request = context.request
    response = context.response
    
    case request.path
    when "/enqueue"
      handle_enqueue(request, response)
    when "/status"
      handle_status(response)
    when "/jobs"
      handle_jobs_list(response)
    when "/job"
      handle_job_detail(request, response)
    else
      response.status = HTTP::Status::NOT_FOUND
      response.puts "Not found"
    end
  end
  
  def handle_enqueue(request : HTTP::Request, response : HTTP::Server::Response)
    body = request.body.try(&.gets_to_end) || "{}"
    
    begin
      data = JSON.parse(body)
      job_type = data["type"]?.to_s || "unknown"
      payload = data["payload"]? || JSON::Any.new(nil)
      
      job_id = @processor.enqueue(job_type, payload)
      
      response.status = HTTP::Status::OK
      response.content_type = "application/json"
      response.puts({"job_id" => job_id, "status" => "enqueued"}.to_json)
    rescue ex
      response.status = HTTP::Status::BAD_REQUEST
      response.content_type = "application/json"
      response.puts({"error" => ex.message}.to_json)
    end
  end
  
  def handle_status(response : HTTP::Server::Response)
    response.status = HTTP::Status::OK
    response.content_type = "application/json"
    
    status_data = {
      "queue_size" => @processor.get_queue_size,
      "processing" => @processor.get_processing_count,
      "workers" => @workers.size,
      "total_jobs" => @processor.get_all_jobs.size,
      "pending" => @processor.get_jobs_by_status("pending").size,
      "running" => @processor.get_jobs_by_status("running").size,
      "completed" => @processor.get_jobs_by_status("completed").size,
      "failed" => @processor.get_jobs_by_status("failed").size
    }
    
    response.puts status_data.to_json
  end
  
  def handle_jobs_list(response : HTTP::Server::Response)
    response.status = HTTP::Status::OK
    response.content_type = "application/json"
    
    jobs = @processor.get_all_jobs.map do |job|
      {
        "id" => job.id,
        "type" => job.type,
        "status" => job.status,
        "created_at" => job.created_at.to_s,
        "started_at" => job.started_at?.try(&.to_s),
        "completed_at" => job.completed_at?.try(&.to_s),
        "retry_count" => job.retry_count,
        "error" => job.error
      }
    end
    
    response.puts({"jobs" => jobs}.to_json)
  end
  
  def handle_job_detail(request : HTTP::Request, response : HTTP::Server::Response)
    job_id = request.query_params["id"]?
    
    unless job_id
      response.status = HTTP::Status::BAD_REQUEST
      response.puts "Missing job id"
      return
    end
    
    job = @processor.get_job(job_id)
    
    unless job
      response.status = HTTP::Status::NOT_FOUND
      response.puts "Job not found"
      return
    end
    
    response.status = HTTP::Status::OK
    response.content_type = "application/json"
    
    job_data = {
      "id" => job.id,
      "type" => job.type,
      "payload" => job.payload,
      "status" => job.status,
      "created_at" => job.created_at.to_s,
      "started_at" => job.started_at?.try(&.to_s),
      "completed_at" => job.completed_at?.try(&.to_s),
      "retry_count" => job.retry_count,
      "result" => job.result,
      "error" => job.error
    }
    
    response.puts job_data.to_json
  end
end

# Demo usage
if __FILE__ == $0
  puts "=== Crystal Worker Demo ==="
  
  # Create job server
  job_server = JobServer.new(8082)
  
  # Enqueue some demo jobs
  processor = job_server.processor
  
  # Email job
  processor.enqueue("send_email", {
    "to" => "user@example.com",
    "subject" => "Welcome!",
    "body" => "Thank you for signing up."
  })
  
  # Report job
  processor.enqueue("generate_report", {
    "type" => "monthly_sales",
    "data_source" => "sales_db"
  })
  
  # Data processing job
  processor.enqueue("data_processing", {
    "operation" => "transform",
    "data" => [1, 2, 3, 4, 5]
  })
  
  puts "\nDemo jobs enqueued:"
  puts "- send_email: Welcome email"
  puts "- generate_report: Monthly sales report"
  puts "- data_processing: Transform operation"
  
  puts "\nAvailable job types:"
  puts "- send_email: Send email notifications"
  puts "- generate_report: Generate various reports"
  puts "- data_processing: Transform/validate/aggregate data"
  puts "- image_processing: Resize/process images"
  puts "- api_call: Make external API calls"
  puts "- cleanup: Perform cleanup operations"
  
  puts "\nHTTP API endpoints:"
  puts "- POST /enqueue: Enqueue a new job"
  puts "- GET  /status: Get job queue status"
  puts "- GET  /jobs: List all jobs"
  puts "- GET  /job?id=JOB_ID: Get job details"
  
  # Note: Server not started for safety in demo
  # To start: job_server.start(3)
end
