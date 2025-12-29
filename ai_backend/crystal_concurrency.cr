# Crystal Concurrency - Fibers and Channels
require "http/server"

class ConcurrencyDemo
  def initialize
    puts "Starting Crystal Concurrency Demo..."
    
    # Create multiple fibers for parallel processing
    fibers = [] of Fiber
    
    5.times do |i|
      fibers << Fiber.new do
        process_task(i)
      end
    end
    
    # Run all fibers
    fibers.each(&.resume)
    
    # Channel communication example
    channel = Channel(Int32).new
    
    Fiber.new do
      send_data_to_channel(channel, 42)
    end.resume
    
    received = channel.receive
    puts "Received from channel: #{received}"
  end
  
  def process_task(task_id : Int32)
    sleep rand(0.1..0.5) # Simulate work
    puts "Task #{task_id} completed by fiber #{Fiber.current.object_id}"
  end
  
  def send_data_to_channel(channel : Channel(Int32), data : Int32)
    sleep 0.1
    channel.send(data)
    puts "Sent #{data} to channel"
  end
end

# Advanced concurrency with shared state
class SharedDataProcessor
  @@counter = 0
  @@lock = Mutex.new
  
  def self.increment_counter
    @@lock.synchronize do
      @@counter += 1
    end
  end
  
  def self.get_counter
    @@lock.synchronize do
      @@counter
    end
  end
end

# Run concurrency demo
demo = ConcurrencyDemo.new

# Test shared state
5.times do |i|
  Fiber.new do
    SharedDataProcessor.increment_counter
    puts "Fiber #{i} incremented counter to #{SharedDataProcessor.get_counter}"
  end.resume
end

sleep 0.6 # Wait for all fibers to complete
puts "Final counter value: #{SharedDataProcessor.get_counter}"
