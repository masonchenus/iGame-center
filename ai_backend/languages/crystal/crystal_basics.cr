# Crystal Basics - Fundamental Concepts
# Crystal is a statically-type compiled language with Ruby-like syntax

class BasicTypes
  def initialize
    # Integer types
    int_val = 42
    float_val = 3.14
    bool_val = true
    string_val = "Hello Crystal"
    char_val = 'C'
    
    # Arrays
    numbers = [1, 2, 3, 4, 5]
    strings = ["hello", "world", "crystal"]
    
    # Hash/Dictionary
    config = {"name" => "Crystal", "version" => "1.0", "awesome" => true}
    
    # Method calls
    puts "Processing basic types..."
    process_int(int_val)
    process_float(float_val)
    process_string(string_val)
    process_array(numbers)
    process_hash(config)
  end
  
  def process_int(value : Int32)
    puts "Integer: #{value} (doubled: #{value * 2})"
  end
  
  def process_float(value : Float64)
    puts "Float: #{value} (squared: #{value ** 2})"
  end
  
  def process_string(value : String)
    puts "String: #{value} (uppercase: #{value.upcase})"
  end
  
  def process_array(arr : Array(Int32))
    puts "Array: #{arr.inspect} (sum: #{arr.sum})"
  end
  
  def process_hash(hash : Hash(String, String | Bool))
    puts "Hash: #{hash.inspect}"
  end
end

# Run the basic types demo
BasicTypes.new
