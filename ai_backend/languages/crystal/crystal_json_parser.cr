# Crystal JSON Parser - Custom JSON Parsing Implementation
require "json"

class JSONParser
  def initialize
    @index = 0
    @input = ""
  end
  
  def parse(json_string : String) : JSON::Any
    @input = json_string
    @index = 0
    parse_value
  end
  
  private def parse_value : JSON::Any
    skip_whitespace
    
    case current_char
    when 'n'
      parse_null
    when 't'
      parse_true
    when 'f'
      parse_false
    when '"'
      parse_string
    when '['
      parse_array
    when '{'
      parse_object
    when '-', '0'..'9'
      parse_number
    else
      raise "Unexpected character: #{current_char}"
    end
  end
  
  private def parse_null : JSON::Any
    expect("null")
    JSON::Any.new(nil)
  end
  
  private def parse_true : JSON::Any
    expect("true")
    JSON::Any.new(true)
  end
  
  private def parse_false : JSON::Any
    expect("false")
    JSON::Any.new(false)
  end
  
  private def parse_string : JSON::Any
    expect('"')
    
    string_builder = String::Builder.new
    while current_char != '"'
      if current_char == '\\'
        advance
        escaped_char = parse_escape_sequence
        string_builder << escaped_char
      else
        string_builder << current_char
        advance
      end
    end
    
    expect('"')
    JSON::Any.new(string_builder.to_s)
  end
  
  private def parse_escape_sequence : Char
    case current_char
    when 'n'
      advance
      '\n'
    when 't'
      advance
      '\t'
    when 'r'
      advance
      '\r'
    when '\\'
      advance
      '\\'
    when '/'
      advance
      '/'
    when '"'
      advance
      '"'
    when '\''
      advance
      '\''
    else
      raise "Invalid escape sequence: \\#{current_char}"
    end
  end
  
  private def parse_array : JSON::Any
    expect('[')
    skip_whitespace
    
    elements = [] of JSON::Any
    
    if current_char == ']'
      expect(']')
      return JSON::Any.new(elements)
    end
    
    loop do
      elements << parse_value
      skip_whitespace
      
      case current_char
      when ','
        expect(',')
        skip_whitespace
      when ']'
        expect(']')
        break
      else
        raise "Expected ',' or ']' in array"
      end
    end
    
    JSON::Any.new(elements)
  end
  
  private def parse_object : JSON::Any
    expect('{')
    skip_whitespace
    
    hash = {} of String => JSON::Any
    
    if current_char == '}'
      expect('}')
      return JSON::Any.new(hash)
    end
    
    loop do
      # Parse key
      key = parse_string
      skip_whitespace
      expect(':')
      skip_whitespace
      
      # Parse value
      value = parse_value
      hash[key.as_s] = value
      
      skip_whitespace
      
      case current_char
      when ','
        expect(',')
        skip_whitespace
      when '}'
        expect('}')
        break
      else
        raise "Expected ',' or '}' in object"
      end
    end
    
    JSON::Any.new(hash)
  end
  
  private def parse_number : JSON::Any
    start = @index
    
    # Parse optional minus sign
    if current_char == '-'
      advance
    end
    
    # Parse integer part
    if current_char == '0'
      advance
    else
      while current_char >= '0' && current_char <= '9'
        advance
      end
    end
    
    # Parse optional decimal part
    if current_char == '.'
      advance
      while current_char >= '0' && current_char <= '9'
        advance
      end
    end
    
    # Parse optional exponent
    if current_char == 'e' || current_char == 'E'
      advance
      
      if current_char == '+' || current_char == '-'
        advance
      end
      
      while current_char >= '0' && current_char <= '9'
        advance
      end
    end
    
    number_string = @input[start...@index]
    JSON::Any.new(number_string.to_f64)
  end
  
  private def current_char : Char
    @input[@index]?
  end
  
  private def advance
    @index += 1
  end
  
  private def expect(char : Char)
    if current_char == char
      advance
    else
      raise "Expected '#{char}' but found '#{current_char}'"
    end
  end
  
  private def expect(string : String)
    string.each_char do |expected_char|
      if current_char == expected_char
        advance
      else
        raise "Expected '#{string}' but found '#{@input[@index...]}'"
      end
    end
  end
  
  private def skip_whitespace
    while current_char && current_char.whitespace?
      advance
    end
  end
end

# JSON Builder for generating JSON
class JSONBuilder
  def initialize
    @output = String::Builder.new
    @indentation = 0
  end
  
  def start_object
    @output << "{"
    @indentation += 1
  end
  
  def end_object
    @indentation -= 1
    @output << "}"
  end
  
  def start_array
    @output << "["
    @indentation += 1
  end
  
  def end_array
    @indentation -= 1
    @output << "]"
  end
  
  def key(key_name : String)
    @output << '"' << key_name << '": '
  end
  
  def value(value : String | Int32 | Float64 | Bool | Nil)
    case value
    when String
      @output << '"' << value.gsub('"', '\\"') << '"'
    when Int32, Float64
      @output << value.to_s
    when Bool
      @output << (value ? "true" : "false")
    when Nil
      @output << "null"
    end
  end
  
  def comma
    @output << ","
  end
  
  def new_line
    @output << "\n"
    @indentation.times { @output << "  " }
  end
  
  def to_s : String
    @output.to_s
  end
end

# JSON Validator
class JSONValidator
  def self.validate(json_string : String) : Tuple(Bool, String?)
    begin
      parser = JSONParser.new
      parser.parse(json_string)
      {true, nil}
    rescue ex
      {false, ex.message}
    end
  end
  
  def self.is_valid?(json_string : String) : Bool
    valid, _ = validate(json_string)
    valid
  end
end

# JSON Utilities
class JSONUtils
  def self.pretty_print(json_string : String, indent : String = "  ") : String
    begin
      parsed = JSON.parse(json_string)
      parsed.to_pretty_json
    rescue
      json_string # Return original if parsing fails
    end
  end
  
  def self.minify(json_string : String) : String
    json_string.gsub(/\s+/, " ").strip
  end
  
  def self.extract(json_string : String, path : String) : JSON::Any?
    begin
      parsed = JSON.parse(json_string)
      extract_path(parsed, path)
    rescue
      nil
    end
  end
  
  private def self.extract_path(data : JSON::Any, path : String) : JSON::Any?
    current = data
    path.split('.').each do |segment|
      if segment =~ /\[(\d+)\]/
        # Array index
        index = $1.to_i
        if current.as_a? && index < current.as_a.size
          current = current.as_a[index]
        else
          return nil
        end
      else
        # Object key
        if current.as_h? && current.as_h.has_key?(segment)
          current = current.as_h[segment]
        else
          return nil
        end
      end
    end
    current
  end
end

# Demo usage
if __FILE__ == $0
  puts "=== Crystal JSON Parser Demo ==="
  
  # Test JSON parsing
  test_json = %({"name": "Crystal", "version": "1.0", "features": ["fast", "type-safe", "concurrent"], "config": {"port": 8080, "debug": true}})
  
  puts "Original JSON:"
  puts test_json
  
  # Validate JSON
  valid, error = JSONValidator.validate(test_json)
  puts "\nValidation result: #{valid}"
  puts "Error: #{error}" if error
  
  # Parse JSON
  parser = JSONParser.new
  begin
    parsed = parser.parse(test_json)
    puts "\nParsed successfully!"
    puts "Name: #{parsed["name"]}"
    puts "Version: #{parsed["version"]}"
    puts "First feature: #{parsed["features"].as_a.first}"
  rescue ex
    puts "Parse error: #{ex.message}"
  end
  
  # Pretty print
  pretty = JSONUtils.pretty_print(test_json)
  puts "\nPretty printed:"
  puts pretty
  
  # Extract using path
  port = JSONUtils.extract(test_json, "config.port")
  puts "\nExtracted port: #{port}"
  
  # Build JSON
  builder = JSONBuilder.new
  builder.start_object
  builder.key("language")
  builder.value("Crystal")
  builder.comma
  builder.key("year")
  builder.value(2014)
  builder.comma
  builder.key("awesome")
  builder.value(true)
  builder.new_line
  builder.end_object
  
  built_json = builder.to_s
  puts "\nBuilt JSON:"
  puts built_json
  
  # Minify
  minified = JSONUtils.minify(pretty)
  puts "\nMinified:"
  puts minified
end
