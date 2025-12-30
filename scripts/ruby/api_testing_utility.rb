#!/usr/bin/env ruby
# frozen_string_literal: true

require 'net/http'
require 'json'
require 'timeout'

# API Testing Utility
# Provides automated API testing and validation functions
class APITestingUtility
  def initialize(config_file = 'config/api_tests.json')
    @config = load_config(config_file)
    @test_results = []
  end

  def load_config(config_file)
    return {} unless File.exist?(config_file)
    JSON.parse(File.read(config_file))
  rescue JSON::ParserError
    {}
  end

  def run_api_tests(test_suite = 'default')
    puts "Running API test suite: #{test_suite}"
    
    suite_config = @config[test_suite] || default_test_config
    test_results = []
    
    suite_config['tests'].each do |test|
      result = run_single_test(test)
      test_results << result
    end
    
    @test_results.concat(test_results)
    generate_test_report(test_results)
  end

  def run_single_test(test_config)
    test_name = test_config['name'] || 'unnamed_test'
    puts "Running test: #{test_name}"
    
    result = {
      test_name: test_name,
      start_time: Time.now.to_s,
      status: 'running'
    }
    
    begin
      response = make_api_request(test_config)
      
      # Validate response
      validation_result = validate_response(response, test_config)
      
      result[:status] = validation_result[:valid] ? 'passed' : 'failed'
      result[:response_time] = validation_result[:response_time]
      result[:status_code] = response.code.to_i if response
      result[:validations] = validation_result[:validations]
      result[:error] = validation_result[:error] if validation_result[:error]
      
    rescue => e
      result[:status] = 'error'
      result[:error] = e.message
    end
    
    result[:end_time] = Time.now.to_s
    puts "Test #{test_name}: #{result[:status]}"
    result
  end

  def make_api_request(test_config)
    uri = URI(test_config['url'])
    request = build_request(uri, test_config)
    
    timeout = test_config['timeout'] || 30
    
    Timeout.timeout(timeout) do
      Net::HTTP.start(uri.host, uri.port, use_ssl: uri.scheme == 'https') do |http|
        http.request(request)
      end
    end
  end

  def build_request(uri, test_config)
    method = test_config['method'] || 'GET'
    request_class = case method.upcase
    when 'GET'    then Net::HTTP::Get
    when 'POST'   then Net::HTTP::Post
    when 'PUT'    then Net::HTTP::Put
    when 'DELETE' then Net::HTTP::Delete
    when 'PATCH'  then Net::HTTP::Patch
    else Net::HTTP::Get
    end
    
    request = request_class.new(uri)
    
    # Add headers
    if test_config['headers']
      test_config['headers'].each do |key, value|
        request[key] = value
      end
    end
    
    # Add body
    if test_config['body']
      request.body = test_config['body'].to_json
      request['Content-Type'] = 'application/json'
    end
    
    request
  end

  def validate_response(response, test_config)
    validations = []
    start_time = Time.now
    
    # Status code validation
    expected_status = test_config['expected_status'] || 200
    if response.code.to_i == expected_status
      validations << { type: 'status_code', passed: true, expected: expected_status, actual: response.code.to_i }
    else
      validations << { type: 'status_code', passed: false, expected: expected_status, actual: response.code.to_i }
    end
    
    # Response time validation
    max_time = test_config['max_response_time'] || 5000
    response_time = ((Time.now - start_time) * 1000).round(2)
    if response_time <= max_time
      validations << { type: 'response_time', passed: true, max_time: max_time, actual: response_time }
    else
      validations << { type: 'response_time', passed: false, max_time: max_time, actual: response_time }
    end
    
    # Content validation
    if test_config['content_checks']
      content_checks = parse_json_response(response)
      test_config['content_checks'].each do |check|
        validation_result = perform_content_check(content_checks, check)
        validations << validation_result
      end
    end
    
    # Header validation
    if test_config['header_checks']
      test_config['header_checks'].each do |header, expected_value|
        actual_value = response[header]
        passed = actual_value == expected_value
        validations << {
          type: 'header',
          name: header,
          passed: passed,
          expected: expected_value,
          actual: actual_value
        }
      end
    end
    
    all_passed = validations.all? { |v| v[:passed] }
    {
      valid: all_passed,
      validations: validations,
      response_time: response_time
    }
  rescue => e
    { valid: false, error: e.message, validations: [] }
  end

  def perform_content_check(content, check)
    case check['type']
    when 'field_exists'
      field_value = get_nested_value(content, check['path'])
      passed = !field_value.nil?
      {
        type: 'field_exists',
        path: check['path'],
        passed: passed,
        actual_value: field_value
      }
    when 'field_equals'
      field_value = get_nested_value(content, check['path'])
      passed = field_value == check['expected']
      {
        type: 'field_equals',
        path: check['path'],
        passed: passed,
        expected: check['expected'],
        actual: field_value
      }
    when 'field_contains'
      field_value = get_nested_value(content, check['path'])
      passed = field_value.to_s.include?(check['expected'].to_s)
      {
        type: 'field_contains',
        path: check['path'],
        passed: passed,
        expected: check['expected'],
        actual: field_value
      }
    when 'field_matches'
      field_value = get_nested_value(content, check['path'])
      passed = field_value.to_s.match?(Regexp.new(check['pattern']))
      {
        type: 'field_matches',
        path: check['path'],
        passed: passed,
        pattern: check['pattern'],
        actual: field_value
      }
    else
      { type: 'unknown', passed: false, error: "Unknown check type: #{check['type']}" }
    end
  end

  def parse_json_response(response)
    JSON.parse(response.body)
  rescue JSON::ParserError
    response.body
  end

  def get_nested_value(obj, path)
    keys = path.split('.')
    current = obj
    
    keys.each do |key|
      return nil unless current.is_a?(Hash) && current.key?(key)
      current = current[key]
    end
    
    current
  end

  def load_test_suite(suite_file)
    JSON.parse(File.read(suite_file))
  rescue => e
    puts "Error loading test suite: #{e.message}"
    {}
  end

  def create_performance_benchmark(api_config, iterations = 100)
    puts "Running performance benchmark with #{iterations} iterations"
    
    results = []
    
    iterations.times do |i|
      start_time = Time.now
      begin
        response = make_api_request(api_config)
        end_time = Time.now
        response_time = (end_time - start_time) * 1000
        
        results << {
          iteration: i + 1,
          status_code: response.code.to_i,
          response_time: response_time.round(2),
          success: response.code.to_i < 400
        }
      rescue => e
        results << {
          iteration: i + 1,
          status_code: 0,
          response_time: 0,
          success: false,
          error: e.message
        }
      end
    end
    
    # Calculate statistics
    successful_results = results.select { |r| r[:success] }
    response_times = successful_results.map { |r| r[:response_time] }
    
    benchmark_result = {
      total_iterations: iterations,
      successful_requests: successful_results.length,
      failed_requests: results.length - successful_results.length,
      success_rate: (successful_results.length.to_f / iterations * 100).round(2),
      response_time_stats: calculate_response_time_stats(response_times),
      raw_results: results
    }
    
    puts "Benchmark completed - Success rate: #{benchmark_result[:success_rate]}%"
    benchmark_result
  end

  def calculate_response_time_stats(response_times)
    return {} if response_times.empty?
    
    sorted_times = response_times.sort
    {
      min: sorted_times.first,
      max: sorted_times.last,
      average: (response_times.sum / response_times.length).round(2),
      median: sorted_times.length.even? ? 
        (sorted_times[sorted_times.length/2 - 1] + sorted_times[sorted_times.length/2]) / 2.0 : 
        sorted_times[sorted_times.length/2],
      p95: sorted_times[(sorted_times.length * 0.95).to_i],
      p99: sorted_times[(sorted_times.length * 0.99).to_i]
    }
  end

  def test_api_endpoints(endpoints_config)
    puts "Testing multiple API endpoints"
    
    results = []
    
    endpoints_config.each do |endpoint|
      test_config = {
        'name' => endpoint['name'],
        'url' => endpoint['url'],
        'method' => endpoint['method'] || 'GET',
        'headers' => endpoint['headers'],
        'body' => endpoint['body'],
        'expected_status' => endpoint['expected_status'] || 200
      }
      
      result = run_single_test(test_config)
      results << result
    end
    
    results
  end

  def generate_test_report(test_results, output_file = 'api_test_report.json')
    report = {
      generated_at: Time.now.to_s,
      total_tests: test_results.length,
      passed_tests: test_results.count { |r| r[:status] == 'passed' },
      failed_tests: test_results.count { |r| r[:status] == 'failed' },
      error_tests: test_results.count { |r| r[:status] == 'error' },
      success_rate: (test_results.count { |r| r[:status] == 'passed' }.to_f / test_results.length * 100).round(2),
      test_results: test_results
    }
    
    File.write(output_file, JSON.pretty_generate(report))
    puts "Test report generated: #{output_file}"
    report
  end

  private

  def default_test_config
    {
      'tests' => [
        {
          'name' => 'Health Check',
          'url' => 'http://localhost:3000/health',
          'method' => 'GET',
          'expected_status' => 200,
          'content_checks' => [
            { 'type' => 'field_exists', 'path' => 'status' }
          ]
        }
      ]
    }
  end
end

# CLI Usage
if __FILE__ == $PROGRAM_NAME
  api_test = APITestingUtility.new
  
  command = ARGV[0]
  case command
  when 'run'
    api_test.run_api_tests(ARGV[1] || 'default')
  when 'benchmark'
    api_config = JSON.parse(File.read(ARGV[1]))
    puts api_test.create_performance_benchmark(api_config, (ARGV[2] || 100).to_i).to_json
  when 'endpoints'
    endpoints_config = JSON.parse(File.read(ARGV[1]))
    puts api_test.test_api_endpoints(endpoints_config).to_json
  when 'single'
    test_config = JSON.parse(File.read(ARGV[1]))
    puts api_test.run_single_test(test_config).to_json
  when 'load-suite'
    puts api_test.load_test_suite(ARGV[1]).to_json
  else
    puts "Usage: ruby api_testing_utility.rb <command> [args]"
    puts "Commands: run, benchmark, endpoints, single, load-suite"
    puts "Examples:"
    puts "  ruby api_testing_utility.rb run default"
    puts "  ruby api_testing_utility.rb benchmark test_config.json 100"
  end
end
