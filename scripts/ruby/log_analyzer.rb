#!/usr/bin/env ruby
# frozen_string_literal: true

require 'time'
require 'set'

# Log Analyzer Utility
# Analyzes game center logs for patterns and errors
class LogAnalyzer
  LOG_LEVELS = %w[DEBUG INFO WARN ERROR FATAL].freeze
  ERROR_PATTERNS = [
    /error/i,
    /exception/i,
    /failed/i,
    /timeout/i,
    /connection.*refused/i,
    /out of memory/i
  ].freeze

  def initialize(log_file = nil)
    @log_file = log_file
    @errors = []
    @warnings = []
    @stats = {}
  end

  def analyze_log(file_path = nil)
    target_file = file_path || @log_file
    return unless target_file && File.exist?(target_file)

    File.readlines(target_file).each do |line|
      parse_log_entry(line)
    end

    generate_statistics
    report_results
  end

  def parse_log_entry(line)
    timestamp = extract_timestamp(line)
    level = extract_log_level(line)
    message = extract_message(line)

    entry = {
      timestamp: timestamp,
      level: level,
      message: message,
      raw_line: line.chomp
    }

    case level
    when 'ERROR', 'FATAL'
      @errors << entry
    when 'WARN'
      @warnings << entry
    end
  end

  def extract_timestamp(line)
    # Common timestamp patterns
    patterns = [
      /(\d{4}-\d{2}-\d{2}[T ]\d{2}:\d{2}:\d{2}(?:\.\d+)?(?:Z|[+-]\d{2}:\d{2})?)/,
      /(\d{2}\/\d{2}\/\d{4} \d{2}:\d{2}:\d{2})/,
      /\[(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2})\]/
    ]

    patterns.each do |pattern|
      match = line.match(pattern)
      return match[1] if match
    end

    Time.now.to_s
  end

  def extract_log_level(line)
    LOG_LEVELS.each do |level|
      return level if line.match?(/\b#{level}\b/)
    end
    'UNKNOWN'
  end

  def extract_message(line)
    # Extract message after timestamp and level
    line.gsub(/^\s*\d{4}-\d{2}-\d{2}[T ]\d{2}:\d{2}:\d{2}.*?\s+/, '')
      .gsub(/^\s*\[.*?\]\s*/, '')
      .strip
  end

  def generate_statistics
    @stats = {
      total_lines: @errors.length + @warnings.length + @stats[:info_lines] || 0,
      errors: @errors.length,
      warnings: @warnings.length,
      error_rate: (@errors.length.to_f / (@errors.length + @warnings.length)) * 100,
      most_common_errors: find_most_common_errors
    }
  end

  def find_most_common_errors
    error_messages = @errors.map { |e| e[:message] }
    frequency = Hash.new(0)
    error_messages.each { |msg| frequency[msg] += 1 }
    frequency.sort_by { |_k, v| -v }.first(5).to_h
  end

  def report_results
    puts "=== Log Analysis Report ==="
    puts "Total Lines Analyzed: #{@stats[:total_lines]}"
    puts "Errors Found: #{@stats[:errors]}"
    puts "Warnings Found: #{@stats[:warnings]}"
    puts "Error Rate: #{@stats[:error_rate].round(2)}%"
    puts "\nMost Common Errors:"
    @stats[:most_common_errors].each do |error, count|
      puts "  #{count}x: #{error[0..100]}..."
    end
  end

  def export_errors(output_file)
    error_data = {
      analysis_time: Time.now.to_s,
      summary: @stats,
      errors: @errors,
      warnings: @warnings
    }
    
    File.write(output_file, JSON.pretty_generate(error_data))
    puts "Error report exported to #{output_file}"
  end

  def find_errors_by_time_range(start_time, end_time)
    @errors.select do |error|
      error_time = Time.parse(error[:timestamp])
      error_time >= start_time && error_time <= end_time
    end
  end
end

# CLI Usage
if __FILE__ == $PROGRAM_NAME
  analyzer = LogAnalyzer.new
  
  if ARGV[0]
    analyzer.analyze_log(ARGV[0])
    
    if ARGV[1]
      analyzer.export_errors(ARGV[1])
    end
  else
    puts "Usage: ruby log_analyzer.rb <log_file> [error_report.json]"
  end
end
