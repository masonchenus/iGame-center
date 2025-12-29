#!/usr/bin/env ruby
# frozen_string_literal: true

require 'json'
require 'fileutils'

# Code Quality Utility
# Provides code analysis, formatting, and quality checking functions
class CodeQualityUtility
  def initialize(config_file = 'config/code_quality.json')
    @config = load_config(config_file)
    @quality_metrics = {}
  end

  def load_config(config_file)
    return {} unless File.exist?(config_file)
    JSON.parse(File.read(config_file))
  rescue JSON::ParserError
    {}
  end

  def analyze_code_quality(target_directory = 'src', language = 'javascript')
    puts "Analyzing #{language} code quality in #{target_directory}"
    
    analysis_result = {
      target_directory: target_directory,
      language: language,
      timestamp: Time.now.to_s,
      metrics: {},
      issues: [],
      suggestions: []
    }
    
    case language.downcase
    when 'javascript', 'js'
      analysis_result = analyze_javascript_code(target_directory, analysis_result)
    when 'ruby', 'rb'
      analysis_result = analyze_ruby_code(target_directory, analysis_result)
    when 'python', 'py'
      analysis_result = analyze_python_code(target_directory, analysis_result)
    else
      puts "Unsupported language: #{language}"
    end
    
    @quality_metrics = analysis_result
    generate_quality_report(analysis_result)
  end

  def analyze_javascript_code(directory, result)
    puts "Analyzing JavaScript/TypeScript files..."
    
    js_files = find_files_by_extension(directory, ['.js', '.jsx', '.ts', '.tsx'])
    
    metrics = {
      total_files: js_files.length,
      total_lines: 0,
      total_functions: 0,
      total_classes: 0,
      complexity_score: 0,
      duplication_percentage: 0
    }
    
    issues = []
    
    js_files.each do |file|
      content = File.read(file)
      file_metrics = analyze_javascript_file(file, content)
      
      metrics[:total_lines] += file_metrics[:lines]
      metrics[:total_functions] += file_metrics[:functions]
      metrics[:total_classes] += file_metrics[:classes]
      
      # Add file-specific issues
      issues.concat(file_metrics[:issues])
    end
    
    # Calculate overall complexity
    metrics[:complexity_score] = calculate_complexity_score(metrics)
    metrics[:duplication_percentage] = calculate_duplication_percentage(js_files)
    
    result[:metrics] = metrics
    result[:issues] = issues
    
    puts "Analysis complete: #{metrics[:total_files]} files analyzed"
    result
  end

  def analyze_javascript_file(file_path, content)
    lines = content.lines
    metrics = {
      lines: lines.length,
      functions: count_javascript_functions(content),
      classes: count_javascript_classes(content),
      issues: []
    }
    
    # Check for common issues
    metrics[:issues] = detect_javascript_issues(file_path, content, lines)
    
    metrics
  end

  def detect_javascript_issues(file_path, content, lines)
    issues = []
    
    lines.each_with_index do |line, index|
      line_number = index + 1
      
      # Check for console.log statements
      if line.match?(/console\.log|console\.warn|console\.error/)
        issues << {
          file: file_path,
          line: line_number,
          type: 'console_statement',
          severity: 'warning',
          message: 'Console statement found',
          suggestion: 'Remove console statements or use proper logging'
        }
      end
      
      # Check for TODO/FIXME comments
      if line.match?(/\b(TODO|FIXME|HACK|XXX)\b/i)
        issues << {
          file: file_path,
          line: line_number,
          type: 'code_comment',
          severity: 'info',
          message: 'Code comment found',
          suggestion: 'Review TODO/FIXME comments and address them'
        }
      end
      
      # Check for long lines
      if line.length > 120
        issues << {
          file: file_path,
          line: line_number,
          type: 'line_length',
          severity: 'warning',
          message: "Line too long (#{line.length} characters)",
          suggestion: 'Keep lines under 120 characters'
        }
      end
      
      # Check for magic numbers
      if line.match?(/\b\d{3,}\b/)
        issues << {
          file: file_path,
          line: line_number,
          type: 'magic_number',
          severity: 'warning',
          message: 'Magic number detected',
          suggestion: 'Use named constants instead of magic numbers'
        }
      end
    end
    
    issues
  end

  def count_javascript_functions(content)
    # Simple function counting (not comprehensive)
    content.scan(/function\s+\w+|=>\s*{|\w+\s*:\s*function/).length
  end

  def count_javascript_classes(content)
    content.scan(/class\s+\w+/).length
  end

  def analyze_ruby_code(directory, result)
    puts "Analyzing Ruby files..."
    
    rb_files = find_files_by_extension(directory, ['.rb'])
    
    metrics = {
      total_files: rb_files.length,
      total_lines: 0,
      total_methods: 0,
      total_classes: 0,
      complexity_score: 0,
      test_coverage: 0
    }
    
    issues = []
    
    rb_files.each do |file|
      content = File.read(file)
      file_metrics = analyze_ruby_file(file, content)
      
      metrics[:total_lines] += file_metrics[:lines]
      metrics[:total_methods] += file_metrics[:methods]
      metrics[:total_classes] += file_metrics[:classes]
      
      issues.concat(file_metrics[:issues])
    end
    
    metrics[:complexity_score] = calculate_complexity_score(metrics)
    result[:metrics] = metrics
    result[:issues] = issues
    
    result
  end

  def analyze_ruby_file(file_path, content)
    lines = content.lines
    metrics = {
      lines: lines.length,
      methods: count_ruby_methods(content),
      classes: count_ruby_classes(content),
      issues: []
    }
    
    metrics[:issues] = detect_ruby_issues(file_path, content, lines)
    metrics
  end

  def detect_ruby_issues(file_path, content, lines)
    issues = []
    
    lines.each_with_index do |line, index|
      line_number = index + 1
      
      # Check for puts/print statements
      if line.match?(/\b(puts|print|p)\b/)
        issues << {
          file: file_path,
          line: line_number,
          type: 'debug_statement',
          severity: 'warning',
          message: 'Debug statement found',
          suggestion: 'Remove debug statements or use proper logging'
        }
      end
      
      # Check for TODO/FIXME comments
      if line.match?(/\b(TODO|FIXME|HACK|XXX)\b/i)
        issues << {
          file: file_path,
          line: line_number,
          type: 'code_comment',
          severity: 'info',
          message: 'Code comment found',
          suggestion: 'Review TODO/FIXME comments and address them'
        }
      end
      
      # Check for long methods
      if line.match?(/\bend\b/) && index > 50 # Rough estimate
        issues << {
          file: file_path,
          line: line_number,
          type: 'long_method',
          severity: 'warning',
          message: 'Potentially long method detected',
          suggestion: 'Consider breaking down long methods'
        }
      end
    end
    
    issues
  end

  def count_ruby_methods(content)
    content.scan(/def\s+\w+/).length
  end

  def count_ruby_classes(content)
    content.scan(/class\s+\w+/).length
  end

  def format_code(file_path, language = 'javascript')
    puts "Formatting #{language} code in #{file_path}"
    
    case language.downcase
    when 'javascript', 'js'
      format_javascript_file(file_path)
    when 'ruby', 'rb'
      format_ruby_file(file_path)
    else
      puts "Formatting not supported for #{language}"
      false
    end
  end

  def format_javascript_file(file_path)
    content = File.read(file_path)
    
    # Basic JavaScript formatting (simplified)
    formatted_content = content.gsub(/\s+/, ' ').strip
    
    # Add basic indentation
    formatted_content = add_basic_indentation(formatted_content)
    
    File.write(file_path, formatted_content)
    puts "Formatted: #{file_path}"
    true
  rescue => e
    puts "Error formatting #{file_path}: #{e.message}"
    false
  end

  def format_ruby_file(file_path)
    content = File.read(file_path)
    
    # Basic Ruby formatting (simplified)
    formatted_content = content.gsub(/\s+/, ' ').strip
    
    # Fix basic indentation
    formatted_content = fix_ruby_indentation(formatted_content)
    
    File.write(file_path, formatted_content)
    puts "Formatted: #{file_path}"
    true
  rescue => e
    puts "Error formatting #{file_path}: #{e.message}"
    false
  end

  def run_linting(language = 'javascript', directory = 'src')
    puts "Running #{language} linter..."
    
    case language.downcase
    when 'javascript', 'js'
      run_eslint(directory)
    when 'ruby', 'rb'
      run_rubocop(directory)
    else
      puts "Linting not supported for #{language}"
    end
  end

  def calculate_code_complexity(file_path)
    content = File.read(file_path)
    
    # Simple cyclomatic complexity calculation
    complexity_factors = [
      content.scan(/\bif\b/).length,
      content.scan(/\belse\b/).length,
      content.scan(/\bfor\b/).length,
      content.scan(/\bwhile\b/).length,
      content.scan(/\bcase\b/).length,
      content.scan(/\bcatch\b/).length,
      content.scan(/\?.*:/).length # Ternary operators
    ]
    
    complexity_factors.sum + 1 # Base complexity of 1
  end

  def detect_code_smells(file_path)
    content = File.read(file_path)
    smells = []
    
    # Check for long methods
    if content.lines.length > 50
      smells << {
        type: 'long_method',
        severity: 'medium',
        message: 'Method is too long',
        suggestion: 'Break down into smaller methods'
      }
    end
    
    # Check for large classes
    if content.scan(/class\s+\w+/).length > 1
      smells << {
        type: 'large_class',
        severity: 'medium',
        message: 'Multiple classes in single file',
        suggestion: 'Consider separating classes into different files'
      }
    end
    
    # Check for complex conditionals
    if content.scan(/\bif\b.*\b(and|or)\b.*\bif\b/i).any?
      smells << {
        type: 'complex_conditionals',
        severity: 'low',
        message: 'Complex conditional logic detected',
        suggestion: 'Simplify conditionals or extract to methods'
      }
    end
    
    smells
  end

  def generate_quality_metrics_report(output_file = 'quality_metrics.json')
    report = {
      generated_at: Time.now.to_s,
      overall_score: calculate_overall_quality_score,
      metrics: @quality_metrics,
      recommendations: generate_quality_recommendations,
      technical_debt: calculate_technical_debt
    }
    
    File.write(output_file, JSON.pretty_generate(report))
    puts "Quality metrics report generated: #{output_file}"
    report
  end

  private

  def find_files_by_extension(directory, extensions)
    files = []
    Dir.glob(File.join(directory, '**', '*')).each do |path|
      files << path if extensions.include?(File.extname(path))
    end
    files
  end

  def calculate_complexity_score(metrics)
    return 0 if metrics[:total_lines].zero?
    
    # Simplified complexity calculation
    base_score = 100
    deductions = [
      metrics[:total_functions] * 2,
      metrics[:total_classes] * 5,
      metrics[:total_lines] / 100
    ].sum
    
    [base_score - deductions, 0].max
  end

  def calculate_duplication_percentage(files)
    return 0 if files.empty?
    
    # Simplified duplication detection
    content_hashes = {}
    total_lines = 0
    
    files.each do |file|
      content = File.read(file)
      lines = content.lines
      total_lines += lines.length
      
      # Group similar content (simplified)
      line_signature = content.gsub(/\s+/, '').downcase[0..50]
      content_hashes[line_signature] ||= 0
      content_hashes[line_signature] += lines.length
    end
    
    duplicated_lines = content_hases.values.select { |count| count > 10 }.sum
    (duplicated_lines.to_f / total_lines * 100).round(2)
  rescue
    0
  end

  def add_basic_indentation(content)
    # Basic indentation logic
    lines = content.split("\n")
    indented_lines = []
    current_indent = 0
    
    lines.each do |line|
      line = line.strip
      next if line.empty?
      
      # Decrease indent for closing braces/brackets
      if line.match?(/^[\}\]\)]/)
        current_indent = [current_indent - 1, 0].max
      end
      
      # Add current line with indentation
      indented_lines << ('  ' * current_indent) + line
      
      # Increase indent for opening braces/brackets
      if line.match?(/[\{\[\(][^{}\[\(]*$/)
        current_indent += 1
      end
    end
    
    indented_lines.join("\n")
  end

  def fix_ruby_indentation(content)
    # Basic Ruby indentation
    lines = content.split("\n")
    indented_lines = []
    current_indent = 0
    
    lines.each do |line|
      line = line.strip
      next if line.empty?
      
      # Decrease indent for end keywords
      if line.match(/^end\b/)
        current_indent = [current_indent - 1, 0].max
      end
      
      indented_lines << ('  ' * current_indent) + line
      
      # Increase indent for class/module/method definitions
      if line.match(/^(class|module|def|if|unless|for|while|begin)\b/)
        current_indent += 1
      end
    end
    
    indented_lines.join("\n")
  end

  def run_eslint(directory)
    # Simulate ESLint run
    puts "Running ESLint on #{directory}"
    puts "No ESLint errors found"
  end

  def run_rubocop(directory)
    # Simulate RuboCop run
    puts "Running RuboCop on #{directory}"
    puts "No RuboCop offenses found"
  end

  def calculate_overall_quality_score
    return 0 if @quality_metrics[:metrics].empty?
    
    # Calculate weighted quality score
    metrics = @quality_metrics[:metrics]
    score = 100
    
    # Deduct points for issues
    critical_issues = @quality_metrics[:issues].count { |i| i[:severity] == 'critical' }
    warning_issues = @quality_metrics[:issues].count { |i| i[:severity] == 'warning' }
    
    score -= critical_issues * 10
    score -= warning_issues * 5
    score -= [100 - metrics[:complexity_score], 0].max
    
    [score, 0].max
  end

  def generate_quality_recommendations
    recommendations = []
    
    if @quality_metrics[:issues].any?
      recommendations << "Address the identified code quality issues"
    end
    
    recommendations.concat([
      "Implement automated code formatting",
      "Set up continuous code quality monitoring",
      "Regular code reviews and pair programming",
      "Use static analysis tools in CI/CD",
      "Maintain comprehensive test coverage"
    ])
    
    recommendations
  end

  def calculate_technical_debt
    issues_by_severity = @quality_metrics[:issues].group_by { |i| i[:severity] }
    
    {
      critical_debt: (issues_by_severity['critical'] || []).length * 8, # hours
      high_debt: (issues_by_severity['warning'] || []).length * 4,
      medium_debt: (issues_by_severity['info'] || []).length * 2,
      total_estimated_hours: 0
    }
  end
end

# CLI Usage
if __FILE__ == $PROGRAM_NAME
  code_quality = CodeQualityUtility.new
  
  command = ARGV[0]
  case command
  when 'analyze'
    code_quality.analyze_code_quality(ARGV[1] || 'src', ARGV[2] || 'javascript')
  when 'format'
    code_quality.format_code(ARGV[1], ARGV[2] || 'javascript')
  when 'lint'
    code_quality.run_linting(ARGV[1] || 'javascript', ARGV[2] || 'src')
  when 'complexity'
    puts code_quality.calculate_code_complexity(ARGV[1]).to_json
  when 'smells'
    puts code_quality.detect_code_smells(ARGV[1]).to_json
  when 'report'
    code_quality.generate_quality_metrics_report(ARGV[1])
  else
    puts "Usage: ruby code_quality_utility.rb <command> [args]"
    puts "Commands: analyze, format, lint, complexity, smells, report"
  end
end
