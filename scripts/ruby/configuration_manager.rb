#!/usr/bin/env ruby
# frozen_string_literal: true

require 'json'
require 'yaml'

# Configuration Manager Utility
# Handles configuration file management and validation
class ConfigurationManager
  def initialize(config_dir = 'config')
    @config_dir = config_dir
    @config_cache = {}
  end

  def load_config(file_name, format = 'json')
    config_path = File.join(@config_dir, file_name)
    
    unless File.exist?(config_path)
      puts "Configuration file not found: #{config_path}"
      return nil
    end

    begin
      case format.downcase
      when 'json'
        config = JSON.parse(File.read(config_path))
      when 'yaml', 'yml'
        config = YAML.load(File.read(config_path))
      when 'env'
        config = parse_env_file(config_path)
      else
        raise "Unsupported format: #{format}"
      end
      
      @config_cache[file_name] = config
      puts "Loaded configuration: #{file_name}"
      config
    rescue => e
      puts "Error loading configuration: #{e.message}"
      nil
    end
  end

  def save_config(file_name, config, format = 'json')
    config_path = File.join(@config_dir, file_name)
    
    FileUtils.mkdir_p(@config_dir)
    
    begin
      case format.downcase
      when 'json'
        File.write(config_path, JSON.pretty_generate(config))
      when 'yaml', 'yml'
        File.write(config_path, YAML.dump(config))
      else
        raise "Unsupported format: #{format}"
      end
      
      @config_cache[file_name] = config
      puts "Saved configuration: #{file_name}"
      true
    rescue => e
      puts "Error saving configuration: #{e.message}"
      false
    end
  end

  def get_config(file_name, key = nil)
    config = @config_cache[file_name] || load_config(file_name)
    return nil unless config
    
    if key
      key.split('.').reduce(config) { |acc, k| acc && acc[k] }
    else
      config
    end
  end

  def set_config(file_name, key, value, format = 'json')
    config = @config_cache[file_name] || load_config(file_name) || {}
    
    # Set nested value
    keys = key.split('.')
    current = config
    keys[0..-2].each do |k|
      current[k] ||= {}
      current = current[k]
    end
    current[keys.last] = value
    
    save_config(file_name, config, format)
  end

  def validate_config(file_name, schema)
    config = get_config(file_name)
    return false unless config
    
    validation_errors = []
    
    schema.each do |key, rules|
      value = config[key]
      
      # Check required fields
      if rules[:required] && value.nil?
        validation_errors << "Missing required field: #{key}"
        next
      end
      
      next if value.nil?
      
      # Type validation
      if rules[:type]
        unless validate_type(value, rules[:type])
          validation_errors << "Invalid type for #{key}: expected #{rules[:type]}"
        end
      end
      
      # Range validation
      if rules[:min] && value < rules[:min]
        validation_errors << "#{key} is below minimum value: #{rules[:min]}"
      end
      
      if rules[:max] && value > rules[:max]
        validation_errors << "#{key} exceeds maximum value: #{rules[:max]}"
      end
      
      # Pattern validation
      if rules[:pattern] && !value.to_s.match?(rules[:pattern])
        validation_errors << "#{key} doesn't match required pattern"
      end
    end
    
    if validation_errors.any?
      puts "Configuration validation failed:"
      validation_errors.each { |error| puts "  - #{error}" }
      false
    else
      puts "Configuration validation passed"
      true
    end
  end

  def list_configs
    Dir.glob(File.join(@config_dir, '*')).map do |file|
      {
        name: File.basename(file),
        size: File.size(file),
        modified: File.mtime(file),
        format: detect_format(file)
      }
    end
  end

  def merge_configs(base_config, override_config)
    merged = base_config.dup
    
    override_config.each do |key, value|
      if merged[key].is_a?(Hash) && value.is_a?(Hash)
        merged[key] = merge_configs(merged[key], value)
      else
        merged[key] = value
      end
    end
    
    merged
  end

  def backup_config(file_name)
    backup_name = "#{file_name}.#{Time.now.strftime('%Y%m%d_%H%M%S')}.backup"
    backup_path = File.join(@config_dir, 'backups', backup_name)
    
    FileUtils.mkdir_p(File.dirname(backup_path))
    FileUtils.cp(File.join(@config_dir, file_name), backup_path)
    
    puts "Configuration backed up: #{backup_name}"
    backup_path
  end

  def restore_config(file_name, backup_name)
    backup_path = File.join(@config_dir, 'backups', backup_name)
    
    unless File.exist?(backup_path)
      puts "Backup file not found: #{backup_name}"
      return false
    end
    
    FileUtils.cp(backup_path, File.join(@config_dir, file_name))
    puts "Configuration restored: #{file_name}"
    true
  end

  def export_environment_vars(file_name, environment = 'production')
    config = get_config(file_name)
    return unless config
    
    env_vars = flatten_config_for_env(config)
    
    env_file_path = File.join(@config_dir, "#{environment}.env")
    File.write(env_file_path, env_vars.map { |k, v| "#{k}=#{v}" }.join("\n"))
    
    puts "Environment variables exported to: #{env_file_path}"
    env_vars
  end

  def generate_sample_config(file_name, format = 'json')
    sample_config = {
      app: {
        name: 'GameCenter',
        version: '1.0.0',
        debug: false
      },
      database: {
        host: 'localhost',
        port: 5432,
        name: 'gamecenter_db',
        pool_size: 10
      },
      security: {
        secret_key: 'your-secret-key-here',
        jwt_expiration: 3600,
        bcrypt_rounds: 12
      },
      logging: {
        level: 'info',
        file: 'logs/app.log',
        max_size: '100MB'
      }
    }
    
    save_config(file_name, sample_config, format)
    sample_config
  end

  def compare_configs(config1_file, config2_file)
    config1 = get_config(config1_file)
    config2 = get_config(config2_file)
    
    return {} unless config1 && config2
    
    differences = find_config_differences(config1, config2)
    differences
  end

  private

  def parse_env_file(file_path)
    config = {}
    
    File.readlines(file_path).each do |line|
      line.strip!
      next if line.empty? || line.start_with?('#')
      
      if line.include?('=')
        key, value = line.split('=', 2)
        config[key.strip] = value.strip
      end
    end
    
    config
  end

  def validate_type(value, expected_type)
    case expected_type.to_s
    when 'string'
      value.is_a?(String)
    when 'integer'
      value.is_a?(Integer)
    when 'float'
      value.is_a?(Float)
    when 'boolean'
      [true, false].include?(value)
    when 'array'
      value.is_a?(Array)
    when 'hash', 'object'
      value.is_a?(Hash)
    else
      true # Allow custom types
    end
  end

  def detect_format(file_path)
    case File.extname(file_path)
    when '.json'
      'json'
    when '.yaml', '.yml'
      'yaml'
    when '.env'
      'env'
    else
      'unknown'
    end
  end

  def flatten_config_for_env(config, prefix = '')
    flattened = {}
    
    config.each do |key, value|
      full_key = prefix.empty? ? key.to_s.upcase : "#{prefix}_#{key}".upcase
      
      if value.is_a?(Hash)
        flattened.merge!(flatten_config_for_env(value, full_key))
      else
        flattened[full_key] = value.to_s
      end
    end
    
    flattened
  end

  def find_config_differences(config1, config2, path = '')
    differences = {}
    
    all_keys = (config1.keys + config2.keys).uniq
    
    all_keys.each do |key|
      current_path = path.empty? ? key : "#{path}.#{key}"
      
      if config1.key?(key) && config2.key?(key)
        if config1[key].is_a?(Hash) && config2[key].is_a?(Hash)
          nested_diffs = find_config_differences(config1[key], config2[key], current_path)
          differences.merge!(nested_diffs)
        elsif config1[key] != config2[key]
          differences[current_path] = {
            old_value: config1[key],
            new_value: config2[key]
          }
        end
      elsif config1.key?(key)
        differences[current_path] = {
          action: 'removed',
          value: config1[key]
        }
      else
        differences[current_path] = {
          action: 'added',
          value: config2[key]
        }
      end
    end
    
    differences
  end
end

# CLI Usage
if __FILE__ == $PROGRAM_NAME
  config_mgr = ConfigurationManager.new
  
  command = ARGV[0]
  case command
  when 'load'
    puts config_mgr.load_config(ARGV[1], ARGV[2]).to_json
  when 'get'
    puts config_mgr.get_config(ARGV[1], ARGV[2]).to_json
  when 'set'
    config_mgr.set_config(ARGV[1], ARGV[2], ARGV[3])
  when 'validate'
    schema = JSON.parse(ARGV[3] || '{}')
    config_mgr.validate_config(ARGV[1], schema)
  when 'list'
    puts config_mgr.list_configs.to_json
  when 'backup'
    config_mgr.backup_config(ARGV[1])
  when 'restore'
    config_mgr.restore_config(ARGV[1], ARGV[2])
  when 'export-env'
    config_mgr.export_environment_vars(ARGV[1], ARGV[2])
  when 'sample'
    config_mgr.generate_sample_config(ARGV[1], ARGV[2])
  when 'compare'
    puts config_mgr.compare_configs(ARGV[1], ARGV[2]).to_json
  else
    puts "Usage: ruby configuration_manager.rb <command> [args]"
    puts "Commands: load, get, set, validate, list, backup, restore, export-env, sample, compare"
  end
end
