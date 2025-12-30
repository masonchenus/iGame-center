#!/usr/bin/env ruby
# frozen_string_literal: true

require 'json'
require 'open3'
require 'fileutils'

# Server Management Utility
# Provides server management and process control functions
class ServerManagement
  def initialize(config_path = 'config/servers.json')
    @config = load_config(config_path)
    @active_servers = {}
  end

  def load_config(config_path)
    return {} unless File.exist?(config_path)
    JSON.parse(File.read(config_path))
  rescue JSON::ParserError
    {}
  end

  def start_server(server_name, config = {})
    server_config = @config[server_name] || config
    return false unless server_config

    puts "Starting server: #{server_name}"
    
    process_info = {
      pid: spawn_server_process(server_config),
      config: server_config,
      start_time: Time.now.to_s,
      status: 'running'
    }
    
    @active_servers[server_name] = process_info
    puts "Server #{server_name} started with PID #{process_info[:pid]}"
    true
  end

  def stop_server(server_name)
    server_info = @active_servers[server_name]
    return false unless server_info

    puts "Stopping server: #{server_name}"
    
    if kill_process(server_info[:pid])
      server_info[:status] = 'stopped'
      server_info[:stop_time] = Time.now.to_s
      puts "Server #{server_name} stopped successfully"
      true
    else
      puts "Failed to stop server #{server_name}"
      false
    end
  end

  def restart_server(server_name)
    puts "Restarting server: #{server_name}"
    stop_server(server_name)
    sleep 2
    start_server(server_name)
  end

  def get_server_status(server_name = nil)
    if server_name
      get_single_server_status(server_name)
    else
      get_all_servers_status
    end
  end

  def get_single_server_status(server_name)
    server_info = @active_servers[server_name]
    
    if server_info
      {
        name: server_name,
        status: check_process_status(server_info[:pid]) ? 'running' : 'stopped',
        pid: server_info[:pid],
        uptime: calculate_uptime(server_info[:start_time]),
        memory_usage: get_process_memory(server_info[:pid]),
        cpu_usage: get_process_cpu(server_info[:pid])
      }
    else
      { name: server_name, status: 'unknown' }
    end
  end

  def get_all_servers_status
    @active_servers.transform_values do |server_info|
      {
        status: check_process_status(server_info[:pid]) ? 'running' : 'stopped',
        pid: server_info[:pid],
        uptime: calculate_uptime(server_info[:start_time])
      }
    end
  end

  def scale_server(server_name, target_instances)
    current_count = get_server_instance_count(server_name)
    difference = target_instances - current_count
    
    if difference > 0
      difference.times { start_server("#{server_name}_#{current_count + 1}") }
    elsif difference < 0
      (-difference).times do |i|
        instance_name = "#{server_name}_#{current_count - i}"
        stop_server(instance_name)
      end
    end
    
    puts "Scaled #{server_name} to #{target_instances} instances"
  end

  def deploy_application(app_name, version, deployment_config = {})
    puts "Deploying #{app_name} version #{version}"
    
    deployment_steps = [
      'Pre-deployment checks',
      'Backup current version',
      'Stop services',
      'Update code',
      'Run migrations',
      'Start services',
      'Health checks',
      'Post-deployment verification'
    ]
    
    deployment_steps.each do |step|
      puts "  #{step}..."
      sleep 1 # Simulate step execution
      
      # Simulate failure in health checks
      if step == 'Health checks' && rand > 0.9
        puts "    Deployment failed at health checks"
        rollback_deployment(app_name, version)
        return false
      end
    end
    
    puts "Deployment completed successfully"
    true
  end

  def rollback_deployment(app_name, version)
    puts "Rolling back #{app_name} to previous version"
    # Simulate rollback process
    rollback_steps = [
      'Stop current version',
      'Restore previous version',
      'Update configuration',
      'Start services',
      'Verify rollback'
    ]
    
    rollback_steps.each do |step|
      puts "  #{step}..."
      sleep 1
    end
    
    puts "Rollback completed"
  end

  def get_server_logs(server_name, lines = 100)
    log_file = @config[server_name]&.dig('log_file') || "logs/#{server_name}.log"
    
    if File.exist?(log_file)
      `tail -n #{lines} #{log_file}`.lines
    else
      puts "Log file not found: #{log_file}"
      []
    end
  end

  def monitor_resource_usage(server_name = nil)
    if server_name
      monitor_single_server(server_name)
    else
      monitor_all_servers
    end
  end

  def monitor_single_server(server_name)
    server_info = @active_servers[server_name]
    return {} unless server_info
    
    {
      server_name => {
        cpu_percent: get_process_cpu(server_info[:pid]),
        memory_mb: get_process_memory(server_info[:pid]) / 1024 / 1024,
        disk_usage: get_disk_usage(server_info[:config]['data_dir'] || '.'),
        network_connections: get_network_connections(server_info[:pid])
      }
    }
  end

  def monitor_all_servers
    @active_servers.transform_keys do |server_name|
      monitor_single_server(server_name)[server_name]
    end
  end

  def generate_server_report(output_file = 'server_report.json')
    report = {
      generated_at: Time.now.to_s,
      servers: get_all_servers_status,
      resource_usage: monitor_resource_usage,
      total_memory_usage: calculate_total_memory,
      total_cpu_usage: calculate_total_cpu
    }
    
    File.write(output_file, JSON.pretty_generate(report))
    puts "Server report generated: #{output_file}"
    report
  end

  private

  def spawn_server_process(config)
    command = config['start_command'] || 'echo "Server process"'
    
    # For demonstration, we'll just simulate process spawning
    pid = rand(1000..9999)
    
    # In real implementation, you would use Process.spawn or similar
    pid
  end

  def kill_process(pid)
    # Simulate process killing
    true
  end

  def check_process_status(pid)
    # Simulate process status check
    rand > 0.1 # 90% chance process is running
  end

  def calculate_uptime(start_time)
    start = Time.parse(start_time)
    (Time.now - start).to_i
  end

  def get_process_memory(pid)
    # Simulate memory usage in bytes
    rand(50_000_000..500_000_000)
  end

  def get_process_cpu(pid)
    # Simulate CPU usage percentage
    rand(1..80)
  end

  def get_server_instance_count(server_name)
    @active_servers.keys.count { |name| name.start_with?(server_name) }
  end

  def get_disk_usage(directory)
    # Simulate disk usage percentage
    rand(10..90)
  end

  def get_network_connections(pid)
    # Simulate network connections count
    rand(1..50)
  end

  def calculate_total_memory
    @active_servers.sum { |_, info| get_process_memory(info[:pid]) }
  end

  def calculate_total_cpu
    @active_servers.sum { |_, info| get_process_cpu(info[:pid]) }
  end
end

# CLI Usage
if __FILE__ == $PROGRAM_NAME
  server_mgmt = ServerManagement.new
  
  command = ARGV[0]
  case command
  when 'start'
    server_mgmt.start_server(ARGV[1])
  when 'stop'
    server_mgmt.stop_server(ARGV[1])
  when 'restart'
    server_mgmt.restart_server(ARGV[1])
  when 'status'
    puts server_mgmt.get_server_status(ARGV[1]).to_json
  when 'scale'
    server_mgmt.scale_server(ARGV[1], ARGV[2].to_i)
  when 'deploy'
    server_mgmt.deploy_application(ARGV[1], ARGV[2])
  when 'logs'
    server_mgmt.get_server_logs(ARGV[1], (ARGV[2] || 100).to_i).each { |line| puts line }
  when 'monitor'
    puts server_mgmt.monitor_resource_usage(ARGV[1]).to_json
  when 'report'
    server_mgmt.generate_server_report(ARGV[1])
  else
    puts "Usage: ruby server_management.rb <command> [args]"
    puts "Commands: start, stop, restart, status, scale, deploy, logs, monitor, report"
  end
end
