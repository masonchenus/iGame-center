d#!/usr/bin/env ruby
# frozen_string_literal: true

require 'json'
require 'time'
require 'socket'

# System Monitoring Utility
# Provides system monitoring and alerting functions
class MonitoringUtility
  def initialize(config_file = 'config/monitoring.json')
    @config = load_config(config_file)
    @metrics = {}
    @alerts = []
  end

  def load_config(config_file)
    return {} unless File.exist?(config_file)
    JSON.parse(File.read(config_file))
  rescue JSON::ParserError
    {}
  end

  def collect_system_metrics
    puts "Collecting system metrics..."
    
    @metrics = {
      timestamp: Time.now.to_s,
      cpu_usage: get_cpu_usage,
      memory_usage: get_memory_usage,
      disk_usage: get_disk_usage,
      network_stats: get_network_stats,
      process_stats: get_process_stats,
      load_average: get_load_average
    }
    
    puts "System metrics collected"
    @metrics
  end

  def check_system_health
    puts "Performing system health check..."
    
    health_status = {
      overall_status: 'healthy',
      timestamp: Time.now.to_s,
      checks: {
        cpu_health: check_cpu_health,
        memory_health: check_memory_health,
        disk_health: check_disk_health,
        network_health: check_network_health,
        process_health: check_process_health
      }
    }
    
    # Determine overall status
    critical_checks = health_status[:checks].count { |_, status| status[:status] == 'critical' }
    warning_checks = health_status[:checks].count { |_, status| status[:status] == 'warning' }
    
    if critical_checks > 0
      health_status[:overall_status] = 'critical'
    elsif warning_checks > 0
      health_status[:overall_status] = 'warning'
    end
    
    health_status
  end

  def monitor_application_performance(app_name, duration_seconds = 300)
    puts "Monitoring #{app_name} for #{duration_seconds} seconds"
    
    performance_data = {
      app_name: app_name,
      start_time: Time.now.to_s,
      duration: duration_seconds,
      measurements: []
    }
    
    start_time = Time.now
    while (Time.now - start_time) < duration_seconds
      measurement = {
        timestamp: Time.now.to_s,
        response_time: measure_app_response_time(app_name),
        cpu_usage: get_app_cpu_usage(app_name),
        memory_usage: get_app_memory_usage(app_name),
        request_count: get_app_request_count(app_name)
      }
      
      performance_data[:measurements] << measurement
      sleep 5 # Collect data every 5 seconds
    end
    
    performance_data[:end_time] = Time.now.to_s
    generate_performance_report(performance_data)
  end

  def setup_monitoring_alerts
    puts "Setting up monitoring alerts..."
    
    alerts_config = @config['alerts'] || default_alert_config
    active_alerts = []
    
    alerts_config.each do |alert|
      if create_alert(alert)
        active_alerts << alert
        puts "Alert configured: #{alert['name']}"
      end
    end
    
    active_alerts
  end

  def create_alert(alert_config)
    alert = {
      id: generate_alert_id,
      name: alert_config['name'],
      metric: alert_config['metric'],
      condition: alert_config['condition'],
      threshold: alert_config['threshold'],
      duration: alert_config['duration'] || 60,
      enabled: true,
      created_at: Time.now.to_s
    }
    
    @alerts << alert
    true
  end

  def check_alerts
    puts "Checking alert conditions..."
    
    triggered_alerts = []
    
    @alerts.each do |alert|
      if evaluate_alert_condition(alert)
        triggered_alerts << trigger_alert(alert)
      end
    end
    
    triggered_alerts
  end

  def generate_monitoring_dashboard(output_file = 'monitoring_dashboard.json')
    dashboard_data = {
      generated_at: Time.now.to_s,
      system_metrics: @metrics,
      health_status: check_system_health,
      active_alerts: @alerts.select { |a| a[:enabled] },
      recent_alerts: @alerts.last(10),
      performance_summary: get_performance_summary
    }
    
    File.write(output_file, JSON.pretty_generate(dashboard_data))
    puts "Monitoring dashboard generated: #{output_file}"
    dashboard_data
  end

  def monitor_log_patterns(log_patterns, directory = 'logs')
    puts "Monitoring log patterns..."
    
    pattern_matches = []
    
    log_patterns.each do |pattern_name, pattern_regex|
      matches = find_log_pattern_matches(pattern_regex, directory)
      if matches.any?
        pattern_matches << {
          pattern_name: pattern_name,
          pattern: pattern_regex,
          matches_count: matches.length,
          matches: matches
        }
      end
    end
    
    puts "Found #{pattern_matches.length} pattern matches"
    pattern_matches
  end

  def track_service_dependencies
    puts "Tracking service dependencies..."
    
    dependencies = {
      services: get_running_services,
      connections: check_service_connections,
      health_checks: perform_dependency_health_checks,
      dependency_graph: build_dependency_graph
    }
    
    puts "Service dependency tracking completed"
    dependencies
  end

  def generate_uptime_report(service_name, days = 7)
    puts "Generating uptime report for #{service_name} (#{days} days)"
    
    end_time = Time.now
    start_time = end_time - (days * 24 * 60 * 60)
    
    uptime_data = {
      service_name: service_name,
      period: "#{start_time.to_date} to #{end_time.to_date}",
      total_checks: 0,
      successful_checks: 0,
      uptime_percentage: 0,
      incidents: [],
      average_response_time: 0
    }
    
    # Simulate uptime data collection
    (days * 24 * 60).times do # Check every minute
      uptime_data[:total_checks] += 1
      
      if rand > 0.01 # 99% uptime simulation
        uptime_data[:successful_checks] += 1
      else
        uptime_data[:incidents] << {
          timestamp: (start_time + rand * (end_time - start_time)).to_s,
          duration_minutes: rand(1..30),
          severity: ['low', 'medium', 'high'].sample
        }
      end
    end
    
    uptime_data[:uptime_percentage] = (uptime_data[:successful_checks].to_f / uptime_data[:total_checks] * 100).round(2)
    uptime_data[:average_response_time] = rand(50..500)
    
    puts "Uptime report generated: #{uptime_data[:uptime_percentage]}% uptime"
    uptime_data
  end

  private

  def get_cpu_usage
    # Simulate CPU usage
    rand(10..80)
  end

  def get_memory_usage
    {
      total_mb: 8192,
      used_mb: rand(2048..6144),
      free_mb: 8192 - rand(2048..6144),
      usage_percent: rand(25..75)
    }
  end

  def get_disk_usage
    {
      total_gb: 500,
      used_gb: rand(100..400),
      free_gb: 500 - rand(100..400),
      usage_percent: rand(20..80)
    }
  end

  def get_network_stats
    {
      bytes_sent: rand(1000000..10000000),
      bytes_received: rand(1000000..10000000),
      packets_sent: rand(1000..10000),
      packets_received: rand(1000..10000)
    }
  end

  def get_process_stats
    {
      total_processes: rand(50..200),
      running_processes: rand(30..150),
      zombie_processes: rand(0..5)
    }
  end

  def get_load_average
    {
      one_minute: rand(0.5..2.0),
      five_minutes: rand(0.3..1.8),
      fifteen_minutes: rand(0.2..1.5)
    }
  end

  def check_cpu_health
    cpu_usage = get_cpu_usage
    {
      status: cpu_usage > 90 ? 'critical' : cpu_usage > 75 ? 'warning' : 'healthy',
      value: cpu_usage,
      threshold: 75,
      message: cpu_usage > 90 ? 'CPU usage critically high' : 
               cpu_usage > 75 ? 'CPU usage elevated' : 'CPU usage normal'
    }
  end

  def check_memory_health
    memory = get_memory_usage
    {
      status: memory[:usage_percent] > 90 ? 'critical' : memory[:usage_percent] > 80 ? 'warning' : 'healthy',
      value: memory[:usage_percent],
      threshold: 80,
      message: memory[:usage_percent] > 90 ? 'Memory usage critically high' : 
               memory[:usage_percent] > 80 ? 'Memory usage elevated' : 'Memory usage normal'
    }
  end

  def check_disk_health
    disk = get_disk_usage
    {
      status: disk[:usage_percent] > 95 ? 'critical' : disk[:usage_percent] > 85 ? 'warning' : 'healthy',
      value: disk[:usage_percent],
      threshold: 85,
      message: disk[:usage_percent] > 95 ? 'Disk usage critically high' : 
               disk[:usage_percent] > 85 ? 'Disk usage elevated' : 'Disk usage normal'
    }
  end

  def check_network_health
    {
      status: 'healthy',
      latency: rand(1..50),
      message: 'Network connectivity normal'
    }
  end

  def check_process_health
    processes = get_process_stats
    {
      status: processes[:zombie_processes] > 0 ? 'warning' : 'healthy',
      value: processes[:zombie_processes],
      threshold: 0,
      message: processes[:zombie_processes] > 0 ? 'Zombie processes detected' : 'Process health normal'
    }
  end

  def measure_app_response_time(app_name)
    # Simulate response time measurement
    rand(50..1000)
  end

  def get_app_cpu_usage(app_name)
    rand(5..50)
  end

  def get_app_memory_usage(app_name)
    rand(100..500)
  end

  def get_app_request_count(app_name)
    rand(10..1000)
  end

  def generate_performance_report(performance_data)
    response_times = performance_data[:measurements].map { |m| m[:response_time] }
    
    report = {
      app_name: performance_data[:app_name],
      period: performance_data[:start_time],
      duration_seconds: performance_data[:duration],
      average_response_time: (response_times.sum / response_times.length).round(2),
      min_response_time: response_times.min,
      max_response_time: response_times.max,
      total_measurements: response_times.length
    }
    
    puts "Performance report generated for #{performance_data[:app_name]}"
    report
  end

  def default_alert_config
    [
      {
        'name' => 'High CPU Usage',
        'metric' => 'cpu_usage',
        'condition' => 'greater_than',
        'threshold' => 80,
        'duration' => 300
      },
      {
        'name' => 'High Memory Usage',
        'metric' => 'memory_usage',
        'condition' => 'greater_than',
        'threshold' => 85,
        'duration' => 300
      }
    ]
  end

  def generate_alert_id
    "alert_#{Time.now.strftime('%Y%m%d_%H%M%S')}_#{rand(1000..9999)}"
  end

  def evaluate_alert_condition(alert)
    current_value = get_metric_value(alert['metric'])
    
    case alert['condition']
    when 'greater_than'
      current_value > alert['threshold']
    when 'less_than'
      current_value < alert['threshold']
    when 'equals'
      current_value == alert['threshold']
    else
      false
    end
  end

  def get_metric_value(metric)
    case metric
    when 'cpu_usage' then get_cpu_usage
    when 'memory_usage' then get_memory_usage[:usage_percent]
    else 0
    end
  end

  def trigger_alert(alert)
    triggered_alert = {
      alert_id: alert[:id],
      name: alert[:name],
      triggered_at: Time.now.to_s,
      metric: alert[:metric],
      current_value: get_metric_value(alert[:metric']),
      threshold: alert[:threshold],
      status: 'triggered'
    }
    
    puts "ALERT TRIGGERED: #{alert[:name']}"
    triggered_alert
  end

  def find_log_pattern_matches(pattern, directory)
    matches = []
    
    Dir.glob(File.join(directory, '*.log')).each do |log_file|
      File.readlines(log_file).each_with_index do |line, index|
        if line.match?(Regexp.new(pattern))
          matches << {
            file: log_file,
            line_number: index + 1,
            content: line.strip
          }
        end
      end
    end
    
    matches
  end

  def get_running_services
    # Simulate service discovery
    ['nginx', 'postgres', 'redis', 'gamecenter-api'].sample(rand(2..4))
  end

  def check_service_connections
    # Simulate connection checks
    services = get_running_services
    services.map do |service|
      {
        service: service,
        status: rand > 0.1 ? 'connected' : 'disconnected',
        response_time: rand(1..100)
      }
    end
  end

  def perform_dependency_health_checks
    # Simulate health checks
    {
      database: rand > 0.05,
      cache: rand > 0.02,
      external_api: rand > 0.15
    }
  end

  def build_dependency_graph
    # Simulate dependency graph
    {
      'gamecenter-app' => ['database', 'cache', 'external-api'],
      'api-gateway' => ['gamecenter-app', 'auth-service'],
      'auth-service' => ['database', 'cache']
    }
  end

  def get_performance_summary
    {
      average_response_time: rand(100..500),
      error_rate: rand(0.1..5.0),
      throughput: rand(100..1000)
    }
  end
end

# CLI Usage
if __FILE__ == $PROGRAM_NAME
  monitoring = MonitoringUtility.new
  
  command = ARGV[0]
  case command
  when 'metrics'
    puts monitoring.collect_system_metrics.to_json
  when 'health'
    puts monitoring.check_system_health.to_json
  when 'performance'
    monitoring.monitor_application_performance(ARGV[1] || 'app', (ARGV[2] || 60).to_i)
  when 'alerts'
    puts monitoring.setup_monitoring_alerts.to_json
  when 'check-alerts'
    puts monitoring.check_alerts.to_json
  when 'dashboard'
    monitoring.generate_monitoring_dashboard(ARGV[1])
  when 'logs'
    patterns = JSON.parse(ARGV[2] || '{"errors": "ERROR", "warnings": "WARN"}')
    puts monitoring.monitor_log_patterns(patterns, ARGV[1] || 'logs').to_json
  when 'dependencies'
    puts monitoring.track_service_dependencies.to_json
  when 'uptime'
    puts monitoring.generate_uptime_report(ARGV[1] || 'app', (ARGV[2] || 7).to_i).to_json
  else
    puts "Usage: ruby monitoring_utility.rb <command> [args]"
    puts "Commands: metrics, health, performance, alerts, check-alerts, dashboard, logs, dependencies, uptime"
  end
end
