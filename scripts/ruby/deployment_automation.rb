#!/usr/bin/env ruby
# frozen_string_literal: true

require 'json'
require 'date'

# Deployment Automation Utility
# Handles application deployment workflows and automation
class DeploymentAutomation
  def initialize(config_file = 'config/deployment.json')
    @config = load_config(config_file)
    @deployment_history = []
  end

  def load_config(config_file)
    return {} unless File.exist?(config_file)
    JSON.parse(File.read(config_file))
  rescue JSON::ParserError
    {}
  end

  def deploy_application(app_name, version, environment = 'production')
    deployment_id = generate_deployment_id
    puts "Starting deployment: #{app_name} v#{version} to #{environment}"
    
    deployment_info = {
      id: deployment_id,
      app_name: app_name,
      version: version,
      environment: environment,
      start_time: Time.now.to_s,
      status: 'in_progress',
      steps: []
    }
    
    begin
      # Pre-deployment validation
      validate_deployment(app_name, version, environment)
      deployment_info[:steps] << { step: 'validation', status: 'completed', time: Time.now.to_s }
      
      # Backup current deployment
      backup_current_deployment(app_name, environment)
      deployment_info[:steps] << { step: 'backup', status: 'completed', time: Time.now.to_s }
      
      # Deploy application
      execute_deployment(app_name, version, environment)
      deployment_info[:steps] << { step: 'deployment', status: 'completed', time: Time.now.to_s }
      
      # Run health checks
      health_check_result = run_health_checks(app_name, environment)
      deployment_info[:steps] << { step: 'health_check', status: health_check_result ? 'completed' : 'failed', time: Time.now.to_s }
      
      if health_check_result
        deployment_info[:status] = 'success'
        deployment_info[:end_time] = Time.now.to_s
        puts "Deployment #{deployment_id} completed successfully"
      else
        deployment_info[:status] = 'failed'
        rollback_deployment(app_name, environment)
        puts "Deployment #{deployment_id} failed - rolled back"
      end
      
    rescue => e
      deployment_info[:status] = 'error'
      deployment_info[:error] = e.message
      deployment_info[:end_time] = Time.now.to_s
      puts "Deployment #{deployment_id} encountered an error: #{e.message}"
    end
    
    @deployment_history << deployment_info
    save_deployment_history
    deployment_info
  end

  def rollback_deployment(app_name, environment)
    puts "Rolling back #{app_name} in #{environment}"
    
    # Find last successful deployment
    last_successful = @deployment_history.reverse.find { |d| d[:status] == 'success' && d[:app_name] == app_name }
    
    if last_successful
      puts "Rolling back to version #{last_successful[:version]}"
      execute_deployment(app_name, last_successful[:version], environment)
    else
      puts "No previous deployment found to rollback to"
    end
  end

  def validate_deployment(app_name, version, environment)
    # Check if version exists
    unless version_exists?(app_name, version)
      raise "Version #{version} not found for #{app_name}"
    end
    
    # Check environment availability
    unless environment_available?(environment)
      raise "Environment #{environment} is not available"
    end
    
    # Check resource availability
    unless check_resource_availability(environment)
      raise "Insufficient resources in #{environment}"
    end
    
    puts "Deployment validation passed"
  end

  def execute_deployment(app_name, version, environment)
    deployment_method = @config.dig('deployment_methods', environment) || 'default'
    
    case deployment_method
    when 'docker'
      execute_docker_deployment(app_name, version, environment)
    when 'kubernetes'
      execute_kubernetes_deployment(app_name, version, environment)
    when 'script'
      execute_script_deployment(app_name, version, environment)
    else
      execute_default_deployment(app_name, version, environment)
    end
  end

  def execute_docker_deployment(app_name, version, environment)
    puts "Deploying with Docker: #{app_name}:#{version}"
    
    commands = [
      "docker pull #{app_name}:#{version}",
      "docker stop #{app_name}-#{environment}",
      "docker rm #{app_name}-#{environment}",
      "docker run -d --name #{app_name}-#{environment} #{app_name}:#{version}"
    ]
    
    commands.each do |cmd|
      puts "  Executing: #{cmd}"
      sleep 1 # Simulate command execution
    end
  end

  def execute_kubernetes_deployment(app_name, version, environment)
    puts "Deploying with Kubernetes: #{app_name}:#{version}"
    
    # Simulate kubectl commands
    commands = [
      "kubectl set image deployment/#{app_name} #{app_name}=#{app_name}:#{version}",
      "kubectl rollout status deployment/#{app_name}"
    ]
    
    commands.each do |cmd|
      puts "  Executing: #{cmd}"
      sleep 1
    end
  end

  def execute_script_deployment(app_name, version, environment)
    puts "Deploying with custom script: #{app_name}:#{version}"
    
    script_path = @config.dig('scripts', 'deploy') || 'scripts/deploy.sh'
    command = "#{script_path} #{app_name} #{version} #{environment}"
    
    puts "  Executing: #{command}"
    sleep 2
  end

  def execute_default_deployment(app_name, version, environment)
    puts "Executing default deployment: #{app_name}:#{version}"
    
    # Simulate file copying and service restart
    steps = [
      'Copy application files',
      'Update configuration',
      'Restart services',
      'Update load balancer'
    ]
    
    steps.each do |step|
      puts "  #{step}..."
      sleep 1
    end
  end

  def run_health_checks(app_name, environment)
    puts "Running health checks for #{app_name} in #{environment}"
    
    checks = [
      { name: 'Application startup', check: -> { check_application_startup(app_name, environment) } },
      { name: 'Database connectivity', check: -> { check_database_connectivity(environment) } },
      { name: 'API endpoints', check: -> { check_api_endpoints(environment) } },
      { name: 'Resource usage', check: -> { check_resource_usage(environment) } }
    ]
    
    all_passed = true
    
    checks.each do |check|
      result = check[:check].call
      status = result ? '✓' : '✗'
      puts "  #{status} #{check[:name]}"
      all_passed = false unless result
    end
    
    all_passed
  end

  def backup_current_deployment(app_name, environment)
    puts "Creating backup of current deployment"
    backup_name = "#{app_name}-#{environment}-#{Time.now.strftime('%Y%m%d_%H%M%S')}"
    
    # Simulate backup creation
    backup_info = {
      name: backup_name,
      app_name: app_name,
      environment: environment,
      created_at: Time.now.to_s,
      location: "backups/#{backup_name}"
    }
    
    puts "Backup created: #{backup_name}"
    backup_info
  end

  def get_deployment_history(app_name = nil, limit = 10)
    history = @deployment_history
    
    history = history.select { |d| d[:app_name] == app_name } if app_name
    history.last(limit).reverse
  end

  def get_deployment_status(deployment_id)
    @deployment_history.find { |d| d[:id] == deployment_id }
  end

  def list_available_versions(app_name)
    # Simulate version listing
    versions = ['1.0.0', '1.0.1', '1.1.0', '1.1.1', '2.0.0']
    versions.select { |v| version_exists?(app_name, v) }
  end

  def list_environments
    @config['environments'] || ['production', 'staging', 'development']
  end

  def generate_deployment_report(output_file = 'deployment_report.json')
    report = {
      generated_at: Time.now.to_s,
      total_deployments: @deployment_history.length,
      successful_deployments: @deployment_history.count { |d| d[:status] == 'success' },
      failed_deployments: @deployment_history.count { |d| d[:status] == 'failed' },
      recent_deployments: get_deployment_history(nil, 20),
      deployment_frequency: calculate_deployment_frequency
    }
    
    File.write(output_file, JSON.pretty_generate(report))
    puts "Deployment report generated: #{output_file}"
    report
  end

  private

  def version_exists?(app_name, version)
    # Simulate version existence check
    true
  end

  def environment_available?(environment)
    # Simulate environment availability check
    true
  end

  def check_resource_availability(environment)
    # Simulate resource availability check
    true
  end

  def check_application_startup(app_name, environment)
    # Simulate application startup check
    rand > 0.1 # 90% success rate
  end

  def check_database_connectivity(environment)
    # Simulate database connectivity check
    rand > 0.05 # 95% success rate
  end

  def check_api_endpoints(environment)
    # Simulate API endpoint check
    rand > 0.1 # 90% success rate
  end

  def check_resource_usage(environment)
    # Simulate resource usage check
    rand > 0.2 # 80% success rate
  end

  def save_deployment_history
    history_file = 'logs/deployment_history.json'
    File.write(history_file, JSON.pretty_generate(@deployment_history))
  end

  def calculate_deployment_frequency
    if @deployment_history.length < 2
      return 0
    end
    
    sorted_history = @deployment_history.sort_by { |d| d[:start_time] }
    first_deployment = DateTime.parse(sorted_history.first[:start_time])
    last_deployment = DateTime.parse(sorted_history.last[:start_time])
    
    days = (last_deployment - first_deployment).to_i
    days > 0 ? (@deployment_history.length.to_f / days) : 0
  end
end

# CLI Usage
if __FILE__ == $PROGRAM_NAME
  deploy_automation = DeploymentAutomation.new
  
  command = ARGV[0]
  case command
  when 'deploy'
    deploy_automation.deploy_application(ARGV[1], ARGV[2], ARGV[3])
  when 'rollback'
    deploy_automation.rollback_deployment(ARGV[1], ARGV[2])
  when 'history'
    puts deploy_automation.get_deployment_history(ARGV[1], (ARGV[2] || 10).to_i).to_json
  when 'status'
    puts deploy_automation.get_deployment_status(ARGV[1]).to_json
  when 'versions'
    puts deploy_automation.list_available_versions(ARGV[1]).to_json
  when 'environments'
    puts deploy_automation.list_environments.to_json
  when 'report'
    deploy_automation.generate_deployment_report(ARGV[1])
  else
    puts "Usage: ruby deployment_automation.rb <command> [args]"
    puts "Commands: deploy, rollback, history, status, versions, environments, report"
    puts "Example: ruby deployment_automation.rb deploy gamecenter 2.0.0 production"
  end
end
