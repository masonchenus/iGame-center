
#!/usr/bin/env ruby
# frozen_string_literal: true

require 'json'
require 'time'

# Task Automation Utility
# Provides task automation and workflow management functions
class TaskAutomation
  def initialize(config_file = 'config/automation.json')
    @config = load_config(config_file)
    @task_queue = []
    @completed_tasks = []
    @active_workflows = {}
  end

  def load_config(config_file)
    return {} unless File.exist?(config_file)
    JSON.parse(File.read(config_file))
  rescue JSON::ParserError
    {}
  end

  def create_task(name, command, options = {})
    puts "Creating task: #{name}"
    
    task = {
      id: generate_task_id,
      name: name,
      command: command,
      status: 'created',
      options: {
        timeout: options[:timeout] || 300,
        retry_count: options[:retry_count] || 0,
        priority: options[:priority] || 'normal',
        dependencies: options[:dependencies] || [],
        environment: options[:environment] || {}
      },
      created_at: Time.now.to_s,
      attempts: 0
    }
    
    @task_queue << task
    puts "Task created: #{task[:id]}"
    task
  end

  def execute_task(task_id, synchronous = true)
    task = find_task(task_id)
    return nil unless task
    
    puts "Executing task: #{task[:name]}"
    
    task[:status] = 'running'
    task[:started_at] = Time.now.to_s
    
    begin
      # Check dependencies
      if check_dependencies(task[:options][:dependencies])
        result = run_task_command(task)
        
        if result[:success]
          task[:status] = 'completed'
          task[:completed_at] = Time.now.to_s
          task[:result] = result
          puts "Task completed: #{task[:name]}"
        else
          handle_task_failure(task, result)
        end
      else
        task[:status] = 'failed'
        task[:error] = 'Dependencies not satisfied'
        puts "Task failed: #{task[:name]} - dependencies not satisfied"
      end
      
    rescue => e
      task[:status] = 'failed'
      task[:error] = e.message
      puts "Task failed: #{task[:name]} - #{e.message}"
    end
    
    @completed_tasks << task
    remove_from_queue(task_id)
    task
  end

  def create_workflow(name, workflow_config)
    puts "Creating workflow: #{name}"
    
    workflow = {
      id: generate_workflow_id,
      name: name,
      steps: workflow_config['steps'] || [],
      triggers: workflow_config['triggers'] || ['manual'],
      conditions: workflow_config['conditions'] || [],
      status: 'created',
      created_at: Time.now.to_s,
      execution_history: []
    }
    
    workflow_file = "config/workflows/#{name}.json"
    FileUtils.mkdir_p(File.dirname(workflow_file))
    File.write(workflow_file, JSON.pretty_generate(workflow))
    
    puts "Workflow created: #{workflow_file}"
    workflow
  end

  def execute_workflow(workflow_name, trigger = 'manual', context = {})
    workflow_file = "config/workflows/#{workflow_name}.json"
    
    unless File.exist?(workflow_file)
      puts "Workflow not found: #{workflow_name}"
      return nil
    end
    
    workflow_config = JSON.parse(File.read(workflow_file))
    
    puts "Executing workflow: #{workflow_name} (trigger: #{trigger})"
    
    execution = {
      workflow_name: workflow_name,
      trigger: trigger,
      context: context,
      start_time: Time.now.to_s,
      status: 'running',
      steps_executed: []
    }
    
    @active_workflows[workflow_name] = execution
    
    begin
      # Evaluate conditions
      if evaluate_workflow_conditions(workflow_config['conditions'], context)
        workflow_config['steps'].each do |step|
          step_result = execute_workflow_step(step, execution)
          execution[:steps_executed] << step_result
          
          if step_result[:status] == 'failed'
            execution[:status] = 'failed'
            break
          end
        end
        
        execution[:status] = 'success' if execution[:status] == 'running'
      else
        execution[:status] = 'skipped'
        execution[:reason] = 'Conditions not satisfied'
      end
      
    rescue => e
      execution[:status] = 'failed'
      execution[:error] = e.message
    end
    
    execution[:end_time] = Time.now.to_s
    @active_workflows.delete(workflow_name)
    
    puts "Workflow execution #{execution[:status]}"
    execution
  end

  def schedule_task(task_id, schedule_time, recurrence = nil)
    task = find_task(task_id)
    return nil unless task
    
    puts "Scheduling task: #{task[:name]} for #{schedule_time}"
    
    scheduled_task = {
      task_id: task_id,
      scheduled_time: schedule_time,
      recurrence: recurrence,
      status: 'scheduled',
      created_at: Time.now.to_s
    }
    
    # In a real implementation, this would be added to a job scheduler
    puts "Task scheduled: #{scheduled_task[:task_id]}"
    scheduled_task
  end

  def create_cron_expression(minute, hour, day, month, weekday, command)
    cron_expr = "#{minute} #{hour} #{day} #{month} #{weekday}"
    
    cron_job = {
      expression: cron_expr,
      command: command,
      created_at: Time.now.to_s,
      active: true
    }
    
    # Generate crontab entry
    crontab_entry = "#{cron_expr} #{command}"
    
    puts "Created cron job: #{cron_expr}"
    { cron_job: cron_job, crontab_entry: crontab_entry }
  end

  def monitor_task_progress(task_id)
    task = find_task(task_id)
    return nil unless task
    
    progress = {
      task_id: task_id,
      task_name: task[:name],
      status: task[:status],
      progress_percentage: calculate_task_progress(task),
      estimated_completion: estimate_completion_time(task),
      logs: get_task_logs(task_id)
    }
    
    progress
  end

  def parallel_execute(task_ids, max_parallel = 5)
    puts "Executing #{task_ids.length} tasks in parallel (max: #{max_parallel})"
    
    results = []
    active_tasks = []
    task_index = 0
    
    # Start initial batch of tasks
    [task_ids.length, max_parallel].min.times do
      break if task_index >= task_ids.length
      
      task_id = task_ids[task_index]
      result = execute_task(task_id, false)
      active_tasks << result if result
      task_index += 1
    end
    
    # Wait for tasks to complete and start new ones
    while active_tasks.any? || task_index < task_ids.length
      # Check for completed tasks
      completed = active_tasks.select { |task| task[:status] == 'completed' }
      active_tasks -= completed
      results.concat(completed)
      
      # Start new tasks if slots available
      while active_tasks.length < max_parallel && task_index < task_ids.length
        task_id = task_ids[task_index]
        result = execute_task(task_id, false)
        active_tasks << result if result
        task_index += 1
      end
      
      sleep 1 # Wait before checking again
    end
    
    # Add remaining active tasks
    active_tasks.each { |task| results << task }
    
    puts "Parallel execution completed: #{results.length} tasks"
    results
  end

  def create_task_template(template_name, template_config)
    puts "Creating task template: #{template_name}"
    
    template = {
      name: template_name,
      command_template: template_config['command_template'],
      default_options: template_config['default_options'] || {},
      variables: template_config['variables'] || [],
      created_at: Time.now.to_s
    }
    
    template_file = "config/templates/#{template_name}.json"
    FileUtils.mkdir_p(File.dirname(template_file))
    File.write(template_file, JSON.pretty_generate(template))
    
    puts "Template created: #{template_file}"
    template
  end

  def execute_template_task(template_name, variables = {})
    template_file = "config/templates/#{template_name}.json"
    
    unless File.exist?(template_file)
      puts "Template not found: #{template_name}"
      return nil
    end
    
    template = JSON.parse(File.read(template_file))
    
    # Process template variables
    command = template['command_template']
    options = template['default_options'].dup
    
    variables.each do |key, value|
      command = command.gsub("{{#{key}}}", value.to_s)
      # Merge variables into options
      options[key] = value if key.is_a?(Symbol) || key.is_a?(String)
    end
    
    create_task("#{template_name}_#{Time.now.to_i}", command, options)
  end

  def get_automation_statistics
    puts "Generating automation statistics..."
    
    stats = {
      timestamp: Time.now.to_s,
      queue_size: @task_queue.length,
      completed_tasks: @completed_tasks.length,
      active_workflows: @active_workflows.length,
      success_rate: calculate_success_rate,
      average_execution_time: calculate_average_execution_time,
      most_used_commands: get_most_used_commands,
      failed_tasks: @completed_tasks.count { |t| t[:status] == 'failed' }
    }
    
    stats
  end

  def cleanup_old_executions(days_to_keep = 30)
    puts "Cleaning up executions older than #{days_to_keep} days"
    
    cutoff_date = Date.today - days_to_keep
    original_count = @completed_tasks.length
    
    @completed_tasks.reject! do |task|
      task_date = Date.parse(task[:created_at])
      task_date < cutoff_date
    end
    
    cleaned_count = original_count - @completed_tasks.length
    puts "Cleaned up #{cleaned_count} old executions"
    cleaned_count
  end

  private

  def generate_task_id
    "task_#{Time.now.strftime('%Y%m%d_%H%M%S')}_#{rand(1000..9999)}"
  end

  def generate_workflow_id
    "workflow_#{Time.now.strftime('%Y%m%d_%H%M%S')}_#{rand(1000..9999)}"
  end

  def find_task(task_id)
    @task_queue.find { |task| task[:id] == task_id } ||
    @completed_tasks.find { |task| task[:id] == task_id }
  end

  def remove_from_queue(task_id)
    @task_queue.reject! { |task| task[:id] == task_id }
  end

  def check_dependencies(dependencies)
    return true if dependencies.empty?
    
    dependencies.all? do |dep|
      dep_task = @completed_tasks.find { |task| task[:id] == dep }
      dep_task && dep_task[:status] == 'completed'
    end
  end

  def run_task_command(task)
    puts "  Executing: #{task[:command]}"
    
    # Simulate command execution
    start_time = Time.now
    
    # Simulate execution time based on task complexity
    execution_time = rand(1..10)
    sleep execution_time
    
    end_time = Time.now
    
    {
      success: rand > 0.1, # 90% success rate
      execution_time: (end_time - start_time).round(2),
      output: "Task output for #{task[:name]}",
      exit_code: rand > 0.1 ? 0 : 1
    }
  end

  def handle_task_failure(task, result)
    task[:attempts] += 1
    max_attempts = task[:options][:retry_count] + 1
    
    if task[:attempts] < max_attempts
      task[:status] = 'retrying'
      task[:next_retry_at] = (Time.now + (task[:attempts] * 60)).to_s
      puts "Task will retry (attempt #{task[:attempts] + 1}/#{max_attempts})"
    else
      task[:status] = 'failed'
      task[:result] = result
      puts "Task failed after #{task[:attempts]} attempts"
    end
  end

  def evaluate_workflow_conditions(conditions, context)
    return true if conditions.empty?
    
    conditions.all? do |condition|
      case condition['type']
      when 'equals'
        context[condition['field']] == condition['value']
      when 'contains'
        context[condition['field']].to_s.include?(condition['value'])
      when 'greater_than'
        context[condition['field']] > condition['value']
      else
        false
      end
    end
  end

  def execute_workflow_step(step, execution)
    puts "Executing workflow step: #{step['name']}"
    
    start_time = Time.now
    
    # Create task for the step
    task = create_task(step['name'], step['command'], step['options'] || {})
    result = execute_task(task[:id])
    
    end_time = Time.now
    
    {
      step_name: step['name'],
      command: step['command'],
      status: result[:status],
      duration: (end_time - start_time).round(2),
      result: result
    }
  end

  def calculate_task_progress(task)
    return 100 if task[:status] == 'completed'
    return 0 if task[:status] == 'created'
    
    # Rough progress estimation
    case task[:status]
    when 'running' then rand(10..90)
    when 'retrying' then rand(50..95)
    else 0
    end
  end

  def estimate_completion_time(task)
    return nil unless task[:status] == 'running'
    
    # Simple estimation based on elapsed time and progress
    elapsed_time = Time.now - Time.parse(task[:started_at])
    progress = calculate_task_progress(task)
    
    if progress > 0
      estimated_total = elapsed_time * (100 / progress)
      (Time.now + (estimated_total - elapsed_time)).to_s
    else
      nil
    end
  end

  def get_task_logs(task_id)
    # Simulate task logs
    [
      { timestamp: Time.now.to_s, message: "Task started: #{task_id}" },
      { timestamp: Time.now.to_s, message: "Processing..." },
      { timestamp: Time.now.to_s, message: "Task completed successfully" }
    ]
  end

  def calculate_success_rate
    return 0 if @completed_tasks.empty?
    
    successful = @completed_tasks.count { |task| task[:status] == 'completed' }
    (successful.to_f / @completed_tasks.length * 100).round(2)
  end

  def calculate_average_execution_time
    completed_with_timing = @completed_tasks.select { |task| task[:started_at] && task[:completed_at] }
    return 0 if completed_with_timing.empty?
    
    total_time = completed_with_timing.sum do |task|
      start = Time.parse(task[:started_at])
      finish = Time.parse(task[:completed_at])
      finish - start
    end
    
    (total_time / completed_with_timing.length).round(2)
  end

  def get_most_used_commands
    command_frequency = Hash.new(0)
    
    @completed_tasks.each do |task|
      command_frequency[task[:command]] += 1
    end
    
    command_frequency.sort_by { |_k, v| -v }.first(10).to_h
  end
end

# CLI Usage
if __FILE__ == $PROGRAM_NAME
  automation = TaskAutomation.new
  
  command = ARGV[0]
  case command
  when 'create-task'
    automation.create_task(ARGV[1], ARGV[2])
  when 'execute'
    automation.execute_task(ARGV[1])
  when 'workflow'
    workflow_config = JSON.parse(File.read(ARGV[2])) if ARGV[2] && File.exist?(ARGV[2])
    automation.create_workflow(ARGV[1], workflow_config || {})
  when 'run-workflow'
    automation.execute_workflow(ARGV[1], ARGV[2] || 'manual')
  when 'schedule'
    automation.schedule_task(ARGV[1], ARGV[2])
  when 'cron'
    automation.create_cron_expression(ARGV[1], ARGV[2], ARGV[3], ARGV[4], ARGV[5], ARGV[6])
  when 'progress'
    puts automation.monitor_task_progress(ARGV[1]).to_json
  when 'parallel'
    task_ids = ARGV[1..-1]
    automation.parallel_execute(task_ids)
  when 'template'
    template_config = JSON.parse(File.read(ARGV[2])) if ARGV[2] && File.exist?(ARGV[2])
    automation.create_task_template(ARGV[1], template_config || {})
  when 'run-template'
    variables = JSON.parse(ARGV[2]) if ARGV[2] && File.exist?(ARGV[2])
    automation.execute_template_task(ARGV[1], variables || {})
  when 'stats'
    puts automation.get_automation_statistics.to_json
  when 'cleanup'
    automation.cleanup_old_executions((ARGV[1] || 30).to_i)
  else
    puts "Usage: ruby task_automation.rb <command> [args]"
    puts "Commands: create-task, execute, workflow, run-workflow, schedule, cron, progress, parallel, template, run-template, stats, cleanup"
  end
end

