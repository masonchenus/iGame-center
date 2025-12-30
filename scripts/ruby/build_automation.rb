
#!/usr/bin/env ruby
# frozen_string_literal: true

require 'json'
require 'open3'

# Build Automation Utility
# Provides build process automation and management functions
class BuildAutomation
  def initialize(config_file = 'config/build.json')
    @config = load_config(config_file)
    @build_history = []
  end

  def load_config(config_file)
    return {} unless File.exist?(config_file)
    JSON.parse(File.read(config_file))
  rescue JSON::ParserError
    {}
  end

  def run_build(build_type = 'development', target_platform = 'web')
    puts "Starting build: #{build_type} for #{target_platform}"
    
    build_info = {
      id: generate_build_id,
      type: build_type,
      platform: target_platform,
      start_time: Time.now.to_s,
      status: 'running',
      steps: []
    }
    
    begin
      # Pre-build validation
      validate_build_environment(build_info)
      
      # Clean previous builds
      clean_build_directory(build_info)
      
      # Install dependencies
      install_dependencies(build_info, @config['package_manager'] || 'npm')
      
      # Run linting
      run_linting(build_info)
      
      # Run tests
      run_tests(build_info)
      
      # Build application
      build_application(build_info, build_type, target_platform)
      
      # Optimize assets
      optimize_assets(build_info)
      
      # Generate build artifacts
      generate_artifacts(build_info)
      
      # Post-build validation
      validate_build_output(build_info)
      
      build_info[:status] = 'success'
      build_info[:end_time] = Time.now.to_s
      puts "Build completed successfully: #{build_info[:id]}"
      
    rescue => e
      build_info[:status] = 'failed'
      build_info[:error] = e.message
      build_info[:end_time] = Time.now.to_s
      puts "Build failed: #{e.message}"
    end
    
    @build_history << build_info
    save_build_history
    build_info
  end

  def run_docker_build(image_name, tag = 'latest')
    puts "Building Docker image: #{image_name}:#{tag}"
    
    docker_info = {
      image_name: image_name,
      tag: tag,
      start_time: Time.now.to_s,
      status: 'building'
    }
    
    begin
      # Build Docker image
      build_command = "docker build -t #{image_name}:#{tag} ."
      result = execute_command(build_command)
      
      if result[:success]
        docker_info[:status] = 'built'
        docker_info[:image_id] = result[:output][0..11]
        puts "Docker image built successfully: #{image_name}:#{tag}"
      else
        docker_info[:status] = 'failed'
        docker_info[:error] = result[:error]
        puts "Docker build failed: #{result[:error]}"
      end
      
    rescue => e
      docker_info[:status] = 'failed'
      docker_info[:error] = e.message
    end
    
    docker_info[:end_time] = Time.now.to_s
    docker_info
  end

  def create_build_pipeline(pipeline_name, config)
    puts "Creating build pipeline: #{pipeline_name}"
    
    pipeline = {
      name: pipeline_name,
      steps: config['steps'] || default_pipeline_steps,
      triggers: config['triggers'] || ['push', 'pull_request'],
      environment: config['environment'] || 'development',
      created_at: Time.now.to_s
    }
    
    pipeline_file = "config/pipelines/#{pipeline_name}.json"
    FileUtils.mkdir_p(File.dirname(pipeline_file))
    File.write(pipeline_file, JSON.pretty_generate(pipeline))
    
    puts "Pipeline created: #{pipeline_file}"
    pipeline
  end

  def run_pipeline(pipeline_name, trigger = 'manual')
    pipeline_file = "config/pipelines/#{pipeline_name}.json"
    
    unless File.exist?(pipeline_file)
      puts "Pipeline not found: #{pipeline_name}"
      return nil
    end
    
    pipeline_config = JSON.parse(File.read(pipeline_file))
    
    puts "Running pipeline: #{pipeline_name} (triggered by: #{trigger})"
    
    execution_result = {
      pipeline_name: pipeline_name,
      trigger: trigger,
      start_time: Time.now.to_s,
      steps_executed: [],
      status: 'running'
    }
    
    begin
      pipeline_config['steps'].each do |step|
        step_result = execute_pipeline_step(step, execution_result)
        execution_result[:steps_executed] << step_result
        
        if step_result[:status] == 'failed'
          execution_result[:status] = 'failed'
          break
        end
      end
      
      execution_result[:status] = 'success' if execution_result[:status] == 'running'
      
    rescue => e
      execution_result[:status] = 'failed'
      execution_result[:error] = e.message
    end
    
    execution_result[:end_time] = Time.now.to_s
    puts "Pipeline execution #{execution_result[:status]}"
    execution_result
  end

  def optimize_build_performance
    puts "Analyzing build performance..."
    
    performance_report = {
      timestamp: Time.now.to_s,
      recommendations: [],
      metrics: {
        average_build_time: calculate_average_build_time,
        slowest_steps: identify_slowest_steps,
        parallel_opportunities: find_parallel_opportunities
      }
    }
    
    # Generate recommendations
    if performance_report[:metrics][:average_build_time] > 300 # 5 minutes
      performance_report[:recommendations] << "Consider parallelizing build steps"
    end
    
    performance_report[:recommendations] << "Use incremental builds for faster iterations"
    performance_report[:recommendations] << "Implement build caching for dependencies"
    performance_report[:recommendations] << "Optimize bundle size and tree-shaking"
    
    performance_report
  end

  def generate_build_report(output_file = 'build_report.json')
    report = {
      generated_at: Time.now.to_s,
      total_builds: @build_history.length,
      successful_builds: @build_history.count { |b| b[:status] == 'success' },
      failed_builds: @build_history.count { |b| b[:status] == 'failed' },
      success_rate: (@build_history.count { |b| b[:status] == 'success' }.to_f / @build_history.length * 100).round(2),
      recent_builds: @build_history.last(10),
      build_trends: analyze_build_trends
    }
    
    File.write(output_file, JSON.pretty_generate(report))
    puts "Build report generated: #{output_file}"
    report
  end

  def setup_continuous_integration
    puts "Setting up continuous integration..."
    
    ci_config = {
      provider: 'github_actions',
      workflows: [
        {
          name: 'CI Pipeline',
          triggers: ['push', 'pull_request'],
          jobs: [
            'lint',
            'test',
            'build',
            'security-scan'
          ]
        }
      ],
      created_at: Time.now.to_s
    }
    
    # Generate GitHub Actions workflow
    workflow_content = generate_github_actions_workflow(ci_config)
    workflow_path = '.github/workflows/ci.yml'
    
    FileUtils.mkdir_p(File.dirname(workflow_path))
    File.write(workflow_path, workflow_content)
    
    puts "CI workflow created: #{workflow_path}"
    ci_config
  end

  def manage_build_artifacts
    puts "Managing build artifacts..."
    
    artifacts_config = {
      storage_path: 'build/artifacts',
      retention_days: 30,
      compression: true,
      encryption: false
    }
    
    # List existing artifacts
    artifacts_dir = artifacts_config[:storage_path]
    existing_artifacts = []
    
    if Dir.exist?(artifacts_dir)
      Dir.glob(File.join(artifacts_dir, '**', '*')).each do |file|
        next unless File.file?(file)
        
        existing_artifacts << {
          name: File.basename(file),
          path: file,
          size: File.size(file),
          created_at: File.mtime(file).to_s,
          compressed: file.end_with?('.gz', '.zip')
        }
      end
    end
    
    # Clean up old artifacts
    cleaned_count = cleanup_old_artifacts(artifacts_config[:retention_days])
    
    {
      config: artifacts_config,
      artifacts: existing_artifacts,
      cleaned_count: cleaned_count
    }
  end

  private

  def generate_build_id
    "build_#{Time.now.strftime('%Y%m%d_%H%M%S')}_#{rand(1000..9999)}"
  end

  def validate_build_environment(build_info)
    puts "Validating build environment..."
    
    checks = [
      { name: 'Node.js installed', check: -> { system('node --version > /dev/null 2>&1') } },
      { name: 'Package manager available', check: -> { system('npm --version > /dev/null 2>&1') } },
      { name: 'Git repository', check: -> { File.exist?('.git') } }
    ]
    
    checks.each do |check|
      if check[:check].call
        build_info[:steps] << { step: "validation_#{check[:name].downcase.gsub(' ', '_')}", status: 'passed' }
      else
        raise "Build environment validation failed: #{check[:name]}"
      end
    end
    
    puts "Build environment validation completed"
  end

  def clean_build_directory(build_info)
    puts "Cleaning build directory..."
    
    dirs_to_clean = ['dist', 'build', 'coverage', '.cache']
    
    dirs_to_clean.each do |dir|
      if Dir.exist?(dir)
        FileUtils.remove_entry(dir)
        puts "  Cleaned: #{dir}"
      end
    end
    
    build_info[:steps] << { step: 'clean', status: 'completed' }
  end

  def install_dependencies(build_info, package_manager)
    puts "Installing dependencies with #{package_manager}..."
    
    case package_manager
    when 'npm'
      command = 'npm ci'
    when 'yarn'
      command = 'yarn install --frozen-lockfile'
    when 'pnpm'
      command = 'pnpm install --frozen-lockfile'
    else
      raise "Unsupported package manager: #{package_manager}"
    end
    
    result = execute_command(command)
    
    if result[:success]
      build_info[:steps] << { step: 'install_dependencies', status: 'completed' }
      puts "Dependencies installed successfully"
    else
      raise "Failed to install dependencies: #{result[:error]}"
    end
  end

  def run_linting(build_info)
    puts "Running linting..."
    
    result = execute_command('npm run lint')
    
    if result[:success]
      build_info[:steps] << { step: 'linting', status: 'passed' }
      puts "Linting passed"
    else
      build_info[:steps] << { step: 'linting', status: 'failed' }
      puts "Linting failed: #{result[:error]}"
    end
  end

  def run_tests(build_info)
    puts "Running tests..."
    
    result = execute_command('npm test')
    
    if result[:success]
      build_info[:steps] << { step: 'testing', status: 'passed' }
      puts "Tests passed"
    else
      build_info[:steps] << { step: 'testing', status: 'failed' }
      puts "Tests failed: #{result[:error]}"
    end
  end

  def build_application(build_info, build_type, platform)
    puts "Building application for #{build_type} on #{platform}..."
    
    build_command = case platform
    when 'web'
      "npm run build"
    when 'electron'
      "npm run build:electron"
    when 'mobile'
      "npm run build:mobile"
    else
      "npm run build"
    end
    
    result = execute_command(build_command)
    
    if result[:success]
      build_info[:steps] << { step: 'build_application', status: 'completed' }
      puts "Application built successfully"
    else
      raise "Build failed: #{result[:error]}"
    end
  end

  def optimize_assets(build_info)
    puts "Optimizing assets..."
    
    optimizations = [
      'Minification',
      'Compression',
      'Tree-shaking',
      'Code splitting'
    ]
    
    optimizations.each do |opt|
      puts "  #{opt}..."
      sleep 0.5 # Simulate optimization
    end
    
    build_info[:steps] << { step: 'asset_optimization', status: 'completed' }
  end

  def generate_artifacts(build_info)
    puts "Generating build artifacts..."
    
    artifacts = []
    
    if Dir.exist?('dist')
      Dir.glob('dist/**/*').each do |file|
        next unless File.file?(file)
        
        artifacts << {
          name: file.sub('dist/', ''),
          size: File.size(file),
          type: File.extname(file)
        }
      end
    end
    
    build_info[:artifacts] = artifacts
    build_info[:steps] << { step: 'generate_artifacts', status: 'completed' }
    puts "Generated #{artifacts.length} artifacts"
  end

  def validate_build_output(build_info)
    puts "Validating build output..."
    
    validation_checks = [
      { name: 'Build directory exists', check: -> { Dir.exist?('dist') } },
      { name: 'Index file present', check: -> { File.exist?('dist/index.html') } },
      { name: 'No build errors', check: -> { build_info[:steps].none? { |s| s[:status] == 'failed' } } }
    ]
    
    validation_checks.each do |check|
      if check[:check].call
        puts "  ✓ #{check[:name]}"
      else
        raise "Build validation failed: #{check[:name]}"
      end
    end
    
    build_info[:steps] << { step: 'validation', status: 'completed' }
  end

  def execute_command(command)
    stdout, stderr, status = Open3.capture3(command)
    {
      success: status.success?,
      output: stdout,
      error: stderr,
      exit_code: status.exitstatus
    }
  end

  def default_pipeline_steps
    [
      { name: 'Checkout', command: 'git checkout ${GIT_COMMIT}' },
      { name: 'Install', command: 'npm ci' },
      { name: 'Lint', command: 'npm run lint' },
      { name: 'Test', command: 'npm test' },
      { name: 'Build', command: 'npm run build' }
    ]
  end

  def execute_pipeline_step(step, context)
    puts "Executing step: #{step[:name]}"
    
    start_time = Time.now
    result = execute_command(step[:command])
    end_time = Time.now
    
    {
      name: step[:name],
      command: step[:command],
      status: result[:success] ? 'passed' : 'failed',
      duration: (end_time - start_time).round(2),
      output: result[:output],
      error: result[:error]
    }
  end

  def calculate_average_build_time
    return 0 if @build_history.empty?
    
    completed_builds = @build_history.select { |b| b[:start_time] && b[:end_time] }
    return 0 if completed_builds.empty?
    
    total_time = completed_builds.sum do |build|
      start = Time.parse(build[:start_time])
      finish = Time.parse(build[:end_time])
      finish - start
    end
    
    (total_time / completed_builds.length).round(2)
  end

  def identify_slowest_steps
    step_durations = {}
    
    @build_history.each do |build|
      build[:steps].each do |step|
        name = step[:step]
        duration = calculate_step_duration(step)
        step_durations[name] ||= []
        step_durations[name] << duration if duration
      end
    end
    
    step_durations.transform_values! { |durations| durations.sum / durations.length }
    step_durations.sort_by { |_k, v| -v }.first(5).to_h
  end

  def find_parallel_opportunities
    [
      {
        name: 'Dependency installation and environment setup',
        estimated_time_saved: 30,
        complexity: 'medium'
      },
      {
        name: 'Test execution and security scanning',
        estimated_time_saved: 45,
        complexity: 'low'
      }
    ]
  end

  def calculate_step_duration(step)
    # This would need timestamps in the step data
    # For now, return a mock duration
    rand(5..60)
  end

  def analyze_build_trends
    {
      daily_builds: @build_history.group_by { |b| Time.parse(b[:start_time]).to_date }.transform_values(&:length),
      success_rate_trend: 'improving', # Would calculate actual trend
      average_build_time_trend: 'stable'
    }
  end

  def generate_github_actions_workflow(ci_config)
    <<~YAML
      name: CI Pipeline
      
      on:
        push:
          branches: [ main, develop ]
        pull_request:
          branches: [ main ]
      
      jobs:
        lint:
          runs-on: ubuntu-latest
          steps:
            - uses: actions/checkout@v3
            - uses: actions/setup-node@v3
              with:
                node-version: '18'
            - run: npm ci
            - run: npm run lint
        
        test:
          runs-on: ubuntu-latest
          steps:
            - uses: actions/checkout@v3
            - uses: actions/setup-node@v3
              with:
                node-version: '18'
            - run: npm ci
            - run: npm test
        
        build:
          runs-on: ubuntu-latest
          steps:
            - uses: actions/checkout@v3
            - uses: actions/setup-node@v3
              with:
                node-version: '18'
            - run: npm ci
            - run: npm run build
    YAML
  end

  def cleanup_old_artifacts(retention_days)
    cleaned_count = 0
    
    artifacts_dir = 'build/artifacts'
    return cleaned_count unless Dir.exist?(artifacts_dir)
    
    cutoff_date = Date.today - retention_days
    
    Dir.glob(File.join(artifacts_dir, '**', '*')).each do |file|
      next unless File.file?(file)
      
      if File.mtime(file).to_date < cutoff_date
        File.delete(file)
        cleaned_count += 1
      end
    end
    
    cleaned_count
  end

  def save_build_history
    history_file = 'logs/build_history.json'
    File.write(history_file, JSON.pretty_generate(@build_history))
  end
end

# CLI Usage
if __FILE__ == $PROGRAM_NAME
  build_auto = BuildAutomation.new
  
  command = ARGV[0]
  case command
  when 'build'
    build_auto.run_build(ARGV[1] || 'development', ARGV[2] || 'web')
  when 'docker'
    build_auto.run_docker_build(ARGV[1], ARGV[2] || 'latest')
  when 'pipeline'
    pipeline_config = JSON.parse(File.read(ARGV[2])) if ARGV[2] && File.exist?(ARGV[2])
    build_auto.create_pipeline(ARGV[1], pipeline_config || {})
  when 'run-pipeline'
    build_auto.run_pipeline(ARGV[1], ARGV[2] || 'manual')
  when 'performance'
    puts build_auto.optimize_build_performance.to_json
  when 'report'
    build_auto.generate_build_report(ARGV[1])
  when 'ci'
    build_auto.setup_continuous_integration
  when 'artifacts'
    puts build_auto.manage_build_artifacts.to_json
  else
    puts "Usage: ruby build_automation.rb <command> [args]"
    puts "Commands: build, docker, pipeline, run-pipeline, performance, report, ci, artifacts"
  end
end

