#!/usr/bin/env ruby
# frozen_string_literal: true

require 'json'
require 'open3'

# Security Scanner Utility
# Provides security analysis and vulnerability scanning functions
class SecurityScanner
  def initialize(config_file = 'config/security.json')
    @config = load_config(config_file)
    @scan_results = []
  end

  def load_config(config_file)
    return {} unless File.exist?(config_file)
    JSON.parse(File.read(config_file))
  rescue JSON::ParserError
    {}
  end

  def run_security_scan(target_directory = '.', scan_type = 'full')
    puts "Running #{scan_type} security scan on #{target_directory}"
    
    scan_info = {
      id: generate_scan_id,
      target: target_directory,
      scan_type: scan_type,
      start_time: Time.now.to_s,
      status: 'running'
    }
    
    case scan_type
    when 'vulnerability'
      scan_vulnerabilities(target_directory, scan_info)
    when 'code_quality'
      scan_code_quality(target_directory, scan_info)
    when 'dependency'
      scan_dependencies(target_directory, scan_info)
    when 'secrets'
      scan_secrets(target_directory, scan_info)
    when 'full'
      scan_info = run_full_security_scan(target_directory)
    end
    
    scan_info
  end

  def scan_vulnerabilities(directory, scan_info = {})
    puts "Scanning for vulnerabilities..."
    
    vulnerabilities = []
    
    # Check for common vulnerability patterns
    patterns = {
      'SQL Injection' => /('|")\s*OR\s*1\s*=\s*1/i,
      'Command Injection' => /system\s*\(|exec\s*\(|shell_exec/i,
      'Path Traversal' => /\.\.\//,
      'Weak Cryptography' => /md5|sha1\s*\(/i,
      'Insecure Random' => /rand\s*\(|srand\s*\(/i,
      'Hardcoded Password' => /(password|passwd|pwd)\s*=\s*["'][^"']+["']/i,
      'Debug Code' => /(console\.log|print_r|var_dump)/i
    }
    
    Find.find(directory) do |path|
      next unless File.file?(path)
      next if path.include?('.git')
      next unless code_file?(path)
      
      content = File.read(path)
      patterns.each do |vuln_type, pattern|
        content.each_line.with_index do |line, line_num|
          if line =~ pattern
            vulnerabilities << {
              file: path,
              line: line_num + 1,
              vulnerability: vuln_type,
              severity: get_vulnerability_severity(vuln_type),
              code_snippet: line.strip,
              recommendation: get_vulnerability_recommendation(vuln_type)
            }
          end
        end
      end
    end
    
    scan_info[:vulnerabilities] = vulnerabilities
    scan_info[:vulnerability_count] = vulnerabilities.length
    puts "Found #{vulnerabilities.length} potential vulnerabilities"
    
    scan_info
  end

  def scan_dependencies(directory, scan_info = {})
    puts "Scanning dependencies for known vulnerabilities..."
    
    dependency_files = find_dependency_files(directory)
    vulnerable_dependencies = []
    
    dependency_files.each do |dep_file|
      dependencies = parse_dependencies(dep_file)
      dependencies.each do |dep|
        if check_known_vulnerabilities(dep)
          vulnerable_dependencies << {
            file: dep_file,
            dependency: dep,
            vulnerabilities: get_known_vulnerabilities(dep)
          }
        end
      end
    end
    
    scan_info[:vulnerable_dependencies] = vulnerable_dependencies
    scan_info[:vulnerable_dependency_count] = vulnerable_dependencies.length
    puts "Found #{vulnerable_dependencies.length} vulnerable dependencies"
    
    scan_info
  end

  def scan_secrets(directory, scan_info = {})
    puts "Scanning for exposed secrets..."
    
    secret_patterns = {
      'AWS Access Key' => /AKIA[0-9A-Z]{16}/,
      'AWS Secret Key' => /[0-9a-zA-Z\/+=]{40}/,
      'Google API Key' => /AIza[0-9A-Za-z\-_]{35}/,
      'GitHub Token' => /ghp_[0-9a-zA-Z]{36}/,
      'Private Key' => /-----BEGIN.*PRIVATE KEY-----/,
      'Database URL' => /(postgresql|mysql):\/\/[^\s]+/i,
      'JWT Token' => /eyJ[A-Za-z0-9\-_]+\.[A-Za-z0-9\-_]+\.[A-Za-z0-9\-_]+/,
      'Credit Card' => /\b\d{4}[\s-]?\d{4}[\s-]?\d{4}[\s-]?\d{4}\b/
    }
    
    found_secrets = []
    
    Find.find(directory) do |path|
      next unless File.file?(path)
      next if path.include?('.git')
      
      content = File.read(path)
      secret_patterns.each do |secret_type, pattern|
        content.scan(pattern) do |match|
          found_secrets << {
            file: path,
            secret_type: secret_type,
            match: match[0..10] + '...' if match.is_a?(String),
            recommendation: get_secret_recommendation(secret_type)
          }
        end
      end
    end
    
    scan_info[:found_secrets] = found_secrets
    scan_info[:secret_count] = found_secrets.length
    puts "Found #{found_secrets.length} potential secrets"
    
    scan_info
  end

  def generate_security_report(output_file = 'security_report.json')
    report = {
      generated_at: Time.now.to_s,
      total_scans: @scan_results.length,
      total_vulnerabilities: @scan_results.sum { |r| r[:vulnerability_count] || 0 },
      total_secrets: @scan_results.sum { |r| r[:secret_count] || 0 },
      security_score: calculate_security_score,
      recent_scans: @scan_results.last(5),
      recommendations: generate_security_recommendations
    }
    
    File.write(output_file, JSON.pretty_generate(report))
    puts "Security report generated: #{output_file}"
    report
  end

  def check_ssl_certificate(domain)
    puts "Checking SSL certificate for #{domain}"
    
    # Simulate SSL certificate check
    cert_info = {
      domain: domain,
      valid: rand > 0.1,
      expiry_date: Date.today + rand(30..365),
      issuer: ['Let\'s Encrypt', 'DigiCert', 'Comodo', 'GlobalSign'].sample,
      algorithm: ['RSA 2048', 'ECDSA'].sample
    }
    
    puts cert_info[:valid] ? "Certificate is valid" : "Certificate has issues"
    cert_info
  end

  def analyze_permissions(directory = '.')
    puts "Analyzing file permissions..."
    
    permission_issues = []
    
    Find.find(directory) do |path|
      next unless File.file?(path)
      next if path.include?('.git')
      
      stat = File.stat(path)
      permissions = stat.mode.to_s(8)[-3..-1]
      
      # Check for insecure permissions
      if permissions.include?('7') # World readable/writable
        permission_issues << {
          file: path,
          permissions: permissions,
          issue: 'World accessible permissions',
          risk: 'HIGH'
        }
      elsif permissions.include?('6')
        permission_issues << {
          file: path,
          permissions: permissions,
          issue: 'Group accessible permissions',
          risk: 'MEDIUM'
        }
      end
    end
    
    puts "Found #{permission_issues.length} permission issues"
    permission_issues
  end

  def run_security_audit(scan_results)
    puts "Running security audit..."
    
    audit_result = {
      overall_score: calculate_security_score(scan_results),
      critical_issues: scan_results.count { |r| r[:severity] == 'CRITICAL' },
      high_issues: scan_results.count { |r| r[:severity] == 'HIGH' },
      medium_issues: scan_results.count { |r| r[:severity] == 'MEDIUM' },
      low_issues: scan_results.count { |r| r[:severity] == 'LOW' },
      compliance_status: check_compliance_status(scan_results)
    }
    
    audit_result
  end

  private

  def generate_scan_id
    "scan_#{Time.now.strftime('%Y%m%d_%H%M%S')}_#{rand(1000..9999)}"
  end

  def code_file?(file_path)
    extensions = ['.js', '.rb', '.py', '.php', '.java', '.cpp', '.c', '.cs', '.go', '.rs']
    extensions.include?(File.extname(file_path))
  end

  def get_vulnerability_severity(vuln_type)
    case vuln_type
    when 'SQL Injection', 'Command Injection', 'Hardcoded Password'
      'CRITICAL'
    when 'Path Traversal', 'Weak Cryptography'
      'HIGH'
    when 'Insecure Random', 'Debug Code'
      'MEDIUM'
    else
      'LOW'
    end
  end

  def get_vulnerability_recommendation(vuln_type)
    recommendations = {
      'SQL Injection' => 'Use parameterized queries or prepared statements',
      'Command Injection' => 'Validate and sanitize all user inputs',
      'Path Traversal' => 'Use path traversal protection and validate paths',
      'Weak Cryptography' => 'Use strong hashing algorithms (bcrypt, Argon2)',
      'Insecure Random' => 'Use secure random number generators',
      'Hardcoded Password' => 'Use environment variables or secure vaults',
      'Debug Code' => 'Remove debug code from production'
    }
    recommendations[vuln_type] || 'Review and fix the identified issue'
  end

  def find_dependency_files(directory)
    dep_files = []
    patterns = ['package.json', 'Gemfile', 'requirements.txt', 'composer.json', 'pom.xml']
    
    patterns.each do |pattern|
      dep_files.concat(Dir.glob(File.join(directory, '**', pattern)))
    end
    
    dep_files
  end

  def parse_dependencies(dep_file)
    # This would parse actual dependency files
    # For demo purposes, return sample data
    case File.basename(dep_file)
    when 'package.json'
      ['lodash', 'express', 'moment']
    when 'Gemfile'
      ['rails', 'json', 'nokogiri']
    when 'requirements.txt'
      ['django', 'requests', 'pillow']
    else
      []
    end
  end

  def check_known_vulnerabilities(dependency)
    # Simulate vulnerability check
    rand > 0.8 # 20% chance of vulnerability
  end

  def get_known_vulnerabilities(dependency)
    # Return sample vulnerabilities
    [
      {
        cve: 'CVE-2023-1234',
        severity: 'HIGH',
        description: 'Vulnerability in dependency parsing'
      }
    ]
  end

  def get_secret_recommendation(secret_type)
    recommendations = {
      'AWS Access Key' => 'Rotate the key immediately and use IAM roles',
      'AWS Secret Key' => 'Rotate the key and store in secure vault',
      'Google API Key' => 'Regenerate key and restrict usage',
      'GitHub Token' => 'Revoke token and use environment variables',
      'Private Key' => 'Store in secure key management system',
      'Database URL' => 'Use environment variables',
      'JWT Token' => 'Use secure token generation and storage',
      'Credit Card' => 'Never store credit card data in plain text'
    }
    recommendations[secret_type] || 'Remove from code and use secure storage'
  end

  def calculate_security_score
    total_issues = @scan_results.sum { |r| r[:vulnerability_count] || 0 }
    base_score = 100
    
    # Deduct points based on issue severity
    critical_deduction = @scan_results.sum { |r| r[:vulnerability_count] || 0 } * 10
    high_deduction = @scan_results.sum { |r| r[:vulnerability_count] || 0 } * 5
    
    [base_score - critical_deduction - high_deduction, 0].max
  end

  def generate_security_recommendations
    [
      'Implement automated security scanning in CI/CD',
      'Use dependency scanning tools',
      'Regular security audits and penetration testing',
      'Keep dependencies updated',
      'Implement secure coding practices training',
      'Use security headers and HTTPS everywhere',
      'Regular backup and disaster recovery testing'
    ]
  end

  def check_compliance_status(scan_results)
    # Simulate compliance check
    {
      owasp_top10: scan_results.any? ? 'PARTIAL' : 'COMPLIANT',
      pci_dss: 'COMPLIANT',
      gdpr: 'COMPLIANT',
      hipaa: 'NOT_APPLICABLE'
    }
  end
end

# CLI Usage
if __FILE__ == $PROGRAM_NAME
  security_scanner = SecurityScanner.new
  
  command = ARGV[0]
  case command
  when 'scan'
    security_scanner.run_security_scan(ARGV[1] || '.', ARGV[2] || 'full')
  when 'vulnerability'
    security_scanner.scan_vulnerabilities(ARGV[1] || '.')
  when 'dependencies'
    security_scanner.scan_dependencies(ARGV[1] || '.')
  when 'secrets'
    security_scanner.scan_secrets(ARGV[1] || '.')
  when 'permissions'
    puts security_scanner.analyze_permissions(ARGV[1] || '.').to_json
  when 'ssl'
    puts security_scanner.check_ssl_certificate(ARGV[1]).to_json
  when 'report'
    security_scanner.generate_security_report(ARGV[1])
  when 'audit'
    puts security_scanner.run_security_audit([]).to_json
  else
    puts "Usage: ruby security_scanner.rb <command> [args]"
    puts "Commands: scan, vulnerability, dependencies, secrets, permissions, ssl, report, audit"
  end
end
