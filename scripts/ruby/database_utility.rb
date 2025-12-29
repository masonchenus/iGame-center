#!/usr/bin/env ruby
# frozen_string_literal: true

require 'json'
require 'fileutils'

# Database Utility Functions
# Provides common database operations for the game center
class DatabaseUtility
  def initialize(config_path = 'config/database.json')
    @config = load_config(config_path)
  end

  def load_config(config_path)
    return {} unless File.exist?(config_path)
    JSON.parse(File.read(config_path))
  rescue JSON::ParserError
    {}
  end

  def create_backup(db_name, backup_dir = 'backups')
    timestamp = Time.now.strftime('%Y%m%d_%H%M%S')
    backup_file = "#{backup_dir}/#{db_name}_#{timestamp}.backup"
    
    FileUtils.mkdir_p(backup_dir)
    
    # Simulate database backup (replace with actual database commands)
    backup_data = {
      database: db_name,
      timestamp: timestamp,
      size_mb: calculate_db_size(db_name),
      tables: list_tables(db_name)
    }
    
    File.write(backup_file, JSON.pretty_generate(backup_data))
    puts "Backup created: #{backup_file}"
    backup_file
  end

  def restore_backup(backup_file, db_name)
    unless File.exist?(backup_file)
      puts "Backup file not found: #{backup_file}"
      return false
    end

    backup_data = JSON.parse(File.read(backup_file))
    puts "Restoring backup from #{backup_file}"
    puts "Database: #{backup_data['database']}"
    puts "Created: #{backup_data['timestamp']}"
    
    # Simulate restore process
    true
  end

  def optimize_database(db_name)
    puts "Optimizing database: #{db_name}"
    
    operations = [
      'VACUUM',
      'ANALYZE',
      'REINDEX'
    ]
    
    operations.each do |op|
      puts "  Executing: #{op}"
      # Simulate operation
      sleep 0.1
    end
    
    puts "Database optimization completed"
  end

  def check_database_health(db_name)
    health_status = {
      database: db_name,
      status: 'healthy',
      checks: {
        connectivity: test_connection(db_name),
        disk_space: check_disk_space,
        table_integrity: check_table_integrity(db_name),
        index_usage: check_index_usage(db_name)
      },
      timestamp: Time.now.to_s
    }
    
    puts "Database Health Check for #{db_name}:"
    health_status[:checks].each do |check, status|
      status_icon = status ? '✓' : '✗'
      puts "  #{status_icon} #{check.to_s.capitalize.gsub('_', ' ')}"
    end
    
    health_status
  end

  def migrate_database(db_name, migration_file)
    unless File.exist?(migration_file)
      puts "Migration file not found: #{migration_file}"
      return false
    end

    puts "Running migration: #{migration_file}"
    
    migration_data = JSON.parse(File.read(migration_file))
    migration_data['migrations'].each do |migration|
      puts "  Applying: #{migration['name']}"
      # Simulate migration execution
      sleep 0.05
    end
    
    puts "Migration completed successfully"
    true
  end

  def export_table_data(db_name, table_name, output_file, format = 'json')
    puts "Exporting table #{table_name} from #{db_name}"
    
    # Simulate data export
    sample_data = {
      table: table_name,
      exported_at: Time.now.to_s,
      record_count: rand(1000..10000),
      data: generate_sample_data(table_name)
    }
    
    case format
    when 'json'
      File.write(output_file, JSON.pretty_generate(sample_data))
    when 'csv'
      write_csv(sample_data[:data], output_file)
    end
    
    puts "Data exported to #{output_file}"
  end

  def import_table_data(db_name, table_name, input_file)
    unless File.exist?(input_file)
      puts "Input file not found: #{input_file}"
      return false
    end

    data = JSON.parse(File.read(input_file))
    puts "Importing #{data['data'].length} records into #{db_name}.#{table_name}"
    
    # Simulate import
    data['data'].each_slice(100) do |batch|
      puts "  Processing batch of #{batch.length} records..."
      sleep 0.01
    end
    
    puts "Import completed successfully"
    true
  end

  private

  def calculate_db_size(db_name)
    # Simulate database size calculation
    rand(50..5000)
  end

  def list_tables(db_name)
    # Simulate table listing
    ['users', 'games', 'scores', 'sessions', 'achievements'].sample(rand(2..5))
  end

  def test_connection(db_name)
    # Simulate connection test
    rand > 0.1
  end

  def check_disk_space
    # Simulate disk space check
    rand > 0.2
  end

  def check_table_integrity(db_name)
    # Simulate integrity check
    rand > 0.05
  end

  def check_index_usage(db_name)
    # Simulate index usage check
    rand(70..99)
  end

  def generate_sample_data(table_name)
    (1..10).map do |i|
      {
        id: i,
        name: "#{table_name}_#{i}",
        created_at: Time.now.iso8601,
        updated_at: Time.now.iso8601
      }
    end
  end

  def write_csv(data, filename)
    CSV.open(filename, 'w') do |csv|
      csv << data.first.keys if data.any?
      data.each { |row| csv << row.values }
    end
  end
end

# CLI Usage
if __FILE__ == $PROGRAM_NAME
  db_util = DatabaseUtility.new
  
  command = ARGV[0]
  case command
  when 'backup'
    db_util.create_backup(ARGV[1] || 'gamecenter')
  when 'restore'
    db_util.restore_backup(ARGV[1], ARGV[2] || 'gamecenter')
  when 'optimize'
    db_util.optimize_database(ARGV[1] || 'gamecenter')
  when 'health'
    db_util.check_database_health(ARGV[1] || 'gamecenter')
  when 'export'
    db_util.export_table_data(ARGV[1], ARGV[2], ARGV[3])
  when 'import'
    db_util.import_table_data(ARGV[1], ARGV[2], ARGV[3])
  when 'migrate'
    db_util.migrate_database(ARGV[1], ARGV[2])
  else
    puts "Usage: ruby database_utility.rb <command> [args]"
    puts "Commands: backup, restore, optimize, health, export, import, migrate"
  end
end
