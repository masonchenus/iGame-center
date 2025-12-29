#!/usr/bin/env ruby
# frozen_string_literal: true

require 'fileutils'
require 'pathname'

# File System Utility Functions
# Provides file system operations and management tools
class FileSystemUtility
  def initialize(base_path = '.')
    @base_path = Pathname.new(base_path)
  end

  def create_directory_structure(structure_hash, base_path = nil)
    target_path = Pathname.new(base_path || @base_path)
    
    structure_hash.each do |name, contents|
      dir_path = target_path + name
      
      if contents.is_a?(Hash)
        FileUtils.mkdir_p(dir_path)
        create_directory_structure(contents, dir_path)
      else
        FileUtils.mkdir_p(dir_path.dirname)
        create_file(dir_path, contents)
      end
    end
  end

  def create_file(file_path, content = '')
    full_path = Pathname.new(file_path)
    full_path.dirname.mkpath
    
    File.write(full_path, content)
    puts "Created file: #{full_path}"
    full_path
  end

  def backup_files(patterns, backup_dir = 'backups')
    FileUtils.mkdir_p(backup_dir)
    timestamp = Time.now.strftime('%Y%m%d_%H%M%S')
    
    patterns.each do |pattern|
      Dir.glob(pattern).each do |file|
        next if File.directory?(file)
        
        backup_name = "#{backup_dir}/#{File.basename(file)}.#{timestamp}.backup"
        FileUtils.cp(file, backup_name)
        puts "Backed up: #{file} -> #{backup_name}"
      end
    end
  end

  def find_large_files(directory = '.', size_limit_mb = 100)
    limit_bytes = size_limit_mb * 1024 * 1024
    large_files = []
    
    Find.find(directory) do |path|
      next unless File.file?(path)
      
      size = File.size(path)
      if size > limit_bytes
        large_files << {
          path: path,
          size: size,
          size_mb: (size.to_f / 1024 / 1024).round(2)
        }
      end
    end
    
    large_files.sort_by { |f| -f[:size] }
  end

  def find_duplicates(directory = '.')
    file_hashes = {}
    duplicates = {}
    
    Find.find(directory) do |path|
      next unless File.file?(path)
      
      hash = Digest::SHA256.file(path).hexdigest
      
      if file_hashes[hash]
        duplicates[path] = file_hashes[hash]
      else
        file_hashes[hash] = path
      end
    end
    
    duplicates
  end

  def clean_temp_files(directory = '.', patterns = ['*.tmp', '*.log', '*~'])
    cleaned_count = 0
    
    patterns.each do |pattern|
      Dir.glob(File.join(directory, '**', pattern)).each do |file|
        File.delete(file)
        cleaned_count += 1
        puts "Deleted: #{file}"
      end
    end
    
    puts "Cleaned #{cleaned_count} temporary files"
    cleaned_count
  end

  def organize_files_by_extension(directory = '.')
    Find.find(directory) do |path|
      next unless File.file?(path)
      
      extension = File.extname(path)[1..-1] || 'no_extension'
      target_dir = File.join(directory, 'organized', extension)
      
      next if path.include?('/organized/')
      
      FileUtils.mkdir_p(target_dir)
      new_path = File.join(target_dir, File.basename(path))
      
      FileUtils.mv(path, new_path) unless File.exist?(new_path)
      puts "Moved: #{path} -> #{new_path}"
    end
  end

  def calculate_directory_sizes(directory = '.')
    sizes = {}
    
    Find.find(directory) do |path|
      next if File.symlink?(path)
      
      if File.directory?(path)
        size = calculate_dir_size(path)
        sizes[path] = size
      end
    end
    
    sizes.sort_by { |_k, v| -v }
  end

  def generate_file_manifest(directory = '.', output_file = 'file_manifest.json')
    manifest = {
      generated_at: Time.now.to_s,
      directory: directory,
      total_files: 0,
      total_size: 0,
      files: []
    }
    
    Find.find(directory) do |path|
      next unless File.file?(path)
      
      file_info = {
        path: path,
        size: File.size(path),
        extension: File.extname(path)[1..-1] || 'none',
        modified_at: File.mtime(path).to_s,
        permissions: File.stat(path).mode.to_s(8)
      }
      
      manifest[:files] << file_info
      manifest[:total_files] += 1
      manifest[:total_size] += file_info[:size]
    end
    
    File.write(output_file, JSON.pretty_generate(manifest))
    puts "Manifest generated: #{output_file}"
    puts "Total files: #{manifest[:total_files]}"
    puts "Total size: #{(manifest[:total_size].to_f / 1024 / 1024).round(2)} MB"
    
    manifest
  end

  def sync_directories(source, destination, options = {})
    options = {
      delete: false,
      verbose: true,
      preserve_permissions: true
    }.merge(options)
    
    if options[:preserve_permissions]
      FileUtils.cp_r(source, destination, preserve: true)
    else
      FileUtils.cp_r(source, destination)
    end
    
    if options[:delete]
      # Remove files from destination that don't exist in source
      # Implementation would go here
    end
    
    puts "Synced #{source} -> #{destination}"
  end

  def find_files_by_content(directory, search_pattern, content_pattern)
    matches = []
    
    Find.find(directory) do |path|
      next unless File.file?(path)
      next unless File.fnmatch(search_pattern, File.basename(path))
      
      begin
        content = File.read(path)
        if content.include?(content_pattern)
          matches << {
            file: path,
            line_number: content.lines.find_index { |line| line.include?(content_pattern) } + 1
          }
        end
      rescue => e
        # Skip files that can't be read
      end
    end
    
    matches
  end

  private

  def calculate_dir_size(dir)
    size = 0
    Find.find(dir) do |path|
      size += File.size(path) if File.file?(path)
    end
    size
  end
end

# CLI Usage
if __FILE__ == $PROGRAM_NAME
  fs_util = FileSystemUtility.new
  
  command = ARGV[0]
  case command
  when 'backup'
    fs_util.backup_files(ARGV[1..-1] || ['*.js', '*.json'])
  when 'large-files'
    files = fs_util.find_large_files(ARGV[1] || '.', (ARGV[2] || 100).to_i)
    files.each { |f| puts "#{f[:path]} - #{f[:size_mb]} MB" }
  when 'duplicates'
    dups = fs_util.find_duplicates(ARGV[1] || '.')
    dups.each { |file, original| puts "#{file} duplicates #{original}" }
  when 'clean'
    fs_util.clean_temp_files(ARGV[1] || '.')
  when 'organize'
    fs_util.organize_files_by_extension(ARGV[1] || '.')
  when 'sizes'
    sizes = fs_util.calculate_directory_sizes(ARGV[1] || '.')
    sizes.each { |dir, size| puts "#{dir}: #{(size.to_f / 1024 / 1024).round(2)} MB" }
  when 'manifest'
    fs_util.generate_file_manifest(ARGV[1] || '.', ARGV[2] || 'file_manifest.json')
  when 'sync'
    fs_util.sync_directories(ARGV[1], ARGV[2])
  when 'search-content'
    matches = fs_util.find_files_by_content(ARGV[1] || '.', ARGV[2] || '*', ARGV[3] || '')
    matches.each { |m| puts "#{m[:file]}:#{m[:line_number]}" }
  else
    puts "Usage: ruby file_system_utility.rb <command> [args]"
    puts "Commands: backup, large-files, duplicates, clean, organize, sizes, manifest, sync, search-content"
  end
end
