#!/usr/bin/env ruby
# frozen_string_literal: true

require 'open3'
require 'json'

# Git Automation Utility
# Provides automated git operations for the game center project
class GitAutomation
  def initialize(repo_path = '.')
    @repo_path = repo_path
    @commands = []
  end

  def run_git_command(command, capture_output = true)
    full_command = "cd #{@repo_path} && git #{command}"
    
    if capture_output
      stdout, stderr, status = Open3.capture3(full_command)
      {
        success: status.success?,
        output: stdout.strip,
        error: stderr.strip,
        exit_code: status.exitstatus
      }
    else
      system(full_command)
    end
  end

  def get_current_branch
    result = run_git_command('rev-parse --abbrev-ref HEAD')
    result[:success] ? result[:output] : 'unknown'
  end

  def get_commit_hash(commit = 'HEAD')
    result = run_git_command("rev-parse #{commit}")
    result[:success] ? result[:output] : nil
  end

  def get_latest_tag
    result = run_git_command('describe --tags --abbrev=0')
    result[:success] ? result[:output] : nil
  end

  def create_tag(tag_name, message = '')
    result = run_git_command("tag -a #{tag_name} -m '#{message}'")
    result[:success] ? puts("Tag #{tag_name} created successfully") : puts(result[:error])
    result[:success]
  end

  def create_branch(branch_name, from_branch = nil)
    base_command = from_branch ? "#{from_branch}" : "HEAD"
    result = run_git_command("checkout -b #{branch_name} #{base_command}")
    result[:success] ? puts("Branch #{branch_name} created") : puts(result[:error])
    result[:success]
  end

  def merge_branch(source_branch, target_branch = nil)
    target = target_branch || get_current_branch
    result = run_git_command("checkout #{target}")
    return puts("Failed to checkout #{target}") unless result[:success]
    
    result = run_git_command("merge #{source_branch}")
    if result[:success]
      puts("Successfully merged #{source_branch} into #{target}")
    else
      puts("Merge failed: #{result[:error]}")
    end
    result[:success]
  end

  def get_branch_status(branch_name = nil)
    branch = branch_name || get_current_branch
    result = run_git_command("status --porcelain")
    
    if result[:success]
      changes = result[:output].lines.map(&:strip).reject(&:empty?)
      {
        branch: branch,
        clean: changes.empty?,
        changes: changes
      }
    else
      { branch: branch, clean: false, error: result[:error] }
    end
  end

  def commit_changes(message, files = nil)
    if files
      result = run_git_command("add #{files.join(' ')}")
      return false unless result[:success]
    else
      result = run_git_command('add -A')
      return false unless result[:success]
    end
    
    result = run_git_command("commit -m '#{message}'")
    result[:success] ? puts("Committed: #{message}") : puts(result[:error])
    result[:success]
  end

  def push_changes(remote = 'origin', branch = nil)
    branch ||= get_current_branch
    result = run_git_command("push #{remote} #{branch}")
    result[:success] ? puts("Pushed #{branch} to #{remote}") : puts(result[:error])
    result[:success]
  end

  def pull_changes(remote = 'origin', branch = nil)
    branch ||= get_current_branch
    result = run_git_command("pull #{remote} #{branch}")
    result[:success] ? puts("Pulled latest changes") : puts(result[:error])
    result[:success]
  end

  def create_release(version, changelog = '')
    tag_name = "v#{version}"
    
    # Create and push tag
    if create_tag(tag_name, changelog)
      push_changes('origin', tag_name)
      puts("Release #{tag_name} created and pushed")
      true
    else
      puts("Failed to create release #{tag_name}")
      false
    end
  end

  def get_commits_since(tag_or_branch)
    result = run_git_command("log --pretty=format:'%h %s' #{tag_or_branch}..HEAD")
    result[:success] ? result[:output].lines : []
  end

  def get_file_history(file_path, limit = 10)
    result = run_git_command("log --oneline -n #{limit} -- #{file_path}")
    result[:success] ? result[:output].lines : []
  end

  def find_conflicts
    result = run_git_command('diff --name-only --diff-filter=U')
    result[:success] ? result[:output].lines.map(&:strip) : []
  end

  def stash_changes(message = '')
    cmd = message.empty? ? 'stash push' : "stash push -m '#{message}'"
    result = run_git_command(cmd)
    result[:success] ? puts("Stashed changes") : puts(result[:error])
    result[:success]
  end

  def pop_stash
    result = run_git_command('stash pop')
    result[:success] ? puts("Popped stash") : puts(result[:error])
    result[:success]
  end

  def get_repository_info
    {
      current_branch: get_current_branch,
      latest_tag: get_latest_tag,
      status: get_branch_status,
      remote_url: get_remote_url
    }
  end

  def get_remote_url(remote = 'origin')
    result = run_git_command("config --get remote.#{remote}.url")
    result[:success] ? result[:output] : nil
  end

  def setup_automation_workflow(branch_name = 'main')
    workflow = {
      steps: [
        'Checkout main branch',
        'Pull latest changes',
        'Create feature branch',
        'Development work',
        'Merge back to main',
        'Deploy'
      ],
      commands: []
    }
    
    # Generate workflow commands
    workflow[:commands] = [
      "git checkout #{branch_name}",
      "git pull origin #{branch_name}",
      "git checkout -b feature/automation",
      "# Your development work here",
      "git add .",
      "git commit -m 'Feature: automation improvements'",
      "git checkout #{branch_name}",
      "git merge feature/automation",
      "git push origin #{branch_name}"
    ]
    
    workflow
  end

  def export_repository_stats(output_file = 'repo_stats.json')
    stats = {
      repository_info: get_repository_info,
      commit_stats: get_commit_statistics,
      branches: list_branches,
      tags: list_tags,
      generated_at: Time.now.to_s
    }
    
    File.write(output_file, JSON.pretty_generate(stats))
    puts("Repository stats exported to #{output_file}")
    stats
  end

  def list_branches
    result = run_git_command('branch -a')
    result[:success] ? result[:output].lines.map(&:strip) : []
  end

  def list_tags
    result = run_git_command('tag')
    result[:success] ? result[:output].lines.map(&:strip) : []
  end

  private

  def get_commit_statistics
    result = run_git_command('log --oneline --since="1 month ago"')
    commits = result[:success] ? result[:output].lines : []
    {
      total_commits: commits.length,
      recent_commits: commits.first(10)
    }
  end
end

# CLI Usage
if __FILE__ == $PROGRAM_NAME
  git_auto = GitAutomation.new
  
  command = ARGV[0]
  case command
  when 'status'
    puts git_auto.get_branch_status.to_json
  when 'branch'
    puts "Current branch: #{git_auto.get_current_branch}"
  when 'create-tag'
    git_auto.create_tag(ARGV[1], ARGV[2] || '')
  when 'create-branch'
    git_auto.create_branch(ARGV[1], ARGV[2])
  when 'merge'
    git_auto.merge_branch(ARGV[1], ARGV[2])
  when 'commit'
    git_auto.commit_changes(ARGV[1], ARGV[2..-1])
  when 'push'
    git_auto.push_changes(ARGV[1], ARGV[2])
  when 'pull'
    git_auto.pull_changes(ARGV[1], ARGV[2])
  when 'release'
    git_auto.create_release(ARGV[1], ARGV[2] || '')
  when 'conflicts'
    puts git_auto.find_conflicts.join("\n")
  when 'stats'
    git_auto.export_repository_stats(ARGV[1])
  when 'workflow'
    workflow = git_auto.setup_automation_workflow(ARGV[1])
    puts JSON.pretty_generate(workflow)
  else
    puts "Usage: ruby git_automation.rb <command> [args]"
    puts "Commands: status, branch, create-tag, create-branch, merge, commit, push, pull, release, conflicts, stats, workflow"
  end
end
