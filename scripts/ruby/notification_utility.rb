#!/usr/bin/env ruby
# frozen_string_literal: true

require 'json'
require 'net/http'
require 'uri'
require 'openssl'

# Notification Utility
# Provides notification management and delivery functions
class NotificationUtility
  def initialize(config_file = 'config/notifications.json')
    @config = load_config(config_file)
    @notification_history = []
  end

  def load_config(config_file)
    return {} unless File.exist?(config_file)
    JSON.parse(File.read(config_file))
  rescue JSON::ParserError
    {}
  end

  def send_notification(type, recipient, message, options = {})
    puts "Sending #{type} notification to #{recipient}"
    
    notification = {
      id: generate_notification_id,
      type: type,
      recipient: recipient,
      message: message,
      timestamp: Time.now.to_s,
      status: 'pending',
      options: options
    }
    
    begin
      result = deliver_notification(notification)
      
      notification[:status] = result[:success] ? 'sent' : 'failed'
      notification[:delivery_result] = result
      notification[:sent_at] = Time.now.to_s if result[:success]
      
      puts "Notification #{notification[:status]}: #{type} to #{recipient}"
      
    rescue => e
      notification[:status] = 'failed'
      notification[:error] = e.message
      puts "Notification failed: #{e.message}"
    end
    
    @notification_history << notification
    save_notification_history
    notification
  end

  def send_email(to, subject, body, options = {})
    email_config = @config['email'] || default_email_config
    
    email_data = {
      to: to,
      subject: subject,
      body: body,
      options: options
    }
    
    send_notification('email', to, email_data, options)
  end

  def send_slack_message(channel, message, username = nil)
    slack_config = @config['slack'] || default_slack_config
    
    slack_payload = {
      channel: channel,
      text: message,
      username: username || slack_config['default_username'] || 'GameCenter Bot',
      icon_emoji: slack_config['icon_emoji'] || ':robot_face:'
    }
    
    # Add attachments if provided
    if @config['slack']['attachments']
      slack_payload[:attachments] = @config['slack']['attachments']
    end
    
    send_notification('slack', channel, slack_payload)
  end

  def send_webhook_notification(url, payload, headers = {})
    webhook_data = {
      url: url,
      payload: payload,
      headers: headers
    }
    
    send_notification('webhook', url, webhook_data)
  end

  def send_sms(phone_number, message)
    sms_config = @config['sms'] || default_sms_config
    
    sms_data = {
      to: phone_number,
      message: message,
      provider: sms_config['provider'] || 'twilio'
    }
    
    send_notification('sms', phone_number, sms_data)
  end

  def send_desktop_notification(title, message, options = {})
    notification_options = {
      title: title,
      message: message,
      icon: options[:icon] || 'info',
      sound: options[:sound] || true,
      timeout: options[:timeout] || 5000
    }
    
    # Try to send native desktop notification
    begin
      if RUBY_PLATFORM =~ /darwin/ # macOS
        send_macos_notification(notification_options)
      elsif RUBY_PLATFORM =~ /linux/ # Linux
        send_linux_notification(notification_options)
      elsif RUBY_PLATFORM =~ /mingw|mswin|cygwin/ # Windows
        send_windows_notification(notification_options)
      else
        puts "Desktop notifications not supported on this platform"
      end
    rescue => e
      puts "Failed to send desktop notification: #{e.message}"
    end
  end

  def create_notification_template(name, template_data)
    puts "Creating notification template: #{name}"
    
    template = {
      name: name,
      type: template_data['type'],
      subject_template: template_data['subject_template'],
      body_template: template_data['body_template'],
      variables: template_data['variables'] || [],
      created_at: Time.now.to_s
    }
    
    template_file = "config/templates/#{name}.json"
    FileUtils.mkdir_p(File.dirname(template_file))
    File.write(template_file, JSON.pretty_generate(template))
    
    puts "Template created: #{template_file}"
    template
  end

  def send_template_notification(template_name, recipient, variables = {})
    template_file = "config/templates/#{template_name}.json"
    
    unless File.exist?(template_file)
      puts "Template not found: #{template_name}"
      return nil
    end
    
    template = JSON.parse(File.read(template_file))
    
    # Process template variables
    subject = template['subject_template']
    body = template['body_template']
    
    variables.each do |key, value|
      subject = subject.gsub("{{#{key}}}", value.to_s)
      body = body.gsub("{{#{key}}}", value.to_s)
    end
    
    case template['type']
    when 'email'
      send_email(recipient, subject, body)
    when 'slack'
      send_slack_message(recipient, body)
    when 'sms'
      send_sms(recipient, body)
    else
      puts "Unsupported template type: #{template['type']}"
    end
  end

  def setup_notification_rules
    puts "Setting up notification rules..."
    
    rules_config = @config['rules'] || default_notification_rules
    
    rules = []
    
    rules_config.each do |rule|
      notification_rule = {
        id: generate_rule_id,
        name: rule['name'],
        condition: rule['condition'],
        actions: rule['actions'],
        enabled: rule['enabled'] || true,
        created_at: Time.now.to_s
      }
      
      rules << notification_rule
      puts "Rule configured: #{rule['name']}"
    end
    
    rules
  end

  def evaluate_notification_rules(event_data)
    puts "Evaluating notification rules for event..."
    
    triggered_rules = []
    
    @config['rules']&.each do |rule|
      if rule['enabled'] && evaluate_rule_condition(rule['condition'], event_data)
        triggered_rules << execute_rule_actions(rule['actions'], event_data)
      end
    end
    
    triggered_rules
  end

  def schedule_notification(notification_config, schedule_time)
    puts "Scheduling notification for #{schedule_time}"
    
    scheduled_notification = {
      id: generate_scheduled_id,
      config: notification_config,
      scheduled_time: schedule_time,
      status: 'scheduled',
      created_at: Time.now.to_s
    }
 a real implementation,    
    # In this would be added to a job scheduler
    puts "Notification scheduled: #{scheduled_notification[:id]}"
    scheduled_notification
  end

  def get_notification_analytics(days = 7)
    puts "Generating notification analytics for last #{days} days"
    
    cutoff_date = Date.today - days
    recent_notifications = @notification_history.select do |n|
      Date.parse(n[:timestamp]) >= cutoff_date
    end
    
    analytics = {
      period: "#{days} days",
      total_notifications: recent_notifications.length,
      successful_deliveries: recent_notifications.count { |n| n[:status] == 'sent' },
      failed_deliveries: recent_notifications.count { |n| n[:status] == 'failed' },
      delivery_rate: (recent_notifications.count { |n| n[:status] == 'sent' }.to_f / recent_notifications.length * 100).round(2),
      notifications_by_type: recent_notifications.group_by { |n| n[:type] }.transform_values(&:length),
      notifications_by_recipient: recent_notifications.group_by { |n| n[:recipient] }.transform_values(&:length)
    }
    
    analytics
  end

  private

  def generate_notification_id
    "notif_#{Time.now.strftime('%Y%m%d_%H%M%S')}_#{rand(1000..9999)}"
  end

  def generate_rule_id
    "rule_#{Time.now.strftime('%Y%m%d_%H%M%S')}_#{rand(1000..9999)}"
  end

  def generate_scheduled_id
    "scheduled_#{Time.now.strftime('%Y%m%d_%H%M%S')}_#{rand(1000..9999)}"
  end

  def deliver_notification(notification)
    case notification[:type]
    when 'email'
      deliver_email(notification)
    when 'slack'
      deliver_slack(notification)
    when 'webhook'
      deliver_webhook(notification)
    when 'sms'
      deliver_sms(notification)
    else
      { success: false, error: "Unsupported notification type: #{notification[:type]}" }
    end
  end

  def deliver_email(notification)
    # Simulate email delivery
    puts "  Sending email to #{notification[:recipient]}"
    puts "  Subject: #{notification[:message][:subject]}"
    
    { success: rand > 0.1, message_id: "msg_#{rand(100000..999999)}" }
  end

  def deliver_slack(notification)
    slack_config = @config['slack'] || default_slack_config
    
    # Simulate Slack API call
    puts "  Sending Slack message to ##{notification[:recipient]}"
    
    { success: rand > 0.05, timestamp: Time.now.to_i }
  end

  def deliver_webhook(notification)
    webhook_config = notification[:message]
    
    begin
      uri = URI.parse(webhook_config[:url])
      http = Net::HTTP.new(uri.host, uri.port)
      http.use_ssl = (uri.scheme == 'https')
      
      request = Net::HTTP::Post.new(uri.path)
      request['Content-Type'] = 'application/json'
      webhook_config[:headers]&.each { |k, v| request[k] = v }
      request.body = webhook_config[:payload].to_json
      
      response = http.request(request)
      
      { success: response.code.to_i < 400, status_code: response.code }
    rescue => e
      { success: false, error: e.message }
    end
  end

  def deliver_sms(notification)
    sms_config = @config['sms'] || default_sms_config
    
    # Simulate SMS delivery
    puts "  Sending SMS to #{notification[:recipient]}"
    
    { success: rand > 0.15, message_id: "sms_#{rand(100000..999999)}" }
  end

  def send_macos_notification(options)
    script = %(
      display notification "#{options[:message]}" with title "#{options[:title]}"
    )
    
    system("osascript -e '#{script}'")
  end

  def send_linux_notification(options)
    # Use notify-send command if available
    icon_option = options[:icon] ? "--icon=#{options[:icon]}" : ""
    timeout_option = "--expire-time=#{options[:timeout]}"
    
    system("notify-send #{icon_option} #{timeout_option} \"#{options[:title]}\" \"#{options[:message]}\"")
  end

  def send_windows_notification(options)
    # Windows notifications would require additional setup
    puts "Windows desktop notifications not implemented"
  end

  def evaluate_rule_condition(condition, event_data)
    # Simple condition evaluation
    case condition['type']
    when 'equals'
      event_data[condition['field']] == condition['value']
    when 'contains'
      event_data[condition['field']].to_s.include?(condition['value'])
    when 'greater_than'
      event_data[condition['field']] > condition['value']
    else
      false
    end
  end

  def execute_rule_actions(actions, event_data)
    executed_actions = []
    
    actions.each do |action|
      case action['type']
      when 'email'
        result = send_email(action['to'], action['subject'], action['body'])
        executed_actions << { type: 'email', result: result }
      when 'slack'
        result = send_slack_message(action['channel'], action['message'])
        executed_actions << { type: 'slack', result: result }
      end
    end
    
    executed_actions
  end

  def default_email_config
    {
      'provider' => 'smtp',
      'smtp_settings' => {
        'address' => 'smtp.gmail.com',
        'port' => 587,
        'authentication' => 'plain'
      }
    }
  end

  def default_slack_config
    {
      'webhook_url' => '',
      'default_channel' => '#general',
      'default_username' => 'GameCenter Bot'
    }
  end

  def default_sms_config
    {
      'provider' => 'twilio',
      'account_sid' => '',
      'auth_token' => ''
    }
  end

  def default_notification_rules
    [
      {
        'name' => 'High CPU Alert',
        'condition' => {
          'type' => 'greater_than',
          'field' => 'cpu_usage',
          'value' => 80
        },
        'actions' => [
          {
            'type' => 'slack',
            'channel' => '#alerts',
            'message' => 'High CPU usage detected: {{cpu_usage}}%'
          }
        ],
        'enabled' => true
      }
    ]
  end

  def save_notification_history
    history_file = 'logs/notifications.json'
    File.write(history_file, JSON.pretty_generate(@notification_history))
  end
end

# CLI Usage
if __FILE__ == $PROGRAM_NAME
  notifier = NotificationUtility.new
  
  command = ARGV[0]
  case command
  when 'send'
    notifier.send_notification(ARGV[1], ARGV[2], ARGV[3])
  when 'email'
    notifier.send_email(ARGV[1], ARGV[2], ARGV[3])
  when 'slack'
    notifier.send_slack_message(ARGV[1], ARGV[2])
  when 'sms'
    notifier.send_sms(ARGV[1], ARGV[2])
  when 'desktop'
    notifier.send_desktop_notification(ARGV[1], ARGV[2])
  when 'template'
    template_data = JSON.parse(File.read(ARGV[2])) if ARGV[2] && File.exist?(ARGV[2])
    notifier.create_notification_template(ARGV[1], template_data || {})
  when 'send-template'
    variables = JSON.parse(ARGV[3]) if ARGV[3] && File.exist?(ARGV[3])
    notifier.send_template_notification(ARGV[1], ARGV[2], variables || {})
  when 'rules'
    notifier.setup_notification_rules
  when 'analytics'
    puts notifier.get_notification_analytics((ARGV[1] || 7).to_i).to_json
  else
    puts "Usage: ruby notification_utility.rb <command> [args]"
    puts "Commands: send, email, slack, sms, desktop, template, send-template, rules, analytics"
  end
end

