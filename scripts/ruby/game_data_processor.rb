#!/usr/bin/env ruby
# frozen_string_literal: true

require 'json'
require 'csv'

# Game Data Processor Utility
# Processes and manipulates game data files
class GameDataProcessor
  def initialize
    @processed_data = []
  end

  def load_json_data(file_path)
    return unless File.exist?(file_path)
    
    JSON.parse(File.read(file_path))
  rescue JSON::ParserError => e
    puts "Error parsing JSON: #{e.message}"
    nil
  end

  def load_csv_data(file_path)
    return unless File.exist?(file_path)
    
    CSV.read(file_path, headers: true, header_converters: :symbol)
  rescue CSV::MalformedCSVError => e
    puts "Error parsing CSV: #{e.message}"
    nil
  end

  def process_game_scores(scores_data)
    scores_data.map do |score|
      {
        player_id: score[:player_id],
        score: score[:score].to_i,
        level: score[:level].to_i,
        timestamp: score[:timestamp],
        processed_at: Time.now.to_s
      }
    end
  end

  def calculate_statistics(data, field)
    values = data.map { |row| row[field].to_f }.compact
    return {} if values.empty?

    {
      count: values.count,
      sum: values.sum,
      average: values.sum / values.count,
      min: values.min,
      max: values.max,
      median: calculate_median(values.sort)
    }
  end

  def export_processed_data(data, output_path, format: 'json')
    case format
    when 'json'
      File.write(output_path, JSON.pretty_generate(data))
    when 'csv'
      CSV.open(output_path, 'w') do |csv|
        csv << data.first.keys
        data.each { |row| csv << row.values }
      end
    end
    puts "Data exported to #{output_path}"
  end

  private

  def calculate_median(sorted_values)
    len = sorted_values.length
    if len.even?
      (sorted_values[len / 2 - 1] + sorted_values[len / 2]) / 2.0
    else
      sorted_values[len / 2]
    end
  end
end

# CLI Usage
if __FILE__ == $PROGRAM_NAME
  processor = GameDataProcessor.new
  
  if ARGV[0] && ARGV[1]
    data = processor.load_json_data(ARGV[0])
    if data
      processed = processor.process_game_scores(data)
      processor.export_processed_data(processed, ARGV[1])
    end
  else
    puts "Usage: ruby game_data_processor.rb input.json output.json"
  end
end
