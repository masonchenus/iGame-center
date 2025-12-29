# Crystal AI Algorithms - Machine Learning and AI Implementations
require "math"

class AIData
  property features : Array(Float64)
  property label : Float64?
  
  def initialize(@features : Array(Float64), @label : Float64? = nil)
  end
  
  def distance_to(other : AIData)
    raise "Feature dimension mismatch" unless @features.size == other.features.size
    Math.sqrt(@features.zip(other.features).map { |a, b| (a - b) ** 2 }.sum)
  end
end

class KNNClassifier
  def initialize(@k : Int32 = 3)
    @training_data = [] of AIData
  end
  
  def train(data : Array(AIData))
    @training_data = data.select { |d| d.label != nil }
  end
  
  def predict(sample : AIData) : Float64
    distances = @training_data.map do |training_point|
      {distance: sample.distance_to(training_point), label: training_point.label.not_nil!}
    end.sort_by { |d| d[:distance] }
    
    # Vote among k nearest neighbors
    k_nearest = distances.first(@k)
    label_counts = Hash(Float64, Int32).new(0)
    
    k_nearest.each do |neighbor|
      label_counts[neighbor[:label]] += 1
    end
    
    label_counts.max_by { |label, count| count }[0]
  end
end

class LinearRegression
  def initialize(learning_rate : Float64 = 0.01, epochs : Int32 = 1000)
    @learning_rate = learning_rate
    @epochs = epochs
    @weights = [] of Float64
    @bias = 0.0
  end
  
  def train(data : Array(AIData))
    return if data.empty?
    
    # Initialize weights
    feature_count = data.first.features.size
    @weights = Array.new(feature_count, 0.0)
    
    @epochs.times do |epoch|
      total_error = 0.0
      
      data.each do |sample|
        prediction = predict_single(sample.features)
        error = prediction - sample.label.not_nil!
        total_error += error ** 2
        
        # Update weights and bias using gradient descent
        @weights.each_with_index do |weight, i|
          gradient = error * sample.features[i]
          @weights[i] -= @learning_rate * gradient
        end
        @bias -= @learning_rate * error
      end
      
      # Print progress every 100 epochs
      if epoch % 100 == 0
        mse = total_error / data.size
        puts "Epoch #{epoch}, MSE: #{mse.round(4)}"
      end
    end
  end
  
  def predict(features : Array(Float64)) : Float64
    prediction = @bias
    features.each_with_index do |feature, i|
      prediction += feature * @weights[i]
    end
    prediction
  end
end

class SimpleNeuralNetwork
  def initialize(input_size : Int32, hidden_size : Int32, output_size : Int32)
    @input_size = input_size
    @hidden_size = hidden_size
    @output_size = output_size
    
    # Initialize weights randomly
    @weights_input_hidden = Array.new(input_size) { Array.new(hidden_size) { rand(-1.0..1.0) } }
    @weights_hidden_output = Array.new(hidden_size) { Array.new(output_size) { rand(-1.0..1.0) } }
    @bias_hidden = Array.new(hidden_size, 0.0)
    @bias_output = Array.new(output_size, 0.0)
  end
  
  def sigmoid(x : Float64) : Float64
    1.0 / (1.0 + Math.exp(-x))
  end
  
  def sigmoid_derivative(x : Float64) : Float64
    sigmoid(x) * (1.0 - sigmoid(x))
  end
  
  def forward(input : Array(Float64)) : Array(Float64)
    # Input to hidden layer
    @hidden_layer = Array.new(@hidden_size) do |i|
      sum = @bias_hidden[i]
      @input_size.times do |j|
        sum += input[j] * @weights_input_hidden[j][i]
      end
      sigmoid(sum)
    end
    
    # Hidden to output layer
    @output_layer = Array.new(@output_size) do |i|
      sum = @bias_output[i]
      @hidden_size.times do |j|
        sum += @hidden_layer[j] * @weights_hidden_output[j][i]
      end
      sigmoid(sum)
    end
    
    @output_layer
  end
  
  def train_batch(samples : Array(AIData), epochs : Int32 = 1000, learning_rate : Float64 = 0.1)
    epochs.times do |epoch|
      total_error = 0.0
      
      samples.each do |sample|
        # Forward pass
        output = forward(sample.features)
        expected = [sample.label.not_nil!]
        
        # Calculate error
        error = 0.0
        @output_size.times do |i|
          error += (output[i] - expected[i]) ** 2
        end
        total_error += error
        
        # Backpropagation (simplified)
        # This is a simplified version for demonstration
        # In practice, you'd implement full backpropagation
      end
      
      if epoch % 100 == 0
        puts "Epoch #{epoch}, Total Error: #{total_error.round(4)}"
      end
    end
  end
end

# Demo usage
if __FILE__ == $0
  puts "=== Crystal AI Algorithms Demo ==="
  
  # KNN Demo
  puts "\n--- K-Nearest Neighbors ---"
  knn = KNNClassifier.new(3)
  
  # Training data for binary classification
  training_data = [
    AIData.new([1.0, 2.0], 0.0),
    AIData.new([2.0, 3.0], 0.0),
    AIData.new([3.0, 4.0], 1.0),
    AIData.new([4.0, 5.0], 1.0),
    AIData.new([5.0, 6.0], 1.0)
  ]
  
  knn.train(training_data)
  test_sample = AIData.new([2.5, 3.5])
  prediction = knn.predict(test_sample)
  puts "KNN Prediction for [2.5, 3.5]: #{prediction}"
  
  # Linear Regression Demo
  puts "\n--- Linear Regression ---"
  lr = LinearRegression.new(0.01, 500)
  
  regression_data = [
    AIData.new([1.0], 2.0),
    AIData.new([2.0], 4.0),
    AIData.new([3.0], 6.0),
    AIData.new([4.0], 8.0),
    AIData.new([5.0], 10.0)
  ]
  
  lr.train(regression_data)
  test_prediction = lr.predict([6.0])
  puts "Linear Regression Prediction for x=6.0: #{test_prediction.round(2)}"
  
  # Neural Network Demo
  puts "\n--- Neural Network ---"
  nn = SimpleNeuralNetwork.new(2, 4, 1)
  
  # Simple XOR problem
  xor_data = [
    AIData.new([0.0, 0.0], 0.0),
    AIData.new([0.0, 1.0], 1.0),
    AIData.new([1.0, 0.0], 1.0),
    AIData.new([1.0, 1.0], 0.0)
  ]
  
  nn.train_batch(xor_data, 500, 0.1)
  result = nn.forward([1.0, 0.0])
  puts "Neural Network XOR(1, 0): #{result[0].round(4)}"
end
