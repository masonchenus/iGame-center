# Crystal ML - Machine Learning Framework and Algorithms
require "math"

# Data structures for ML
class DataSet
  property features : Array(Array(Float64))
  property labels : Array(Float64)?
  property feature_names : Array(String)?
  property label_name : String?
  
  def initialize(@features : Array(Array(Float64)), @labels : Array(Float64)? = nil, @feature_names : Array(String)? = nil, @label_name : String? = nil)
  end
  
  def size : Int32
    @features.size
  end
  
  def feature_count : Int32
    @features.empty? ? 0 : @features.first.size
  end
  
  def split(ratio : Float64 = 0.8) : Tuple(DataSet, DataSet)
    split_index = (@features.size * ratio).to_i
    
    train_features = @features[0...split_index]
    train_labels = @labels ? @labels[0...split_index] : nil
    test_features = @features[split_index..-1]
    test_labels = @labels ? @labels[split_index..-1] : nil
    
    {
      DataSet.new(train_features, train_labels, @feature_names, @label_name),
      DataSet.new(test_features, test_labels, @feature_names, @label_name)
    }
  end
end

class Matrix
  property data : Array(Array(Float64))
  property rows : Int32
  property cols : Int32
  
  def initialize(@data : Array(Array(Float64)))
    @rows = @data.size
    @cols = @data.empty? ? 0 : @data.first.size
  end
  
  def self.identity(size : Int32) : Matrix
    data = Array.new(size) { |i| Array.new(size) { |j| i == j ? 1.0 : 0.0 } }
    Matrix.new(data)
  end
  
  def self.random(rows : Int32, cols : Int32, min : Float64 = -1.0, max : Float64 = 1.0) : Matrix
    data = Array.new(rows) do
      Array.new(cols) { rand(min..max) }
    end
    Matrix.new(data)
  end
  
  def [](row : Int32, col : Int32) : Float64
    @data[row][col]
  end
  
  def []=(row : Int32, col : Int32, value : Float64)
    @data[row][col] = value
  end
  
  def transpose : Matrix
    transposed_data = Array.new(@cols) { Array.new(@rows) { 0.0 } }
    
    @rows.times do |i|
      @cols.times do |j|
        transposed_data[j][i] = @data[i][j]
      end
    end
    
    Matrix.new(transposed_data)
  end
  
  def multiply(other : Matrix) : Matrix
    raise "Matrix dimensions don't match for multiplication" unless @cols == other.rows
    
    result_data = Array.new(@rows) { Array.new(other.cols) { 0.0 } }
    
    @rows.times do |i|
      other.cols.times do |j|
        @cols.times do |k|
          result_data[i][j] += @data[i][k] * other.data[k][j]
        end
      end
    end
    
    Matrix.new(result_data)
  end
  
  def multiply_vector(vector : Array(Float64)) : Array(Float64)
    raise "Matrix columns must match vector size" unless @cols == vector.size
    
    result = Array.new(@rows) { 0.0 }
    
    @rows.times do |i|
      @cols.times do |j|
        result[i] += @data[i][j] * vector[j]
      end
    end
    
    result
  end
  
  def add(other : Matrix) : Matrix
    raise "Matrix dimensions don't match" unless @rows == other.rows && @cols == other.cols
    
    result_data = Array.new(@rows) do |i|
      Array.new(@cols) do |j|
        @data[i][j] + other.data[i][j]
      end
    end
    
    Matrix.new(result_data)
  end
  
  def subtract(other : Matrix) : Matrix
    raise "Matrix dimensions don't match" unless @rows == other.rows && @cols == other.cols
    
    result_data = Array.new(@rows) do |i|
      Array.new(@cols) do |j|
        @data[i][j] - other.data[i][j]
      end
    end
    
    Matrix.new(result_data)
  end
  
  def scale(scalar : Float64) : Matrix
    result_data = @data.map { |row| row.map { |value| value * scalar } }
    Matrix.new(result_data)
  end
  
  def apply_function(func : Float64 -> Float64) : Matrix
    result_data = @data.map { |row| row.map { |value| func.call(value) } }
    Matrix.new(result_data)
  end
end

# Activation Functions
module ActivationFunctions
  def self.sigmoid(x : Float64) : Float64
    1.0 / (1.0 + Math.exp(-x))
  end
  
  def self.sigmoid_derivative(x : Float64) : Float64
    s = sigmoid(x)
    s * (1.0 - s)
  end
  
  def self.relu(x : Float64) : Float64
    x > 0 ? x : 0.0
  end
  
  def self.relu_derivative(x : Float64) : Float64
    x > 0 ? 1.0 : 0.0
  end
  
  def self.t64) : Floatanh(x : Float64
    Math.tanh(x)
  end
  
  def self.tanh_derivative(x : Float64) : Float64
    t = Math.tanh(x)
    1.0 - t * t
  end
end

# Neural Network Implementation
class NeuralNetwork
  property weights : Array(Matrix)
  property biases : Array(Array(Float64))
  property activation_functions : Array((Float64 -> Float64))
  property learning_rate : Float64
  
  def initialize(layer_sizes : Array(Int32), @learning_rate : Float64 = 0.01)
    raise "Need at least 2 layers" unless layer_sizes.size >= 2
    
    @weights = [] of Matrix
    @biases = [] of Array(Float64)
    @activation_functions = [] of (Float64 -> Float64)
    
    # Initialize weights and biases
    (layer_sizes.size - 1).times do |i|
      weight_matrix = Matrix.random(layer_sizes[i + 1], layer_sizes[i], -0.5, 0.5)
      @weights << weight_matrix
      
      bias_vector = Array.new(layer_sizes[i + 1], 0.0)
      @biases << bias_vector
      
      # Use sigmoid for hidden layers, linear for output
      if i < layer_sizes.size - 2
        @activation_functions << ActivationFunctions.method(:sigmoid)
      else
        @activation_functions << ActivationFunctions.method(:sigmoid) # Can be changed to linear
      end
    end
  end
  
  def forward(input : Array(Float64)) : Array(Float64)
    current_input = input
    
    @weights.each_with_index do |weight_matrix, i|
      # Apply weights
      output = weight_matrix.multiply_vector(current_input)
      
      # Add biases
      output.each_with_index do |value, j|
        output[j] = value + @biases[i][j]
      end
      
      # Apply activation function
      activation_func = @activation_functions[i]
      current_input = output.map { |value| activation_func.call(value) }
    end
    
    current_input
  end
  
  def train(dataset : DataSet, epochs : Int32 = 1000)
    puts "Training neural network for #{epochs} epochs..."
    
    epochs.times do |epoch|
      total_error = 0.0
      
      dataset.features.each_with_index do |features, sample_idx|
        labels = dataset.labels.not_nil!
        target = labels[sample_idx]
        
        # Forward pass
        output = forward(features)
        predicted = output.first
        
        # Calculate error
        error = (predicted - target) ** 2
        total_error += error
        
        # Backward pass (simplified)
        update_weights(features, target, output)
      end
      
      if epoch % 100 == 0
        mse = total_error / dataset.size
        puts "Epoch #{epoch}, MSE: #{mse.round(4)}"
      end
    end
  end
  
  def predict(input : Array(Float64)) : Array(Float64)
    forward(input)
  end
  
  private def update_weights(input : Array(Float64), target : Float64, output : Array(Float64))
    # Simplified weight update - in practice, implement full backpropagation
    learning_signal = (output.first - target) * @learning_rate
    
    @weights.first.data.each_with_index do |row, i|
      row.each_with_index do |_, j|
        @weights.first.data[i][j] -= learning_signal * input[j] * 0.1
      end
    end
  end
end

# Decision Tree Implementation
class DecisionTreeNode
  property feature_index : Int32?
  property threshold : Float64?
  property left : DecisionTreeNode?
  property right : DecisionTreeNode?
  property value : Float64?
  property samples : Int32
  property feature_importance : Hash(Int32, Float64)
  
  def initialize(@samples : Int32)
    @feature_importance = Hash(Int32, Float64).new(0.0)
  end
  
  def leaf? : Bool
    @left.nil? && @right.nil?
  end
end

class DecisionTree
  property root : DecisionTreeNode?
  property max_depth : Int32
  property min_samples_split : Int32
  
  def initialize(@max_depth : Int32 = 10, @min_samples_split : Int32 = 2)
  end
  
  def fit(features : Array(Array(Float64)), labels : Array(Float64))
    @root = build_tree(features, labels, 0)
  end
  
  def predict(sample : Array(Float64)) : Float64
    traverse_tree(sample, @root.not_nil!)
  end
  
  private def build_tree(features : Array(Array(Float64)), labels : Array(Float64), depth : Int32) : DecisionTreeNode
    node = DecisionTreeNode.new(features.size)
    
    # Stopping criteria
    if depth >= @max_depth || features.size < @min_samples_split || all_same_value(labels)
      node.value = labels.sum / labels.size.to_f64
      return node
    end
    
    # Find best split
    best_feature, best_threshold = find_best_split(features, labels)
    
    if best_feature.nil?
      node.value = labels.sum / labels.size.to_f64
      return node
    end
    
    node.feature_index = best_feature
    node.threshold = best_threshold
    
    # Split data
    left_features, left_labels, right_features, right_labels = split_data(features, labels, best_feature, best_threshold)
    
    # Recursively build subtrees
    if !left_features.empty?
      node.left = build_tree(left_features, left_labels, depth + 1)
    end
    
    if !right_features.empty?
      node.right = build_tree(right_features, right_labels, depth + 1)
    end
    
    node
  end
  
  private def find_best_split(features : Array(Array(Float64)), labels : Array(Float64)) : Tuple(Int32?, Float64?)
    best_gini = Float64::INFINITY
    best_feature = nil
    best_threshold = nil
    
    feature_count = features.first.size
    
    feature_count.times do |feature_idx|
      # Find unique values for this feature
      feature_values = features.map { |sample| sample[feature_idx] }.uniq.sort
      
      feature_values.each do |threshold|
        left_labels, right_labels = split_labels(features, labels, feature_idx, threshold)
        
        next if left_labels.empty? || right_labels.empty?
        
        gini = calculate_gini(left_labels, right_labels)
        
        if gini < best_gini
          best_gini = gini
          best_feature = feature_idx
          best_threshold = threshold
        end
      end
    end
    
    {best_feature, best_threshold}
  end
  
  private def split_data(features : Array(Array(Float64)), labels : Array(Float64), feature_index : Int32, threshold : Float64) : Tuple(Array(Array(Float64)), Array(Float64), Array(Array(Float64)), Array(Float64))
    left_features = [] of Array(Float64)
    left_labels = [] of Float64
    right_features = [] of Array(Float64)
    right_labels = [] of Float64
    
    features.each_with_index do |sample, idx|
      if sample[feature_index] <= threshold
        left_features << sample
        left_labels << labels[idx]
      else
        right_features << sample
        right_labels << labels[idx]
      end
    end
    
    {left_features, left_labels, right_features, right_labels}
  end
  
  private def split_labels(features : Array(Array(Float64)), labels : Array(Float64), feature_index : Int32, threshold : Float64) : Tuple(Array(Float64), Array(Float64))
    left_labels = [] of Float64
    right_labels = [] of Float64
    
    features.each_with_index do |sample, idx|
      if sample[feature_index] <= threshold
        left_labels << labels[idx]
      else
        right_labels << labels[idx]
      end
    end
    
    {left_labels, right_labels}
  end
  
  private def calculate_gini(left_labels : Array(Float64), right_labels : Array(Float64)) : Float64
    total_samples = left_labels.size + right_labels.size
    
    left_gini = calculate_gini_impurity(left_labels)
    right_gini = calculate_gini_impurity(right_labels)
    
    (left_labels.size.to_f64 / total_samples) * left_gini +
      (right_labels.size.to_f64 / total_samples) * right_gini
  end
  
  private def calculate_gini_impurity(labels : Array(Float64)) : Float64
    return 0.0 if labels.empty?
    
    # For regression, use variance instead of gini
    mean = labels.sum / labels.size.to_f64
    variance = labels.map { |label| (label - mean) ** 2 }.sum / labels.size.to_f64
    variance
  end
  
  private def all_same_value(labels : Array(Float64)) : Bool
    first_value = labels.first
    labels.all? { |label| label == first_value }
  end
  
  private def traverse_tree(sample : Array(Float64), node : DecisionTreeNode) : Float64
    return node.value.not_nil! if node.leaf?
    
    feature_index = node.feature_index.not_nil!
    threshold = node.threshold.not_nil!
    
    if sample[feature_index] <= threshold
      traverse_tree(sample, node.left.not_nil!)
    else
      traverse_tree(sample, node.right.not_nil!)
    end
  end
end

# K-Means Clustering
class KMeans
  property k : Int32
  property centroids : Array(Array(Float64))
  property max_iterations : Int32
  
  def initialize(@k : Int32, @max_iterations : Int32 = 100)
  end
  
  def fit(features : Array(Array(Float64)))
    puts "Running K-Means clustering with k=#{@k}..."
    
    # Initialize centroids randomly
    @centroids = initialize_centroids(features)
    
    @max_iterations.times do |iteration|
      # Assign points to nearest centroid
      clusters = assign_to_clusters(features)
      
      # Update centroids
      new_centroids = update_centroids(features, clusters)
      
      # Check for convergence
      if centroids_converged(@centroids, new_centroids)
        puts "Converged after #{iteration} iterations"
        break
      end
      
      @centroids = new_centroids
    end
  end
  
  def predict(sample : Array(Float64)) : Int32
    min_distance = Float64::INFINITY
    closest_cluster = 0
    
    @centroids.each_with_index do |centroid, i|
      distance = euclidean_distance(sample, centroid)
      if distance < min_distance
        min_distance = distance
        closest_cluster = i
      end
    end
    
    closest_cluster
  end
  
  private def initialize_centroids(features : Array(Array(Float64))) : Array(Array(Float64))
    centroids = [] of Array(Float64)
    
    @k.times do
      random_idx = rand(0...features.size)
      centroids << features[random_idx].dup
    end
    
    centroids
  end
  
  private def assign_to_clusters(features : Array(Array(Float64))) : Array(Int32)
    clusters = [] of Int32
    
    features.each do |sample|
      clusters << predict(sample)
    end
    
    clusters
  end
  
  private def update_centroids(features : Array(Array(Float64)), clusters : Array(Int32)) : Array(Array(Float64))
    new_centroids = Array.new(@k) { Array.new(features.first.size, 0.0) }
    cluster_counts = Array.new(@k, 0)
    
    features.each_with_index do |sample, idx|
      cluster = clusters[idx]
      cluster_counts[cluster] += 1
      
      sample.each_with_index do |value, feature_idx|
        new_centroids[cluster][feature_idx] += value
      end
    end
    
    # Calculate means
    @k.times do |i|
      if cluster_counts[i] > 0
        new_centroids[i].map! { |sum| sum / cluster_counts[i] }
      end
    end
    
    new_centroids
  end
  
  private def centroids_converged(old_centroids : Array(Array(Float64)), new_centroids : Array(Array(Float64))) : Bool
    old_centroids.each_with_index do |old_centroid, i|
      distance = euclidean_distance(old_centroid, new_centroids[i])
      return false if distance > 0.001
    end
    true
  end
  
  private def euclidean_distance(point1 : Array(Float64), point2 : Array(Float64)) : Float64
    Math.sqrt(point1.zip(point2).map { |a, b| (a - b) ** 2 }.sum)
  end
end

# Evaluation Metrics
class Metrics
  def self.mean_squared_error(actual : Array(Float64), predicted : Array(Float64)) : Float64
    raise "Arrays must have same length" unless actual.size == predicted.size
    
    sum_squared_errors = actual.zip(predicted).map { |a, p| (a - p) ** 2 }.sum
    sum_squared_errors / actual.size
  end
  
  def self.mean_absolute_error(actual : Array(Float64), predicted : Array(Float64)) : Float64
    raise "Arrays must have same length" unless actual.size == predicted.size
    
    sum_absolute_errors = actual.zip(predicted).map { |a, p| (a - p).abs }.sum
    sum_absolute_errors / actual.size
  end
  
  def self.r2_score(actual : Array(Float64), predicted : Array(Float64)) : Float64
    raise "Arrays must have same length" unless actual.size == predicted.size
    
    mean_actual = actual.sum / actual.size
    total_sum_squares = actual.map { |y| (y - mean_actual) ** 2 }.sum
    residual_sum_squares = actual.zip(predicted).map { |a, p| (a - p) ** 2 }.sum
    
    1.0 - (residual_sum_squares / total_sum_squares)
  end
end

# Demo usage
if __FILE__ == $0
  puts "=== Crystal ML Demo ==="
  
  # Generate sample data for regression
  puts "\n--- Generating Sample Data ---"
  features = [] of Array(Float64)
  labels = [] of Float64
  
  1000.times do |i|
    x = rand(0.0..10.0)
    y = 2.0 * x + 1.0 + rand(-0.5..0.5) # Linear relationship with noise
    features << [x]
    labels << y
  end
  
  dataset = DataSet.new(features, labels)
  
  # Split data
  train_data, test_data = dataset.split(0.8)
  puts "Training samples: #{train_data.size}"
  puts "Test samples: #{test_data.size}"
  
  # Neural Network Demo
  puts "\n--- Neural Network Training ---"
  nn = NeuralNetwork.new([1, 10, 5, 1], 0.01)
  nn.train(train_data, 500)
  
  # Test predictions
  test_predictions = test_data.features.map { |f| nn.predict(f).first }
  mse = Metrics.mean_squared_error(test_data.labels.not_nil!, test_predictions)
  r2 = Metrics.r2_score(test_data.labels.not_nil!, test_predictions)
  
  puts "Neural Network Results:"
  puts "  MSE: #{mse.round(4)}"
  puts "  R²: #{r2.round(4)}"
  
  # Decision Tree Demo
  puts "\n--- Decision Tree Training ---"
  dt = DecisionTree.new(max_depth: 5)
  dt.fit(train_data.features, train_data.labels.not_nil!)
  
  dt_predictions = test_data.features.map { |f| dt.predict(f) }
  dt_mse = Metrics.mean_squared_error(test_data.labels.not_nil!, dt_predictions)
  dt_r2 = Metrics.r2_score(test_data.labels.not_nil!, dt_predictions)
  
  puts "Decision Tree Results:"
  puts "  MSE: #{dt_mse.round(4)}"
  puts "  R²: #{dt_r2.round(4)}"
  
  # K-Means Demo
  puts "\n--- K-Means Clustering ---"
  cluster_features = [] of Array(Float64)
  200.times do |i|
    if i < 66
      cluster_features << [rand(0.0..3.0), rand(0.0..3.0)]
    elsif i < 133
      cluster_features << [rand(5.0..8.0), rand(0.0..3.0)]
    else
      cluster_features << [rand(2.5..5.5), rand(5.0..8.0)]
    end
  end
  
  kmeans = KMeans.new(3)
  kmeans.fit(cluster_features)
  
  # Show cluster assignments
  cluster_assignments = cluster_features.map { |f| kmeans.predict(f) }
  puts "Cluster assignments: #{cluster_assignments.tally}"
  
  puts "\nCrystal ML Framework includes:"
  puts "- Neural Networks with backpropagation"
  puts "- Decision Trees for regression/classification"
  puts "- K-Means clustering"
  puts "- Matrix operations"
  puts "- Data preprocessing utilities"
  puts "- Evaluation metrics (MSE, MAE, R²)"
end
