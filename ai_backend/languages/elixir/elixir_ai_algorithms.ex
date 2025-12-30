# Elixir AI Algorithms - Machine Learning and AI Implementation
defmodule ElixirAIAlgorithms do
  # Linear Regression implementation
  defmodule LinearRegression do
    defstruct weights: [], bias: 0.0, learning_rate: 0.01, epochs: 1000

    def new(features_count, learning_rate \\ 0.01, epochs \\ 1000) do
      %LinearRegression{
        weights: List.duplicate(0.0, features_count),
        bias: 0.0,
        learning_rate: learning_rate,
        epochs: epochs
      }
    end

    def fit(model, features, targets) do
      weights = model.weights
      bias = model.bias
      learning_rate = model.learning_rate
      epochs = model.epochs

      # Training loop
      {final_weights, final_bias} = Enum.reduce(1..epochs, {weights, bias}, fn _epoch, {w, b} ->
        {new_weights, new_bias} = train_step(w, b, features, targets, learning_rate)
        {new_weights, new_bias}
      end)

      %{model | weights: final_weights, bias: final_bias}
    end

    def predict(model, features) do
      prediction = Enum.zip(model.weights, features)
                   |> Enum.map(fn {w, x} -> w * x end)
                   |> Enum.sum()
                   |> Kernel.+(model.bias)

      prediction
    end

    def predict_batch(model, features_batch) do
      Enum.map(features_batch, &predict(model, &1))
    end

    def mean_squared_error(predictions, targets) do
      errors = Enum.zip(predictions, targets)
               |> Enum.map(fn {pred, target} -> (pred - target) ** 2 end)

      Enum.sum(errors) / length(errors)
    end

    def r_squared(predictions, targets) do
      mean_target = Enum.sum(targets) / length(targets)

      ss_res = Enum.zip(predictions, targets)
               |> Enum.map(fn {pred, target} -> (target - pred) ** 2 end)
               |> Enum.sum()

      ss_tot = Enum.map(targets, fn target -> (target - mean_target) ** 2 end)
               |> Enum.sum()

      1 - (ss_res / ss_tot)
    end

    defp train_step(weights, bias, features, targets, learning_rate) do
      # Calculate gradients
      {weight_gradients, bias_gradient} = calculate_gradients(weights, bias, features, targets)

      # Update parameters
      new_weights = Enum.zip(weights, weight_gradients)
                   |> Enum.map(fn {w, grad} -> w - learning_rate * grad end)

      new_bias = bias - learning_rate * bias_gradient

      {new_weights, new_bias}
    end

    defp calculate_gradients(weights, bias, features, targets) do
      m = length(features)

      # Calculate predictions
      predictions = Enum.map(features, fn feature ->
        Enum.zip(weights, feature)
        |> Enum.map(fn {w, x} -> w * x end)
        |> Enum.sum()
        |> Kernel.+(bias)
      end)

      # Calculate errors
      errors = Enum.zip(predictions, targets)
               |> Enum.map(fn {pred, target} -> pred - target end)

      # Calculate weight gradients
      weight_gradients = Enum.map(0..(length(weights) - 1), fn i ->
        Enum.zip(features, errors)
        |> Enum.map(fn {feature, error} -> Enum.at(feature, i) * error end)
        |> Enum.sum()
        |> Kernel./(m)
      end)

      # Calculate bias gradient
      bias_gradient = Enum.sum(errors) / m

      {weight_gradients, bias_gradient}
    end
  end

  # K-Nearest Neighbors implementation
  defmodule KNN do
    defstruct k: 3, data: [], labels: []

    def new(k \\ 3) do
      %KNN{k: k, data: [], labels: []}
    end

    def fit(model, data, labels) do
      %{model | data: data, labels: labels}
    end

    def predict(model, point) do
      # Calculate distances to all training points
      distances_with_labels = Enum.zip(model.data, model.labels)
                              |> Enum.map(fn {data_point, label} ->
                                {euclidean_distance(point, data_point), label}
                              end)

      # Sort by distance and take k nearest
      nearest_neighbors = distances_with_labels
                          |> Enum.sort_by(fn {distance, _} -> distance end)
                          |> Enum.take(model.k)

      # Majority vote
      neighbor_labels = Enum.map(nearest_neighbors, fn {_, label} -> label end)
      majority_vote(neighbor_labels)
    end

    def predict_batch(model, points) do
      Enum.map(points, &predict(model, &1))
    end

    def accuracy(predictions, actual_labels) do
      correct = Enum.zip(predictions, actual_labels)
                |> Enum.count(fn {pred, actual} -> pred == actual end)

      correct / length(predictions)
    end

    defp euclidean_distance(point1, point2) do
      Enum.zip(point1, point2)
      |> Enum.map(fn {a, b} -> (a - b) ** 2 end)
      |> Enum.sum()
      |> :math.sqrt()
    end

    defp majority_vote(labels) do
      labels
      |> Enum.group_by(&(&1))
      |> Enum.max_by(fn {_, group} -> length(group) end)
      |> elem(0)
    end
  end

  # Neural Network implementation
  defmodule NeuralNetwork do
    defstruct layers: [], learning_rate: 0.01, epochs: 1000

    def new(layer_sizes, learning_rate \\ 0.01, epochs \\ 1000) do
      layers = initialize_layers(layer_sizes)
      %NeuralNetwork{layers: layers, learning_rate: learning_rate, epochs: epochs}
    end

    def fit(model, features, targets) do
      layers = model.layers
      learning_rate = model.learning_rate
      epochs = model.epochs

      # Convert targets to one-hot encoding if needed
      encoded_targets = encode_targets(targets)

      # Training loop
      final_layers = Enum.reduce(1..epochs, layers, fn epoch, current_layers ->
        # Forward pass and backward pass for each sample
        updated_layers = Enum.reduce(Enum.zip(features, encoded_targets), current_layers, fn {feature, target}, acc_layers ->
          {output, activations} = forward_pass(acc_layers, feature)
          gradients = backward_pass(acc_layers, activations, target)
          update_weights(acc_layers, gradients, learning_rate)
        end)

        # Print progress
        if rem(epoch, 100) == 0 do
          loss = calculate_loss(updated_layers, features, encoded_targets)
          IO.puts("Epoch #{epoch}, Loss: #{Float.round(loss, 4)}")
        end

        updated_layers
      end)

      %{model | layers: final_layers}
    end

    def predict(model, features) do
      {output, _} = forward_pass(model.layers, features)
      # Return the index of the highest probability
      Enum.max_index(output)
    end

    def predict_batch(model, features_batch) do
      Enum.map(features_batch, &predict(model, &1))
    end

    def accuracy(predictions, actual_labels) do
      correct = Enum.zip(predictions, actual_labels)
                |> Enum.count(fn {pred, actual} -> pred == actual end)

      correct / length(predictions)
    end

    defp initialize_layers(layer_sizes) do
      Enum.zip(layer_sizes, Enum.drop(layer_sizes, 1))
      |> Enum.map(fn {input_size, output_size} ->
        weights = Enum.map(1..output_size, fn _ ->
          Enum.map(1..input_size, fn _ -> :rand.uniform() - 0.5 end)
        end)
        biases = List.duplicate(0.0, output_size)
        {weights, biases}
      end)
    end

    defp forward_pass(layers, input) do
      {output, activations} = Enum.reduce(layers, {input, [input]}, fn {weights, biases}, {current_input, acc_activations} ->
        # Calculate weighted sum + bias
        weighted_sum = Enum.map(weights, fn neuron_weights ->
          Enum.zip(neuron_weights, current_input)
          |> Enum.map(fn {w, x} -> w * x end)
          |> Enum.sum()
        end)

        # Add biases
        with_bias = Enum.zip(weighted_sum, biases)
                    |> Enum.map(fn {sum, bias} -> sum + bias end)

        # Apply activation function (sigmoid for hidden layers, softmax for output)
        activated = if Enum.empty?(Enum.drop(layers, 1)) do
          softmax(with_bias)
        else
          Enum.map(with_bias, &sigmoid/1)
        end

        {activated, acc_activations ++ [activated]}
      end)

      {output, activations}
    end

    defp backward_pass(layers, activations, target) do
      # Calculate output layer error
      output_activation = List.last(activations)
      output_error = Enum.zip(output_activation, target)
                    |> Enum.map(fn {a, t} -> a - t end)

      # Backpropagate through layers
      {layer_errors, _} = Enum.zip(Enum.reverse(layers), Enum.reverse(Enum.drop(activations, 1)))
                          |> Enum.reduce({[output_error], output_error}, fn {{weights, _}, activation}, {errors, prev_error} ->
                            # Calculate error for current layer
                            layer_error = Enum.map(weights, fn neuron_weights ->
                              Enum.zip(neuron_weights, prev_error)
                              |> Enum.map(fn {w, e} -> w * e end)
                              |> Enum.sum()
                            end)
                            |> Enum.map(&sigmoid_derivative/1)
                            |> Enum.zip(prev_error)
                            |> Enum.map(fn {d, e} -> d * e end)

                            {[layer_error | errors], layer_error}
                          end)

      # Calculate gradients
      Enum.zip(Enum.reverse(layer_errors), Enum.reverse(Enum.drop(activations, 1)))
      |> Enum.zip(layers)
      |> Enum.map(fn {{error, activation}, {weights, _}} ->
        # Weight gradients
        weight_gradients = Enum.map(error, fn e ->
          Enum.map(activation, fn a -> e * a end)
        end)

        # Bias gradients
        bias_gradients = error

        {weight_gradients, bias_gradients}
      end)
    end

    defp update_weights(layers, gradients, learning_rate) do
      Enum.zip(layers, gradients)
      |> Enum.map(fn {{weights, biases}, {weight_grads, bias_grads}} ->
        # Update weights
        new_weights = Enum.zip(weights, weight_grads)
                      |> Enum.map(fn {neuron_weights, neuron_grads} ->
                        Enum.zip(neuron_weights, neuron_grads)
                        |> Enum.map(fn {w, g} -> w - learning_rate * g end)
                      end)

        # Update biases
        new_biases = Enum.zip(biases, bias_grads)
                     |> Enum.map(fn {b, g} -> b - learning_rate * g end)

        {new_weights, new_biases}
      end)
    end

    defp calculate_loss(layers, features, targets) do
      total_loss = Enum.zip(features, targets)
                   |> Enum.map(fn {feature, target} ->
                     {output, _} = forward_pass(layers, feature)
                     cross_entropy_loss(output, target)
                   end)
                   |> Enum.sum()

      total_loss / length(features)
    end

    defp encode_targets(targets) do
      num_classes = Enum.max(targets) + 1
      Enum.map(targets, fn target ->
        List.duplicate(0.0, num_classes)
        |> List.replace_at(target, 1.0)
      end)
    end

    defp sigmoid(x), do: 1 / (1 + :math.exp(-x))
    defp sigmoid_derivative(x), do: x * (1 - x)

    defp softmax(values) do
      max_val = Enum.max(values)
      exps = Enum.map(values, fn x -> :math.exp(x - max_val) end)
      sum_exps = Enum.sum(exps)
      Enum.map(exps, &(&1 / sum_exps))
    end

    defp cross_entropy_loss(predictions, targets) do
      Enum.zip(predictions, targets)
      |> Enum.map(fn {pred, target} ->
        if target == 0.0 do
          0.0
        else
          -target * :math.log(pred + 1.0e-15)
        end
      end)
      |> Enum.sum()
    end
  end

  # Decision Tree implementation
  defmodule DecisionTree do
    defstruct root: nil, max_depth: 10, min_samples_split: 2

    def new(max_depth \\ 10, min_samples_split \\ 2) do
      %DecisionTree{max_depth: max_depth, min_samples_split: min_samples_split}
    end

    def fit(model, features, labels) do
      root = build_tree(features, labels, 0, model.max_depth, model.min_samples_split)
      %{model | root: root}
    end

    def predict(model, features) do
      traverse_tree(model.root, features)
    end

    def predict_batch(model, features_batch) do
      Enum.map(features_batch, &predict(model, &1))
    end

    def accuracy(predictions, actual_labels) do
      correct = Enum.zip(predictions, actual_labels)
                |> Enum.count(fn {pred, actual} -> pred == actual end)

      correct / length(predictions)
    end

    defp build_tree(features, labels, depth, max_depth, min_samples_split) do
      # Check stopping conditions
      if depth >= max_depth or length(features) < min_samples_split or all_same_labels?(labels) do
        # Return leaf node with majority class
        majority_class = majority_vote(labels)
        {:leaf, majority_class}
      else
        # Find best split
        {feature_index, threshold} = find_best_split(features, labels)

        if feature_index == nil do
          # No good split found
          majority_class = majority_vote(labels)
          {:leaf, majority_class}
        else
          # Split data
          {left_features, left_labels, right_features, right_labels} =
            split_data(features, labels, feature_index, threshold)

          # Recursively build subtrees
          left_tree = build_tree(left_features, left_labels, depth + 1, max_depth, min_samples_split)
          right_tree = build_tree(right_features, right_labels, depth + 1, max_depth, min_samples_split)

          {:node, feature_index, threshold, left_tree, right_tree}
        end
      end
    end

    defp traverse_tree({:leaf, class}, _features), do: class
    defp traverse_tree({:node, feature_index, threshold, left_tree, right_tree}, features) do
      feature_value = Enum.at(features, feature_index)

      if feature_value <= threshold do
        traverse_tree(left_tree, features)
      else
        traverse_tree(right_tree, features)
      end
    end

    defp find_best_split(features, labels) do
      num_features = length(List.first(features))
      best_gini = 1.0
      best_feature = nil
      best_threshold = nil

      Enum.each(0..(num_features - 1), fn feature_index ->
        # Get unique values for this feature
        feature_values = features
                         |> Enum.map(&Enum.at(&1, feature_index))
                         |> Enum.uniq()
                         |> Enum.sort()

        # Try each possible threshold
        Enum.each(feature_values, fn threshold ->
          {left_labels, right_labels} = split_labels(features, labels, feature_index, threshold)

          if length(left_labels) > 0 and length(right_labels) > 0 do
            gini = calculate_gini(left_labels, right_labels)

            if gini < best_gini do
              best_gini = gini
              best_feature = feature_index
              best_threshold = threshold
            end
          end
        end)
      end)

      {best_feature, best_threshold}
    end

    defp split_data(features, labels, feature_index, threshold) do
      {left_features, left_labels, right_features, right_labels} =
        Enum.zip(features, labels)
        |> Enum.reduce({[], [], [], []}, fn {feature, label}, {lf, ll, rf, rl} ->
          if Enum.at(feature, feature_index) <= threshold do
            {[feature | lf], [label | ll], rf, rl}
          else
            {lf, ll, [feature | rf], [label | rl]}
          end
        end)

      {Enum.reverse(left_features), Enum.reverse(left_labels),
       Enum.reverse(right_features), Enum.reverse(right_labels)}
    end

    defp split_labels(features, labels, feature_index, threshold) do
      Enum.zip(features, labels)
      |> Enum.reduce({[], []}, fn {feature, label}, {left, right} ->
        if Enum.at(feature, feature_index) <= threshold do
          {[label | left], right}
        else
          {left, [label | right]}
        end
      end)
    end

    defp calculate_gini(left_labels, right_labels) do
      total_samples = length(left_labels) + length(right_labels)
      left_weight = length(left_labels) / total_samples
      right_weight = length(right_labels) / total_samples

      left_gini = gini_impurity(left_labels)
      right_gini = gini_impurity(right_labels)

      left_weight * left_gini + right_weight * right_gini
    end

    defp gini_impurity(labels) do
      total = length(labels)
      if total == 0, do: 0.0

      label_counts = Enum.group_by(labels, &(&1))
                    |> Enum.map(fn {_, group} -> length(group) end)

      1.0 - Enum.sum(Enum.map(label_counts, fn count ->
        proportion = count / total
        proportion * proportion
      end))
    end

    defp all_same_labels?(labels) do
      length(Enum.uniq(labels)) == 1
    end

    defp majority_vote(labels) do
      labels
      |> Enum.group_by(&(&1))
      |> Enum.max_by(fn {_, group} -> length(group) end)
      |> elem(0)
    end
  end

  # K-Means Clustering implementation
  defmodule KMeans do
    defstruct centroids: [], k: 3, max_iterations: 100

    def new(k, max_iterations \\ 100) do
      %KMeans{k: k, max_iterations: max_iterations}
    end

    def fit(model, data) do
      centroids = initialize_centroids(data, model.k)

      final_centroids = Enum.reduce(1..model.max_iterations, centroids, fn _iteration, current_centroids ->
        # Assign points to clusters
        clusters = assign_to_clusters(data, current_centroids)

        # Update centroids
        new_centroids = update_centroids(data, clusters, model.k)

        # Check for convergence
        if centroids_converged?(current_centroids, new_centroids) do
          throw {:converged, new_centroids}
        end

        new_centroids
      end) |> catch_converged()

      %{model | centroids: final_centroids}
    end

    def predict(model, point) do
      distances = Enum.map(model.centroids, &euclidean_distance(point, &1))
      Enum.min_index(distances)
    end

    def predict_batch(model, points) do
      Enum.map(points, &predict(model, &1))
    end

    def get_cluster_centers(model), do: model.centroids

    def inertia(model, data) do
      clusters = assign_to_clusters(data, model.centroids)

      Enum.zip(data, clusters)
      |> Enum.map(fn {point, cluster_id} ->
        centroid = Enum.at(model.centroids, cluster_id)
        euclidean_distance(point, centroid) ** 2
      end)
      |> Enum.sum()
    end

    defp initialize_centroids(data, k) do
      # Random initialization
      Enum.take_random(data, k)
    end

    defp assign_to_clusters(data, centroids) do
      Enum.map(data, fn point ->
        distances = Enum.map(centroids, &euclidean_distance(point, &1))
        Enum.min_index(distances)
      end)
    end

    defp update_centroids(data, clusters, k) do
      Enum.map(0..(k - 1), fn cluster_id ->
        cluster_points = Enum.zip(data, clusters)
                        |> Enum.filter(fn {_, c} -> c == cluster_id end)
                        |> Enum.map(fn {point, _} -> point end)

        if Enum.empty?(cluster_points) do
          # Keep old centroid if no points assigned
          Enum.at(data, cluster_id) || List.duplicate(0.0, length(List.first(data)))
        else
          # Calculate mean of points in cluster
          num_features = length(List.first(cluster_points))
          Enum.map(0..(num_features - 1), fn feature_index ->
            feature_values = Enum.map(cluster_points, &Enum.at(&1, feature_index))
            Enum.sum(feature_values) / length(feature_values)
          end)
        end
      end)
    end

    defp centroids_converged?(old_centroids, new_centroids) do
      Enum.zip(old_centroids, new_centroids)
      |> Enum.all?(fn {old, new} ->
        euclidean_distance(old, new) < 0.001
      end)
    end

    defp euclidean_distance(point1, point2) do
      Enum.zip(point1, point2)
      |> Enum.map(fn {a, b} -> (a - b) ** 2 end)
      |> Enum.sum()
      |> :math.sqrt()
    end

    defp catch_converged(result) do
      case result do
        {:converged, centroids} -> centroids
        centroids -> centroids
      end
    end
  end

  # Utility functions for data preprocessing
  defmodule DataPreprocessing do
    def train_test_split(data, labels, test_size \\ 0.2) do
      total_samples = length(data)
      test_count = round(total_samples * test_size)
      train_count = total_samples - test_count

      # Shuffle data
      shuffled = Enum.zip(data, labels) |> Enum.shuffle()

      # Split
      train_data = shuffled |> Enum.take(train_count) |> Enum.map(&elem(&1, 0))
      train_labels = shuffled |> Enum.take(train_count) |> Enum.map(&elem(&1, 1))
      test_data = shuffled |> Enum.drop(train_count) |> Enum.map(&elem(&1, 0))
      test_labels = shuffled |> Enum.drop(train_count) |> Enum.map(&elem(&1, 1))

      {train_data, train_labels, test_data, test_labels}
    end

    def standardize_features(data) do
      num_features = length(List.first(data))

      # Calculate mean and std for each feature
      stats = Enum.map(0..(num_features - 1), fn feature_index ->
        feature_values = Enum.map(data, &Enum.at(&1, feature_index))
        mean = Enum.sum(feature_values) / length(feature_values)
        variance = Enum.map(feature_values, &((&1 - mean) ** 2)) |> Enum.sum() |> Kernel./(length(feature_values))
        std = :math.sqrt(variance)
        {mean, std}
      end)

      # Standardize
      Enum.map(data, fn sample ->
        Enum.zip(sample, stats)
        |> Enum.map(fn {value, {mean, std}} ->
          if std == 0, do: 0.0, else: (value - mean) / std
        end)
      end)
    end

    def normalize_features(data) do
      num_features = length(List.first(data))

      # Calculate min and max for each feature
      stats = Enum.map(0..(num_features - 1), fn feature_index ->
        feature_values = Enum.map(data, &Enum.at(&1, feature_index))
        min_val = Enum.min(feature_values)
        max_val = Enum.max(feature_values)
        {min_val, max_val}
      end)

      # Normalize
      Enum.map(data, fn sample ->
        Enum.zip(sample, stats)
        |> Enum.map(fn {value, {min_val, max_val}} ->
          if max_val == min_val, do: 0.0, else: (value - min_val) / (max_val - min_val)
        end)
      end)
    end

    def one_hot_encode(labels) do
      unique_labels = Enum.uniq(labels) |> Enum.sort()
      label_to_index = Enum.with_index(unique_labels) |> Enum.into(%{})

      Enum.map(labels, fn label ->
        index = Map.get(label_to_index, label)
        List.duplicate(0, length(unique_labels))
        |> List.replace_at(index, 1)
      end)
    end
  end

  # Model evaluation utilities
  defmodule ModelEvaluation do
    def confusion_matrix(predictions, actual_labels) do
      unique_labels = (predictions ++ actual_labels) |> Enum.uniq() |> Enum.sort()

      # Initialize matrix
      matrix = Enum.map(unique_labels, fn _ ->
        Enum.map(unique_labels, fn _ -> 0 end)
      end)

      # Fill matrix
      Enum.zip(predictions, actual_labels)
      |> Enum.each(fn {pred, actual} ->
        pred_index = Enum.find_index(unique_labels, &(&1 == pred))
        actual_index = Enum.find_index(unique_labels, &(&1 == actual))
        current_value = get_in(matrix, [pred_index, actual_index])
        put_in(matrix, [pred_index, actual_index], current_value + 1)
      end)

      {matrix, unique_labels}
    end

    def classification_report(predictions, actual_labels) do
      {matrix, labels} = confusion_matrix(predictions, actual_labels)

      reports = Enum.map(Enum.with_index(labels), fn {label, index} ->
        tp = Enum.at(Enum.at(matrix, index), index)
        fp = Enum.sum(Enum.at(matrix, index)) - tp
        fn = Enum.sum(Enum.map(matrix, &Enum.at(&1, index))) - tp
        tn = Enum.sum(Enum.concat(matrix)) - tp - fp - fn

        precision = if tp + fp == 0, do: 0.0, else: tp / (tp + fp)
        recall = if tp + fn == 0, do: 0.0, else: tp / (tp + fn)
        f1_score = if precision + recall == 0, do: 0.0, else: 2 * precision * recall / (precision + recall)

        %{
          label: label,
          precision: Float.round(precision, 3),
          recall: Float.round(recall, 3),
          f1_score: Float.round(f1_score, 3),
          support: tp + fn
        }
      end)

      # Calculate macro averages
      avg_precision = Enum.map(reports, & &1.precision) |> Enum.sum() |> Kernel./(length(reports))
      avg_recall = Enum.map(reports, & &1.recall) |> Enum.sum() |> Kernel./(length(reports))
      avg_f1 = Enum.map(reports, & &1.f1_score) |> Enum.sum() |> Kernel./(length(reports))

      %{
        reports: reports,
        macro_avg: %{
          precision: Float.round(avg_precision, 3),
          recall: Float.round(avg_recall, 3),
          f1_score: Float.round(avg_f1, 3)
        }
      }
    end

    def cross_validation_score(model_module, data, labels, folds \\ 5) do
      fold_size = div(length(data), folds)
      scores = []

      Enum.each(0..(folds - 1), fn fold ->
        start_idx = fold * fold_size
        end_idx = if fold == folds - 1, do: length(data), else: (fold + 1) * fold_size

        test_data = Enum.slice(data, start_idx, end_idx - start_idx)
        test_labels = Enum.slice(labels, start_idx, end_idx - start_idx)

        train_data = Enum.slice(data, 0, start_idx) ++ Enum.slice(data, end_idx, length(data) - end_idx)
        train_labels = Enum.slice(labels, 0, start_idx) ++ Enum.slice(labels, end_idx, length(labels) - end_idx)

        # Train model
        model = model_module.fit(model_module.new(), train_data, train_labels)

        # Test model
        predictions = model_module.predict_batch(model, test_data)
        accuracy = model_module.accuracy(predictions, test_labels)

        scores = [accuracy | scores]
      end)

      mean_score = Enum.sum(scores) / length(scores)
      std_score = :math.sqrt(Enum.map(scores, &((&1 - mean_score) ** 2)) |> Enum.sum() |> Kernel./(length(scores)))

      %{mean: Float.round(mean_score, 3), std: Float.round(std_score, 3), scores: scores}
    end
  end
end

# Demo usage
defmodule ElixirAIAlgorithmsDemo do
  def run do
    IO.puts("=== Elixir AI Algorithms Demo ===")

    # Generate sample data
    IO.puts("\n--- Generating Sample Data ---")
    {features, labels} = generate_sample_data()

    IO.puts("Generated #{length(features)} samples with #{length(List.first(features))} features")

    # Linear Regression Demo
    IO.puts("\n--- Linear Regression ---")
    lr_model = ElixirAIAlgorithms.LinearRegression.new(length(List.first(features)))
    trained_lr = ElixirAIAlgorithms.LinearRegression.fit(lr_model, features, labels)

    test_features = [[1.5, 2.0], [2.5, 3.0], [3.5, 4.0]]
    lr_predictions = ElixirAIAlgorithms.LinearRegression.predict_batch(trained_lr, test_features)
    IO.puts("Linear Regression predictions: #{inspect(lr_predictions)}")

    # KNN Demo
    IO.puts("\n--- K-Nearest Neighbors ---")
    knn_model = ElixirAIAlgorithms.KNN.new(3)
    trained_knn = ElixirAIAlgorithms.KNN.fit(knn_model, features, labels)

    knn_predictions = ElixirAIAlgorithms.KNN.predict_batch(trained_knn, test_features)
    IO.puts("KNN predictions: #{inspect(knn_predictions)}")

    # Neural Network Demo
    IO.puts("\n--- Neural Network ---")
    nn_model = ElixirAIAlgorithms.NeuralNetwork.new([2, 4, 2])
    # Convert continuous labels to class indices for classification
    class_labels = Enum.map(labels, fn x -> if x > 2.0, do: 1, else: 0 end)
    trained_nn = ElixirAIAlgorithms.NeuralNetwork.fit(nn_model, features, class_labels)

    nn_predictions = ElixirAIAlgorithms.NeuralNetwork.predict_batch(trained_nn, test_features)
    IO.puts("Neural Network predictions: #{inspect(nn_predictions)}")

    # Decision Tree Demo
    IO.puts("\n--- Decision Tree ---")
    dt_model = ElixirAIAlgorithms.DecisionTree.new(5)
    trained_dt = ElixirAIAlgorithms.DecisionTree.fit(dt_model, features, class_labels)

    dt_predictions = ElixirAIAlgorithms.DecisionTree.predict_batch(trained_dt, test_features)
    IO.puts("Decision Tree predictions: #{inspect(dt_predictions)}")

    # K-Means Demo
    IO.puts("\n--- K-Means Clustering ---")
    kmeans_model = ElixirAIAlgorithms.KMeans.new(3)
    trained_kmeans = ElixirAIAlgorithms.KMeans.fit(kmeans_model, features)

    kmeans_predictions = ElixirAIAlgorithms.KMeans.predict_batch(trained_kmeans, test_features)
    IO.puts("K-Means cluster assignments: #{inspect(kmeans_predictions)}")
    IO.puts("Cluster centers: #{inspect(ElixirAIAlgorithms.KMeans.get_cluster_centers(trained_kmeans))}")

    # Data Preprocessing Demo
    IO.puts("\n--- Data Preprocessing ---")
    {train_data, train_labels, test_data, test_labels} =
      ElixirAIAlgorithms.DataPreprocessing.train_test_split(features, labels, 0.3)

    IO.puts("Train/Test split: #{length(train_data)}/#{length(test_data)} samples")

    standardized_data = ElixirAIAlgorithms.DataPreprocessing.standardize_features(features)
    IO.puts("Data standardized (first sample): #{inspect(Enum.at(standardized_data, 0))}")

    # Model Evaluation Demo
    IO.puts("\n--- Model Evaluation ---")
    predictions = ElixirAIAlgorithms.KNN.predict_batch(trained_knn, test_data)
    accuracy = ElixirAIAlgorithms.KNN.accuracy(predictions, test_labels)
    IO.puts("KNN Test Accuracy: #{Float.round(accuracy * 100, 2)}%")

    IO.puts("\n=== AI Algorithms Demo Complete ===")
  end

  defp generate_sample_data do
    # Generate synthetic regression data
    features = Enum.map(1..100, fn i ->
      x1 = :rand.uniform() * 10
      x2 = :rand.uniform() * 10
      [x1, x2]
    end)

    labels = Enum.map(features, fn [x1, x2] ->
      # Linear relationship with some noise
      2.0 * x1 + 1.5 * x2 + :rand.normal(0, 0.5)
    end)

    {features, labels}
  end
end

# Run the demo
ElixirAIAlgorithmsDemo.run()
