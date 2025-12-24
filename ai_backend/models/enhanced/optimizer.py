"""
Parameter Optimizer
Advanced optimization algorithms for AI model parameters
"""
import json
import os
import numpy as np
from typing import Dict, List, Tuple, Optional, Any, Callable
from dataclasses import dataclass, asdict
from datetime import datetime
import logging
from ai_backend.models.enhanced.parameter_manager import ParameterManager, ParameterConfig
from ai_backend.models.enhanced.evaluator import ModelEvaluator
from ai_backend.models.config.ai_config import ai_config

@dataclass
class OptimizationConfig:
    """Configuration for optimization process"""
    algorithm: str = "genetic"  # 'genetic', 'simulated_annealing', 'gradient_descent', 'bayesian'
    population_size: int = 50
    generations: int = 100
    mutation_rate: float = 0.1
    crossover_rate: float = 0.8
    elite_size: int = 5
    convergence_threshold: float = 1e-6
    max_iterations: int = 1000
    patience: int = 20
    verbose: bool = True

@dataclass
class OptimizationResult:
    """Results from parameter optimization"""
    algorithm: str
    best_parameters: Dict[str, Any]
    best_score: float
    optimization_history: List[Dict[str, Any]]
    convergence_iteration: int
    total_evaluations: int
    optimization_time_seconds: float
    final_metrics: Dict[str, float]

class ParameterOptimizer:
    """Advanced parameter optimization for AI models"""
    
    def __init__(self, model_name: str = "nexus", config: OptimizationConfig = None):
        self.model_name = model_name
        self.config = config or OptimizationConfig()
        self.logger = logging.getLogger(__name__)
        
        # Initialize components
        self.parameter_manager = ParameterManager(model_name)
        self.evaluator = ModelEvaluator(model_name)
        
        # Optimization state
        self.optimization_history = []
        self.best_parameters = None
        self.best_score = float('-inf')
        self.convergence_iteration = 0
        
        # Results storage
        self.optimization_dir = f"ai_backend/models/optimization/{model_name}"
        os.makedirs(self.optimization_dir, exist_ok=True)
    
    def evaluate_parameters(self, parameters: Dict[str, Any], evaluation_data: List[Dict[str, Any]]) -> float:
        """
        Evaluate parameter set using the model evaluator
        
        Args:
            parameters: Dictionary of parameter values
            evaluation_data: Data for evaluation
            
        Returns:
            Fitness score (higher is better)
        """
        try:
            # Apply parameters to parameter manager
            if 'w_learning_rate' in parameters:
                self.parameter_manager.config.w_learning_rate = parameters['w_learning_rate']
            if 'b_learning_rate' in parameters:
                self.parameter_manager.config.b_learning_rate = parameters['b_learning_rate']
            if 'w_regularization' in parameters:
                self.parameter_manager.config.w_regularization = parameters['w_regularization']
            if 'b_regularization' in parameters:
                self.parameter_manager.config.b_regularization = parameters['b_regularization']
            if 'w_size' in parameters:
                self.parameter_manager.active_w_size = parameters['w_size']
            if 'b_size' in parameters:
                self.parameter_manager.active_b_size = parameters['b_size']
            
            # Evaluate model
            metrics = self.evaluator.evaluate_model(evaluation_data)
            
            # Combine metrics into fitness score
            # Higher R2 score and accuracy = better fitness
            # Lower MSE and inference time = better fitness
            fitness = (
                metrics.r2_score * 100 +  # Weight R2 heavily
                metrics.accuracy_within_tolerance * 50 +  # Weight accuracy
                (1.0 / (1.0 + metrics.mse)) * 20 +  # Inverse MSE
                (1.0 / (1.0 + metrics.inference_time_ms / 1000)) * 10  # Speed bonus
            )
            
            return fitness
            
        except Exception as e:
            self.logger.warning(f"Parameter evaluation failed: {e}")
            return 0.0  # Return low score for failed evaluations
    
    def genetic_algorithm_optimization(self, evaluation_data: List[Dict[str, Any]]) -> OptimizationResult:
        """Genetic Algorithm optimization"""
        start_time = datetime.now()
        
        # Define parameter bounds
        param_bounds = {
            'w_learning_rate': (0.0001, 0.01),
            'b_learning_rate': (0.0001, 0.01),
            'w_regularization': (0.001, 0.1),
            'b_regularization': (0.001, 0.1),
            'w_size': (5000, 20000),
            'b_size': (5000, 20000)
        }
        
        # Initialize population
        population = self._initialize_population(param_bounds)
        generations_without_improvement = 0
        
        for generation in range(self.config.generations):
            # Evaluate fitness for all individuals
            fitness_scores = []
            for individual in population:
                fitness = self.evaluate_parameters(individual, evaluation_data)
                fitness_scores.append(fitness)
            
            # Track best individual
            best_idx = np.argmax(fitness_scores)
            if fitness_scores[best_idx] > self.best_score:
                self.best_score = fitness_scores[best_idx]
                self.best_parameters = population[best_idx].copy()
                self.convergence_iteration = generation
                generations_without_improvement = 0
            else:
                generations_without_improvement += 1
            
            # Log progress
            if self.config.verbose and generation % 10 == 0:
                self.logger.info(f"Generation {generation}: Best fitness = {self.best_score:.4f}")
            
            # Check for convergence
            if generations_without_improvement >= self.config.patience:
                self.logger.info(f"Converged at generation {generation}")
                break
            
            # Create next generation
            population = self._evolve_population(population, fitness_scores, param_bounds)
        
        # Final evaluation
        final_metrics = self._get_final_metrics(evaluation_data)
        optimization_time = (datetime.now() - start_time).total_seconds()
        
        result = OptimizationResult(
            algorithm="genetic_algorithm",
            best_parameters=self.best_parameters,
            best_score=self.best_score,
            optimization_history=self.optimization_history,
            convergence_iteration=self.convergence_iteration,
            total_evaluations=len(population) * (self.convergence_iteration + 1),
            optimization_time_seconds=optimization_time,
            final_metrics=final_metrics
        )
        
        return result
    
    def simulated_annealing_optimization(self, evaluation_data: List[Dict[str, Any]]) -> OptimizationResult:
        """Simulated Annealing optimization"""
        start_time = datetime.now()
        
        # Define parameter bounds
        param_bounds = {
            'w_learning_rate': (0.0001, 0.01),
            'b_learning_rate': (0.0001, 0.01),
            'w_regularization': (0.001, 0.1),
            'b_regularization': (0.001, 0.1),
            'w_size': (5000, 20000),
            'b_size': (5000, 20000)
        }
        
        # Initialize with random parameters
        current_params = self._random_parameters(param_bounds)
        current_fitness = self.evaluate_parameters(current_params, evaluation_data)
        
        # Annealing parameters
        initial_temperature = 100.0
        final_temperature = 0.01
        cooling_rate = 0.95
        
        self.best_parameters = current_params.copy()
        self.best_score = current_fitness
        
        for iteration in range(self.config.max_iterations):
            temperature = initial_temperature * (cooling_rate ** iteration)
            
            if temperature < final_temperature:
                break
            
            # Generate neighbor solution
            neighbor_params = self._perturb_parameters(current_params, param_bounds, temperature / initial_temperature)
            neighbor_fitness = self.evaluate_parameters(neighbor_params, evaluation_data)
            
            # Accept or reject neighbor
            if neighbor_fitness > current_fitness:
                # Always accept better solution
                current_params = neighbor_params
                current_fitness = neighbor_fitness
                
                if neighbor_fitness > self.best_score:
                    self.best_score = neighbor_fitness
                    self.best_parameters = neighbor_params.copy()
                    self.convergence_iteration = iteration
            else:
                # Accept worse solution with probability
                probability = np.exp((neighbor_fitness - current_fitness) / temperature)
                if np.random.random() < probability:
                    current_params = neighbor_params
                    current_fitness = neighbor_fitness
            
            # Log progress
            if self.config.verbose and iteration % 50 == 0:
                self.logger.info(f"Iteration {iteration}: Best fitness = {self.best_score:.4f}, Temperature = {temperature:.4f}")
        
        # Final evaluation
        final_metrics = self._get_final_metrics(evaluation_data)
        optimization_time = (datetime.now() - start_time).total_seconds()
        
        result = OptimizationResult(
            algorithm="simulated_annealing",
            best_parameters=self.best_parameters,
            best_score=self.best_score,
            optimization_history=self.optimization_history,
            convergence_iteration=self.convergence_iteration,
            total_evaluations=self.convergence_iteration + 1,
            optimization_time_seconds=optimization_time,
            final_metrics=final_metrics
        )
        
        return result
    
    def _initialize_population(self, param_bounds: Dict[str, Tuple[float, float]]) -> List[Dict[str, Any]]:
        """Initialize population for genetic algorithm"""
        population = []
        for _ in range(self.config.population_size):
            individual = self._random_parameters(param_bounds)
            population.append(individual)
        return population
    
    def _random_parameters(self, param_bounds: Dict[str, Tuple[float, float]]) -> Dict[str, Any]:
        """Generate random parameters within bounds"""
        parameters = {}
        for param_name, (min_val, max_val) in param_bounds.items():
            if param_name in ['w_size', 'b_size']:
                parameters[param_name] = np.random.randint(int(min_val), int(max_val))
            else:
                parameters[param_name] = np.random.uniform(min_val, max_val)
        return parameters
    
    def _perturb_parameters(self, parameters: Dict[str, Any], param_bounds: Dict[str, Tuple[float, float]], perturbation_factor: float) -> Dict[str, Any]:
        """Perturb parameters for simulated annealing"""
        perturbed = parameters.copy()
        
        for param_name, value in perturbed.items():
            min_val, max_val = param_bounds[param_name]
            range_size = max_val - min_val
            
            if param_name in ['w_size', 'b_size']:
                # Integer perturbation
                perturbation = np.random.randint(-int(range_size * perturbation_factor * 0.1), 
                                               int(range_size * perturbation_factor * 0.1) + 1)
                perturbed[param_name] = max(min_val, min(max_val, value + perturbation))
            else:
                # Float perturbation
                perturbation = np.random.normal(0, range_size * perturbation_factor * 0.1)
                perturbed[param_name] = max(min_val, min(max_val, value + perturbation))
        
        return perturbed
    
    def _evolve_population(self, population: List[Dict[str, Any]], fitness_scores: List[float], 
                          param_bounds: Dict[str, Tuple[float, float]]) -> List[Dict[str, Any]]:
        """Evolve population for genetic algorithm"""
        # Sort by fitness (descending)
        sorted_indices = np.argsort(fitness_scores)[::-1]
        
        # Select elite individuals
        elite = [population[i] for i in sorted_indices[:self.config.elite_size]]
        
        # Create offspring
        offspring = []
        while len(offspring) < self.config.population_size - self.config.elite_size:
            # Selection (tournament selection)
            parent1 = self._tournament_selection(population, fitness_scores)
            parent2 = self._tournament_selection(population, fitness_scores)
            
            # Crossover
            if np.random.random() < self.config.crossover_rate:
                child1, child2 = self._crossover(parent1, parent2, param_bounds)
            else:
                child1, child2 = parent1.copy(), parent2.copy()
            
            # Mutation
            if np.random.random() < self.config.mutation_rate:
                child1 = self._mutate(child1, param_bounds)
            if np.random.random() < self.config.mutation_rate:
                child2 = self._mutate(child2, param_bounds)
            
            offspring.extend([child1, child2])
        
        return elite + offspring[:self.config.population_size - self.config.elite_size]
    
    def _tournament_selection(self, population: List[Dict[str, Any]], fitness_scores: List[float], tournament_size: int = 3) -> Dict[str, Any]:
        """Tournament selection"""
        tournament_indices = np.random.choice(len(population), tournament_size, replace=False)
        tournament_fitness = [fitness_scores[i] for i in tournament_indices]
        winner_idx = tournament_indices[np.argmax(tournament_fitness)]
        return population[winner_idx].copy()
    
    def _crossover(self, parent1: Dict[str, Any], parent2: Dict[str, Any], param_bounds: Dict[str, Tuple[float, float]]) -> Tuple[Dict[str, Any], Dict[str, Any]]:
        """Uniform crossover"""
        child1, child2 = {}, {}
        
        for param_name in parent1.keys():
            if np.random.random() < 0.5:
                child1[param_name] = parent1[param_name]
                child2[param_name] = parent2[param_name]
            else:
                child1[param_name] = parent2[param_name]
                child2[param_name] = parent1[param_name]
        
        return child1, child2
    
    def _mutate(self, individual: Dict[str, Any], param_bounds: Dict[str, Tuple[float, float]]) -> Dict[str, Any]:
        """Gaussian mutation"""
        mutated = individual.copy()
        
        for param_name, value in mutated.items():
            min_val, max_val = param_bounds[param_name]
            range_size = max_val - min_val
            
            if param_name in ['w_size', 'b_size']:
                # Integer mutation
                mutation = np.random.normal(0, range_size * 0.05)
                mutated[param_name] = max(min_val, min(max_val, int(value + mutation)))
            else:
                # Float mutation
                mutation = np.random.normal(0, range_size * 0.05)
                mutated[param_name] = max(min_val, min(max_val, value + mutation))
        
        return mutated
    
    def _get_final_metrics(self, evaluation_data: List[Dict[str, Any]]) -> Dict[str, float]:
        """Get final evaluation metrics"""
        if self.best_parameters:
            # Apply best parameters
            for param_name, value in self.best_parameters.items():
                if param_name == 'w_learning_rate':
                    self.parameter_manager.config.w_learning_rate = value
                elif param_name == 'b_learning_rate':
                    self.parameter_manager.config.b_learning_rate = value
                elif param_name == 'w_regularization':
                    self.parameter_manager.config.w_regularization = value
                elif param_name == 'b_regularization':
                    self.parameter_manager.config.b_regularization = value
                elif param_name == 'w_size':
                    self.parameter_manager.active_w_size = value
                elif param_name == 'b_size':
                    self.parameter_manager.active_b_size = value
            
            # Evaluate with best parameters
            metrics = self.evaluator.evaluate_model(evaluation_data)
            return {
                "mse": metrics.mse,
                "r2_score": metrics.r2_score,
                "accuracy": metrics.accuracy_within_tolerance,
                "inference_time_ms": metrics.inference_time_ms
            }
        
        return {}
    
    def optimize(self, evaluation_data: List[Dict[str, Any]], algorithm: str = None) -> OptimizationResult:
        """
        Main optimization function
        
        Args:
            evaluation_data: Data for optimization
            algorithm: Optimization algorithm to use
            
        Returns:
            Optimization results
        """
        algo = algorithm or self.config.algorithm
        
        self.logger.info(f"Starting {algo} optimization for model {self.model_name}")
        
        if algo == "genetic":
            result = self.genetic_algorithm_optimization(evaluation_data)
        elif algo == "simulated_annealing":
            result = self.simulated_annealing_optimization(evaluation_data)
        else:
            raise ValueError(f"Unsupported optimization algorithm: {algo}")
        
        # Save results
        self._save_optimization_result(result)
        
        self.logger.info(f"Optimization completed. Best score: {result.best_score:.4f}")
        return result
    
    def _save_optimization_result(self, result: OptimizationResult):
        """Save optimization result to file"""
        try:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            result_file = os.path.join(self.optimization_dir, f"optimization_{result.algorithm}_{timestamp}.json")
            
            with open(result_file, 'w') as f:
                json.dump(asdict(result), f, indent=2)
                
        except Exception as e:
            self.logger.error(f"Failed to save optimization result: {e}")
    
    def apply_optimized_parameters(self, result: OptimizationResult):
        """Apply optimized parameters to the model"""
        if result.best_parameters:
            for param_name, value in result.best_parameters.items():
                if param_name == 'w_learning_rate':
                    self.parameter_manager.config.w_learning_rate = value
                elif param_name == 'b_learning_rate':
                    self.parameter_manager.config.b_learning_rate = value
                elif param_name == 'w_regularization':
                    self.parameter_manager.config.w_regularization = value
                elif param_name == 'b_regularization':
                    self.parameter_manager.config.b_regularization = value
                elif param_name == 'w_size':
                    self.parameter_manager.active_w_size = value
                elif param_name == 'b_size':
                    self.parameter_manager.active_b_size = value
            
            # Save updated parameters
            self.parameter_manager._save_state()
            self.logger.info("Applied optimized parameters to model")
    
    def compare_algorithms(self, evaluation_data: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Compare different optimization algorithms"""
        algorithms = ["genetic", "simulated_annealing"]
        results = {}
        
        for algo in algorithms:
            self.logger.info(f"Testing {algo} algorithm...")
            result = self.optimize(evaluation_data, algorithm=algo)
            results[algo] = {
                "best_score": result.best_score,
                "optimization_time": result.optimization_time_seconds,
                "total_evaluations": result.total_evaluations,
                "convergence_iteration": result.convergence_iteration
            }
        
        # Determine best algorithm
        best_algo = max(results.keys(), key=lambda x: results[x]["best_score"])
        
        return {
            "comparison_results": results,
            "best_algorithm": best_algo,
            "best_score": results[best_algo]["best_score"]
        }

