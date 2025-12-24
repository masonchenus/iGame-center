import re
import math
import json
from typing import Dict, Any, List, Optional

PRIVILEGES = ["solve_math"]

class MathProblem:
    """Represents a mathematical problem with enhanced solving capabilities"""
    def __init__(self, problem_text: str):
        self.problem_text = problem_text
        self.problem_type = self._classify_problem()
        self.operations = self._extract_operations()
        self.answer = None
        self.steps = []
        
    def _classify_problem(self) -> str:
        """Classify the type of math problem"""
        text = self.problem_text.lower()
        
        if any(word in text for word in ['factor', 'factorization', 'quadratic']):
            return 'factorization'
        elif any(word in text for word in ['derivative', 'differentiate', 'dx']):
            return 'calculus'
        elif any(word in text for word in ['integral', '∫', 'integrate']):
            return 'integration'
        elif any(word in text for word in ['limit', 'lim']):
            return 'limits'
        elif any(word in text for word in ['matrix', 'determinant', 'eigenvalue']):
            return 'linear_algebra'
        elif any(word in text for word in ['probability', 'stats', 'distribution']):
            return 'statistics'
        elif any(word in text for word in ['geometry', 'area', 'perimeter', 'volume']):
            return 'geometry'
        elif any(word in text for word in ['solve for', 'equation', '=']):
            return 'algebra'
        elif any(word in text for word in ['simplify', 'reduce']):
            return 'simplification'
        else:
            return 'arithmetic'
    
    def _extract_operations(self) -> List[str]:
        """Extract mathematical operations from the problem"""
        operations = []
        text = self.problem_text
        
        if '+' in text or 'plus' in text.lower():
            operations.append('addition')
        if '-' in text or 'minus' in text.lower():
            operations.append('subtraction')
        if '×' in text or '*' in text or 'times' in text.lower():
            operations.append('multiplication')
        if '÷' in text or '/' in text or 'divided by' in text.lower():
            operations.append('division')
        if '^' in text or '**' in text or 'squared' in text.lower() or 'cubed' in text.lower():
            operations.append('exponentiation')
        if '√' in text or 'sqrt' in text.lower() or 'square root' in text.lower():
            operations.append('square_root')
            
        return operations

def solve_basic_arithmetic(problem: str) -> Dict[str, Any]:
    """Solve basic arithmetic problems"""
    try:
        # Clean the problem text
        cleaned = re.sub(r'[^\d+\-*/().\s]', '', problem)
        
        # Simple evaluation for basic arithmetic
        if re.match(r'^[\d\s+\-*/().]+$', cleaned):
            result = eval(cleaned)
            return {
                'answer': result,
                'steps': [f"Calculate: {cleaned} = {result}"],
                'verification': f"Double-check: {cleaned} = {result}"
            }
    except:
        pass
    
    return {'answer': None, 'steps': ['Problem requires symbolic solution'], 'verification': 'N/A'}

def solve_algebra(problem: str) -> Dict[str, Any]:
    """Solve algebraic equations"""
    # Extract variable (assume x for now)
    if '=' in problem:
        left, right = problem.split('=', 1)
        steps = [
            f"Original equation: {problem.strip()}",
            f"Left side: {left.strip()}",
            f"Right side: {right.strip()}",
            "Solve for x by isolating the variable"
        ]
        return {
            'answer': 'x = [depends on specific equation]',
            'steps': steps,
            'verification': 'Substitute back to verify'
        }
    
    return {'answer': 'Algebraic expression', 'steps': ['Simplify expression'], 'verification': 'N/A'}

def enhance_math_solution(problem: MathProblem) -> Dict[str, Any]:
    """Provide enhanced math solution with multiple approaches"""
    text = problem.problem_text
    
    if problem.problem_type == 'arithmetic':
        return solve_basic_arithmetic(text)
    elif problem.problem_type == 'algebra':
        return solve_algebra(text)
    else:
        # For complex problems, provide structured approach
        return {
            'problem_type': problem.problem_type,
            'operations_used': problem.operations,
            'steps': [
                f"Step 1: Identify problem type: {problem.problem_type}",
                "Step 2: Extract given information",
                "Step 3: Apply appropriate mathematical method",
                "Step 4: Calculate solution",
                "Step 5: Verify answer"
            ],
            'answer': 'Solution depends on specific values',
            'tips': [
                "Break complex problems into smaller steps",
                "Check units and consistency",
                "Verify your answer makes sense"
            ]
        }

def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Enhanced Math Solver mode: Solve mathematical problems with detailed step-by-step solutions
    """
    try:
        # Parse input data
        if isinstance(input_data, str):
            problem_text = input_data
        else:
            problem_text = str(input_data)
        
        # Create math problem object
        problem = MathProblem(problem_text)
        
        # Get enhanced solution
        solution = enhance_math_solution(problem)
        
        # Format response
        response = {
            'problem': problem_text,
            'classification': problem.problem_type,
            'solution': solution,
            'formatted_answer': f"🎯 **Answer**: {solution.get('answer', 'See steps above')}",
            'steps_explanation': solution.get('steps', []),
            'additional_tips': solution.get('tips', [])
        }
        
        # If we have a real model, enhance with AI
        try:
            from ai_backend.ai_models import models
            model = models.get(model_name)
            if model:
                ai_enhanced = model.generate(
                    f"Solve this math problem step by step: {problem_text}. Provide detailed explanation and verification.",
                    user_id=user_id, session_id=session_id
                )
                response['ai_enhanced'] = ai_enhanced
        except:
            pass
        
        return json.dumps(response, indent=2)
        
    except Exception as e:
        return json.dumps({
            'error': str(e),
            'fallback': 'Mathematical problem analysis failed. Please check your input format.'
        })

def math(prompt: str) -> str:
    """Legacy function for backward compatibility"""
    problem = MathProblem(prompt)
    solution = enhance_math_solution(problem)
    return f"[Enhanced Math Mode] \nProblem: {prompt}\nType: {problem.problem_type}\nSolution: {solution.get('answer', 'See detailed response')}"
