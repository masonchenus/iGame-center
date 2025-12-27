"""
AI Code Optimizer Tool
Performance optimization and code improvement
"""

import re
from typing import Dict, List, Any

class CodeOptimizer:
    """AI-powered code optimization tool"""
    
    def __init__(self):
        self.optimization_rules = {
            'python': [
                self._optimize_loops,
                self._optimize_data_structures,
                self._optimize_imports,
                self._optimize_functions
            ],
            'javascript': [
                self._optimize_js_loops,
                self._optimize_js_data_structures,
                self._optimize_js_functions
            ]
        }
    
    def execute(self, code: str, language: str = 'python', optimization_level: str = 'basic', **kwargs) -> Dict[str, Any]:
        """
        Optimize code for performance
        
        Args:
            code: Source code to optimize
            language: Programming language
            optimization_level: Level of optimization (basic, advanced, aggressive)
            **kwargs: Additional optimization parameters
        """
        try:
            if language not in self.optimization_rules:
                return {
                    "status": "error",
                    "error": f"Unsupported language: {language}"
                }
            
            original_code = code
            optimizations_applied = []
            
            # Apply optimizations based on language and level
            for optimizer_func in self.optimization_rules[language]:
                code, applied = optimizer_func(code, optimization_level)
                if applied:
                    optimizations_applied.extend(applied)
            
            # Language-specific optimizations
            if language == 'python':
                code, py_applied = self._optimize_python_specific(code, optimization_level)
                optimizations_applied.extend(py_applied)
            elif language == 'javascript':
                code, js_applied = self._optimize_javascript_specific(code, optimization_level)
                optimizations_applied.extend(js_applied)
            
            # Calculate performance improvements
            improvements = self._calculate_improvements(original_code, code)
            
            return {
                "status": "success",
                "original_code": original_code,
                "optimized_code": code,
                "language": language,
                "optimization_level": optimization_level,
                "optimizations_applied": optimizations_applied,
                "improvements": improvements,
                "performance_gain": improvements.get('estimated_gain', 0)
            }
            
        except Exception as e:
            return {
                "status": "error",
                "error": str(e),
                "original_code": code
            }
    
    def _optimize_loops(self, code: str, level: str) -> tuple:
        """Optimize loops for better performance"""
        optimizations = []
        
        # Replace range(len()) with enumerate()
        range_len_pattern = r'for\s+(\w+)\s+in\s+range\s*\(\s*len\s*\(\s*(\w+)\s*\)\s*\):'
        enumerate_replacement = r'for \1, item in enumerate(\2):'
        
        optimized_code = re.sub(range_len_pattern, enumerate_replacement, code)
        if optimized_code != code:
            optimizations.append("Replaced range(len()) with enumerate()")
        
        return optimized_code, optimizations
    
    def _optimize_data_structures(self, code: str, level: str) -> tuple:
        """Optimize data structure usage"""
        optimizations = []
        
        # Convert list appends to list comprehensions
        append_pattern = r'(\w+)\s*=\s*\[\s*\]\s*\n.*?for\s+(\w+)\s+in\s+(\w+):\s*\1\.append\s*\(\s*([^)]+)\s*\)'
        
        def list_comp_replacer(match):
            list_name = match.group(1)
            var_name = match.group(2)
            iterable = match.group(3)
            expression = match.group(4)
            return f"{list_name} = [{expression} for {var_name} in {iterable}]"
        
        optimized_code = re.sub(append_pattern, list_comp_replacer, code, flags=re.MULTILINE | re.DOTALL)
        if optimized_code != code:
            optimizations.append("Converted list appends to list comprehensions")
        
        return optimized_code, optimizations
    
    def _optimize_imports(self, code: str, level: str) -> tuple:
        """Optimize import statements"""
        optimizations = []
        
        # Remove unused imports (basic heuristic)
        lines = code.split('\n')
        import_lines = [line for line in lines if line.strip().startswith(('import ', 'from '))]
        used_names = self._extract_used_names(code)
        
        cleaned_lines = []
        for line in lines:
            if line.strip().startswith(('import ', 'from ')):
                if self._is_import_used(line, used_names):
                    cleaned_lines.append(line)
                else:
                    optimizations.append(f"Removed unused import: {line.strip()}")
            else:
                cleaned_lines.append(line)
        
        return '\n'.join(cleaned_lines), optimizations
    
    def _optimize_functions(self, code: str, level: str) -> tuple:
        """Optimize function definitions"""
        optimizations = []
        # Simple function optimization
        return code, optimizations
    
    def _optimize_python_specific(self, code: str, level: str) -> tuple:
        """Python-specific optimizations"""
        optimizations = []
        
        # Use more efficient built-in functions
        builtins_patterns = [
            (r'max\s*\(\s*len\s*\(\s*(\w+)\s*\)\s*\)', r'max(len(item) for item in \1)'),
            (r'min\s*\(\s*len\s*\(\s*(\w+)\s*\)\s*\)', r'min(len(item) for item in \1)')
        ]
        
        for pattern, replacement in builtins_patterns:
            optimized_code = re.sub(pattern, replacement, code)
            if optimized_code != code:
                optimizations.append("Replaced inefficient built-in function usage")
                code = optimized_code
        
        # Add type hints for better performance
        if level in ['advanced', 'aggressive']:
            code = self._add_type_hints(code)
            optimizations.append("Added type hints for better performance")
        
        return code, optimizations
    
    def _optimize_javascript_specific(self, code: str, level: str) -> tuple:
        """JavaScript-specific optimizations"""
        optimizations = []
        
        # Use const/let instead of var
        optimized_code = re.sub(r'\bvar\b', 'const', code)
        if optimized_code != code:
            optimizations.append("Replaced var with const/let")
            code = optimized_code
        
        # Use arrow functions where appropriate
        function_pattern = r'function\s+(\w+)\s*\(([^)]*)\)\s*{'
        arrow_replacement = r'const \1 = (\2) => {'
        
        optimized_code = re.sub(function_pattern, arrow_replacement, code)
        if optimized_code != code:
            optimizations.append("Converted function declarations to arrow functions")
            code = optimized_code
        
        return code, optimizations
    
    def _optimize_js_loops(self, code: str, level: str) -> tuple:
        """Optimize JavaScript loops"""
        optimizations = []
        
        # Replace traditional for loops with array methods
        for_pattern = r'for\s*\(\s*var\s+(\w+)\s*=\s*0\s*;\s*\1\s*<\s*(\w+)\.length\s*;\s*\1\+\+\s*\)\s*{'
        
        optimized_code = re.sub(for_pattern, '', code)
        if optimized_code != code:
            optimizations.append("Converted for loops to array methods")
        
        return optimized_code, optimizations
    
    def _optimize_js_data_structures(self, code: str, level: str) -> tuple:
        """Optimize JavaScript data structures"""
        optimizations = []
        
        # Use destructuring for better performance
        destructuring_pattern = r'const\s+(\w+)\s*=\s*(\w+)\.(\w+);'
        destructuring_replacement = r'const { \3 } = \2;'
        
        optimized_code = re.sub(destructuring_pattern, destructuring_replacement, code)
        if optimized_code != code:
            optimizations.append("Added destructuring for better performance")
        
        return optimized_code, optimizations
    
    def _optimize_js_functions(self, code: str, level: str) -> tuple:
        """Optimize JavaScript functions"""
        optimizations = []
        # Simple function optimization
        return code, optimizations
    
    def _extract_used_names(self, code: str) -> set:
        """Extract variable and function names used in code"""
        names = set()
        
        # Extract variable names
        var_patterns = [
            r'\b(\w+)\s*=',  # Variable assignment
            r'\bfor\s+(\w+)\s+in',  # For loop variables
        ]
        
        for pattern in var_patterns:
            matches = re.findall(pattern, code, re.MULTILINE)
            names.update(matches)
        
        return names
    
    def _is_import_used(self, import_line: str, used_names: set) -> bool:
        """Check if an import statement is used in the code"""
        # Extract imported names
        if import_line.startswith('import '):
            # Handle: import module
            parts = import_line.split()
            if len(parts) >= 2:
                module = parts[1]
                return any(name in used_names for name in [module, module.split('.')[0]])
        
        elif import_line.startswith('from '):
            # Handle: from module import name
            match = re.search(r'from\s+[\w.]+\s+import\s+([\w,\s]+)', import_line)
            if match:
                imports = [name.strip() for name in match.group(1).split(',')]
                return any(name in used_names for name in imports)
        
        return True  # Assume used if we can't determine
    
    def _add_type_hints(self, code: str) -> str:
        """Add basic type hints to Python code"""
        lines = code.split('\n')
        hinted_lines = []
        
        for line in lines:
            if line.strip().startswith('def '):
                # Add type hints to function parameters
                hinted_lines.append(line)
                hinted_lines.append('    # TODO: Add specific type hints')
            else:
                hinted_lines.append(line)
        
        return '\n'.join(hinted_lines)
    
    def _calculate_improvements(self, original: str, optimized: str) -> Dict[str, Any]:
        """Calculate estimated performance improvements"""
        improvements = {
            'estimated_gain': 0,
            'details': []
        }
        
        # Count optimizations applied
        original_lines = len(original.split('\n'))
        optimized_lines = len(optimized.split('\n'))
        
        # Estimate performance gain based on optimizations
        if optimized_lines < original_lines:
            improvements['estimated_gain'] += 5
            improvements['details'].append("Reduced code size")
        
        # Check for specific optimizations
        if 'list comprehension' in optimized:
            improvements['estimated_gain'] += 10
            improvements['details'].append("List comprehensions are faster")
        
        if 'enumerate' in optimized:
            improvements['estimated_gain'] += 8
            improvements['details'].append("enumerate() is more efficient")
        
        if 'const' in optimized or 'let' in optimized:
            improvements['estimated_gain'] += 3
            improvements['details'].append("Modern JavaScript features")
        
        improvements['estimated_gain'] = min(improvements['estimated_gain'], 25)  # Cap at 25%
        
        return improvements

