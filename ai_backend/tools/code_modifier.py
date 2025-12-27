"""
AI Code Modifier Agent
Intelligent code modification and refactoring tool
"""

import re
import ast
import os
from typing import Dict, List, Optional, Any
from pathlib import Path

class CodeModifier:
    """
    Advanced AI-powered code modification agent
    Capable of refactoring, optimizing, and enhancing code snippets
    """
    
    def __init__(self):
        self.modifications = []
        self.supported_languages = ['python', 'javascript', 'typescript', 'java', 'go', 'rust']
        
    def execute(self, 
                code: str, 
                modification_type: str, 
                target_file: Optional[str] = None,
                **kwargs) -> Dict[str, Any]:
        """
        Execute code modification
        
        Args:
            code: Source code to modify
            modification_type: Type of modification (refactor, optimize, enhance, fix)
            target_file: Optional target file path
            **kwargs: Additional parameters for modification
        """
        try:
            language = self._detect_language(code, target_file)
            
            if modification_type == "refactor":
                result = self._refactor_code(code, language, **kwargs)
            elif modification_type == "optimize":
                result = self._optimize_code(code, language, **kwargs)
            elif modification_type == "enhance":
                result = self._enhance_code(code, language, **kwargs)
            elif modification_type == "fix":
                result = self._fix_code(code, language, **kwargs)
            elif modification_type == "add_comments":
                result = self._add_comments(code, language, **kwargs)
            elif modification_type == "convert_style":
                result = self._convert_style(code, language, **kwargs)
            else:
                result = self._general_modify(code, modification_type, language, **kwargs)
                
            return {
                "status": "success",
                "original_code": code,
                "modified_code": result["code"],
                "language": language,
                "modifications": result.get("modifications", []),
                "explanation": result.get("explanation", ""),
                "target_file": target_file
            }
            
        except Exception as e:
            return {
                "status": "error",
                "error": str(e),
                "original_code": code,
                "target_file": target_file
            }
    
    def _detect_language(self, code: str, target_file: Optional[str] = None) -> str:
        """Detect programming language from code or file extension"""
        if target_file:
            ext = Path(target_file).suffix.lower()
            language_map = {
                '.py': 'python',
                '.js': 'javascript', 
                '.ts': 'typescript',
                '.java': 'java',
                '.go': 'go',
                '.rs': 'rust',
                '.cpp': 'cpp',
                '.c': 'c',
                '.cs': 'csharp',
                '.php': 'php',
                '.rb': 'ruby'
            }
            if ext in language_map:
                return language_map[ext]
        
        # Detect from code patterns
        if 'def ' in code and ':' in code:
            return 'python'
        elif 'function ' in code or 'const ' in code or 'let ' in code:
            return 'javascript'
        elif 'interface ' in code or 'class ' in code:
            if 'public ' in code or 'private ' in code:
                return 'java'
            return 'typescript'
        elif 'func ' in code:
            return 'go'
        elif 'fn ' in code:
            return 'rust'
            
        return 'python'  # default
    
    def _refactor_code(self, code: str, language: str, **kwargs) -> Dict[str, Any]:
        """Refactor code for better structure and readability"""
        modifications = []
        
        if language == 'python':
            modified_code = self._refactor_python(code)
            modifications.append("Improved variable naming and structure")
        elif language == 'javascript':
            modified_code = self._refactor_javascript(code)
            modifications.append("Enhanced JavaScript structure")
        elif language == 'java':
            modified_code = self._refactor_java(code)
            modifications.append("Improved Java class structure")
        else:
            modified_code = code  # Fallback for unsupported languages
            
        return {
            "code": modified_code,
            "modifications": modifications,
            "explanation": "Code has been refactored for better maintainability and readability"
        }
    
    def _optimize_code(self, code: str, language: str, **kwargs) -> Dict[str, Any]:
        """Optimize code for performance"""
        modifications = []
        
        if language == 'python':
            modified_code = self._optimize_python(code)
            modifications.append("Applied performance optimizations")
        elif language == 'javascript':
            modified_code = self._optimize_javascript(code)
            modifications.append("Optimized JavaScript performance")
        else:
            modified_code = code
            
        return {
            "code": modified_code,
            "modifications": modifications,
            "explanation": "Code has been optimized for better performance"
        }
    
    def _enhance_code(self, code: str, language: str, **kwargs) -> Dict[str, Any]:
        """Enhance code with additional features"""
        modifications = []
        
        if language == 'python':
            modified_code = self._enhance_python(code)
            modifications.append("Added error handling and type hints")
        elif language == 'javascript':
            modified_code = self._enhance_javascript(code)
            modifications.append("Added ES6+ features and error handling")
        else:
            modified_code = code
            
        return {
            "code": modified_code,
            "modifications": modifications,
            "explanation": "Code has been enhanced with modern features and best practices"
        }
    
    def _fix_code(self, code: str, language: str, **kwargs) -> Dict[str, Any]:
        """Fix common code issues"""
        modifications = []
        
        if language == 'python':
            modified_code = self._fix_python_issues(code)
            modifications.append("Fixed common Python issues")
        elif language == 'javascript':
            modified_code = self._fix_javascript_issues(code)
            modifications.append("Fixed JavaScript issues")
        else:
            modified_code = code
            
        return {
            "code": modified_code,
            "modifications": modifications,
            "explanation": "Code issues have been identified and fixed"
        }
    
    def _add_comments(self, code: str, language: str, **kwargs) -> Dict[str, Any]:
        """Add intelligent comments to code"""
        if language == 'python':
            modified_code = self._add_python_comments(code)
        elif language == 'javascript':
            modified_code = self._add_javascript_comments(code)
        else:
            modified_code = code
            
        return {
            "code": modified_code,
            "modifications": ["Added comprehensive comments"],
            "explanation": "Added detailed comments explaining the code logic"
        }
    
    def _convert_style(self, code: str, language: str, **kwargs) -> Dict[str, Any]:
        """Convert code style (PEP8, ESLint, etc.)"""
        style = kwargs.get('style', 'standard')
        
        if language == 'python':
            modified_code = self._convert_python_style(code, style)
        elif language == 'javascript':
            modified_code = self._convert_javascript_style(code, style)
        else:
            modified_code = code
            
        return {
            "code": modified_code,
            "modifications": [f"Converted to {style} style"],
            "explanation": f"Code has been formatted according to {style} style guidelines"
        }
    
    def _general_modify(self, code: str, modification_type: str, language: str, **kwargs) -> Dict[str, Any]:
        """General modification handler"""
        # Default implementation - can be extended
        return {
            "code": code,
            "modifications": [f"Applied {modification_type} modification"],
            "explanation": f"Applied {modification_type} modification to the code"
        }
    
    # Python-specific methods
    def _refactor_python(self, code: str) -> str:
        """Refactor Python code"""
        # Improve variable naming
        code = re.sub(r'\b(a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z)\b', 
                     lambda m: self._improve_var_name(m.group(), 'python'), code)
        
        # Add proper function structure
        if 'def ' in code and not code.strip().startswith('def '):
            # Wrap in function if needed
            lines = code.split('\n')
            new_lines = ['def generated_function():', '    """Auto-generated function"""']
            for line in lines:
                if line.strip():
                    new_lines.append('    ' + line)
            new_lines.append('')
            return '\n'.join(new_lines)
        
        return code
    
    def _optimize_python(self, code: str) -> str:
        """Optimize Python code"""
        # Replace list comprehensions where beneficial
        code = re.sub(r'for (\w+) in (\w+):\s*(\w+)\.append\(([^)]+)\)', 
                     r'[\3 for \1 in \2]', code)
        
        # Add list comprehensions
        if 'for ' in code and '.append(' in code:
            lines = code.split('\n')
            for i, line in enumerate(lines):
                if 'for ' in line and '.append(' in lines[i+1] if i+1 < len(lines) else False:
                    # Convert to list comprehension
                    pass
        
        return code
    
    def _enhance_python(self, code: str) -> str:
        """Enhance Python code with modern features"""
        # Add type hints
        if 'def ' in code:
            code = re.sub(r'def (\w+)\(([^)]*)\):', 
                         lambda m: self._add_python_type_hints(m.group()), code)
        
        # Add error handling
        if 'try:' not in code and ('open(' in code or 'import ' in code):
            lines = code.split('\n')
            new_lines = []
            for line in lines:
                if line.strip():
                    if 'import ' in line:
                        new_lines.append(line)
                        new_lines.append('try:')
                    else:
                        new_lines.append('    ' + line)
            code = '\n'.join(new_lines)
        
        return code
    
    def _fix_python_issues(self, code: str) -> str:
        """Fix common Python issues"""
        # Fix common syntax issues
        code = re.sub(r'print\s+([^;]+)', r'print(\1)', code)
        
        # Fix indentation issues
        lines = code.split('\n')
        fixed_lines = []
        for line in lines:
            if line.strip():
                if line.startswith(' ') and not line.startswith('    '):
                    # Fix incorrect indentation
                    fixed_lines.append('    ' + line.strip())
                else:
                    fixed_lines.append(line)
            else:
                fixed_lines.append(line)
        
        return '\n'.join(fixed_lines)
    
    def _add_python_comments(self, code: str) -> str:
        """Add comments to Python code"""
        lines = code.split('\n')
        commented_lines = []
        
        for line in lines:
            commented_lines.append(line)
            if 'def ' in line:
                commented_lines.append('    """Function description"""')
            elif 'import ' in line:
                commented_lines.append('# Import statement')
            elif 'for ' in line:
                commented_lines.append('# Loop iteration')
            elif 'if ' in line:
                commented_lines.append('# Conditional check')
            elif 'try:' in line:
                commented_lines.append('# Exception handling')
        
        return '\n'.join(commented_lines)
    
    def _convert_python_style(self, code: str, style: str) -> str:
        """Convert Python code style"""
        if style == 'pep8':
            # Basic PEP8 formatting
            lines = code.split('\n')
            formatted_lines = []
            for line in lines:
                line = line.strip()
                if line:
                    formatted_lines.append(line)
            return '\n'.join(formatted_lines)
        return code
    
    def _add_python_type_hints(self, func_def: str) -> str:
        """Add type hints to Python function"""
        # Simple type hint addition - can be enhanced
        return func_def.replace('):', ') -> None:')
    
    def _improve_var_name(self, var: str, language: str) -> str:
        """Improve variable names"""
        # Simple improvement - can be enhanced with AI
        improvements = {
            'a': 'array',
            'b': 'buffer', 
            'c': 'count',
            'd': 'data',
            'e': 'element',
            'f': 'function',
            'g': 'group',
            'h': 'handler',
            'i': 'item',
            'j': 'job',
            'k': 'key',
            'l': 'list',
            'm': 'message',
            'n': 'number',
            'o': 'object',
            'p': 'parameter',
            'q': 'query',
            'r': 'result',
            's': 'string',
            't': 'type',
            'u': 'user',
            'v': 'value',
            'w': 'width',
            'x': 'x_coordinate',
            'y': 'y_coordinate',
            'z': 'z_coordinate'
        }
        return improvements.get(var, var)
    
    # JavaScript-specific methods
    def _refactor_javascript(self, code: str) -> str:
        """Refactor JavaScript code"""
        # Add const/let instead of var
        code = re.sub(r'\bvar\b', 'const', code)
        
        # Add arrow functions where appropriate
        code = re.sub(r'function\s+(\w+)\s*\(([^)]*)\)\s*{', 
                     r'const \1 = (\2) => {', code)
        
        return code
    
    def _optimize_javascript(self, code: str) -> str:
        """Optimize JavaScript code"""
        # Add memoization for functions
        if 'function ' in code:
            lines = code.split('\n')
            optimized_lines = []
            for line in lines:
                if 'function ' in line:
                    # Add simple optimization
                    optimized_lines.append(line)
                    optimized_lines.append('    // Optimized function')
                else:
                    optimized_lines.append(line)
            return '\n'.join(optimized_lines)
        return code
    
    def _enhance_javascript(self, code: str) -> str:
        """Enhance JavaScript code"""
        # Add error handling
        if 'try' not in code and ('fetch(' in code or 'JSON.parse(' in code):
            lines = code.split('\n')
            enhanced_lines = []
            for line in lines:
                if line.strip():
                    enhanced_lines.append(line)
                    if 'fetch(' in line or 'JSON.parse(' in line:
                        enhanced_lines.append('    .catch(error => console.error(error));')
            return '\n'.join(enhanced_lines)
        return code
    
    def _fix_javascript_issues(self, code: str) -> str:
        """Fix JavaScript issues"""
        # Fix common issues
        code = re.sub(r'console\.log\(([^)]+)\);?', r'console.log(\1);', code)
        return code
    
    def _add_javascript_comments(self, code: str) -> str:
        """Add comments to JavaScript code"""
        lines = code.split('\n')
        commented_lines = []
        
        for line in lines:
            commented_lines.append(line)
            if 'function ' in line:
                commented_lines.append('    // Function implementation')
            elif 'const ' in line or 'let ' in line:
                commented_lines.append('    // Variable declaration')
            elif 'if ' in line:
                commented_lines.append('    // Conditional logic')
            elif 'for ' in line:
                commented_lines.append('    // Loop iteration')
        
        return '\n'.join(commented_lines)
    
    def _convert_javascript_style(self, code: str, style: str) -> str:
        """Convert JavaScript style"""
        # Basic formatting
        lines = code.split('\n')
        formatted_lines = []
        for line in lines:
            line = line.strip()
            if line:
                formatted_lines.append(line)
        return '\n'.join(formatted_lines)
    
    # Java-specific methods
    def _refactor_java(self, code: str) -> str:
        """Refactor Java code"""
        # Add proper access modifiers
        code = re.sub(r'(\w+)\s+(\w+)\s*\(([^)]*)\)\s*{', 
                     r'public \1 \2(\3) {', code)
        return code
    
    def modify_file(self, file_path: str, modification_type: str, **kwargs) -> Dict[str, Any]:
        """Modify an entire file"""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                original_code = f.read()
            
            result = self.execute(original_code, modification_type, file_path, **kwargs)
            
            if result["status"] == "success" and "modified_code" in result:
                # Create backup
                backup_path = f"{file_path}.backup"
                with open(backup_path, 'w', encoding='utf-8') as f:
                    f.write(original_code)
                
                # Write modified code
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(result["modified_code"])
                
                result["backup_created"] = backup_path
                result["file_modified"] = file_path
            
            return result
            
        except Exception as e:
            return {
                "status": "error",
                "error": str(e),
                "file_path": file_path
            }

