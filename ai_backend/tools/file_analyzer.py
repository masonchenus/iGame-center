"""
AI File Analyzer Tool
Comprehensive file analysis and code review
"""

import os
import ast
import re
import hashlib
from pathlib import Path
from typing import Dict, List, Any, Optional

class FileAnalyzer:
    """AI-powered file analysis and code review tool"""
    
    def __init__(self):
        self.analysis_results = {}
        
    def execute(self, file_path: str, analysis_type: str = "full", **kwargs) -> Dict[str, Any]:
        """
        Analyze a file with various analysis types
        
        Args:
            file_path: Path to the file to analyze
            analysis_type: Type of analysis (security, performance, structure, quality, full)
            **kwargs: Additional analysis parameters
        """
        try:
            if not os.path.exists(file_path):
                return {"status": "error", "error": f"File not found: {file_path}"}
            
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            file_info = self._get_file_info(file_path)
            
            if analysis_type == "security":
                result = self._analyze_security(content, file_path)
            elif analysis_type == "performance":
                result = self._analyze_performance(content, file_path)
            elif analysis_type == "structure":
                result = self._analyze_structure(content, file_path)
            elif analysis_type == "quality":
                result = self._analyze_quality(content, file_path)
            elif analysis_type == "dependencies":
                result = self._analyze_dependencies(content, file_path)
            else:
                result = self._full_analysis(content, file_path)
            
            return {
                "status": "success",
                "file_path": file_path,
                "file_info": file_info,
                "analysis_type": analysis_type,
                "results": result
            }
            
        except Exception as e:
            return {"status": "error", "error": str(e), "file_path": file_path}
    
    def _get_file_info(self, file_path: str) -> Dict[str, Any]:
        """Get basic file information"""
        stat = os.stat(file_path)
        with open(file_path, 'rb') as f:
            content_hash = hashlib.md5(f.read()).hexdigest()
        
        return {
            "size": stat.st_size,
            "modified": stat.st_mtime,
            "created": stat.st_ctime,
            "hash": content_hash,
            "extension": Path(file_path).suffix,
            "lines": len(open(file_path, 'r', encoding='utf-8').readlines())
        }
    
    def _full_analysis(self, content: str, file_path: str) -> Dict[str, Any]:
        """Perform comprehensive file analysis"""
        return {
            "security": self._analyze_security(content, file_path),
            "performance": self._analyze_performance(content, file_path),
            "structure": self._analyze_structure(content, file_path),
            "quality": self._analyze_quality(content, file_path),
            "dependencies": self._analyze_dependencies(content, file_path)
        }
    
    def _analyze_security(self, content: str, file_path: str) -> Dict[str, Any]:
        """Analyze security issues in the file"""
        issues = []
        suggestions = []
        
        # Common security patterns
        security_patterns = {
            r'password\s*=\s*["\'][^"\']*["\']': 'Hardcoded password detected',
            r'api_key\s*=\s*["\'][^"\']*["\']': 'Hardcoded API key detected',
            r'secret\s*=\s*["\'][^"\']*["\']': 'Hardcoded secret detected',
            r'eval\s*\(': 'Use of eval() function - potential security risk',
            r'exec\s*\(': 'Use of exec() function - potential security risk',
            r'os\.system\s*\(': 'Use of os.system() - potential command injection',
            r'subprocess\.call\s*\([^)]*shell\s*=\s*True': 'Shell=True in subprocess - potential security risk',
            r'input\s*\([^)]*\)': 'Unvalidated user input detected',
            r'http://': 'HTTP URL detected - use HTTPS for security',
            r'SQL\s*\(\s*["\'][^"\']*["\']': 'Potential SQL injection vulnerability'
        }
        
        for pattern, description in security_patterns.items():
            matches = re.finditer(pattern, content, re.IGNORECASE)
            for match in matches:
                issues.append({
                    "type": "security",
                    "severity": "high" if "password" in description or "api_key" in description else "medium",
                    "line": content[:match.start()].count('\n') + 1,
                    "description": description,
                    "code": match.group()
                })
        
        suggestions = [
            "Use environment variables for sensitive data",
            "Implement input validation",
            "Use parameterized queries",
            "Enable HTTPS",
            "Review and sanitize all user inputs"
        ]
        
        return {
            "issues": issues,
            "score": max(0, 100 - len(issues) * 10),
            "suggestions": suggestions
        }
    
    def _analyze_performance(self, content: str, file_path: str) -> Dict[str, Any]:
        """Analyze performance issues"""
        issues = []
        suggestions = []
        
        # Performance patterns
        perf_patterns = {
            r'for\s+\w+\s+in\s+range\s*\(\s*len\s*\(\s*\w+\s*\)\s*\)': 'Inefficient loop - use enumerate()',
            r'\w+\.append\s*\(\s*\w+\s*\)': 'Consider list comprehension for better performance',
            r'while\s+True:': 'Potential infinite loop - add break condition',
            r'sorted\s*\(\s*\w+\s*\)': 'Repeated sorting detected - cache results',
            r'\.join\s*\(\s*\[\s*\]': 'Inefficient string concatenation'
        }
        
        for pattern, description in perf_patterns.items():
            matches = re.finditer(pattern, content, re.IGNORECASE)
            for match in matches:
                issues.append({
                    "type": "performance",
                    "severity": "medium",
                    "line": content[:match.start()].count('\n') + 1,
                    "description": description,
                    "code": match.group()
                })
        
        suggestions = [
            "Use list comprehensions where appropriate",
            "Cache expensive computations",
            "Optimize database queries",
            "Use appropriate data structures",
            "Implement lazy loading for large datasets"
        ]
        
        return {
            "issues": issues,
            "score": max(0, 100 - len(issues) * 5),
            "suggestions": suggestions
        }
    
    def _analyze_structure(self, content: str, file_path: str) -> Dict[str, Any]:
        """Analyze code structure and architecture"""
        structure_info = {}
        
        # Python-specific analysis
        if file_path.endswith('.py'):
            try:
                tree = ast.parse(content)
                
                functions = []
                classes = []
                imports = []
                
                for node in ast.walk(tree):
                    if isinstance(node, ast.FunctionDef):
                        functions.append({
                            "name": node.name,
                            "line": node.lineno,
                            "args": [arg.arg for arg in node.args.args]
                        })
                    elif isinstance(node, ast.ClassDef):
                        classes.append({
                            "name": node.name,
                            "line": node.lineno,
                            "methods": [n.name for n in node.body if isinstance(n, ast.FunctionDef)]
                        })
                    elif isinstance(node, (ast.Import, ast.ImportFrom)):
                        if isinstance(node, ast.Import):
                            imports.extend([alias.name for alias in node.names])
                        else:
                            imports.append(node.module or '')
                
                structure_info = {
                    "functions": functions,
                    "classes": classes,
                    "imports": imports,
                    "complexity": self._calculate_complexity(tree)
                }
                
            except SyntaxError:
                structure_info = {"error": "Syntax error in Python file"}
        
        suggestions = [
            "Follow single responsibility principle",
            "Keep functions small and focused",
            "Use meaningful names",
            "Group related functionality",
            "Maintain consistent code style"
        ]
        
        return {
            "structure": structure_info,
            "score": 85,  # Default score
            "suggestions": suggestions
        }
    
    def _analyze_quality(self, content: str, file_path: str) -> Dict[str, Any]:
        """Analyze code quality and best practices"""
        issues = []
        suggestions = []
        
        # Quality patterns
        quality_issues = [
            (r'# TODO:', 'TODO comments found'),
            (r'# FIXME:', 'FIXME comments found'),
            (r'def \w+\([^)]*\):\s*$', 'Missing docstring'),
            (r'\b[a-z]\b', 'Single character variable names'),
            (r'import \w+\nimport \w+', 'Multiple import statements'),
            (r'print\s*\(', 'Debug print statements found'),
        ]
        
        for pattern, description in quality_issues:
            matches = re.finditer(pattern, content, re.MULTILINE)
            for match in matches:
                issues.append({
                    "type": "quality",
                    "severity": "low",
                    "line": content[:match.start()].count('\n') + 1,
                    "description": description,
                    "code": match.group()
                })
        
        suggestions = [
            "Add comprehensive docstrings",
            "Use meaningful variable names",
            "Remove debug statements",
            "Follow PEP 8 style guidelines",
            "Write unit tests",
            "Use type hints"
        ]
        
        return {
            "issues": issues,
            "score": max(0, 100 - len(issues) * 3),
            "suggestions": suggestions
        }
    
    def _analyze_dependencies(self, content: str, file_path: str) -> Dict[str, Any]:
        """Analyze dependencies and imports"""
        dependencies = []
        outdated = []
        vulnerabilities = []
        
        if file_path.endswith('.py'):
            # Extract imports
            imports = re.findall(r'^import\s+(\w+)', content, re.MULTILINE)
            from_imports = re.findall(r'^from\s+(\w+)', content, re.MULTILINE)
            
            all_imports = set(imports + from_imports)
            
            # Check for known vulnerable packages (simplified)
            vulnerable_packages = {
                'requests': ['< 2.20.0'],
                'urllib3': ['< 1.24.2'],
                'flask': ['< 1.0'],
                'django': ['< 2.0']
            }
            
            for imp in all_imports:
                if imp in vulnerable_packages:
                    vulnerabilities.append({
                        "package": imp,
                        "issue": f"Potential vulnerability in {imp}",
                        "recommendation": f"Update {imp} to latest version"
                    })
        
        suggestions = [
            "Keep dependencies updated",
            "Use dependency scanning tools",
            "Pin dependency versions",
            "Regular security audits",
            "Remove unused dependencies"
        ]
        
        return {
            "dependencies": list(dependencies),
            "outdated": outdated,
            "vulnerabilities": vulnerabilities,
            "score": max(0, 100 - len(vulnerabilities) * 20),
            "suggestions": suggestions
        }
    
    def _calculate_complexity(self, tree: ast.AST) -> int:
        """Calculate cyclomatic complexity"""
        complexity = 1  # Base complexity
        
        for node in ast.walk(tree):
            if isinstance(node, (ast.If, ast.While, ast.For, ast.With, ast.Try, ast.ExceptHandler)):
                complexity += 1
            elif isinstance(node, ast.BoolOp):
                complexity += len(node.values) - 1
        
        return complexity
    
    def batch_analyze(self, directory: str, file_patterns: List[str] = None) -> Dict[str, Any]:
        """Analyze multiple files in a directory"""
        if file_patterns is None:
            file_patterns = ['*.py', '*.js', '*.ts', '*.java', '*.go', '*.rs']
        
        results = {}
        
        for pattern in file_patterns:
            for file_path in Path(directory).glob(pattern):
                if file_path.is_file():
                    result = self.execute(str(file_path))
                    results[str(file_path)] = result
        
        return {
            "status": "success",
            "total_files": len(results),
            "results": results
        }

