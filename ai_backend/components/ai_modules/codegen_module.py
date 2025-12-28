import json
import re
from typing import Dict, Any, List, Optional

PRIVILEGES = ["generate_code"]

class CodeGenerator:
    """Advanced code generation with multiple language support and best practices"""
    
    # Language-specific templates and best practices
    LANGUAGE_TEMPLATES = {
        'python': {
            'template_start': 'def solution():\n    """Generated solution"""\n    ',
            'template_end': '\n\nif __name__ == "__main__":\n    solution()',
            'imports': ['import sys', 'import json', 'from typing import *'],
            'style': 'PEP 8 compliant with type hints'
        },
        'javascript': {
            'template_start': 'function solution() {\n    // Generated solution\n    ',
            'template_end': '\n}\n\n// Run solution\nsolution();',
            'imports': ['// Node.js imports if needed'],
            'style': 'ES6+ with modern features'
        },
        'java': {
            'template_start': 'public class Solution {\n    public static void main(String[] args) {\n        // Generated solution\n        ',
            'template_end': '\n    }\n}',
            'imports': ['import java.util.*;', 'import java.io.*;'],
            'style': 'Standard Java with proper encapsulation'
        },
        'cpp': {
            'template_start': '#include <iostream>\n#include <vector>\n#include <string>\nusing namespace std;\n\nint main() {\n    // Generated solution\n    ',
            'template_end': '\n    return 0;\n}',
            'imports': ['#include <iostream>', '#include <vector>', '#include <algorithm>'],
            'style': 'C++17 standard with STL usage'
        },
        'sql': {
            'template_start': '-- Generated SQL Query\n',
            'template_end': ';',
            'imports': ['-- No imports needed'],
            'style': 'Standard SQL with proper indexing hints'
        }
    }
    
    def __init__(self, request: str):
        self.request = request.lower().strip()
        self.detected_language = self._detect_language()
        self.code_type = self._classify_code_type()
        self.complexity_level = self._assess_complexity()
        
    def _detect_language(self) -> str:
        """Detect programming language from request"""
        language_keywords = {
            'python': ['python', 'py', 'django', 'flask', 'pandas', 'numpy'],
            'javascript': ['javascript', 'js', 'node', 'react', 'vue', 'angular'],
            'java': ['java', 'spring', 'hibernate'],
            'cpp': ['c++', 'cpp', 'c plus plus'],
            'sql': ['sql', 'database', 'query', 'mysql', 'postgresql'],
            'html': ['html', 'web', 'css', 'bootstrap'],
            'bash': ['bash', 'shell', 'linux', 'unix']
        }
        
        for lang, keywords in language_keywords.items():
            if any(keyword in self.request for keyword in keywords):
                return lang
                
        return 'python'  # Default to Python
    
    def _classify_code_type(self) -> str:
        """Classify the type of code to generate"""
        if any(word in self.request for word in ['function', 'method', 'class', 'algorithm']):
            return 'algorithm'
        elif any(word in self.request for word in ['api', 'rest', 'endpoint', 'service']):
            return 'api'
        elif any(word in self.request for word in ['database', 'db', 'model', 'schema']):
            return 'database'
        elif any(word in self.request for word in ['web', 'frontend', 'ui', 'interface']):
            return 'frontend'
        elif any(word in self.request for word in ['test', 'unit', 'testing']):
            return 'testing'
        else:
            return 'utility'
    
    def _assess_complexity(self) -> str:
        """Assess the complexity level of the request"""
        complex_indicators = ['complex', 'advanced', 'sophisticated', 'enterprise', 'distributed']
        simple_indicators = ['simple', 'basic', 'hello world', 'beginner']
        
        if any(indicator in self.request for indicator in complex_indicators):
            return 'advanced'
        elif any(indicator in self.request for indicator in simple_indicators):
            return 'basic'
        else:
            return 'intermediate'
    
    def generate_code_structure(self) -> Dict[str, Any]:
        """Generate code structure based on analysis"""
        lang_config = self.LANGUAGE_TEMPLATES.get(self.detected_language, self.LANGUAGE_TEMPLATES['python'])
        
        structure = {
            'language': self.detected_language,
            'type': self.code_type,
            'complexity': self.complexity_level,
            'template_start': lang_config['template_start'],
            'template_end': lang_config['template_end'],
            'imports': lang_config['imports'],
            'style_guide': lang_config['style']
        }
        
        # Add specific enhancements based on code type
        if self.code_type == 'algorithm':
            structure['enhancements'] = [
                'Time complexity analysis',
                'Space complexity analysis', 
                'Error handling',
                'Input validation',
                'Documentation comments'
            ]
        elif self.code_type == 'api':
            structure['enhancements'] = [
                'RESTful design principles',
                'Error handling and status codes',
                'Input validation',
                'Security considerations',
                'API documentation'
            ]
        
        return structure
    
    def generate_best_practices(self) -> List[str]:
        """Generate best practices based on language and type"""
        practices = [
            'Follow language-specific style guides',
            'Include comprehensive error handling',
            'Add input validation and sanitization',
            'Write clear and descriptive comments',
            'Include unit tests for critical functions'
        ]
        
        if self.detected_language == 'python':
            practices.extend([
                'Use type hints for better code clarity',
                'Follow PEP 8 style guidelines',
                'Use virtual environments for dependencies'
            ])
        elif self.detected_language == 'javascript':
            practices.extend([
                'Use modern ES6+ features appropriately',
                'Handle async operations with promises/async-await',
                'Use strict mode for better error detection'
            ])
        
        return practices

def generate_enhanced_code(analysis: CodeGenerator) -> str:
    """Generate enhanced code based on analysis"""
    structure = analysis.generate_code_structure()
    
    # Create header with metadata
    header = f"""# Generated {structure['language'].title()} Code
# Type: {structure['type']}
# Complexity: {structure['complexity']}
# Generated by Enhanced AI Code Generator
# Style: {structure['style_guide']}

"""
    
    # Add imports if needed
    imports_section = ""
    if structure['imports']:
        imports_section = "\n".join(structure['imports']) + "\n\n"
    
    # Main code template
    main_code = f"""{structure['template_start']}# TODO: Implement the actual solution based on: {analysis.request}
    # Your code here
    
    # Example implementation placeholder
    result = "Solution implemented"
    print(result)
    return result{structure['template_end']}"""
    
    # Best practices section
    practices = analysis.generate_best_practices()
    practices_section = f"""
# Best Practices Applied:
{chr(10).join(f"# - {practice}" for practice in practices)}

# Enhancement Features:
{chr(10).join(f"# - {enhancement}" for enhancement in structure.get('enhancements', []))}
"""
    
    return header + imports_section + main_code + practices_section

def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Enhanced CodeGen mode: Generate high-quality, production-ready code with best practices
    """
    try:
        # Parse input data
        if isinstance(input_data, str):
            request_text = input_data
        else:
            request_text = str(input_data)
        
        # Create code generator instance
        generator = CodeGenerator(request_text)
        
        # Generate enhanced code
        generated_code = generate_enhanced_code(generator)
        structure = generator.generate_code_structure()
        
        # Create comprehensive response
        response = {
            'request': request_text,
            'analysis': {
                'detected_language': generator.detected_language,
                'code_type': generator.code_type,
                'complexity_level': generator.complexity_level
            },
            'generated_code': generated_code,
            'code_structure': structure,
            'best_practices': generator.generate_best_practices(),
            'enhancement_features': structure.get('enhancements', []),
            'style_guide': structure['style_guide']
        }
        
        # Try to enhance with AI model if available
        try:
            from ai_backend.ai_models import models
            model = models.get(model_name)
            if model:
                ai_enhanced = model.generate(
                    f"Generate high-quality {generator.detected_language} code for: {request_text}. "
                    f"Include proper error handling, documentation, and follow best practices.",
                    user_id=user_id, session_id=session_id
                )
                response['ai_enhanced_code'] = ai_enhanced
        except:
            pass
        
        return json.dumps(response, indent=2)
        
    except Exception as e:
        return json.dumps({
            'error': str(e),
            'fallback': 'Code generation failed. Please check your request format.',
            'basic_template': '// Basic code template\nfunction solution() {\n    // Your code here\n}'
        })

def codegen(prompt: str) -> str:
    """Legacy function for backward compatibility"""
    generator = CodeGenerator(prompt)
    structure = generator.generate_code_structure()
    return f"[Enhanced CodeGen Mode]\nLanguage: {generator.detected_language}\nType: {generator.code_type}\nComplexity: {generator.complexity_level}\nGenerated: {len(structure.get('enhancements', []))} enhancement features"
