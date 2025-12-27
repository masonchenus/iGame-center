"""
AI Code Generator Tool
Advanced code generation and scaffolding
"""

import os
import json
from typing import Dict, List, Any, Optional

class CodeGenerator:
    """AI-powered code generation tool"""
    
    def __init__(self):
        self.templates = {
            'python': {
                'web_app': self._generate_python_web_app,
                'api': self._generate_python_api,
                'class': self._generate_python_class,
                'function': self._generate_python_function,
                'test': self._generate_python_test,
                'script': self._generate_python_script
            },
            'javascript': {
                'react_component': self._generate_react_component,
                'api': self._generate_js_api,
                'class': self._generate_js_class,
                'function': self._generate_js_function,
                'module': self._generate_js_module
            },
            'typescript': {
                'interface': self._generate_ts_interface,
                'class': self._generate_ts_class,
                'api': self._generate_ts_api
            },
            'java': {
                'class': self._generate_java_class,
                'interface': self._generate_java_interface,
                'service': self._generate_java_service
            }
        }
    
    def execute(self, 
                generation_type: str, 
                language: str = 'python',
                project_name: str = 'generated_project',
                requirements: Dict[str, Any] = None,
                **kwargs) -> Dict[str, Any]:
        """
        Generate code based on specifications
        
        Args:
            generation_type: Type of code to generate
            language: Programming language
            project_name: Name of the project
            requirements: Additional requirements and specifications
            **kwargs: Generation parameters
        """
        try:
            if requirements is None:
                requirements = {}
            
            if language not in self.templates or generation_type not in self.templates[language]:
                return {
                    "status": "error",
                    "error": f"Unsupported generation type: {generation_type} for {language}"
                }
            
            generator_func = self.templates[language][generation_type]
            result = generator_func(project_name, requirements, **kwargs)
            
            return {
                "status": "success",
                "language": language,
                "generation_type": generation_type,
                "project_name": project_name,
                "files": result["files"],
                "structure": result.get("structure", {}),
                "instructions": result.get("instructions", [])
            }
            
        except Exception as e:
            return {
                "status": "error",
                "error": str(e),
                "generation_type": generation_type,
                "language": language
            }
    
    def _generate_python_web_app(self, project_name: str, requirements: Dict[str, Any]) -> Dict[str, Any]:
        """Generate Python web application"""
        files = {}
        
        # Main app file
        files[f"{project_name}/app.py"] = f'''from flask import Flask, render_template, request, jsonify
import os

app = Flask(__name__)

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/api/health')
def health():
    return jsonify({{"status": "ok", "message": "Application is running"}})

@app.route('/api/process', methods=['POST'])
def process_data():
    data = request.get_json()
    # Process data here
    result = {{
        "processed": True,
        "data": data,
        "result": "Success"
    }}
    return jsonify(result)

if __name__ == '__main__':
    app.run(debug=True, host='0.0.0.0', port=5000)
'''
        
        # Templates
        files[f"{project_name}/templates/index.html"] = '''<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Generated Web App</title>
</head>
<body>
    <h1>Generated Web Application</h1>
    <p>Welcome to your new web application!</p>
    
    <script>
        // Simple API call example
        fetch('/api/health')
            .then(response => response.json())
            .then(data => console.log(data));
    </script>
</body>
</html>'''
        
        # Requirements
        files[f"{project_name}/requirements.txt"] = '''Flask==2.3.3
Werkzeug==2.3.7
Jinja2==3.1.2
click==8.1.7
itsdangerous==2.1.2
MarkupSafe==2.1.3'''
        
        # README
        files[f"{project_name}/README.md"] = f'''# {project_name}

A generated Python web application using Flask.

## Setup

1. Install dependencies:
   ```bash
   pip install -r requirements.txt
   ```

2. Run the application:
   ```bash
   python app.py
   ```

3. Open your browser and navigate to http://localhost:5000

## Features

- Web interface
- API endpoints
- Basic health check
- Data processing endpoint
'''
        
        return {
            "files": files,
            "structure": {
                "main_file": f"{project_name}/app.py",
                "templates": f"{project_name}/templates/",
                "static": f"{project_name}/static/"
            },
            "instructions": [
                "Install dependencies: pip install -r requirements.txt",
                "Run the application: python app.py",
                "Access at http://localhost:5000"
            ]
        }
    
    def _generate_python_api(self, project_name: str, requirements: Dict[str, Any]) -> Dict[str, Any]:
        """Generate Python API"""
        endpoints = requirements.get('endpoints', ['users', 'items', 'auth'])
        
        files = {}
        
        # FastAPI main file
        api_content = f'''from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
from typing import List, Optional
import uvicorn

app = FastAPI(title="{project_name} API", version="1.0.0")

# Pydantic models
class Item(BaseModel):
    id: int
    name: str
    description: Optional[str] = None
    price: float
    category: str

class User(BaseModel):
    id: int
    username: str
    email: str
    full_name: Optional[str] = None

# In-memory storage (replace with database)
items_db = []
users_db = []

'''
        
        for endpoint in endpoints:
            if endpoint == 'users':
                api_content += '''@app.get("/api/users", response_model=List[User])
async def get_users():
    return users_db

@app.post("/api/users", response_model=User)
async def create_user(user: User):
    users_db.append(user)
    return user

@app.get("/api/users/{{user_id}}", response_model=User)
async def get_user(user_id: int):
    for user in users_db:
        if user.id == user_id:
            return user
    raise HTTPException(status_code=404, detail="User not found")

'''
            elif endpoint == 'items':
                api_content += '''@app.get("/api/items", response_model=List[Item])
async def get_items():
    return items_db

@app.post("/api/items", response_model=Item)
async def create_item(item: Item):
    items_db.append(item)
    return item

@app.get("/api/items/{{item_id}}", response_model=Item)
async def get_item(item_id: int):
    for item in items_db:
        if item.id == item_id:
            return item
    raise HTTPException(status_code=404, detail="Item not found")

'''
            elif endpoint == 'auth':
                api_content += '''@app.post("/api/auth/login")
async def login(username: str, password: str):
    # Implement authentication logic
    return {{"access_token": "fake_token", "token_type": "bearer"}}

'''
        
        api_content += '''
if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=8000)
'''
        
        files[f"{project_name}/main.py"] = api_content
        files[f"{project_name}/requirements.txt"] = "fastapi==0.104.1\nuvicorn[standard]==0.24.0\npydantic==2.5.0"
        
        return {
            "files": files,
            "instructions": [
                "Install dependencies: pip install -r requirements.txt",
                "Run the API: python main.py",
                "Access API docs at http://localhost:8000/docs"
            ]
        }
    
    def _generate_python_class(self, project_name: str, requirements: Dict[str, Any]) -> Dict[str, Any]:
        """Generate Python class"""
        class_name = requirements.get('class_name', 'MyClass')
        attributes = requirements.get('attributes', ['name', 'value'])
        methods = requirements.get('methods', ['get_name', 'set_name', 'calculate'])
        
        files = {}
        
        class_code = f'''class {class_name}:
    """Auto-generated class with basic functionality"""
    
    def __init__(self, {', '.join([f'{attr}="default"' for attr in attributes])}):
        """Initialize {class_name} instance"""
        {chr(10).join([f'self.{attr} = {attr}' for attr in attributes])}
    
    def get_name(self):
        """Get the name attribute"""
        return self.name
    
    def set_name(self, new_name):
        """Set the name attribute"""
        self.name = new_name
    
    def calculate(self, value):
        """Perform a calculation"""
        return value * 2
    
    def __str__(self):
        """String representation"""
        return f"{class_name}(name={{self.name}}, value={{self.value}})"
    
    def __repr__(self):
        """Official string representation"""
        return self.__str__()

# Example usage
if __name__ == "__main__":
    # Create instance
    obj = {class_name}(name="example", value=42)
    print(obj)
    
    # Use methods
    print(f"Name: {{obj.get_name()}}")
    result = obj.calculate(10)
    print(f"Calculation result: {{result}}")
'''
        
        files[f"{project_name}/{class_name.lower()}.py"] = class_code
        files[f"{project_name}/requirements.txt"] = "# No dependencies required for basic class"
        
        return {
            "files": files,
            "instructions": [
                "Import the class in your code",
                "Create instances with desired parameters",
                "Use the provided methods for functionality"
            ]
        }
    
    def _generate_python_function(self, project_name: str, requirements: Dict[str, Any]) -> Dict[str, Any]:
        """Generate Python function"""
        func_name = requirements.get('function_name', 'my_function')
        params = requirements.get('parameters', ['data'])
        return_type = requirements.get('return_type', 'str')
        
        files = {}
        
        func_code = f'''def {func_name}({', '.join(params)}):
    """
    Auto-generated function
    
    Args:
        {chr(10).join([f'{param}: Description for {param}' for param in params])}
    
    Returns:
        {return_type}: Description of return value
    """
    # Add your implementation here
    result = {{"processed": True, "data": data}}
    
    return result

# Example usage
if __name__ == "__main__":
    # Test the function
    result = {func_name}("test_data")
    print(f"Function result: {{result}}")
'''
        
        files[f"{project_name}/functions.py"] = func_code
        
        return {
            "files": files,
            "instructions": [
                "Import the function in your code",
                "Call with required parameters",
                "Handle the return value"
            ]
        }
    
    def _generate_python_test(self, project_name: str, requirements: Dict[str, Any]) -> Dict[str, Any]:
        """Generate Python test file"""
        files = {}
        
        test_code = '''import unittest
import sys
import os

# Add parent directory to path to import modules
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

class TestGeneratedCode(unittest.TestCase):
    """Test cases for generated code"""
    
    def setUp(self):
        """Set up test fixtures"""
        pass
    
    def tearDown(self):
        """Clean up after tests"""
        pass
    
    def test_basic_functionality(self):
        """Test basic functionality"""
        # Add your test cases here
        result = True
        self.assertTrue(result)
    
    def test_edge_cases(self):
        """Test edge cases"""
        # Add edge case tests here
        pass
    
    def test_error_handling(self):
        """Test error handling"""
        # Add error handling tests here
        pass

if __name__ == '__main__':
    unittest.main()
'''
        
        files[f"{project_name}/test_{project_name}.py"] = test_code
        files[f"{project_name}/requirements.txt"] = "# Add your project dependencies here"
        
        return {
            "files": files,
            "instructions": [
                "Run tests with: python -m unittest test_" + project_name,
                "Or use pytest: pytest test_" + project_name + ".py",
                "Add more test cases as needed"
            ]
        }
    
    def _generate_python_script(self, project_name: str, requirements: Dict[str, Any]) -> Dict[str, Any]:
        """Generate Python script"""
        files = {}
        
        script_code = f'''#!/usr/bin/env python3
"""
{project_name} - Auto-generated script
"""

import argparse
import logging
from datetime import datetime

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)

def main():
    """Main function"""
    parser = argparse.ArgumentParser(description='{project_name} script')
    parser.add_argument('--input', '-i', help='Input file path')
    parser.add_argument('--output', '-o', help='Output file path')
    parser.add_argument('--verbose', '-v', action='store_true', help='Verbose output')
    
    args = parser.parse_args()
    
    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)
    
    logging.info("Starting {project_name} script")
    
    # Add your script logic here
    print(f"Processing input: {{args.input}}")
    print(f"Output destination: {{args.output}}")
    
    # Main processing logic
    try:
        # Process data
        result = process_data(args.input)
        
        # Save result
        if args.output:
            save_result(result, args.output)
        
        logging.info("Script completed successfully")
        
    except Exception as e:
        logging.error(f"Error during processing: {{e}}")
        return 1
    
    return 0

def process_data(input_path):
    """Process input data"""
    # Add your processing logic here
    return {{"processed": True, "timestamp": datetime.now().isoformat()}}

def save_result(result, output_path):
    """Save processing result"""
    import json
    with open(output_path, 'w') as f:
        json.dump(result, f, indent=2)

if __name__ == "__main__":
    exit(main())
'''
        
        files[f"{project_name}.py"] = script_code
        
        return {
            "files": files,
            "instructions": [
                f"Run script: python {project_name}.py --input input.txt --output result.json",
                "Use --help for all available options",
                "Make script executable: chmod +x " + project_name + ".py"
            ]
        }
    
    # JavaScript generators
    def _generate_react_component(self, project_name: str, requirements: Dict[str, Any]) -> Dict[str, Any]:
        """Generate React component"""
        component_name = requirements.get('component_name', 'MyComponent')
        
        files = {}
        
        files[f"{project_name}/{component_name}.jsx"] = f'''import React, {{ useState, useEffect }} from 'react';
import './{component_name}.css';

const {component_name} = ({{ prop1, prop2 }}) => {{
  const [state, setState] = useState({{}});
  
  useEffect(() => {{
    // Component mount logic
  }}, []);
  
  const handleClick = () => {{
    setState(prevState => ({{ ...prevState, clicked: true }}));
  }};
  
  return (
    <div className="{component_name.toLowerCase()}">
      <h2>{component_name}</h2>
      <p>Component generated by AI</p>
      <button onClick={{handleClick}}>Click me</button>
      {{state.clicked && <p>Button was clicked!</p>}}
    </div>
  );
}};

export default {component_name};
'''
        
        files[f"{project_name}/{component_name}.css"] = f'''
.{component_name.toLowerCase()} {{
  padding: 20px;
  border: 1px solid #ccc;
  border-radius: 8px;
  margin: 10px;
}}

.{component_name.toLowerCase()} h2 {{
  color: #333;
  margin-bottom: 15px;
}}

.{component_name.toLowerCase()} button {{
  background: #007bff;
  color: white;
  border: none;
  padding: 10px 20px;
  border-radius: 4px;
  cursor: pointer;
}}

.{component_name.toLowerCase()} button:hover {{
  background: #0056b3;
}}
'''
        
        return {
            "files": files,
            "instructions": [
                "Import and use the component in your React app",
                "Add CSS styling as needed",
                "Pass props to customize component behavior"
            ]
        }
    
    def _generate_js_api(self, project_name: str, requirements: Dict[str, Any]) -> Dict[str, Any]:
        """Generate JavaScript API module"""
        files = {}
        
        files[f"{project_name}/api.js"] = '''class API {
    constructor(baseURL = '/api') {
        this.baseURL = baseURL;
    }
    
    async request(endpoint, options = {}) {
        const url = `${this.baseURL}${endpoint}`;
        const config = {
            headers: {
                'Content-Type': 'application/json',
                ...options.headers
            },
            ...options
        };
        
        try {
            const response = await fetch(url, config);
            
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }
            
            return await response.json();
        } catch (error) {
            console.error('API request failed:', error);
            throw error;
        }
    }
    
    async get(endpoint) {
        return this.request(endpoint);
    }
    
    async post(endpoint, data) {
        return this.request(endpoint, {
            method: 'POST',
            body: JSON.stringify(data)
        });
    }
    
    async put(endpoint, data) {
        return this.request(endpoint, {
            method: 'PUT',
            body: JSON.stringify(data)
        });
    }
    
    async delete(endpoint) {
        return this.request(endpoint, {
            method: 'DELETE'
        });
    }
}

export default API;
'''
        
        return {
            "files": files,
            "instructions": [
                "Import API class in your components",
                "Create instance with base URL",
                "Use methods for API calls"
            ]
        }
    
    def _generate_js_class(self, project_name: str, requirements: Dict[str, Any]) -> Dict[str, Any]:
        """Generate JavaScript class"""
        class_name = requirements.get('class_name', 'MyClass')
        
        files = {}
        
        files[f"{project_name}/{class_name}.js"] = f'''class {class_name} {{
    constructor(options = {{}}) {{
        this.options = {{ ...options }};
        this.initialized = false;
    }}
    
    init() {{
        // Initialization logic
        this.initialized = true;
        console.log('{class_name} initialized');
    }}
    
    process(data) {{
        if (!this.initialized) {{
            throw new Error('{class_name} not initialized');
        }}
        
        // Processing logic
        return {{
            processed: true,
            data: data,
            timestamp: new Date().toISOString()
        }};
    }}
    
    destroy() {{
        // Cleanup logic
        this.initialized = false;
        console.log('{class_name} destroyed');
    }}
}}

export default {class_name};
'''
        
        return {
            "files": files,
            "instructions": [
                "Import the class in your modules",
                "Initialize with new " + class_name + "()",
                "Call init() before using other methods"
            ]
        }
    
    def _generate_js_function(self, project_name: str, requirements: Dict[str, Any]) -> Dict[str, Any]:
        """Generate JavaScript function"""
        func_name = requirements.get('function_name', 'myFunction')
        
        files = {}
        
        files[f"{project_name}/functions.js"] = f'''/**
 * {func_name} - Auto-generated function
 * @param {{string}} param1 - Description of param1
 * @param {{number}} param2 - Description of param2
 * @returns {{object}} Result object
 */
export function {func_name}(param1, param2) {{
    // Validation
    if (typeof param1 !== 'string') {{
        throw new Error('param1 must be a string');
    }}
    
    if (typeof param2 !== 'number') {{
        throw new Error('param2 must be a number');
    }}
    
    // Processing logic
    const result = {{
        input: {{ param1, param2 }},
        processed: true,
        timestamp: new Date().toISOString(),
        calculation: param2 * 2
    }};
    
    return result;
}}

// Example usage
if (typeof window !== 'undefined') {{
    console.log({func_name}('test', 42));
}}
'''
        
        return {
            "files": files,
            "instructions": [
                "Import the function in your modules",
                "Call with required parameters",
                "Handle return value appropriately"
            ]
        }
    
    def _generate_js_module(self, project_name: str, requirements: Dict[str, Any]) -> Dict[str, Any]:
        """Generate JavaScript module"""
        files = {}
        
        files[f"{project_name}/index.js"] = f'''// {project_name} module

import {{ {project_name}Service }} from './{project_name}Service.js';
import {{ {project_name}Utils }} from './{project_name}Utils.js';

class {project_name.title().replace(' ', '')}Module {{
    constructor(config = {{}}) {{
        this.config = {{ ...config }};
        this.service = new {project_name.title().replace(' ', '')}Service(this.config);
        this.utils = new {project_name.title().replace(' ', '')}Utils();
    }}
    
    async initialize() {{
        // Module initialization
        await this.service.initialize();
        return this;
    }}
    
    async process(data) {{
        return this.service.process(data);
    }}
    
    getStatus() {{
        return {{
            initialized: this.service.isInitialized(),
            version: '1.0.0'
        }};
    }}
}}

export default {project_name.title().replace(' ', '')}Module;
export {{ {project_name.title().replace(' ', '')}Service, {project_name.title().replace(' ', '')}Utils }};
'''
        
        files[f"{project_name}/{project_name}Service.js"] = f'''export class {project_name.title().replace(' ', '')}Service {{
    constructor(config) {{
        this.config = config;
        this.initialized = false;
    }}
    
    async initialize() {{
        // Service initialization
        this.initialized = true;
    }}
    
    isInitialized() {{
        return this.initialized;
    }}
    
    async process(data) {{
        if (!this.initialized) {{
            throw new Error('Service not initialized');
        }}
        
        return {{
            processed: true,
            data: data,
            timestamp: new Date().toISOString()
        }};
    }}
}}
'''
        
        return {
            "files": files,
            "instructions": [
                "Import the module in your application",
                "Initialize with new module()",
                "Use the provided methods"
            ]
        }
    
    # TypeScript generators
    def _generate_ts_interface(self, project_name: str, requirements: Dict[str, Any]) -> Dict[str, Any]:
        """Generate TypeScript interface"""
        interface_name = requirements.get('interface_name', 'IUser')
        properties = requirements.get('properties', ['id', 'name', 'email'])
        
        files = {}
        
        interface_code = f'''interface {interface_name} {{
{chr(10).join([f'  {prop}: string;' for prop in properties])}
  createdAt: Date;
  updatedAt?: Date;
}}

export interface {interface_name}List {{
  users: {interface_name}[];
  total: number;
  page: number;
  limit: number;
}}

export default {interface_name};
'''
        
        files[f"{project_name}/{interface_name.lower()}.ts"] = interface_code
        
        return {
            "files": files,
            "instructions": [
                "Import interfaces in your TypeScript files",
                "Use for type checking and validation",
                "Extend interfaces as needed"
            ]
        }
    
    def _generate_ts_class(self, project_name: str, requirements: Dict[str, Any]) -> Dict[str, Any]:
        """Generate TypeScript class"""
        class_name = requirements.get('class_name', 'MyClass')
        
        files = {}
        
        files[f"{project_name}/{class_name}.ts"] = f'''interface {class_name}Options {{
  debug?: boolean;
  timeout?: number;
}}

class {class_name} {{
  private options: {class_name}Options;
  private initialized: boolean = false;
  
  constructor(options: {class_name}Options = {{}}) {{
    this.options = {{ debug: false, timeout: 5000, ...options }};
  }}
  
  async initialize(): Promise<void> {{
    // Initialization logic
    this.initialized = true;
  }}
  
  process<T>(data: T): T {{
    if (!this.initialized) {{
      throw new Error('{class_name} not initialized');
    }}
    
    // Processing logic
    return data;
  }}
  
  getStatus(): {class_name}Options & {{ initialized: boolean }} {{
    return {{
      ...this.options,
      initialized: this.initialized
    }};
  }}
}}

export default {class_name};
export {{ {class_name}Options }};
'''
        
        return {
            "files": files,
            "instructions": [
                "Import and use with TypeScript type checking",
                "Initialize before using other methods",
                "Specify proper types for all parameters"
            ]
        }
    
    def _generate_ts_api(self, project_name: str, requirements: Dict[str, Any]) -> Dict[str, Any]:
        """Generate TypeScript API"""
        files = {}
        
        files[f"{project_name}/api.ts"] = '''interface APIResponse<T = any> {
  data: T;
  status: number;
  message: string;
}

interface APIError {
  error: string;
  code: number;
}

class APIClient {
  private baseURL: string;
  
  constructor(baseURL: string = '/api') {
    this.baseURL = baseURL;
  }
  
  async request<T>(endpoint: string, options: RequestInit = {}): Promise<T> {
    const url = `${this.baseURL}${endpoint}`;
    const config: RequestInit = {
      headers: {
        'Content-Type': 'application/json',
        ...options.headers
      },
      ...options
    };
    
    try {
      const response = await fetch(url, config);
      
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      
      return await response.json();
    } catch (error) {
      console.error('API request failed:', error);
      throw error;
    }
  }
  
  async get<T>(endpoint: string): Promise<T> {
    return this.request<T>(endpoint);
  }
  
  async post<T>(endpoint: string, data: any): Promise<T> {
    return this.request<T>(endpoint, {
      method: 'POST',
      body: JSON.stringify(data)
    });
  }
}

export default APIClient;
export { APIResponse, APIError };
'''
        
        return {
            "files": files,
            "instructions": [
                "Import APIClient with TypeScript types",
                "Use generic types for type safety",
                "Handle API errors appropriately"
            ]
        }
    
    # Java generators
    def _generate_java_class(self, project_name: str, requirements: Dict[str, Any]) -> Dict[str, Any]:
        """Generate Java class"""
        class_name = requirements.get('class_name', 'MyClass')
        
        files = {}
        
        files[f"{project_name}/{class_name}.java"] = f'''public class {class_name} {{
    private String name;
    private int value;
    
    public {class_name}() {{
        this.name = "default";
        this.value = 0;
    }}
    
    public {class_name}(String name, int value) {{
        this.name = name;
        this.value = value;
    }}
    
    public String getName() {{
        return name;
    }}
    
    public void setName(String name) {{
        this.name = name;
    }}
    
    public int getValue() {{
        return value;
    }}
    
    public void setValue(int value) {{
        this.value = value;
    }}
    
    public void process() {{
        System.out.println("Processing " + name + " with value " + value);
    }}
    
    @Override
    public String toString() {{
        return "{class_name}{{name='" + name + "', value=" + value + "}}";
    }}
    
    public static void main(String[] args) {{
        {class_name} obj = new {class_name}("example", 42);
        obj.process();
        System.out.println(obj);
    }}
}}
'''
        
        return {
            "files": files,
            "instructions": [
                "Compile with: javac " + class_name + ".java",
                "Run with: java " + class_name,
                "Import in other Java classes as needed"
            ]
        }
    
    def _generate_java_interface(self, project_name: str, requirements: Dict[str, Any]) -> Dict[str, Any]:
        """Generate Java interface"""
        interface_name = requirements.get('interface_name', 'MyInterface')
        
        files = {}
        
        files[f"{project_name}/{interface_name}.java"] = f'''public interface {interface_name} {{
    
    void initialize();
    
    void process();
    
    String getStatus();
    
    boolean isInitialized();
    
    default void log(String message) {{
        System.out.println("[{interface_name}] " + message);
    }}
}}
'''
        
        return {
            "files": files,
            "instructions": [
                "Implement in Java classes",
                "Override all abstract methods",
                "Use default methods for common functionality"
            ]
        }
    
    def _generate_java_service(self, project_name: str, requirements: Dict[str, Any]) -> Dict[str, Any]:
        """Generate Java service class"""
        service_name = requirements.get('service_name', 'MyService')
        
        files = {}
        
        files[f"{project_name}/{service_name}.java"] = f'''public class {service_name} {{
    private boolean initialized = false;
    
    public {service_name}() {{
        // Constructor
    }}
    
    public void initialize() {{
        if (!initialized) {{
            // Initialization logic
            initialized = true;
            log("{service_name} initialized successfully");
        }}
    }}
    
    public Object process(Object data) {{
        if (!initialized) {{
            throw new IllegalStateException("{service_name} not initialized");
        }}
        
        // Processing logic
        log("Processing data: " + data);
        return data;
    }}
    
    public String getStatus() {{
        return String.format("{service_name} status: initialized=%s", initialized);
    }}
    
    public boolean isInitialized() {{
        return initialized;
    }}
    
    private void log(String message) {{
        System.out.println("[{service_name}] " + message);
    }}
}}
'''
        
        return {
            "files": files,
            "instructions": [
                "Initialize before use",
                "Call process() with data to process",
                "Check status with getStatus() method"
            ]
        }

