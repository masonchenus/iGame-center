import json
import re
from typing import Dict, Any, List, Optional

PRIVILEGES = ["generate_rust_code", "analyze_systems"]

class RustGenerator:
    """Advanced Rust programming language code generator for systems programming and WebAssembly"""
    
    # Rust-specific templates
    RUST_TEMPLATES = {
        'webassembly': {
            'imports': ['use wasm_bindgen::prelude::*;'],
            'template': '''use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn greet(name: &str) -> String {
    format!("Hello, {}!", name)
}

#[wasm_bindgen]
pub struct Calculator {
    result: f64,
}

#[wasm_bindgen]
impl Calculator {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Calculator {
        Calculator { result: 0.0 }
    }
    
    #[wasm_bindgen]
    pub fn add(&mut self, value: f64) -> f64 {
        self.result += value;
        self.result
    }
    
    #[wasm_bindgen]
    pub fn multiply(&mut self, value: f64) -> f64 {
        self.result *= value;
        self.result
    }
    
    #[wasm_bindgen]
    pub fn reset(&mut self) {
        self.result = 0.0;
    }
    
    #[wasm_bindgen(getter)]
    pub fn result(&self) -> f64 {
        self.result
    }
}''',
            'usage': 'WebAssembly modules with wasm-bindgen'
        },
        'async_programming': {
            'imports': ['use tokio::time::{sleep, Duration};', 'use tokio::task;'],
            'template': '''use tokio::time::{sleep, Duration};
use tokio::task;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Starting async Rust application...");
    
    // Spawn multiple concurrent tasks
    let handles: Vec<_> = (1..=5)
        .map(|i| tokio::spawn(async move {
            worker_task(i).await
        }))
        .collect();
    
    // Wait for all tasks to complete
    for handle in handles {
        handle.await??;
    }
    
    println!("All tasks completed successfully!");
    Ok(())
}

async fn worker_task(id: u32) -> Result<String, Box<dyn std::error::Error>> {
    println!("Worker {} starting...", id);
    
    // Simulate async work
    sleep(Duration::from_secs(2)).await;
    
    println!("Worker {} completed!", id);
    Ok(format!("Worker {} result", id))
}

async fn fetch_data(url: &str) -> Result<String, reqwest::Error> {
    let response = reqwest::get(url).await?;
    let body = response.text().await?;
    Ok(body)
}''',
            'usage': 'Asynchronous programming with Tokio'
        },
        'data_structures': {
            'imports': ['use std::collections::{HashMap, BTreeMap};', 'use std::cmp::Ordering;'],
            'template': '''use std::collections::{HashMap, BTreeMap};
use std::cmp::Ordering;

#[derive(Debug, Clone)]
pub struct Person {
    pub name: String,
    pub age: u32,
    pub email: String,
}

impl Person {
    pub fn new(name: String, age: u32, email: String) -> Self {
        Person { name, age, email }
    }
    
    pub fn is_adult(&self) -> bool {
        self.age >= 18
    }
    
    pub fn compare_by_age(&self, other: &Person) -> Ordering {
        self.age.cmp(&other.age)
    }
}

pub struct Database {
    people: HashMap<String, Person>,
    sorted_by_age: BTreeMap<u32, Vec<Person>>,
}

impl Database {
    pub fn new() -> Self {
        Database {
            people: HashMap::new(),
            sorted_by_age: BTreeMap::new(),
        }
    }
    
    pub fn add_person(&mut self, person: Person) {
        let email = person.email.clone();
        self.people.insert(email.clone(), person.clone());
        self.sorted_by_age
            .entry(person.age)
            .or_insert_with(Vec::new)
            .push(person);
    }
    
    pub fn get_person(&self, email: &str) -> Option<&Person> {
        self.people.get(email)
    }
    
    pub fn get_adults(&self) -> Vec<&Person> {
        self.people.values()
            .filter(|person| person.is_adult())
            .collect()
    }
}''',
            'usage': 'Custom data structures with proper Rust patterns'
        }
    }
    
    def __init__(self, request: str):
        self.request = request.lower().strip()
        self.rust_type = self._detect_rust_type()
        self.complexity = self._assess_complexity()
        
    def _detect_rust_type(self) -> str:
        """Detect Rust task type"""
        if any(word in self.request for word in ['webassembly', 'wasm', 'web', 'browser']):
            return 'webassembly'
        elif any(word in self.request for word in ['async', 'await', 'tokio', 'concurrent']):
            return 'async_programming'
        elif any(word in self.request for word in ['struct', 'class', 'data', 'collection']):
            return 'data_structures'
        elif any(word in self.request for word in ['cli', 'command', 'tool']):
            return 'cli_tool'
        elif any(word in self.request for word in ['performance', 'optimization', 'memory']):
            return 'performance'
        else:
            return 'general_rust'
    
    def _assess_complexity(self) -> str:
        """Assess Rust complexity level"""
        if any(word in self.request for word in ['advanced', 'complex', 'unsafe', 'performance']):
            return 'advanced'
        elif any(word in self.request for word in ['beginner', 'simple', 'basic']):
            return 'beginner'
        else:
            return 'intermediate'
    
    def generate_rust_structure(self) -> Dict[str, Any]:
        """Generate Rust code structure"""
        template = self.RUST_TEMPLATES.get(self.rust_type, self.RUST_TEMPLATES['data_structures'])
        
        return {
            'rust_type': self.rust_type,
            'complexity': self.complexity,
            'imports': template['imports'],
            'template': template['template'],
            'usage': template['usage'],
            'enhancements': [
                'Memory safety guarantees',
                'Zero-cost abstractions',
                'Ownership and borrowing patterns',
                'Error handling with Result',
                'Performance optimizations'
            ],
            'best_practices': [
                'Use Result for error handling',
                'Implement traits for abstractions',
                'Use cargo for dependency management',
                'Write comprehensive tests',
                'Profile and benchmark critical code'
            ]
        }

def generate_enhanced_rust_code(analysis: RustGenerator) -> str:
    """Generate enhanced Rust code"""
    structure = analysis.generate_rust_structure()
    
    header = f"""// Rust Programming Language Code Generator
// Task: {structure['rust_type'].replace('_', ' ').title()}
// Complexity: {structure['complexity'].title()}
// Generated by Enhanced AI System

"""
    
    imports = "\n".join(structure['imports']) + "\n\n"
    
    main_code = f"""{structure['template']}

{chr(10).join(f"// {practice}" for practice in structure['best_practices'])}
"""
    
    enhancements = f"""
// Enhancement Features:
{chr(10).join(f"// - {enhancement}" for enhancement in structure['enhancements'])}

// Rust Best Practices Applied:
// - Memory safety without garbage collection
// - Zero-cost abstractions
// - Ownership and borrowing for memory management
// - Result types for error handling
// - Pattern matching for control flow
// - Cargo for build system and dependency management
"""
    
    return header + imports + main_code + enhancements

def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Enhanced Rust mode: Generate systems programming and WebAssembly code
    """
    try:
        if isinstance(input_data, str):
            request_text = input_data
        else:
            request_text = str(input_data)
        
        # Create Rust generator instance
        generator = RustGenerator(request_text)
        
        # Generate enhanced Rust code
        generated_code = generate_enhanced_rust_code(generator)
        structure = generator.generate_rust_structure()
        
        response = {
            'request': request_text,
            'analysis': {
                'rust_type': generator.rust_type,
                'complexity': generator.complexity
            },
            'generated_code': generated_code,
            'rust_structure': structure,
            'language_info': {
                'recommended_crates': structure['imports'],
                'usage_guide': structure['usage'],
                'best_practices': structure['best_practices']
            },
            'ai_enhancement': {
                'memory_safety': ['Ownership System', 'Borrow Checker', 'Lifetime Analysis'],
                'performance': ['Zero-Cost Abstractions', 'Inline Assembly', 'Compiler Optimizations'],
                'concurrency': ['Async/Await', 'Message Passing', 'Arc/Mutex Patterns']
            }
        }
        
        # Try to enhance with AI model if available
        try:
            from ai_backend.ai_models import models
            model = models.get(model_name)
            if model:
                ai_enhanced = model.generate(
                    f"Generate high-quality Rust code for: {request_text}. "
                    f"Include proper error handling, memory safety, and performance optimizations.",
                    user_id=user_id, session_id=session_id
                )
                response['ai_enhanced_code'] = ai_enhanced
        except:
            pass
        
        return json.dumps(response, indent=2)
        
    except Exception as e:
        return json.dumps({
            'error': str(e),
            'fallback': 'Rust code generation failed.',
            'basic_template': 'fn main() {\n    println!("Hello, World!");\n}'
        })

def rust_response(prompt: str) -> str:
    """Legacy function for backward compatibility"""
    generator = RustGenerator(prompt)
    structure = generator.generate_rust_structure()
    return f"[Rust Mode]\nType: {generator.rust_type}\nComplexity: {generator.complexity}\nGenerated: {len(structure.get('enhancements', []))} enhancement features"
