import json
import re
from typing import Dict, Any, List, Optional

PRIVILEGES = ["generate_java_code", "analyze_enterprise"]


class JavaGenerator:
    """Advanced Java programming language code generator for enterprise applications"""
    
    # Java-specific templates
    JAVA_TEMPLATES = {
        'general_java': {
            'imports': ['// Basic Java application'],
            'template': '''public class Main {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
    }
}''',
            'usage': 'General Java development'
        },
        'spring_boot': {
            'imports': ['import org.springframework.boot.*;', 'import org.springframework.web.bind.annotation.*;'],
            'template': '''import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.bind.annotation.*;

@SpringBootApplication
public class Application {
    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }
}

@RestController
@RequestMapping("/api")
public class UserController {
    
    @GetMapping("/users")
    public ResponseEntity<List<User>> getAllUsers() {
        List<User> users = Arrays.asList(
            new User(1L, "John Doe", "john@example.com"),
            new User(2L, "Jane Smith", "jane@example.com")
        );
        return ResponseEntity.ok(users);
    }
    
    @GetMapping("/users/{id}")
    public ResponseEntity<User> getUserById(@PathVariable Long id) {
        User user = new User(id, "User " + id, "user" + id + "@example.com");
        return ResponseEntity.ok(user);
    }
    
    @PostMapping("/users")
    public ResponseEntity<User> createUser(@RequestBody User user) {
        user.setId(System.currentTimeMillis());
        return ResponseEntity.created(URI.create("/api/users/" + user.getId())).body(user);
    }
}

class User {
    private Long id;
    private String name;
    private String email;
    
    public User() {}
    
    public User(Long id, String name, String email) {
        this.id = id;
        this.name = name;
        this.email = email;
    }
    
    // Getters and setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    public String getEmail() { return email; }
    public void setEmail(String email) { this.email = email; }
}''',
            'usage': 'Spring Boot REST APIs with enterprise patterns'
        },
        'design_patterns': {
            'imports': ['import java.util.*;'],
            'template': '''import java.util.*;

// Singleton Pattern
public class Singleton {
    private static Singleton instance;
    private Singleton() {}
    
    public static Singleton getInstance() {
        if (instance == null) {
            instance = new Singleton();
        }
        return instance;
    }
}

// Factory Pattern
interface Shape {
    void draw();
}

class Circle implements Shape {
    @Override
    public void draw() {
        System.out.println("Drawing a circle");
    }
}

class Square implements Shape {
    @Override
    public void draw() {
        System.out.println("Drawing a square");
    }
}

class ShapeFactory {
    public Shape createShape(String type) {
        switch (type.toLowerCase()) {
            case "circle": return new Circle();
            case "square": return new Square();
            default: throw new IllegalArgumentException("Unknown shape type: " + type);
        }
    }
}

// Strategy Pattern
interface PaymentStrategy {
    void pay(int amount);
}

class CreditCardPayment implements PaymentStrategy {
    private String cardNumber;
    
    public CreditCardPayment(String cardNumber) {
        this.cardNumber = cardNumber;
    }
    
    @Override
    public void pay(int amount) {
        System.out.println("Paid " + amount + " using credit card ending with " + cardNumber.substring(cardNumber.length() - 4));
    }
}

class PayPalPayment implements PaymentStrategy {
    private String email;
    
    public PayPalPayment(String email) {
        this.email = email;
    }
    
    @Override
    public void pay(int amount) {
        System.out.println("Paid " + amount + " using PayPal account: " + email);
    }
}''',
            'usage': 'Enterprise design patterns and best practices'
        },
        'multithreading': {
            'imports': ['import java.util.concurrent.*;', 'import java.util.concurrent.atomic.*;'],
            'template': '''import java.util.concurrent.*;
import java.util.concurrent.atomic.*;

public class MultiThreadingExample {
    
    public static void main(String[] args) throws InterruptedException {
        // Using ExecutorService
        ExecutorService executor = Executors.newFixedThreadPool(4);
        
        List<Future<String>> futures = new ArrayList<>();
        
        for (int i = 1; i <= 10; i++) {
            final int taskId = i;
            Future<String> future = executor.submit(() -> {
                try {
                    Thread.sleep(1000);
                    return "Task " + taskId + " completed by " + Thread.currentThread().getName();
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    return "Task " + taskId + " was interrupted";
                }
            });
            futures.add(future);
        }
        
        // Collect results
        for (Future<String> future : futures) {
            try {
                System.out.println(future.get());
            } catch (ExecutionException e) {
                System.err.println("Task execution failed: " + e.getMessage());
            }
        }
        
        executor.shutdown();
    }
    
    // Using Atomic variables for thread-safe operations
    public static class Counter {
        private final AtomicInteger count = new AtomicInteger(0);
        
        public void increment() {
            count.incrementAndGet();
        }
        
        public int getCount() {
            return count.get();
        }
        
        public void reset() {
            count.set(0);
        }
    }
    
    // Producer-Consumer pattern
    public static class ProducerConsumerExample {
        private final BlockingQueue<String> queue = new ArrayBlockingQueue<>(10);
        
        public void start() throws InterruptedException {
            // Producer thread
            Thread producer = new Thread(() -> {
                try {
                    for (int i = 1; i <= 20; i++) {
                        queue.put("Item " + i);
                        System.out.println("Produced: Item " + i);
                        Thread.sleep(100);
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            });
            
            // Consumer thread
            Thread consumer = new Thread(() -> {
                try {
                    while (true) {
                        String item = queue.take();
                        System.out.println("Consumed: " + item);
                        Thread.sleep(200);
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            });
            
            producer.start();
            consumer.start();
            
            producer.join();
            consumer.interrupt();
        }
    }
}''',
            'usage': 'Multithreading and concurrency patterns'
        }
    }
    
    def __init__(self, request: str):
        self.request = request.lower().strip()
        self.java_type = self._detect_java_type()
        self.framework = self._detect_framework()
        
    def _detect_java_type(self) -> str:
        """Detect Java task type"""
        if any(word in self.request for word in ['spring', 'boot', 'rest', 'api']):
            return 'spring_boot'
        elif any(word in self.request for word in ['pattern', 'design', 'singleton', 'factory']):
            return 'design_patterns'
        elif any(word in self.request for word in ['thread', 'concurrent', 'async', 'multithread']):
            return 'multithreading'
        elif any(word in self.request for word in ['hibernate', 'jpa', 'database', 'orm']):
            return 'database'
        elif any(word in self.request for word in ['test', 'junit', 'mockito']):
            return 'testing'
        else:
            return 'general_java'
    
    def _detect_framework(self) -> str:
        """Detect Java framework preference"""
        if any(word in self.request for word in ['spring', 'boot']):
            return 'spring'
        elif any(word in self.request for word in ['hibernate', 'jpa']):
            return 'hibernate'
        elif any(word in self.request for word in ['play', 'vertx']):
            return 'reactive'
        else:
            return 'standard'
    
    def generate_java_structure(self) -> Dict[str, Any]:
        """Generate Java code structure"""
        template = self.JAVA_TEMPLATES.get(self.java_type, self.JAVA_TEMPLATES['general_java'])
        
        return {
            'java_type': self.java_type,
            'framework': self.framework,
            'imports': template['imports'],
            'template': template['template'],
            'usage': template['usage'],
            'enhancements': [
                'Enterprise architecture patterns',
                'Dependency injection',
                'Aspect-oriented programming',
                'Transaction management',
                'Security implementation'
            ],
            'best_practices': [
                'SOLID principles',
                'Clean code architecture',
                'Proper exception handling',
                'Unit and integration testing',
                'Performance optimization'
            ]
        }

def generate_enhanced_java_code(analysis: JavaGenerator) -> str:
    """Generate enhanced Java code"""
    structure = analysis.generate_java_structure()
    
    header = f"""// Java Programming Language Code Generator
// Task: {structure['java_type'].replace('_', ' ').title()}
// Framework: {structure['framework'].title()}
// Generated by Enhanced AI System

"""
    
    imports = "\n".join(structure['imports']) + "\n\n"
    
    main_code = f"""{structure['template']}

{chr(10).join(f"// {practice}" for practice in structure['best_practices'])}
"""
    
    enhancements = f"""
// Enhancement Features:
{chr(10).join(f"// - {enhancement}" for enhancement in structure['enhancements'])}

// Java Best Practices Applied:
// - Object-oriented design principles
// - Enterprise architecture patterns
// - Proper exception handling
// - Thread-safe programming
// - Dependency injection
// - Comprehensive testing strategies
"""
    
    return header + imports + main_code + enhancements

def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Enhanced Java mode: Generate enterprise applications and design patterns
    """
    try:
        if isinstance(input_data, str):
            request_text = input_data
        else:
            request_text = str(input_data)
        
        # Create Java generator instance
        generator = JavaGenerator(request_text)
        
        # Generate enhanced Java code
        generated_code = generate_enhanced_java_code(generator)
        structure = generator.generate_java_structure()
        
        response = {
            'request': request_text,
            'analysis': {
                'java_type': generator.java_type,
                'framework': generator.framework
            },
            'generated_code': generated_code,
            'java_structure': structure,
            'language_info': {
                'recommended_packages': structure['imports'],
                'usage_guide': structure['usage'],
                'best_practices': structure['best_practices']
            },
            'ai_enhancement': {
                'frameworks': ['Spring Boot', 'Hibernate', 'Jakarta EE'],
                'patterns': ['Factory', 'Singleton', 'Observer', 'Strategy', 'Decorator'],
                'testing': ['JUnit 5', 'Mockito', 'TestContainers', 'Spring Test']
            }
        }
        
        # Try to enhance with AI model if available
        try:
            from ai_backend.ai_models import models
            model = models.get(model_name)
            if model:
                ai_enhanced = model.generate(
                    f"Generate enterprise Java code for: {request_text}. "
                    f"Include proper design patterns, error handling, and best practices.",
                    user_id=user_id, session_id=session_id
                )
                response['ai_enhanced_code'] = ai_enhanced
        except:
            pass
        
        return json.dumps(response, indent=2)
        
    except Exception as e:
        return json.dumps({
            'error': str(e),
            'fallback': 'Java code generation failed.',
            'basic_template': 'public class Main {\n    public static void main(String[] args) {\n        System.out.println("Hello, World!");\n    }\n}'
        })

def java_response(prompt: str) -> str:
    """Legacy function for backward compatibility"""
    generator = JavaGenerator(prompt)
    structure = generator.generate_java_structure()
    return f"[Java Mode]\nType: {generator.java_type}\nFramework: {generator.framework}\nGenerated: {len(structure.get('enhancements', []))} enhancement features"
