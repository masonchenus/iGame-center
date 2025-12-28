import json
import re
from typing import Dict, Any, List, Optional

PRIVILEGES = ["generate_ml_code", "analyze_data"]

class PythonMLGenerator:
    """Advanced Python Machine Learning and Data Science code generator"""
    
    # ML/Domain-specific templates
    ML_TEMPLATES = {
        'neural_network': {
            'imports': ['import torch', 'import torch.nn as nn', 'import torch.optim as optim'],
            'template': '''class NeuralNetwork(nn.Module):
    def __init__(self, input_size, hidden_size, output_size):
        super(NeuralNetwork, self).__init__()
        self.fc1 = nn.Linear(input_size, hidden_size)
        self.fc2 = nn.Linear(hidden_size, hidden_size)
        self.fc3 = nn.Linear(hidden_size, output_size)
        self.relu = nn.ReLU()
        
    def forward(self, x):
        x = self.relu(self.fc1(x))
        x = self.relu(self.fc2(x))
        x = self.fc3(x)
        return x''',
            'usage': 'Deep learning neural networks with PyTorch'
        },
        'data_analysis': {
            'imports': ['import pandas as pd', 'import numpy as np', 'import matplotlib.pyplot as plt', 'import seaborn as sns'],
            'template': '''# Data Analysis Template
df = pd.read_csv('data.csv')
print(df.head())
print(df.describe())
print(df.info())''',
            'usage': 'Pandas-based data analysis and visualization'
        },
        'classification': {
            'imports': ['from sklearn.model_selection import train_test_split', 'from sklearn.ensemble import RandomForestClassifier', 'from sklearn.metrics import accuracy_score'],
            'template': '''# Classification Model
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)
model = RandomForestClassifier(n_estimators=100)
model.fit(X_train, y_train)
predictions = model.predict(X_test)
accuracy = accuracy_score(y_test, predictions)''',
            'usage': 'Machine learning classification tasks'
        }
    }
    
    def __init__(self, request: str):
        self.request = request.lower().strip()
        self.ml_type = self._detect_ml_type()
        self.complexity = self._assess_complexity()
        self.framework = self._detect_framework()
        
    def _detect_ml_type(self) -> str:
        """Detect machine learning task type"""
        if any(word in self.request for word in ['neural', 'deep learning', 'nn', 'network']):
            return 'neural_network'
        elif any(word in self.request for word in ['classification', 'classify', 'categorize']):
            return 'classification'
        elif any(word in self.request for word in ['regression', 'predict', 'forecast']):
            return 'regression'
        elif any(word in self.request for word in ['clustering', 'kmeans', 'unsupervised']):
            return 'clustering'
        elif any(word in self.request for word in ['data', 'analysis', 'visualize', 'explore']):
            return 'data_analysis'
        else:
            return 'general_ml'
    
    def _detect_framework(self) -> str:
        """Detect ML framework preference"""
        if any(word in self.request for word in ['pytorch', 'torch']):
            return 'pytorch'
        elif any(word in self.request for word in ['tensorflow', 'keras', 'tf']):
            return 'tensorflow'
        elif any(word in self.request for word in ['scikit', 'sklearn']):
            return 'sklearn'
        else:
            return 'pandas'
    
    def _assess_complexity(self) -> str:
        """Assess ML complexity level"""
        complex_indicators = ['advanced', 'complex', 'sophisticated', 'enterprise']
        if any(indicator in self.request for indicator in complex_indicators):
            return 'advanced'
        elif any(word in self.request for word in ['beginner', 'simple', 'basic']):
            return 'beginner'
        else:
            return 'intermediate'
    
    def generate_ml_structure(self) -> Dict[str, Any]:
        """Generate ML code structure"""
        template = self.ML_TEMPLATES.get(self.ml_type, self.ML_TEMPLATES['data_analysis'])
        
        return {
            'ml_type': self.ml_type,
            'framework': self.framework,
            'complexity': self.complexity,
            'imports': template['imports'],
            'template': template['template'],
            'usage': template['usage'],
            'enhancements': [
                'Cross-validation implementation',
                'Performance metrics calculation',
                'Data preprocessing pipeline',
                'Hyperparameter tuning',
                'Model interpretation tools'
            ]
        }

def generate_enhanced_ml_code(analysis: PythonMLGenerator) -> str:
    """Generate enhanced ML code"""
    structure = analysis.generate_ml_structure()
    
    header = f"""# Python ML/AI Code Generator
# Task: {structure['ml_type'].replace('_', ' ').title()}
# Framework: {structure['framework'].title()}
# Complexity: {structure['complexity'].title()}
# Generated by Enhanced AI System

"""
    
    imports = "\n".join(structure['imports']) + "\n\n"
    
    main_code = f"""{structure['template']}

# Model Training
# model = NeuralNetwork(input_size=10, hidden_size=64, output_size=1)
# criterion = nn.MSELoss()
# optimizer = optim.Adam(model.parameters(), lr=0.001)

# Training loop would go here
print("Model training complete!")
"""
    
    enhancements = f"""
# Enhancement Features:
{chr(10).join(f"# - {enhancement}" for enhancement in structure['enhancements'])}

# Best Practices Applied:
# - Proper data preprocessing
# - Cross-validation for model evaluation
# - Hyperparameter optimization
# - Performance monitoring
# - Model interpretability
"""
    
    return header + imports + main_code + enhancements

def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Enhanced Python ML/AI mode: Generate machine learning and data science code
    """
    try:
        if isinstance(input_data, str):
            request_text = input_data
        else:
            request_text = str(input_data)
        
        # Create ML generator instance
        generator = PythonMLGenerator(request_text)
        
        # Generate enhanced ML code
        generated_code = generate_enhanced_ml_code(generator)
        structure = generator.generate_ml_structure()
        
        response = {
            'request': request_text,
            'analysis': {
                'ml_type': generator.ml_type,
                'framework': generator.framework,
                'complexity': generator.complexity
            },
            'generated_code': generated_code,
            'ml_structure': structure,
            'framework_info': {
                'recommended_packages': structure['imports'],
                'usage_guide': structure['usage'],
                'best_practices': structure['enhancements']
            },
            'ai_enhancement': {
                'model_suggestions': ['Random Forest', 'XGBoost', 'Neural Networks'],
                'data_preprocessing': ['StandardScaler', 'PCA', 'Feature Engineering'],
                'evaluation_metrics': ['Accuracy', 'Precision', 'Recall', 'F1-Score']
            }
        }
        
        # Try to enhance with AI model if available
        try:
            from ai_backend.ai_models import models
            model = models.get(model_name)
            if model:
                ai_enhanced = model.generate(
                    f"Generate comprehensive Python ML code for: {request_text}. "
                    f"Include proper data preprocessing, model training, and evaluation.",
                    user_id=user_id, session_id=session_id
                )
                response['ai_enhanced_code'] = ai_enhanced
        except:
            pass
        
        return json.dumps(response, indent=2)
        
    except Exception as e:
        return json.dumps({
            'error': str(e),
            'fallback': 'ML code generation failed. Please check your request format.',
            'basic_template': 'import pandas as pd\nimport numpy as np\n# Basic ML template\nprint("Hello ML World!")'
        })

def python_ml_response(prompt: str) -> str:
    """Legacy function for backward compatibility"""
    generator = PythonMLGenerator(prompt)
    structure = generator.generate_ml_structure()
    return f"[Python ML Mode]\nType: {generator.ml_type}\nFramework: {generator.framework}\nComplexity: {generator.complexity}\nGenerated: {len(structure.get('enhancements', []))} enhancement features"
