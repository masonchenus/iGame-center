import json
import re
from typing import Dict, Any, List, Optional, Tuple
from datetime import datetime

PRIVILEGES = ["provide_assistance"]

class IntelligentHelper:
    """Advanced AI helper with multiple assistance modes and contextual understanding"""
    
    # Help categories and their specific assistance patterns
    HELP_CATEGORIES = {
        'technical': {
            'keywords': ['error', 'bug', 'code', 'debug', 'technical', 'programming', 'software'],
            'response_template': 'technical_assistance',
            'tools': ['code_review', 'debugging', 'best_practices']
        },
        'academic': {
            'keywords': ['study', 'research', 'academic', 'essay', 'assignment', 'homework'],
            'response_template': 'academic_assistance',
            'tools': ['research', 'writing', 'analysis']
        },
        'creative': {
            'keywords': ['creative', 'writing', 'story', 'poem', 'brainstorm', 'ideas'],
            'response_template': 'creative_assistance',
            'tools': ['brainstorming', 'content_creation', 'ideation']
        },
        'problem_solving': {
            'keywords': ['solve', 'problem', 'issue', 'challenge', 'difficult'],
            'response_template': 'problem_solving_assistance',
            'tools': ['step_by_step', 'alternatives', 'solutions']
        },
        'learning': {
            'keywords': ['learn', 'understand', 'explain', 'how to', 'tutorial'],
            'response_template': 'learning_assistance',
            'tools': ['explanation', 'examples', 'step_by_step']
        },
        'productivity': {
            'keywords': ['productivity', 'workflow', 'organization', 'time management', 'efficient'],
            'response_template': 'productivity_assistance',
            'tools': ['workflow_optimization', 'organization', 'efficiency']
        }
    }
    
    def __init__(self, request: str):
        self.request = request.lower().strip()
        self.request_length = len(request)
        self.detected_category = self._categorize_request()
        self.urgency_level = self._assess_urgency()
        self.complexity = self._assess_complexity()
        self.assistance_type = self._determine_assistance_type()
        
    def _categorize_request(self) -> str:
        """Categorize the help request"""
        for category, config in self.HELP_CATEGORIES.items():
            if any(keyword in self.request for keyword in config['keywords']):
                return category
        return 'general'  # Default category
    
    def _assess_urgency(self) -> str:
        """Assess urgency level of the request"""
        urgent_keywords = ['urgent', 'asap', 'immediately', 'emergency', 'critical', 'now']
        medium_keywords = ['soon', 'quickly', 'priority', 'important']
        
        if any(keyword in self.request for keyword in urgent_keywords):
            return 'high'
        elif any(keyword in self.request for keyword in medium_keywords):
            return 'medium'
        else:
            return 'normal'
    
    def _assess_complexity(self) -> str:
        """Assess complexity of the request"""
        complex_indicators = ['complex', 'advanced', 'sophisticated', 'detailed', 'comprehensive']
        simple_indicators = ['simple', 'basic', 'quick', 'easy', 'straightforward']
        
        if any(indicator in self.request for indicator in complex_indicators):
            return 'complex'
        elif any(indicator in self.request for indicator in simple_indicators):
            return 'simple'
        else:
            return 'moderate'
    
    def _determine_assistance_type(self) -> str:
        """Determine the type of assistance needed"""
        if 'step' in self.request or 'how' in self.request:
            return 'step_by_step_guide'
        elif '?' in self.request or 'what' in self.request or 'why' in self.request:
            return 'explanation'
        elif 'recommend' in self.request or 'suggest' in self.request:
            return 'recommendations'
        elif 'compare' in self.request or 'versus' in self.request:
            return 'comparison'
        else:
            return 'general_guidance'
    
    def generate_structured_response(self) -> Dict[str, Any]:
        """Generate a structured response based on analysis"""
        category_config = self.HELP_CATEGORIES.get(self.detected_category, {})
        
        response_structure = {
            'category': self.detected_category,
            'urgency': self.urgency_level,
            'complexity': self.complexity,
            'assistance_type': self.assistance_type,
            'tools_suggested': category_config.get('tools', []),
            'response_approach': self._select_response_approach()
        }
        
        # Add category-specific enhancements
        if self.detected_category == 'technical':
            response_structure['enhancements'] = [
                'Code examples and snippets',
                'Error troubleshooting steps',
                'Best practices and guidelines',
                'Alternative solutions',
                'Debugging techniques'
            ]
        elif self.detected_category == 'academic':
            response_structure['enhancements'] = [
                'Research methodology guidance',
                'Citation and referencing help',
                'Structure and organization tips',
                'Academic writing standards',
                'Source evaluation criteria'
            ]
        elif self.detected_category == 'creative':
            response_structure['enhancements'] = [
                'Creative brainstorming techniques',
                'Idea development frameworks',
                'Content structure suggestions',
                'Inspiration and examples',
                'Creative process optimization'
            ]
        
        return response_structure
    
    def _select_response_approach(self) -> str:
        """Select the best response approach based on request analysis"""
        approaches = {
            'step_by_step_guide': 'Provide a detailed step-by-step approach with clear milestones',
            'explanation': 'Give a clear, concise explanation with examples',
            'recommendations': 'Offer multiple options with pros and cons',
            'comparison': 'Present a balanced comparison with key differentiators',
            'general_guidance': 'Provide comprehensive guidance with actionable insights'
        }
        
        return approaches.get(self.assistance_type, 'Provide helpful, context-aware assistance')
    
    def generate_actionable_tips(self) -> List[str]:
        """Generate actionable tips based on the request"""
        general_tips = [
            "Break down complex problems into smaller, manageable parts",
            "Consider multiple perspectives and approaches",
            "Document your progress and learnings",
            "Don't hesitate to ask follow-up questions",
            "Validate your understanding with examples"
        ]
        
        category_specific_tips = {
            'technical': [
                "Check for typos and syntax errors first",
                "Consult official documentation",
                "Search for similar issues in community forums",
                "Test your solution with edge cases",
                "Keep your code modular and well-documented"
            ],
            'academic': [
                "Start with a clear thesis or main argument",
                "Use credible, recent sources",
                "Follow proper citation formats",
                "Structure your work logically",
                "Proofread and edit carefully"
            ],
            'creative': [
                "Start with a clear vision or theme",
                "Don't be afraid to experiment",
                "Seek inspiration from multiple sources",
                "Iterate and refine your work",
                "Get feedback from others"
            ]
        }
        
        tips = general_tips.copy()
        if self.detected_category in category_specific_tips:
            tips.extend(category_specific_tips[self.detected_category])
        
        return tips[:5]  # Return top 5 most relevant tips

def create_enhanced_helper_response(analysis: IntelligentHelper) -> str:
    """Create an enhanced helper response"""
    structure = analysis.generate_structured_response()
    tips = analysis.generate_actionable_tips()
    
    # Create header
    header = f"""# 🤖 Enhanced AI Assistant Response
**Category**: {structure['category'].title()}
**Urgency**: {structure['urgency'].title()}
**Complexity**: {structure['complexity'].title()}
**Assistance Type**: {structure['assistance_type'].replace('_', ' ').title()}

"""
    
    # Main response section
    main_response = f"""
## 📋 **Your Request Analysis**
- **What you need**: {structure['response_approach']}
- **Recommended tools**: {', '.join(structure['tools_suggested']) if structure['tools_suggested'] else 'General guidance'}

## 💡 **Enhanced Guidance**
Based on your request "{analysis.request}", here's comprehensive assistance:

"""
    
    # Add enhancements based on category
    if 'enhancements' in structure:
        enhancements_section = f"""
## 🎯 **Specialized Enhancements**
{chr(10).join(f"• {enhancement}" for enhancement in structure['enhancements'])}
"""
    else:
        enhancements_section = ""
    
    # Add actionable tips
    tips_section = f"""
## 🛠️ **Actionable Tips**
{chr(10).join(f"{i+1}. {tip}" for i, tip in enumerate(tips))}
"""
    
    # Add follow-up suggestions
    follow_up = f"""
## 🔄 **Next Steps**
1. Try the suggestions above and let me know how it goes
2. If you need more specific help, provide additional details
3. Consider breaking your request into smaller, focused questions
4. Don't hesitate to ask for clarification on any point

---
*Generated by Enhanced AI Helper at {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}*
"""
    
    return header + main_response + enhancements_section + tips_section + follow_up

def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Enhanced Helper mode: Provide intelligent, context-aware assistance with multiple approaches
    """
    try:
        # Parse input data
        if isinstance(input_data, str):
            request_text = input_data
        else:
            request_text = str(input_data)
        
        # Create intelligent helper instance
        helper = IntelligentHelper(request_text)
        
        # Generate enhanced response
        enhanced_response = create_enhanced_helper_response(helper)
        structure = helper.generate_structured_response()
        tips = helper.generate_actionable_tips()
        
        # Create comprehensive response
        response = {
            'request': request_text,
            'analysis': {
                'category': helper.detected_category,
                'urgency_level': helper.urgency_level,
                'complexity_assessment': helper.complexity,
                'assistance_type': helper.assistance_type
            },
            'enhanced_response': enhanced_response,
            'response_structure': structure,
            'actionable_tips': tips,
            'suggested_tools': structure['tools_suggested'],
            'response_approach': structure['response_approach']
        }
        
        # Try to enhance with AI model if available
        try:
            from ai_backend.ai_models import models
            model = models.get(model_name)
            if model:
                ai_enhanced = model.generate(
                    f"Provide comprehensive help and guidance for: {request_text}. "
                    f"Be specific, actionable, and helpful.",
                    user_id=user_id, session_id=session_id
                )
                response['ai_enhanced_guidance'] = ai_enhanced
        except:
            pass
        
        return json.dumps(response, indent=2)
        
    except Exception as e:
        return json.dumps({
            'error': str(e),
            'fallback': 'Helper assistance failed. Please check your request format.',
            'basic_help': 'I\'m here to help! Please describe what you need assistance with in more detail.'
        })

def helper_response(prompt: str) -> str:
    """Legacy function for backward compatibility"""
    helper = IntelligentHelper(prompt)
    structure = helper.generate_structured_response()
    return f"[Enhanced Helper Mode]\nCategory: {helper.detected_category}\nType: {helper.assistance_type}\nTips: {len(helper.generate_actionable_tips())} provided"
