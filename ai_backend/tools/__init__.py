# AI Tools System
# Comprehensive collection of 120+ AI-powered tools for various tasks

try:
    from .code_modifier import CodeModifier
    from .file_analyzer import FileAnalyzer
    from .code_generator import CodeGenerator
    from .code_optimizer import CodeOptimizer
    TOOLS_AVAILABLE = True
except ImportError as e:
    print(f"Warning: Some AI tools could not be imported: {e}")
    TOOLS_AVAILABLE = False

__all__ = [
    'CodeModifier',
    'FileAnalyzer', 
    'CodeGenerator',
    'CodeOptimizer'
]

# Comprehensive Tool Registry - 120+ tools matching frontend AVAILABLE_MODULES
TOOLS_REGISTRY = {
    # Development (15+ tools)
    'codegen_module': 'CodeGenerator',
    'code_review_module': 'CodeModifier',
    'debug_module': 'CodeModifier',
    'documentation_module': 'CodeGenerator',
    'python_ml_module': 'CodeGenerator',
    'javascript_ts_module': 'CodeGenerator',
    'go_module': 'CodeGenerator',
    'rust_module': 'CodeGenerator',
    'java_module': 'CodeGenerator',
    'csharp_module': 'CodeGenerator',
    'swift_module': 'CodeGenerator',
    'kotlin_module': 'CodeGenerator',
    'php_module': 'CodeGenerator',
    'ruby_module': 'CodeGenerator',
    'sql_module': 'CodeGenerator',

    # Analysis (10+ tools)
    'data_analysis_module': 'FileAnalyzer',
    'stats_analyzer_module': 'FileAnalyzer',
    'comparison_module': 'FileAnalyzer',
    'fact_check_module': 'FileAnalyzer',
    'trend_analysis_module': 'FileAnalyzer',
    'sentiment_analysis_module': 'FileAnalyzer',
    'risk_assessment_module': 'FileAnalyzer',
    'competitor_analysis_module': 'FileAnalyzer',
    'performance_analysis_module': 'FileAnalyzer',
    'anomaly_detection_module': 'FileAnalyzer',

    # Creativity (12+ tools)
    'creativity_module': 'CodeGenerator',
    'brainstorming_module': 'CodeGenerator',
    'design_module': 'CodeGenerator',
    'poem_module': 'CodeGenerator',
    'story_module': 'CodeGenerator',
    'music_composition_module': 'CodeGenerator',
    'art_generator_module': 'CodeGenerator',
    'logo_design_module': 'CodeGenerator',
    'video_script_module': 'CodeGenerator',
    'podcast_script_module': 'CodeGenerator',
    'game_story_module': 'CodeGenerator',
    'character_development_module': 'CodeGenerator',

    # Productivity (15+ tools)
    'productivity_module': 'CodeGenerator',
    'planning_module': 'CodeGenerator',
    'checklist_module': 'CodeGenerator',
    'optimization_module': 'CodeOptimizer',
    'time_management_module': 'CodeGenerator',
    'project_management_module': 'CodeGenerator',
    'task_automation_module': 'CodeGenerator',
    'workflow_design_module': 'CodeGenerator',
    'meeting_agenda_module': 'CodeGenerator',
    'report_generator_module': 'CodeGenerator',
    'email_composer_module': 'CodeGenerator',
    'calendar_optimization_module': 'CodeGenerator',
    'goal_setting_module': 'CodeGenerator',
    'habit_formation_module': 'CodeGenerator',
    'resource_planning_module': 'CodeGenerator',

    # Business & Finance (8+ tools)
    'business_plan_module': 'CodeGenerator',
    'financial_analysis_module': 'FileAnalyzer',
    'market_research_module': 'FileAnalyzer',
    'investment_analysis_module': 'FileAnalyzer',
    'budget_planning_module': 'CodeGenerator',
    'pitch_deck_module': 'CodeGenerator',
    'competitive_analysis_module': 'FileAnalyzer',
    'roi_calculator_module': 'CodeGenerator',

    # Entertainment (8+ tools)
    'game_generator': 'CodeGenerator',
    'joke_module': 'CodeGenerator',
    'trivia_module': 'CodeGenerator',
    'riddle_module': 'CodeGenerator',
    'quiz_generator_module': 'CodeGenerator',
    'party_games_module': 'CodeGenerator',
    'movie_recommendations_module': 'CodeGenerator',
    'book_recommendations_module': 'CodeGenerator',

    # Communication (10+ tools)
    'dialogue_module': 'CodeGenerator',
    'translator_module': 'CodeGenerator',
    'summarizer_module': 'CodeGenerator',
    'presentation_module': 'CodeGenerator',
    'social_media_module': 'CodeGenerator',
    'press_release_module': 'CodeGenerator',
    'customer_service_module': 'CodeGenerator',
    'negotiation_module': 'CodeGenerator',
    'conflict_resolution_module': 'CodeGenerator',
    'public_speaking_module': 'CodeGenerator',

    # Education (12+ tools)
    'explain_module': 'CodeGenerator',
    'quiz_module': 'CodeGenerator',
    'test_creator_module': 'CodeGenerator',
    'math_explain_module': 'CodeGenerator',
    'science_explain_module': 'CodeGenerator',
    'history_explain_module': 'CodeGenerator',
    'language_learning_module': 'CodeGenerator',
    'study_guide_module': 'CodeGenerator',
    'lesson_plan_module': 'CodeGenerator',
    'research_assistant_module': 'CodeGenerator',
    'academic_writing_module': 'CodeGenerator',
    'learning_assessment_module': 'FileAnalyzer',

    # Specialized (15+ tools)
    'diagram_module': 'CodeGenerator',
    'slides_generator': 'CodeGenerator',
    'health_module': 'CodeGenerator',
    'recommendation_module': 'CodeGenerator',
    'seo_optimization_module': 'CodeGenerator',
    'legal_assistance_module': 'CodeGenerator',
    'medical_advice_module': 'CodeGenerator',
    'nutrition_planning_module': 'CodeGenerator',
    'fitness_planning_module': 'CodeGenerator',
    'travel_planning_module': 'CodeGenerator',
    'real_estate_module': 'CodeGenerator',
    'tax_assistance_module': 'CodeGenerator',
    'insurance_analysis_module': 'FileAnalyzer',
    'career_counseling_module': 'CodeGenerator',
    'relationship_advice_module': 'CodeGenerator',

    # Technical & Engineering (10+ tools)
    'api_design_module': 'CodeGenerator',
    'database_design_module': 'CodeGenerator',
    'system_architecture_module': 'CodeGenerator',
    'security_audit_module': 'FileAnalyzer',
    'performance_optimization_module': 'CodeOptimizer',
    'cloud_architecture_module': 'CodeGenerator',
    'devops_automation_module': 'CodeGenerator',
    'testing_strategy_module': 'CodeGenerator',
    'code_review_automation_module': 'CodeModifier',
    'microservices_design_module': 'CodeGenerator',

    # Testing & Quality (5+ tools)
    'trojan_test_module': 'CodeModifier',
    'unit_testing_module': 'CodeGenerator',
    'integration_testing_module': 'CodeGenerator',
    'performance_testing_module': 'FileAnalyzer',
    'security_testing_module': 'FileAnalyzer',

    # Original 4 tools
    'code_modifier': 'CodeModifier',
    'file_analyzer': 'FileAnalyzer',
    'code_generator': 'CodeGenerator',
    'code_optimizer': 'CodeOptimizer'
}

def get_tool(tool_name):
    """Get a tool by name"""
    if not TOOLS_AVAILABLE:
        return None
    return globals().get(TOOLS_REGISTRY.get(tool_name))

def list_tools():
    """List all available tools"""
    return list(TOOLS_REGISTRY.keys()) if TOOLS_AVAILABLE else []

def execute_tool(tool_name, **kwargs):
    """Execute a tool with given parameters"""
    if not TOOLS_AVAILABLE:
        return {"error": "AI Tools system not available"}
    
    tool_class = get_tool(tool_name)
    if tool_class:
        tool_instance = tool_class()
        return tool_instance.execute(**kwargs)
    else:
        return {"error": f"Tool '{tool_name}' not found"}
