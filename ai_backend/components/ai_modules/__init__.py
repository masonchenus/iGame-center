# Expose submodules so `from ai_backend.modules import <name>` works.
from . import (
	game_generator, math_solver, slides_generator, helper_module, agent_module,
	tester_module, codegen_module, explain_module, summarizer_module, story_module,
	dialogue_module, brainstorming_module, outline_module, quiz_module, trivia_module,
	fact_check_module, translator_module, lyrics_module, poem_module, joke_module,
	cheat_sheet_module, code_review_module, optimization_module, design_module, diagram_module,
	debug_module, math_explain_module, data_analysis_module, recommendation_module, productivity_module,
	health_module, research_module, coding_challenge_module, game_tips_module, ranking_module,
	simulation_module, test_creator_module, logic_puzzle_module, riddle_module, comparison_module,
	planning_module, checklist_module, prompt_idea_module, generator_module, naming_module,
	stats_analyzer_module, visualization_module, feedback_module, creativity_module, documentation_module
)

__all__ = [
	'game_generator', 'math_solver', 'slides_generator', 'helper_module', 'agent_module',
	'tester_module', 'codegen_module', 'explain_module', 'summarizer_module', 'story_module',
	'brainstorming_module', 'outline_module', 'quiz_module', 'trivia_module',
	'fact_check_module', 'translator_module', 'lyrics_module', 'poem_module', 'joke_module',
	'cheat_sheet_module', 'code_review_module', 'optimization_module', 'design_module', 'diagram_module',
	'debug_module', 'math_explain_module', 'data_analysis_module', 'recommendation_module', 'productivity_module',
	'health_module', 'research_module', 'coding_challenge_module', 'game_tips_module', 'ranking_module',
	'simulation_module', 'test_creator_module', 'logic_puzzle_module', 'riddle_module', 'comparison_module',
	'planning_module', 'checklist_module', 'prompt_idea_module', 'generator_module', 'naming_module',
	'stats_analyzer_module', 'visualization_module', 'feedback_module', 'creativity_module', 'documentation_module'
]
