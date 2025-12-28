import unittest
import sys
import os
from unittest.mock import patch, MagicMock

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from modules.quiz_generator import create_quiz, evaluate_response


class TestQuizGenerator(unittest.TestCase):
    """Comprehensive tests for quiz_generator module."""

    @patch('modules.quiz_generator.llm_provider.generate_text')
    def test_create_quiz_basic_topic(self, mock_generate):
        """Test create_quiz with a basic topic."""
        mock_generate.return_value = "Mocked LLM response"
        topic = "Python Programming"
        
        result = create_quiz(topic)
        
        self.assertIsInstance(result, dict)
        self.assertEqual(result["topic"], topic)
        self.assertIn("questions", result)
        mock_generate.assert_called_once()

    @patch('modules.quiz_generator.llm_provider.generate_text')
    def test_create_quiz_structure(self, mock_generate):
        """Test create_quiz returns correct structure."""
        mock_generate.return_value = "Mocked response"
        topic = "Mathematics"
        
        result = create_quiz(topic)
        
        # Check top-level structure
        self.assertIn("topic", result)
        self.assertIn("questions", result)
        self.assertIsInstance(result["questions"], list)
        self.assertEqual(len(result["questions"]), 3)

    @patch('modules.quiz_generator.llm_provider.generate_text')
    def test_create_quiz_question_types(self, mock_generate):
        """Test create_quiz generates correct question types."""
        mock_generate.return_value = "Mocked response"
        topic = "History"
        
        result = create_quiz(topic)
        questions = result["questions"]
        
        # Should have 2 multiple-choice and 1 open-ended
        mc_count = sum(1 for q in questions if q["type"] == "multiple-choice")
        oe_count = sum(1 for q in questions if q["type"] == "open-ended")
        
        self.assertEqual(mc_count, 2)
        self.assertEqual(oe_count, 1)

    @patch('modules.quiz_generator.llm_provider.generate_text')
    def test_create_quiz_multiple_choice_structure(self, mock_generate):
        """Test multiple-choice questions have correct structure."""
        mock_generate.return_value = "Mocked response"
        topic = "Geography"
        
        result = create_quiz(topic)
        mc_questions = [q for q in result["questions"] if q["type"] == "multiple-choice"]
        
        for question in mc_questions:
            self.assertIn("type", question)
            self.assertIn("question", question)
            self.assertIn("options", question)
            self.assertIn("answer", question)
            self.assertIsInstance(question["options"], list)
            self.assertIsInstance(question["question"], str)

    @patch('modules.quiz_generator.llm_provider.generate_text')
    def test_create_quiz_open_ended_structure(self, mock_generate):
        """Test open-ended questions have correct structure."""
        mock_generate.return_value = "Mocked response"
        topic = "Science"
        
        result = create_quiz(topic)
        oe_questions = [q for q in result["questions"] if q["type"] == "open-ended"]
        
        for question in oe_questions:
            self.assertIn("type", question)
            self.assertIn("question", question)
            self.assertNotIn("options", question)
            self.assertNotIn("answer", question)

    @patch('modules.quiz_generator.llm_provider.generate_text')
    def test_create_quiz_topic_in_questions(self, mock_generate):
        """Test that topic appears in generated questions."""
        mock_generate.return_value = "Mocked response"
        topic = "France"
        
        result = create_quiz(topic)
        
        # Check that topic is mentioned in questions
        for question in result["questions"]:
            self.assertIn(topic, question["question"])

    @patch('modules.quiz_generator.llm_provider.generate_text')
    def test_create_quiz_calls_llm_with_correct_prompt(self, mock_generate):
        """Test create_quiz calls LLM with properly formatted prompt."""
        mock_generate.return_value = "Mocked response"
        topic = "Artificial Intelligence"
        
        create_quiz(topic)
        
        call_args = mock_generate.call_args
        prompt = call_args[0][0]
        model = call_args[0][1]
        
        self.assertIn(topic, prompt)
        self.assertIn("2 multiple-choice", prompt)
        self.assertIn("1 open-ended", prompt)
        self.assertEqual(model, "gemini-pro")

    @patch('modules.quiz_generator.llm_provider.generate_text')
    def test_create_quiz_with_empty_topic(self, mock_generate):
        """Test create_quiz with empty topic."""
        mock_generate.return_value = "Mocked response"
        topic = ""
        
        result = create_quiz(topic)
        
        self.assertIsInstance(result, dict)
        self.assertEqual(result["topic"], topic)

    @patch('modules.quiz_generator.llm_provider.generate_text')
    def test_create_quiz_with_special_characters_topic(self, mock_generate):
        """Test create_quiz with special characters in topic."""
        mock_generate.return_value = "Mocked response"
        special_topics = [
            "C++ Programming",
            "HTML & CSS",
            "Node.js",
            "Test-Driven Development",
            "Machine Learning (ML)"
        ]
        
        for topic in special_topics:
            result = create_quiz(topic)
            self.assertEqual(result["topic"], topic)

    @patch('modules.quiz_generator.llm_provider.generate_text')
    def test_create_quiz_multiple_choice_options_format(self, mock_generate):
        """Test multiple-choice options are properly formatted."""
        mock_generate.return_value = "Mocked response"
        topic = "Chemistry"
        
        result = create_quiz(topic)
        mc_questions = [q for q in result["questions"] if q["type"] == "multiple-choice"]
        
        # First MC question should have 4 options (A, B, C, D)
        self.assertEqual(mc_questions[0]["options"], ["A", "B", "C", "D"])
        self.assertIn(mc_questions[0]["answer"], ["A", "B", "C", "D"])
        
        # Second MC question should have specific options
        self.assertEqual(len(mc_questions[1]["options"]), 4)

    @patch('modules.quiz_generator.llm_provider.generate_text')
    def test_evaluate_response_basic(self, mock_generate):
        """Test evaluate_response with basic inputs."""
        mock_generate.return_value = "Good answer, well explained."
        question = "What is photosynthesis?"
        answer = "It's the process plants use to make food from sunlight."
        
        result = evaluate_response(question, answer)
        
        self.assertIsInstance(result, str)
        self.assertEqual(result, "Good answer, well explained.")
        mock_generate.assert_called_once()

    @patch('modules.quiz_generator.llm_provider.generate_text')
    def test_evaluate_response_calls_llm_correctly(self, mock_generate):
        """Test evaluate_response calls LLM with correct prompt."""
        mock_generate.return_value = "Evaluation result"
        question = "Explain gravity"
        answer = "Gravity is a force"
        
        evaluate_response(question, answer)
        
        call_args = mock_generate.call_args
        prompt = call_args[0][0]
        model = call_args[0][1]
        
        self.assertIn("Evaluate", prompt)
        self.assertIn(question, prompt)
        self.assertIn(answer, prompt)
        self.assertEqual(model, "gemini-pro")

    @patch('modules.quiz_generator.llm_provider.generate_text')
    def test_evaluate_response_empty_answer(self, mock_generate):
        """Test evaluate_response with empty answer."""
        mock_generate.return_value = "No answer provided"
        question = "What is DNA?"
        answer = ""
        
        result = evaluate_response(question, answer)
        
        self.assertIsInstance(result, str)
        mock_generate.assert_called_once()

    @patch('modules.quiz_generator.llm_provider.generate_text')
    def test_evaluate_response_long_answer(self, mock_generate):
        """Test evaluate_response with very long answer."""
        mock_generate.return_value = "Comprehensive answer"
        question = "Explain the theory of relativity"
        answer = "A" * 5000  # Very long answer
        
        result = evaluate_response(question, answer)
        
        self.assertIsInstance(result, str)

    @patch('modules.quiz_generator.llm_provider.generate_text')
    def test_evaluate_response_special_characters(self, mock_generate):
        """Test evaluate_response with special characters."""
        mock_generate.return_value = "Valid evaluation"
        question = "What is <script>?"
        answer = "It's an HTML tag with \"quotes\" and 'apostrophes'"
        
        result = evaluate_response(question, answer)
        
        self.assertIsInstance(result, str)

    @patch('modules.quiz_generator.llm_provider.generate_text')
    def test_create_quiz_multiple_calls_consistency(self, mock_generate):
        """Test create_quiz produces consistent structure across calls."""
        mock_generate.return_value = "Mocked response"
        topics = ["Topic1", "Topic2", "Topic3"]
        
        results = [create_quiz(topic) for topic in topics]
        
        # All should have same structure
        for result in results:
            self.assertIn("topic", result)
            self.assertIn("questions", result)
            self.assertEqual(len(result["questions"]), 3)


if __name__ == '__main__':
    unittest.main()