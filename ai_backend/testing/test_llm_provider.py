import unittest
import sys
import os

# Add parent directory to path to import modules
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from modules.llm_provider import generate_text


class TestLLMProvider(unittest.TestCase):
    """Comprehensive tests for llm_provider module."""

    def test_generate_text_with_valid_gemini_pro(self):
        """Test generate_text with valid gemini-pro model."""
        prompt = "What is the weather today?"
        model = "gemini-pro"
        result = generate_text(prompt, model)
        
        self.assertIsInstance(result, str)
        self.assertIn("gemini-pro", result)
        self.assertIn(prompt, result)
        self.assertTrue(result.startswith("Response from gemini-pro"))

    def test_generate_text_with_valid_gemini_fast(self):
        """Test generate_text with valid gemini-fast model."""
        prompt = "Explain quantum physics"
        model = "gemini-fast"
        result = generate_text(prompt, model)
        
        self.assertIsInstance(result, str)
        self.assertIn("gemini-fast", result)
        self.assertIn(prompt, result)
        self.assertTrue(result.startswith("Response from gemini-fast"))

    def test_generate_text_with_valid_chatgpt(self):
        """Test generate_text with valid chatgpt-3.5 model."""
        prompt = "Write a poem"
        model = "chatgpt-3.5"
        result = generate_text(prompt, model)
        
        self.assertIsInstance(result, str)
        self.assertIn("chatgpt-3.5", result)
        self.assertIn(prompt, result)
        self.assertTrue(result.startswith("Response from chatgpt-3.5"))

    def test_generate_text_with_invalid_model(self):
        """Test generate_text returns error for invalid model."""
        prompt = "Hello world"
        invalid_models = ["gpt-4", "claude", "llama", "", "invalid-model"]
        
        for model in invalid_models:
            result = generate_text(prompt, model)
            self.assertIsInstance(result, str)
            self.assertIn("Error", result)
            self.assertIn(model, result)
            self.assertIn("not a valid option", result)

    def test_generate_text_with_empty_prompt(self):
        """Test generate_text with empty prompt."""
        prompt = ""
        model = "gemini-pro"
        result = generate_text(prompt, model)
        
        self.assertIsInstance(result, str)
        self.assertIn("gemini-pro", result)
        self.assertIn(prompt, result)

    def test_generate_text_with_long_prompt(self):
        """Test generate_text with very long prompt."""
        prompt = "A" * 10000  # Very long prompt
        model = "gemini-pro"
        result = generate_text(prompt, model)
        
        self.assertIsInstance(result, str)
        self.assertIn("gemini-pro", result)

    def test_generate_text_with_special_characters(self):
        """Test generate_text with special characters in prompt."""
        special_prompts = [
            "Hello <script>alert('xss')</script>",
            "Test with \n newlines \t tabs",
            "Emojis 🚀 and symbols @#$%",
            "Unicode: 你好世界 مرحبا العالم",
            "Single quotes ' and double quotes \"",
        ]
        model = "gemini-pro"
        
        for prompt in special_prompts:
            result = generate_text(prompt, model)
            self.assertIsInstance(result, str)
            self.assertIn(prompt, result)

    def test_generate_text_case_sensitivity(self):
        """Test that model names are case-sensitive."""
        prompt = "Test prompt"
        invalid_cases = ["Gemini-Pro", "GEMINI-PRO", "ChatGPT-3.5", "CHATGPT-3.5"]
        
        for model in invalid_cases:
            result = generate_text(prompt, model)
            self.assertIn("Error", result)
            self.assertIn("not a valid option", result)

    def test_generate_text_whitespace_handling(self):
        """Test generate_text with whitespace in model names."""
        prompt = "Test"
        models_with_whitespace = [" gemini-pro", "gemini-pro ", " gemini-pro "]
        
        for model in models_with_whitespace:
            result = generate_text(prompt, model)
            # Should fail because whitespace makes it invalid
            self.assertIn("Error", result)

    def test_generate_text_response_format(self):
        """Test that response follows expected format."""
        prompt = "Test prompt"
        valid_models = ["gemini-pro", "gemini-fast", "chatgpt-3.5"]
        
        for model in valid_models:
            result = generate_text(prompt, model)
            # Check format: "Response from {model} for prompt: '{prompt}'"
            self.assertTrue(result.startswith(f"Response from {model}"))
            self.assertIn("for prompt:", result)
            self.assertIn(f"'{prompt}'", result)

    def test_generate_text_all_valid_models(self):
        """Test all valid models are supported."""
        prompt = "Test"
        valid_models = ["gemini-pro", "gemini-fast", "chatgpt-3.5"]
        
        for model in valid_models:
            result = generate_text(prompt, model)
            self.assertNotIn("Error", result)
            self.assertIn(model, result)


if __name__ == '__main__':
    unittest.main()