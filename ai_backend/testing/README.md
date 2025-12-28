# AI Backend Tests

This directory contains comprehensive unit tests for the AI backend modules.

## Running Tests

### Run all tests:
\`\`\`bash
cd ai_bakend/tests
python -m unittest discover
\`\`\`

### Run a specific test file:
\`\`\`bash
python -m unittest test_llm_provider
python -m unittest test_privileges
python -m unittest test_quiz_generator
python -m unittest test_slides_generator
\`\`\`

### Run with verbose output:
\`\`\`bash
python -m unittest discover -v
\`\`\`

## Test Coverage

- **test_llm_provider.py**: Tests for the LLM provider module
  - Valid and invalid model names
  - Prompt handling with special characters
  - Response format validation
  - Edge cases and error handling

- **test_privileges.py**: Tests for the user privileges module
  - Permission checks for all user roles (admin, editor, viewer)
  - Invalid user and permission handling
  - Case sensitivity tests
  - Edge cases with whitespace and None values

- **test_quiz_generator.py**: Tests for the quiz generator module
  - Quiz creation with various topics
  - Question structure validation
  - Multiple-choice and open-ended questions
  - Answer evaluation functionality
  - LLM integration mocking

- **test_slides_generator.py**: Tests for the slides generator module
  - Slide creation with permission checks
  - PowerPoint presentation generation
  - Download and present functionality
  - User role authorization
  - Special character handling

## Dependencies

Tests use Python's built-in `unittest` framework and `unittest.mock` for mocking.
The only external dependency is `python-pptx` for the slides generator.

Install dependencies:
\`\`\`bash
pip install -r requirements.txt
\`\`\`