def generate_text(prompt: str, model_name: str) -> str:
    """
    Simulates generating text using a large language model.

    In a real application, this function would contain the logic to call the
    API of the selected model provider (e.g., OpenAI, Google Gemini).
    """
    if model_name not in ["gemini-pro", "gemini-fast", "chatgpt-3.5"]:
        return f"Error: Model '{model_name}' is not a valid option."

    # Simulate a response from the selected model
    return f"Response from {model_name} for prompt: '{prompt}'"
