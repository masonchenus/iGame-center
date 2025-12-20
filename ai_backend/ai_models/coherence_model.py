import os
import cohere  # pip install cohere

class CohereModel:
    def __init__(self, model_name="xlarge"):
        # Load API key from environment
        self.api_key = os.getenv("COHERE_API_KEY")
        if not self.api_key:
            raise ValueError("COHERE_API_KEY not found in environment")
        
        # Initialize Cohere client (HTTPS requests handled internally)
        self.client = cohere.Client(self.api_key)
        self.model_name = model_name

    def generate(self, prompt: str, max_tokens: int = 200) -> str:
        """
        Generates text using the specified Cohere model.
        """
        response = self.client.generate(
            model=self.model_name,
            prompt=prompt,
            max_tokens=max_tokens
        )
        # Cohere returns a list of generations; we take the first
        return response.generations[0].text
