import os
import requests

class ClaudeModel:
    def __init__(self):
        self.api_key = os.getenv("CLAUDE_API_KEY")
        if not self.api_key:
            raise ValueError("CLAUDE_API_KEY not found in environment")
        self.base_url = "https://api.anthropic.com/v1"

    def generate(self, prompt: str, max_tokens: int = 200) -> str:
        headers = {"x-api-key": self.api_key, "Content-Type": "application/json"}
        payload = {"model": "claude-v1", "prompt": prompt, "max_tokens_to_sample": max_tokens}
        response = requests.post(f"{self.base_url}/complete", json=payload, headers=headers)
        response.raise_for_status()
        return response.json()["completion"]
