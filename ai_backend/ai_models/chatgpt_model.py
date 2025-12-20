import os
import openai  # pip install openai

class ChatGPTModel:
    def __init__(self):
        self.api_key = os.getenv("OPENAI_API_KEY")
        if not self.api_key:
            raise ValueError("OPENAI_API_KEY not found in environment")
        openai.api_key = self.api_key

    def generate(self, prompt: str) -> str:
        # HTTPS request handled internally by OpenAI SDK
        completion = openai.ChatCompletion.create(
            model="gpt-4",
            messages=[{"role": "user", "content": prompt}]
        )
        return completion.choices[0].message["content"]
