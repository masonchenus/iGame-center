import os

class ChatGPTModel:
    def __init__(self):
        self.api_key = os.getenv("OPENAI_API_KEY")
        self.available = bool(self.api_key)
        
        if not self.api_key:
            print("Warning: OPENAI_API_KEY not found in environment. Using fallback response.")

    def generate(self, prompt: str) -> str:
        if not self.available:
            return f"[chatgpt-fallback] This is a simulated response for demo purposes. Prompt: {prompt}"
        
        try:
            import openai
            openai.api_key = self.api_key
            # HTTPS request handled internally by OpenAI SDK
            completion = openai.ChatCompletion.create(
                model="gpt-4",
                messages=[{"role": "user", "content": prompt}]
            )
            return completion.choices[0].message["content"]
        except ImportError:
            return f"[chatgpt-fallback] OpenAI library not installed. Prompt: {prompt}"
        except Exception as e:
            return f"[chatgpt-error] {str(e)}"
