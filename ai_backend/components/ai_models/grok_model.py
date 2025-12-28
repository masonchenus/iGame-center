import os
import requests

class GrokModel:
    def __init__(self):
        self.api_key = os.getenv("GROK_API_KEY")
        self.available = bool(self.api_key)
        self.base_url = "https://api.x.ai/v1"
        
        if not self.api_key:
            print("Warning: GROK_API_KEY not found in environment. Using fallback response.")
    
    def generate(self, prompt: str) -> str:
        if not self.available:
            return f"[grok-fallback] This is a simulated response for demo purposes. Prompt: {prompt}"
        
        headers = {"Authorization": f"Bearer {self.api_key}"}
        payload = {"model": "grok-4-1-fast-non-reasoning", "messages": [{"role": "user", "content": prompt}]}
        response = requests.post(f"{self.base_url}/chat/completions", json=payload, headers=headers)
        response.raise_for_status()
        return response.json().get("choices", [{}])[0].get("message", {}).get("content", "")
