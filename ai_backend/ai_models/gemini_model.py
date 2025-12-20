import os
import requests
from ai_backend.logger import usage_logger, error_logger, billing_logger

class GeminiModel:
    def __init__(self):
        self.api_key = os.getenv("GEMINI_API_KEY")
        self.base_url = "https://api.generativeai.google/v1beta2"

    def generate(self, prompt: str, user_id="[<user_id>]", session_id="[<session_id>]", max_tokens=10000) -> str:
        try:
            headers = {"Authorization": f"Bearer {self.api_key}"}
            payload = {"prompt": prompt, "maxOutputTokens": max_tokens}
            response = requests.post(f"{self.base_url}/models/text-bison:generate", json=payload, headers=headers)
            response.raise_for_status()
            result = response.json()["candidates"][0]["content"]

            # Usage log
            usage_logger.info(f"{user_id},{session_id},gemini,[<mode_name>],{len(prompt)},{len(result)},success")

            # Billing log (example rate)
            tokens_used = len(result.split())
            cost = tokens_used * 0.00005
            billing_logger.info(f"{user_id},{session_id},gemini,[<mode_name>],{tokens_used},{cost:.6f}")

            return result
        except Exception as e:
            error_logger.error(f"{user_id},{session_id},gemini,[<mode_name>],{str(e)}")
            raise
