import os
import requests
from ai_backend.logger import usage_logger, error_logger, billing_logger

class GeminiModel:
    def __init__(self):
        self.api_key = os.getenv("GEMINI_API_KEY")
        self.base_url = "https://api.generativeai.google/v1beta2"

    def generate(self, prompt: str, user_id="unknown", session_id="unknown", max_tokens=10000) -> str:
        try:
            if not self.api_key:
                raise RuntimeError("GEMINI_API_KEY environment variable is not set")

            headers = {
                "Authorization": f"Bearer {self.api_key}",
                "Content-Type": "application/json"
            }
            payload = {"prompt": prompt, "maxOutputTokens": max_tokens}
            response = requests.post(
                f"{self.base_url}/models/text-bison:generate",
                json=payload,
                headers=headers,
                timeout=30
            )
            response.raise_for_status()
            data = response.json()

            candidates = data.get("candidates") or []
            if not candidates:
                raise RuntimeError(f"Unexpected response format, no candidates: {data}")

            result = candidates[0].get("content", "")
            # Usage log
            usage_logger.info(f"{user_id},{session_id},gemini,default_mode,{len(prompt)},{len(result)},success")

            # Billing log (example rate)
            tokens_used = len((prompt + " " + result).split())
            cost = tokens_used * 0.00005
            billing_logger.info(f"{user_id},{session_id},gemini,default_mode,{tokens_used},{cost:.6f}")

            return result
        except Exception as e:
            # Ensure we log something even if user_id/session_id are missing
            uid = user_id if user_id is not None else "unknown"
            sid = session_id if session_id is not None else "unknown"
            error_logger.error(f"{uid},{sid},gemini,default_mode,{str(e)}")
            raise
