try:
    from .chatgpt_model import ChatGPTModel
except Exception:
    class ChatGPTModel:
        def __init__(self):
            self.name = "chatgpt-shim"

        def generate(self, prompt: str) -> str:
            return f"[chatgpt-shim] {prompt}"

try:
    from .claude_model import ClaudeModel
except Exception:
    class ClaudeModel:
        def __init__(self):
            self.name = "claude-shim"

        def generate(self, prompt: str) -> str:
            return f"[claude-shim] {prompt}"

try:
    from .gemini_model import GeminiModel
except Exception:
    class GeminiModel:
        def __init__(self):
            self.name = "gemini-shim"

        def generate(self, prompt: str) -> str:
            return f"[gemini-shim] {prompt}"

models = {
    "chatgpt": ChatGPTModel,
    "gemini": GeminiModel,
    "claude": ClaudeModel,
}