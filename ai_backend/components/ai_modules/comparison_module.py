from ai_models import models
PRIVILEGES = ["compare_items"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Comparison mode: Compare items or concepts from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Compare the following items: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def comparison(prompt: str) -> str:
    return f"[Comparison Mode] Compared items: {prompt}"
