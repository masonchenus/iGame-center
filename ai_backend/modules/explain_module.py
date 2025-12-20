from ai_models import models
PRIVILEGES = ["explain_concepts"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Explanation mode: Provide detailed explanations for user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Explain the following: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def explain(prompt: str) -> str:
    return f"[Explain Mode] Explanation for: {prompt}"
