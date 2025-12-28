from ai_models import models
PRIVILEGES = ["create_outline"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Outline mode: Create structured outlines from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Create an outline for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def outline(prompt: str) -> str:
    return f"[Outline Mode] Structured outline for: {prompt}"
