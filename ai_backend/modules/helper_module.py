from ai_models import models
PRIVILEGES = ["provide_assistance"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Helper mode: Provide helpful assistance and guidance for user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Provide help with: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def helper_response(prompt: str) -> str:
    return f"[Helper Mode] Helped with prompt: {prompt}"
