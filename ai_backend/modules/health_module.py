from ai_models import models
PRIVILEGES = ["provide_health_info"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Health mode: Provide health and wellness information from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Provide wellness information about: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def health(prompt: str) -> str:
    return f"[Health Mode] Provided wellness info for: {prompt}"
