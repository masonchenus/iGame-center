from ai_models import models
PRIVILEGES = ["suggest_productivity_tips"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Productivity mode: Suggest productivity tips and strategies from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Suggest productivity tips for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def productivity(prompt: str) -> str:
    return f"[Productivity Mode] Suggested productivity tips for: {prompt}"
