from ai_models import models
PRIVILEGES = ["suggest_names"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Naming mode: Suggest names and naming ideas from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Suggest names for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def naming(prompt: str) -> str:
    return f"[Naming Mode] Suggested names for: {prompt}"
