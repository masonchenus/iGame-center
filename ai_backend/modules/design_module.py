from ai_models import models
PRIVILEGES = ["generate_design"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Design mode: Generate design ideas and suggestions from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Generate design ideas for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def design(prompt: str) -> str:
    return f"[Design Mode] Generated design ideas: {prompt}"
