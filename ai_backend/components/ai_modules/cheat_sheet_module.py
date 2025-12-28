from ai_models import models
PRIVILEGES = ["create_cheat_sheet"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Cheat Sheet mode: Create a comprehensive cheat sheet from the user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Create a cheat sheet for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def cheat_sheet(prompt: str) -> str:
    return f'[Cheat-Sheet Mode] Created cheat sheet for "{prompt}"'
