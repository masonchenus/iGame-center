from ai_models import models
PRIVILEGES = ["generate_game_tips"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Game Tips mode: Generate gaming tips and strategies from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Generate game tips for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def game_tips(prompt: str) -> str:
    return f"[Game Tips Mode] Generated game tips for: {prompt}"
