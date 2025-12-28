from ai_backend.ai_models import models
PRIVILEGES = ["generate_game"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Game Generator mode: Generate game content and game-related content from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Generate game content for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def game(prompt: str) -> str:
    return f"[Game Mode] Generated game content for: {prompt}"
