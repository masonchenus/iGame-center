from ai_models import models
PRIVILEGES = ["generate_lyrics"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Lyrics mode: Generate song lyrics from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Generate lyrics about: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def lyrics(prompt: str) -> str:
    return f"[Lyrics Mode] Generated lyrics: {prompt}"
