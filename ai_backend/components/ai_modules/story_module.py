from ai_models import models
PRIVILEGES = ["generate_story"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Story mode: Generate stories and narratives from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Generate a story about: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def story(prompt: str) -> str:
    return f"[Story Mode] Generated story: {prompt}"
