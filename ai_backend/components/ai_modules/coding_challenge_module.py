from ai_models import models
PRIVILEGES = ["create_coding_challenge"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Coding Challenge mode: Generate coding challenges from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Create a coding challenge for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)

def coding_challenge(prompt: str) -> str:
    return f"[Coding Challenge Mode] Generated challenge for: {prompt}"
