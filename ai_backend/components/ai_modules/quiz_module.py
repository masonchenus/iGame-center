from ai_models import models
PRIVILEGES = ["create_quiz"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Quiz mode: Create quiz questions from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Create quiz questions about: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def quiz(prompt: str) -> str:
    return f"[Quiz Mode] Quiz questions created: {prompt}"
