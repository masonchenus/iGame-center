from ai_models import models
PRIVILEGES = ["create_trivia"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Trivia mode: Generate trivia questions from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Generate trivia questions about: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def trivia(prompt: str) -> str:
    return f"[Trivia Mode] Trivia questions: {prompt}"
