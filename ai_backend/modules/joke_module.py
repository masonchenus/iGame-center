from ai_models import models
PRIVILEGES = ["generate_joke"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Joke mode: Generate jokes and humorous content from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Generate a joke about: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def joke(prompt: str) -> str:
    return f"[Joke Mode] Generated joke: {prompt}"
