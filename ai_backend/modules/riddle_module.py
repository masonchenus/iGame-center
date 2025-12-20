from ai_models import models
PRIVILEGES = ["create_riddle"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Riddle mode: Generate riddles from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Generate a riddle about: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def riddle(prompt: str) -> str:
    return f"[Riddle Mode] Generated riddle: {prompt}"
