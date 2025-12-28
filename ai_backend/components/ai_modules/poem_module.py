from ai_models import models
PRIVILEGES = ["create_poem"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Poem mode: Generate poems from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Generate a poem about: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def poem(prompt: str) -> str:
    return f"[Poem Mode] Generated poem: {prompt}"
