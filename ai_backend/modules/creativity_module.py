from ai_models import models
PRIVILEGES = ["generate_creative_ideas"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Creativity mode: Generate creative and innovative ideas from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Generate creative ideas for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def creativity(prompt: str) -> str:
    return f"[Creativity Mode] Generated creative ideas for: {prompt}"
