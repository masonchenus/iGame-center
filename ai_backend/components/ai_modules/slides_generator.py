from ai_models import models
PRIVILEGES = ["create_slides"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Slides mode: Generate slide content and presentations from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Create a slide outline and key points from the following input: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def slides(prompt: str) -> str:
    return f"[Slides Mode] Generated slide content: {prompt}"
