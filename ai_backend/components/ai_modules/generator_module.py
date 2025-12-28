from ai_models import models
PRIVILEGES = ["generate_content"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Generator mode: Generate general content from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Generate content based on: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def generator(prompt: str) -> str:
    return f"[Generator Mode] Generated content: {prompt}"
