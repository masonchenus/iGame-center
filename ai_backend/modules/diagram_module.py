from ai_models import models
PRIVILEGES = ["create_diagram"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Diagram mode: Generate diagram content and visual representations from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Create diagram content for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def diagram(prompt: str) -> str:
    return f"[Diagram Mode] Generated diagram content for: {prompt}"
