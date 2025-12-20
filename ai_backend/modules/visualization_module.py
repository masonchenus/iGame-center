from ai_models import models
PRIVILEGES = ["create_visualization"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Visualization mode: Generate visualization and visual representations from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Generate a visualization for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def visualization(prompt: str) -> str:
    return f"[Visualization Mode] Generated visualization for: {prompt}"
