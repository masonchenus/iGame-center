from ai_models import models
PRIVILEGES = ["create_checklist"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Checklist mode: Generate a structured checklist from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Create a checklist for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def checklist(prompt: str) -> str:
    return f"[Checklist Mode] Generated checklist for: {prompt}"
