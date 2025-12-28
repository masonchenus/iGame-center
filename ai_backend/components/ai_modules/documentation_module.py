from ai_models import models
PRIVILEGES = ["create_documentation"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Documentation mode: Create comprehensive documentation from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Create documentation for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def documentation(prompt: str) -> str:
    return f"[Documentation Mode] Created documentation for: {prompt}"
