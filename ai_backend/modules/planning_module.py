from ai_models import models
PRIVILEGES = ["create_plan"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Planning mode: Create step-by-step plans from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Create a step-by-step plan for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def planning(prompt: str) -> str:
    return f"[Planning Mode] Created step-by-step plan: {prompt}"
