from ai_models import models
PRIVILEGES = ["provide_feedback"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Feedback mode: Provide constructive feedback on user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Provide feedback on: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def feedback(prompt: str) -> str:
    return f"[Feedback Mode] Provided feedback on: {prompt}"
