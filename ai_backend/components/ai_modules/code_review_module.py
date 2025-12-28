from ai_models import models
PRIVILEGES = ["review_code", "provide_code_feedback"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Code Review mode: Perform code review and provide feedback on code from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Review and provide feedback on this code: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def code_review(prompt: str) -> str:
    return f"[Code Review Mode] Reviewed code: {prompt}"
