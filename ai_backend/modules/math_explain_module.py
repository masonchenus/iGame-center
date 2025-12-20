from ai_models import models
PRIVILEGES = ["explain_math"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Math Explanation mode: Explain mathematical concepts and problems from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Explain this math problem: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def math_explain(prompt: str) -> str:
    return f"[Math-Explain Mode] Explained math problem: {prompt}"
