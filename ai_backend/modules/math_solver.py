from ai_models import models
PRIVILEGES = ["solve_math"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Math Solver mode: Solve mathematical problems from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Solve this math problem: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def math(prompt: str) -> str:
    return f"[Math Mode] Solved math problem: {prompt}"
