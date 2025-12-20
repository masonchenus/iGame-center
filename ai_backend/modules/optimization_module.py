from ai_models import models
PRIVILEGES = ["optimize_code", "suggest_optimizations"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Optimization mode: Suggest optimizations and improvements from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Suggest optimizations for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def optimize(prompt: str) -> str:
    return f"[Optimization Mode] Suggested optimizations for: {prompt}"
