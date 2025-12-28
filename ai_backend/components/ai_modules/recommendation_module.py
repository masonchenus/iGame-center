from ai_models import models
PRIVILEGES = ["generate_recommendations"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Recommendation mode: Suggest recommendations from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Suggest recommendations for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def recommendation(prompt: str) -> str:
    return f"[Recommendation Mode] Suggested recommendations for: {prompt}"
