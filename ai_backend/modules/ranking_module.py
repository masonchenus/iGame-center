from ai_models import models
PRIVILEGES = ["create_ranking"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Ranking mode: Create rankings and rankings from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Create a ranking for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def ranking(prompt: str) -> str:
    return f"[Ranking Mode] Created ranking for: {prompt}"
