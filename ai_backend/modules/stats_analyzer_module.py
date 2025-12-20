from ai_models import models
PRIVILEGES = ["analyze_statistics"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Stats Analyzer mode: Analyze statistics and data from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Analyze these statistics: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def stats_analyzer(prompt: str) -> str:
    return f"[Stats Analyzer Mode] Analyzed stats for: {prompt}"
