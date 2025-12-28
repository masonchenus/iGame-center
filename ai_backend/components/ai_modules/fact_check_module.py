from ai_models import models
PRIVILEGES = ["fact_check"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Fact Check mode: Verify and fact-check information from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Fact-check the following: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def fact_check(prompt: str) -> str:
    return f"[Fact-Check Mode] Checked facts for: {prompt}"
