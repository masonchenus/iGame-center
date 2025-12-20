from ai_models import models
PRIVILEGES = ["summarize_content"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Summarizer mode: Summarize and condense content from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Summarize the following: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def summarize(prompt: str) -> str:
    return f"[Summarizer Mode] Summary for: {prompt}"
