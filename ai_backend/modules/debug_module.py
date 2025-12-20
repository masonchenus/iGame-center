from ai_models import models
PRIVILEGES = ["debug_code", "troubleshoot_issues"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Debug mode: Debug and troubleshoot issues from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Debug this issue: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def debug(prompt: str) -> str:
    return f"[Debug Mode] Debugged input: {prompt}"
