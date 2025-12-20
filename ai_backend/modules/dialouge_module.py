from ai_models import models
PRIVILEGES = ["simulate_dialogue"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Dialogue mode: Simulate conversation and dialogue from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Simulate a dialogue for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def dialogue(prompt: str) -> str:
    return f"[Dialogue Mode] Simulated conversation: {prompt}"
