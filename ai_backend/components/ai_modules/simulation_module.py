from ai_models import models
PRIVILEGES = ["simulate_scenario"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Simulation mode: Simulate scenarios and situations from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Simulate a scenario for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def simulation(prompt: str) -> str:
    return f"[Simulation Mode] Simulated scenario for: {prompt}"
