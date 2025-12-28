from ai_models import models
PRIVILEGES = ["test_logic"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Tester mode: Test logic and functionality from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Test the following logic: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def tester(prompt: str) -> str:
    return f"[Tester Mode] Tested input or logic: {prompt}"
