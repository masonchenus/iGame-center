from ai_models import models
PRIVILEGES = ["create_tests"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Test Creator mode: Create test questions and test cases from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Create test questions for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)

def test_creator(prompt: str) -> str:
    return f"[Test Creator Mode] Created test questions for: {prompt}."
#Logging function to debug
print("Created test questions for: {prompt}.")
