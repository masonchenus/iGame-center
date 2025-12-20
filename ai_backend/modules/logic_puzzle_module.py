from ai_models import models
PRIVILEGES = ["create_logic_puzzle"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Logic Puzzle mode: Generate logic puzzles from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Create a logic puzzle for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def logic_puzzle(prompt: str) -> str:
    return f"[Logic Puzzle Mode] Generated logic puzzle: {prompt}"
