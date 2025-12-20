from ai_models import models
PRIVILEGES = ["conduct_research"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Research mode: Research and gather information on topics from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Research the following topic: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def research(prompt: str) -> str:
    return f"[Research Mode] Researched topic: {prompt}"
