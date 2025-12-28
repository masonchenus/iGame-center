from ai_models import models
PRIVILEGES = ["create_brainstorm_ideas"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Brainstorming mode: Generate creative ideas and brainstorming content from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Generate brainstorming ideas for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def brainstorm(prompt: str) -> str:
    return f"[Brainstorming Mode] Ideas generated: {prompt}"
