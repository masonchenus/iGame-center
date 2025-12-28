from ai_models import models
PRIVILEGES = ["generate_prompt_ideas"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Prompt Ideas mode: Generate creative prompt ideas from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Generate prompt ideas for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def prompt_idea(prompt: str) -> str:
    return f"[Prompt Idea Mode] Generated prompt ideas: {prompt}"
