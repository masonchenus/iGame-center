from ai_models import models
PRIVILEGES = ["generate_agent_response"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Agent mode: AI agent response generation from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Act as an AI agent to respond to: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def agent_response(prompt: str) -> str:
    return f"[Agent Mode] Acted as AI agent for: {prompt}"
