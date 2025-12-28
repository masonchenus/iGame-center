from ai_models import models
PRIVILEGES = ["translate_text"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Translator mode: Translate text from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Translate the following text: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def translate(prompt: str) -> str:
    return f"[Translator Mode] Translated text: {prompt}"
