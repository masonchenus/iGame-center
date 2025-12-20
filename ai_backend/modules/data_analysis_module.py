from ai_models import models
PRIVILEGES = ["analyze_data"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    Data Analysis mode: Analyze data and provide insights from user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Analyze this data: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def data_analysis(prompt: str) -> str:
    return f"[Data Analysis Mode] Analyzed data: {prompt}"
