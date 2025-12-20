from ai_models import models
PRIVILEGES = ["generate_code"]


def run(input_data="[<input_data>]", user_id="[<user_id>]", session_id="[<session_id>]", model_name="[<model_name>]"):
    """
    CodeGen mode: Generate code snippets from the user input using the selected AI model
    """
    model = models.get(model_name)
    if not model:
        raise ValueError(f"Unknown model: {model_name}")
    prompt = f"Generate code for: {input_data}"
    return model.generate(prompt, user_id=user_id, session_id=session_id)


def codegen(prompt: str) -> str:
    return f"[CodeGen Mode] Generated code snippet: {prompt}"
