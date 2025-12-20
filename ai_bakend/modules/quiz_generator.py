from . import llm_provider

def create_quiz(topic: str) -> dict:
    """
    Generates a quiz on a given topic.

    The quiz will contain a mix of multiple-choice and open-ended questions.
    """
    prompt = f"Create a quiz about {topic} with 2 multiple-choice questions and 1 open-ended question."
    quiz_text = llm_provider.generate_text(prompt, "gemini-pro")

    # In a real application, you would parse the response from the LLM.
    # For this simulation, we'll return a fixed JSON structure.
    return {
        "topic": topic,
        "questions": [
            {
                "type": "multiple-choice",
                "question": f"What is the capital of {topic}?",
                "options": ["A", "B", "C", "D"],
                "answer": "A"
            },
            {
                "type": "multiple-choice",
                "question": f"What is the population of {topic}?",
                "options": ["1 million", "5 million", "10 million", "20 million"],
                "answer": "C"
            },
            {
                "type": "open-ended",
                "question": f"Describe the culture of {topic}."
            }
        ]
    }

def evaluate_response(question: str, user_answer: str) -> str:
    """
    Simulates an AI evaluation of a user's answer to an open-ended question.
    """
    prompt = f"Evaluate the following answer for the question '{question}': '{user_answer}'"
    evaluation = llm_provider.generate_text(prompt, "gemini-pro")
    return evaluation
