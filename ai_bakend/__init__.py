# ai_backend/__init__.py
import os
import json
import logging

# -----------------------
# Logging setup
# -----------------------
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("iGame-AI")

# -----------------------
# Ensure necessary folders exist
# -----------------------
for folder in ["stats", "billing"]:
    os.makedirs(folder, exist_ok=True)

# -----------------------
# Initialize stats file
# -----------------------
stats_file = "stats/stats.json"
if not os.path.exists(stats_file):
    with open(stats_file, "w") as f:
        json.dump({"requests": 0}, f)

# -----------------------
# Ultra billing setup
# -----------------------
billing_file = "billing/billing.json"
if not os.path.exists(billing_file):
    ultra_credits = 10**12  # ultra dev mode
    with open(billing_file, "w") as f:
        json.dump({"credits": ultra_credits}, f)

# -----------------------
# Import and initialize all model providers
# -----------------------
from .models.chatgpt_model import ChatGPTModel
from .models.perplexity_model import PerplexityModel
from .models.gemini_model import GeminiModel
from .models.deepseek_model import DeepseekModel
from .models.grok_model import GrokModel

# Initialize models once for the whole backend
models = {
    "chatgpt": ChatGPTModel(),
    "perplexity": PerplexityModel(),
    "gemini": GeminiModel(),
    "deepseek": DeepseekModel(),
    "grok": GrokModel()
}

logger.info("iGame-AI package initialized with all models loaded.")
