import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT))

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from ai_backend.orchestrator import run_mode

app = FastAPI(title="iGame AI API")

# Enable CORS for all origins (allow frontend to call backend)
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # Allow all origins
    allow_credentials=True,
    allow_methods=["*"],  # Allow all HTTP methods
    allow_headers=["*"],  # Allow all headers
)

class RunRequest(BaseModel):
    mode: str
    model: str
    input: str
    user_id: str = "web"
    session_id: str = "default"

class RunResponse(BaseModel):
    output: str

@app.post("/api/run", response_model=RunResponse)
def run_ai(req: RunRequest):
    output = run_mode(
        mode_name=req.mode,
        model_name=req.model,
        input_data=req.input,
        user_id=req.user_id,
        session_id=req.session_id,
    )
    return {"output": output}
