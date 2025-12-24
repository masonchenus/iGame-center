import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT))

from fastapi import FastAPI
from pydantic import BaseModel
from ai_backend.orchestrator import run_mode, run_user_mode
from ai_backend.models.unified_ai_manager import get_ai_manager

app = FastAPI(title="iGame AI API")

class RunRequest(BaseModel):
    mode: str
    model: str
    input: str
    user_id: str = "web"
    session_id: str = "default"

class RunResponse(BaseModel):
    output: str

class ModelSwitchRequest(BaseModel):
    model_name: str

class ModelStatusResponse(BaseModel):
    status: str
    current_model: str
    available_models: list
    model_info: dict = None

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

@app.post("/api/run/dynamic", response_model=RunResponse)
def run_ai_dynamic(req: RunRequest):
    """Test endpoint for dynamic module imports - this will show module not found errors"""
    output = run_user_mode(
        mode_name=req.mode,
        input_data=req.input,
        user_id=req.user_id,
        session_id=req.session_id,
        model_name=req.model,
    )
    return {"output": output}

@app.get("/api/models", response_model=ModelStatusResponse)
def get_available_models():
    """Get list of available AI models"""
    try:
        ai_manager = get_ai_manager()
        result = ai_manager.get_available_models()
        return ModelStatusResponse(
            status="success",
            current_model=result["current_model"],
            available_models=result["available_models"]
        )
    except Exception as e:
        return ModelStatusResponse(
            status="error",
            current_model="nexus",
            available_models=[{
                "name": "nexus",
                "display_name": "Nexus AI",
                "available": True,
                "description": "Default system model"
            }]
        )

@app.post("/api/models/switch", response_model=ModelStatusResponse)
def switch_model(req: ModelSwitchRequest):
    """Switch to a different AI model"""
    try:
        ai_manager = get_ai_manager()
        result = ai_manager.switch_model(req.model_name)
        
        if result["status"] == "success":
            return ModelStatusResponse(
                status="success",
                current_model=result["model_switched"],
                available_models=[],
                model_info={
                    "previous_model": result.get("previous_model"),
                    "new_config": result.get("new_config", {})
                }
            )
        else:
            return ModelStatusResponse(
                status="error",
                current_model=ai_manager.model_name,
                available_models=[],
                model_info={"error": result.get("error", "Unknown error")}
            )
    except Exception as e:
        return ModelStatusResponse(
            status="error",
            current_model="nexus",
            available_models=[],
            model_info={"error": str(e)}
        )
