import sys
import os
from pathlib import Path
from ai_backend.security.middleware import OAuth2

# Add all needed modules to Python path
from fastapi import FastAPI, HTTPException, UploadFile, File, Form
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel
from ai_backend.orchestrator import run_mode, run_user_mode
from ai_backend.models.unified_ai_manager import get_ai_manager
from ai_backend.tools import execute_tool, list_tools, get_tool

ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT))

# Create FastAPI app
app = FastAPI(
    title="Enhanced AI System API",
    description="Advanced AI with Code Tools, Analysis, and Generation",
    version="2.0.0"
)

# Add CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

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

# ====================
# AI Tools Endpoints
# ====================

class ToolRequest(BaseModel):
    tool_name: str
    parameters: dict = {}

class ToolResponse(BaseModel):
    status: str
    result: dict
    tool_name: str
    execution_time: float

@app.get("/api/tools", response_model=dict)
def get_available_tools():
    """Get list of available AI tools"""
    try:
        tools = list_tools()
        return {
            "status": "success",
            "available_tools": tools,
            "total_count": len(tools)
        }
    except Exception as e:
        return {
            "status": "error",
            "error": str(e),
            "available_tools": [],
            "total_count": 0
        }

@app.post("/api/tools/execute", response_model=ToolResponse)
def execute_ai_tool(req: ToolRequest):
    """Execute an AI tool with given parameters"""
    import time
    start_time = time.time()
    
    try:
        result = execute_tool(req.tool_name, **req.parameters)
        execution_time = time.time() - start_time
        
        return ToolResponse(
            status="success",
            result=result,
            tool_name=req.tool_name,
            execution_time=execution_time
        )
    except Exception as e:
        execution_time = time.time() - start_time
        return ToolResponse(
            status="error",
            result={"error": str(e)},
            tool_name=req.tool_name,
            execution_time=execution_time
        )

# Code Modification Tool Endpoint
class CodeModifyRequest(BaseModel):
    code: str
    modification_type: str = "refactor"
    target_file: str = None
    language: str = "python"
    parameters: dict = {}

@app.post("/api/tools/code/modify", response_model=ToolResponse)
def modify_code(req: CodeModifyRequest):
    """Modify code using AI code modifier tool"""
    import time
    start_time = time.time()
    
    try:
        tool = get_tool("code_modifier")
        if not tool:
            raise Exception("Code modifier tool not found")
        
        modifier = tool()
        result = modifier.execute(
            code=req.code,
            modification_type=req.modification_type,
            target_file=req.target_file,
            language=req.language,
            **req.parameters
        )
        
        execution_time = time.time() - start_time
        
        return ToolResponse(
            status="success",
            result=result,
            tool_name="code_modifier",
            execution_time=execution_time
        )
    except Exception as e:
        execution_time = time.time() - start_time
        return ToolResponse(
            status="error",
            result={"error": str(e)},
            tool_name="code_modifier",
            execution_time=execution_time
        )

# File Analysis Tool Endpoint
class FileAnalyzeRequest(BaseModel):
    file_path: str
    analysis_type: str = "full"
    parameters: dict = {}

@app.post("/api/tools/file/analyze", response_model=ToolResponse)
def analyze_file(req: FileAnalyzeRequest):
    """Analyze file using AI file analyzer tool"""
    import time
    start_time = time.time()
    
    try:
        tool = get_tool("file_analyzer")
        if not tool:
            raise Exception("File analyzer tool not found")
        
        analyzer = tool()
        result = analyzer.execute(
            file_path=req.file_path,
            analysis_type=req.analysis_type,
            **req.parameters
        )
        
        execution_time = time.time() - start_time
        
        return ToolResponse(
            status="success",
            result=result,
            tool_name="file_analyzer",
            execution_time=execution_time
        )
    except Exception as e:
        execution_time = time.time() - start_time
        return ToolResponse(
            status="error",
            result={"error": str(e)},
            tool_name="file_analyzer",
            execution_time=execution_time
        )

# Code Generation Tool Endpoint
class CodeGenerateRequest(BaseModel):
    generation_type: str
    language: str = "python"
    project_name: str = "generated_project"
    requirements: dict = {}
    parameters: dict = {}

@app.post("/api/tools/code/generate", response_model=ToolResponse)
def generate_code(req: CodeGenerateRequest):
    """Generate code using AI code generator tool"""
    import time
    start_time = time.time()
    
    try:
        tool = get_tool("code_generator")
        if not tool:
            raise Exception("Code generator tool not found")
        
        generator = tool()
        result = generator.execute(
            generation_type=req.generation_type,
            language=req.language,
            project_name=req.project_name,
            requirements=req.requirements,
            **req.parameters
        )
        
        execution_time = time.time() - start_time
        
        return ToolResponse(
            status="success",
            result=result,
            tool_name="code_generator",
            execution_time=execution_time
        )
    except Exception as e:
        execution_time = time.time() - start_time
        return ToolResponse(
            status="error",
            result={"error": str(e)},
            tool_name="code_generator",
            execution_time=execution_time
        )

# Code Optimization Tool Endpoint
class CodeOptimizeRequest(BaseModel):
    code: str
    language: str = "python"
    optimization_level: str = "basic"
    parameters: dict = {}

@app.post("/api/tools/code/optimize", response_model=ToolResponse)
def optimize_code(req: CodeOptimizeRequest):
    """Optimize code using AI code optimizer tool"""
    import time
    start_time = time.time()
    
    try:
        tool = get_tool("code_optimizer")
        if not tool:
            raise Exception("Code optimizer tool not found")
        
        optimizer = tool()
        result = optimizer.execute(
            code=req.code,
            language=req.language,
            optimization_level=req.optimization_level,
            **req.parameters
        )
        
        execution_time = time.time() - start_time
        
        return ToolResponse(
            status="success",
            result=result,
            tool_name="code_optimizer",
            execution_time=execution_time
        )
    except Exception as e:
        execution_time = time.time() - start_time
        return ToolResponse(
            status="error",
            result={"error": str(e)},
            tool_name="code_optimizer",
            execution_time=execution_time
        )

# Health Check Endpoint
@app.get("/api/health")
def health_check():
    """Health check endpoint to verify server status"""
    try:
        # Test basic functionality
        tools = list_tools()
        
        return {
            "status": "healthy",
            "service": "Enhanced AI System",
            "version": "2.0.0",
            "timestamp": str(time.time()),
            "tools_available": len(tools),
            "tools": tools,
            "message": "Server is running and ready to handle requests"
        }
    except Exception as e:
        return {
            "status": "unhealthy",
            "service": "Enhanced AI System",
            "version": "2.0.0",
            "timestamp": str(time.time()),
            "error": str(e),
            "message": "Server has issues"
        }

# Enhanced AI Chat Endpoint
class EnhancedChatRequest(BaseModel):
    message: str
    context: dict = {}
    use_tools: bool = True
    max_tokens: int = 1000

@app.post("/api/chat/enhanced", response_model=dict)
def enhanced_chat(req: EnhancedChatRequest):
    """Enhanced chat endpoint with AI tools integration"""
    try:
        # Parse the message to determine if tools should be used
        message_lower = req.message.lower()
        
        # Check for code-related requests
        if any(keyword in message_lower for keyword in ['code', 'function', 'class', 'script']):
            # Use code generation/modification tools
            tool_result = execute_tool("code_generator", 
                                     generation_type="function",
                                     language="python",
                                     requirements={"function_name": "generated_function"})
            
            response = f"🔧 **AI Code Assistant Response:**\n\n"
            response += f"I've analyzed your request about code. Here's what I can help you with:\n\n"
            response += f"**Generated Code Example:**\n```python\n{tool_result.get('files', {}).get('functions.py', '# Generated function')}\n```\n\n"
            response += f"**Available Tools:**\n"
            response += f"- Code Generator: Create functions, classes, APIs, and more\n"
            response += f"- Code Modifier: Refactor, optimize, and enhance existing code\n"
            response += f"- File Analyzer: Analyze code for security, performance, and quality\n"
            response += f"- Code Optimizer: Optimize performance and improve efficiency\n"
            
        elif any(keyword in message_lower for keyword in ['analyze', 'review', 'check']):
            # Use file analysis tools
            response = f"🔍 **AI Analysis Assistant Response:**\n\n"
            response += f"I've detected you want to analyze something. I can help with:\n\n"
            response += f"**Analysis Capabilities:**\n"
            response += f"- Security analysis: Find vulnerabilities and security issues\n"
            response += f"- Performance analysis: Identify bottlenecks and optimization opportunities\n"
            response += f"- Code quality analysis: Check best practices and code standards\n"
            response += f"- Dependency analysis: Review imports and external dependencies\n"
            response += f"- Structure analysis: Analyze architecture and design patterns\n"
            
        else:
            # Use the regular AI orchestrator
            result = run_mode(
                mode_name="helper",
                model_name="nexus",
                input_data=req.message,
                user_id="enhanced_chat",
                session_id="chat_session"
            )
            
            import json
            try:
                result_data = json.loads(result)
                response = result_data.get("response", "I'm here to help! What would you like to know?")
            except:
                response = result
        
        return {
            "status": "success",
            "response": response,
            "tools_used": req.use_tools,
            "context": req.context,
            "timestamp": str(time.time())
        }
        
    except Exception as e:
        return {
            "status": "error",
            "error": str(e),
            "response": "I apologize, but I encountered an error while processing your request. Please try again.",
            "tools_used": False,
            "timestamp": str(time.time())
        }

# Add time import
import time
