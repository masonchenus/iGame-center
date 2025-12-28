# Folder Organization Plan

## Current Issues Identified:
1. Multiple server files scattered at root level
2. Duplicate configuration files (ruff.toml, eslint configs)
3. Performance logs cluttering directories
4. Scattered documentation files
5. Root level file clutter
6. Duplicate ai_models directories
7. Test files in multiple locations

## Organization Strategy (Preserving Both Systems):

### Keep As-Is (Core Functionality):
- ✅ `src/` - JavaScript Game Center (keep intact)
- ✅ `ai_backend/` - Python AI Backend (keep intact)
- ✅ `package.json` - Main project configuration
- ✅ `README.md` - Main project documentation

### To Organize/Consolidate:

#### 1. **Root Level Cleanup**
- Move scattered Python server files → `backend_servers/`
- Move performance logs → `logs/perf/`
- Move duplicate configs → consolidate to single instances
- Move root-level Python scripts → `scripts/`

#### 2. **Documentation Consolidation**
- Move all .md files from root → `docs/`
- Remove duplicates, keep latest versions
- Consolidate similar documents

#### 3. **Configuration Management**
- Single ruff.toml at root
- Single .eslintrc.json at root
- Remove duplicate config files

#### 4. **Test Organization**
- Keep src/tests/ for frontend tests
- Keep ai_backend/testing/ for AI backend tests
- Remove duplicate test directories

#### 5. **Server Scripts**
- Consolidate all server startup scripts → `scripts/`
- Create clear entry points for both systems

## New Structure Preview:
```
/Users/mason/Game Center Project/
├── src/                    # ✅ JavaScript Game Center (unchanged)
├── ai_backend/            # ✅ Python AI Backend (unchanged)
├── backend_servers/       # 🆕 All Python server files
├── scripts/               # 🆕 Server startup scripts
├── logs/                  # 🆕 Performance and error logs
│   ├── perf/             # 🆕 Performance JSON files
│   └── errors/           # 🆕 Error logs
├── docs/                  # 🆕 All documentation
├── package.json           # ✅ Main config
└── [single config files] # 🆕 Consolidated configs
```

## Benefits:
- Maintains full functionality of both systems
- Reduces root level clutter
- Creates logical separation
- Easier to maintain and deploy
- Follows better project organization practices

