# Programming Languages Scattering TODO

## Overview
Scatter contents from `programming-languages/` to appropriate locations based on file type.

## New Directory Structure
```
ai_backend/
├── languages/           # Keep Crystal, Elixir, Haskell files
├── components/
│   ├── ai_modules/
│   │   └── games/       # NEW: All game implementations
│   └── game_engines/    # NEW: Game engine components
├── infrastructure/
│   └── database/        # NEW: SQL schemas
├── physics/             # NEW: Physics engines (C++, Fortran)
├── data_structures/     # NEW: Data structure implementations
├── web/                 # NEW: Web servers and APIs
└── tools/
```

## Phase 1: Create New Directories

### Create directories
- [ ] Create `ai_backend/components/game_engines/`
- [ ] Create `ai_backend/components/ai_modules/games/`
- [ ] Create `ai_backend/physics/`
- [ ] Create `ai_backend/infrastructure/database/`
- [ ] Create `ai_backend/data_structures/`
- [ ] Create `ai_backend/web/`

## Phase 2: Move Game Engines

### C++ Engine Files → `ai_backend/components/game_engines/`
- [ ] audio-engine.h
- [ ] core-engine.cpp
- [ ] core-engine.h
- [ ] graphics-engine.h
- [ ] physics-engine.cpp
- [ ] physics-engine.h

### D Engine Files → `ai_backend/components/game_engines/`
- [ ] entity-system.d
- [ ] game-manager.d

### Rust Engine Files → `ai_backend/components/game_engines/`
- [ ] ai-utils.rs
- [ ] audio-manager.rs
- [ ] collision-system.rs
- [ ] config-manager.rs
- [ ] game-manager.rs
- [ ] input-manager.rs
- [ ] network-utils.rs
- [ ] physics-utils.rs
- [ ] player-manager.rs
- [ ] resource-manager.rs

## Phase 3: Move Game Examples

### C Games → `ai_backend/components/ai_modules/games/`
- [ ] 2048-game.c
- [ ] blackjack.c
- [ ] conways-game-of-life.c
- [ ] memory-match.c
- [ ] memory-manager.c
- [ ] minesweeper.c
- [ ] number-guessing.c
- [ ] rock-paper-scissors.c
- [ ] simple-platformer.c
- [ ] sudoku-solver.c
- [ ] text-adventure.c
- [ ] tic-tac-toe.c

### C++ Games → `ai_backend/components/ai_modules/games/`
- [ ] asteroids-game.cpp
- [ ] breakout-game.cpp
- [ ] pacman-game.cpp
- [ ] pong-game.cpp
- [ ] snake-game.cpp
- [ ] space-invaders.cpp
- [ ] tetris-game.cpp

### C# Games → `ai_backend/components/ai_modules/games/`
- [ ] SnakeGame.cs

### D Games → `ai_backend/components/ai_modules/games/`
- [ ] multi-threaded-server.d

### PHP Games → `ai_backend/components/ai_modules/games/`
- [ ] tic-tac-toe.php

## Phase 4: Move Physics

### C++ Physics → `ai_backend/physics/`
- [ ] physics-engine.cpp (from game-engine/)
- [ ] physics-engine.h (from game-engine/)

### Fortran Physics → `ai_backend/physics/`
- [ ] physics-simulation.i
- [ ] collision-detection.i

## Phase 5: Move Database

### SQL Files → `ai_backend/infrastructure/database/`
- [ ] schema.sql
- [ ] player-tables.sql
- [ ] leaderboard.sql
- [ ] achievements.sql
- [ ] player-stats.sql

## Phase 6: Move Web Infrastructure

### Crystal Web → `ai_backend/web/`
- [ ] crystal_api_client.cr
- [ ] crystal_microservice.cr
- [ ] crystal_web_server.cr
- [ ] crystal_websocket.cr

### Elixir Web → `ai_backend/web/`
- [ ] elixir_agent.ex
- [ ] elixir_phoenix_app.ex
- [ ] elixir_plug.ex
- [ ] elixir_web_server.ex
- [ ] elixir_websockets.ex

### PHP Server → `ai_backend/web/`
- [ ] GameServer.php
- [ ] PlayerAPI.php
- [ ] GameLogic.php

### D Server → `ai_backend/web/`
- [ ] game-server.d

### Go Managers → `ai_backend/web/` (or infrastructure/)
- [ ] achievements-manager.go
- [ ] audio-manager.go
- [ ] collision-system.go
- [ ] config-manager.go
- [ ] database-manager.go
- [ ] game-manager.go
- [ ] game-state-manager.go
- [ ] graphics-renderer.go
- [ ] input-manager.go
- [ ] leaderboard-manager.go
- [ ] logger.go
- [ ] network-manager.go
- [ ] physics-engine.go
- [ ] player-manager.go
- [ ] resource-manager.go

## Phase 7: Data Structures

### Crystal → `ai_backend/data_structures/`
- [ ] crystal_cache.cr
- [ ] crystal_concurrency.cr
- [ ] crystal_data_structures.cr

### Elixir → `ai_backend/data_structures/`
- [ ] elixir_concurrency.ex
- [ ] elixir_data_structures.ex
- [ ] elixir_gen_server.ex
- [ ] elixir_otp_supervisor.ex
- [ ] elixir_task_supervisor.ex

### Haskell → `ai_backend/data_structures/`
- [ ] haskell_concurrency.hs
- [ ] haskell_data_structures.hs
- [ ] haskell_monads.hs
- [ ] haskell_parallel.hs

## Phase 8: Cleanup

- [ ] Remove empty `programming-languages/` directory
- [ ] Remove empty `ai_backend/languages/` (except crystal/, elixir/, haskell/)
- [ ] Update imports/references if needed

## File Count Summary

| Source                        | Count | Destination                     |
| ----------------------------- | ----- | ------------------------------- |
| programming-languages/c       | 12    | games/, tools/                  |
| programming-languages/cpp     | 11    | game_engines/, physics/, games/ |
| programming-languages/csharp  | 1     | games/                          |
| programming-languages/d       | 3     | game_engines/, games/, web/     |
| programming-languages/fortran | 2     | physics/                        |
| programming-languages/go      | 15    | web/                            |
| programming-languages/php     | 3     | web/, games/                    |
| programming-languages/rs      | 10    | game_engines/                   |
| programming-languages/sql     | 5     | infrastructure/database/        |

**Total Files to Move: 62 files**

