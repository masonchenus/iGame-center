# Programming Languages Scattering Plan

## Overview
Scatter contents from `ai_backend/languages/` and `programming-languages/` to appropriate locations based on file type.

## Source Directories
1. `ai_backend/languages/` - Contains Crystal (16), Elixir (17), Haskell (16) files
2. `programming-languages/` - Contains C, C++, C#, D, Fortran, Go, PHP, Rust, SQL files

## Destination Plan by File Type

### 📁 AI/ML Algorithms → `ai_backend/languages/`
Keep existing structure, files already organized:
- `ai_backend/languages/crystal/` - AI algorithms, ML, concurrency
- `ai_backend/languages/elixir/` - AI pipelines, OTP, agents
- `ai_backend/languages/haskell/` - ML core, algorithms, optimization

### 📁 Game Engines → `ai_backend/components/game_engines/`
Create new directory and move:
- C++: core-engine.cpp, core-engine.h, physics-engine.cpp, physics-engine.h, audio-engine.h, graphics-engine.h
- D: entity-system.d, game-manager.d
- Rust: (from programming-languages/rs/)

### 📁 Game Examples → `ai_backend/components/ai_modules/games/`
Create new directory and move all game implementations:
- C: text-adventure.c, 2048-game.c, blackjack.c, conways-game-of-life.c, memory-match.c, minesweeper.c, number-guessing.c, rock-paper-scissors.c, simple-platformer.c, sudoku-solver.c, tic-tac-toe.c, memory-manager.c
- C++: asteroids-game.cpp, breakout-game.cpp, pacman-game.cpp, pong-game.cpp, snake-game.cpp, space-invaders.cpp, tetris-game.cpp
- C#: SnakeGame.cs
- D: multi-threaded-server.d
- PHP: tic-tac-toe.php

### 📁 Physics → `ai_backend/physics/`
Create new directory and move:
- C++: physics-engine.cpp, physics-engine.h (from game-engine/)
- Fortran: physics-simulation.i, collision-detection.i

### 📁 Database → `ai_backend/infrastructure/database/`
Move:
- SQL: schema.sql, player-tables.sql, leaderboard.sql, achievements.sql, player-stats.sql

### 📁 Web Servers → `ai_backend/infrastructure/`
Move server implementations:
- Crystal: crystal_web_server.cr, crystal_api_client.cr, crystal_websocket.cr
- Elixir: elixir_web_server.ex, elixir_websockets.ex, elixir_phoenix_app.ex, elixir_plug.ex
- PHP: GameServer.php, PlayerAPI.php, GameLogic.php
- D: game-server.d

### 📁 Data Structures/Concurrency → `ai_backend/data_structures/`
Create new directory and move:
- Crystal: crystal_data_structures.cr, crystal_concurrency.cr, crystal_cache.cr
- Elixir: elixir_data_structures.ex, elixir_concurrency.ex, elixir_gen_server.ex, elixir_task_supervisor.ex, elixir_otp_supervisor.ex
- Haskell: haskell_data_structures.hs, haskell_concurrency.hs, haskell_parallel.hs, haskell_monads.hs

### 📁 Web Development → `ai_backend/web/`
Create new directory and move:
- Crystal: crystal_api_client.cr, crystal_websocket.cr, crystal_microservice.cr
- Elixir: elixir_agent.ex, elixir_phoenix_app.ex, elixir_plug.ex, elixir_websockets.ex
- PHP: GameServer.php, PlayerAPI.php, GameLogic.php

### 📁 Networking → `ai_backend/infrastructure/`
Move:
- Haskell: haskell_networking.hs
- Crystal: crystal_api_client.cr, crystal_websocket.cr

### 📁 Utils/Tools → `ai_backend/tools/`
Move utility files:
- C: memory-manager.c
- Crystal: crystal_basics.cr, crystal_cache.cr
- Elixir: elixir_basics.ex

## New Directory Structure

```
ai_backend/
├── languages/           # Keep Crystal, Elixir, Haskell files
│   ├── crystal/
│   ├── elixir/
│   └── haskell/
├── components/
│   ├── ai_modules/
│   │   └── games/      # NEW: All game implementations
│   └── game_engines/   # NEW: Game engine components
├── infrastructure/
│   ├── database/       # NEW: SQL schemas
│   └── server.py       # existing
├── physics/            # NEW: Physics engines (C++, Fortran)
├── data_structures/    # NEW: Data structure implementations
├── web/                # NEW: Web servers and APIs
├── tools/
│   └── code_optimizer.py
└── ...
```

## Implementation Steps

### Phase 1: Create New Directories
- [ ] Create `ai_backend/components/game_engines/`
- [ ] Create `ai_backend/components/ai_modules/games/`
- [ ] Create `ai_backend/physics/`
- [ ] Create `ai_backend/infrastructure/database/`
- [ ] Create `ai_backend/data_structures/`
- [ ] Create `ai_backend/web/`

### Phase 2: Move Game Engines
- [ ] Move C++ engine files to `ai_backend/components/game_engines/`
- [ ] Move D engine files to `ai_backend/components/game_engines/`
- [ ] Move Rust engine files to `ai_backend/components/game_engines/`

### Phase 3: Move Game Examples
- [ ] Move C games to `ai_backend/components/ai_modules/games/`
- [ ] Move C++ games to `ai_backend/components/ai_modules/games/`
- [ ] Move C# games to `ai_backend/components/ai_modules/games/`
- [ ] Move PHP games to `ai_backend/components/ai_modules/games/`
- [ ] Move D games to `ai_backend/components/ai_modules/games/`

### Phase 4: Move Physics
- [ ] Move C++ physics to `ai_backend/physics/`
- [ ] Move Fortran physics to `ai_backend/physics/`

### Phase 5: Move Database
- [ ] Move SQL files to `ai_backend/infrastructure/database/`

### Phase 6: Move Web Infrastructure
- [ ] Move Crystal web files to `ai_backend/web/`
- [ ] Move Elixir web files to `ai_backend/web/`
- [ ] Move PHP server files to `ai_backend/web/`
- [ ] Move D server files to `ai_backend/web/`

### Phase 7: Cleanup
- [ ] Remove empty `programming-languages/` directory
- [ ] Remove empty `ai_backend/languages/` subdirectories (keep if not empty)
- [ ] Update any imports/references

## File Count Summary

| Source                        | Count | Destination                     |
| ----------------------------- | ----- | ------------------------------- |
| programming-languages/c       | 12    | games/, tools/                  |
| programming-languages/cpp     | 11    | game_engines/, physics/, games/ |
| programming-languages/csharp  | 1     | games/                          |
| programming-languages/d       | 3     | game_engines/, games/, web/     |
| programming-languages/fortran | 2     | physics/                        |
| programming-languages/go      | 6     | TBD - need exploration          |
| programming-languages/php     | 3     | web/, games/                    |
| programming-languages/rs      | 4     | TBD - need exploration          |
| programming-languages/sql     | 2     | infrastructure/database/        |

## Total Files to Move: ~60+ files

