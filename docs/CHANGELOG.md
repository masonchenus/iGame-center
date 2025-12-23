# Changelog

All notable changes to Game Center are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/),
and this project adheres to [Semantic Versioning](https://semver.org/).

## [Unreleased]

### Added
- Powerup drops from defeated enemies (20% drop rate)
- Chaos weapon visualization with colored particles
- Service Worker for offline support
- Progressive Web App (PWA) manifest
- Performance optimization files
- Accessibility guidelines and documentation
- Security policy documentation
- Deployment guides for multiple platforms

### Changed
- Chaos bullet rendering with alpha, scale, and rotation support
- Improved weapon firing to handle function-based parameters

### Fixed
- Chaos bullets now render correctly when fired
- Firearm properties now callable as functions

## [1.0.1] - 2025-12-01

### Added
- Multiple weapon types (Normal, Boost, Spreadshot, Laser, Plasma, Missile, Nova, Tempest, Chaos)
- Enemy destruction particle effects
- Score tracking and leaderboard
- Power-ups (damage boost, speed boost, fire rate boost, invincibility, shield)
- Game levels and wave progression
- Boss battles
- Sound effects and visual effects

### Changed
- Improved game physics calculations
- Enhanced canvas rendering performance

### Fixed
- Bullet collision detection
- Enemy spawning logic

## [1.0.0] - 2025-11-01

### Added
- Initial Game Center release
- Multiple game modes:
  - Space Invaders Classic
  - Space Invaders But Better (with weapons)
  - Space Shooter
  # Changelog

  All notable changes to Game Center are documented in this file.

  The format is based on [Keep a Changelog](https://keepachangelog.com/),
  and this project adheres to [Semantic Versioning](https://semver.org/).

  ## [Unreleased] - 2025-12-22

  The following commits since `2025-12-01` have been included here (most recent first):

  - 2025-12-22 — `13d6bec` — Testing
  - 2025-12-22 — `1e7dd9b` — 2.0.1.1
  - 2025-12-22 — `f5edb53` — Add Java setup step to GitHub Actions workflow
  - 2025-12-22 — `355e6a7` — 2.0.1-minor
  - 2025-12-21 — `bcc589f` — 2.0.0-minor
  - 2025-12-21 — `941f5f3` — 2.0.0-minor
  - 2025-12-21 — `76bd4b0` — Rename project to Multi repository and add AI info
  - 2025-12-21 — `132f8a9` — Update 1.4.0-minor
  - 2025-12-20 — `daad0e7` — `Removed AI model implementations from ai_backend repository`
  - 2025-12-20 — `0560702` — Update 3A2C1d2usd832784323te
  - 2025-12-18 — `a76a880` — Create tester_module.py
  - 2025-12-18 — `f792dbe` — Create agent_module.py
  - 2025-12-18 — `3e51576` — Create helper_module.py
  - 2025-12-18 — `5f3ae56` — Create slides_generator.py
  - 2025-12-18 — `07459d4` — Create math_solver.py
  - 2025-12-18 — `12e6d9a` — Create game_generator.py
  - 2025-12-18 — `45eddcf` — Create python-ai-multipurpose-ai-file-structure.graphql temp. file
  - 2025-12-18 — `b48f561` — Create __init__.py
  - 2025-12-17 — `e2de652` — update
  - 2025-12-17 — `a25b3c7` — destory ai-agent-root.html
  - 2025-12-17 — `9044abe` — feat: Add variety to enemy types in initMegaGame
  - 2025-12-17 — `98de68a` — update 1.3.4-minor
  - 2025-12-17 — `19cff16` — fix(game): Randomize enemy generation in Mega Game
  - 2025-12-17 — `8403913` — fix(game): Randomize enemy generation in Mega Game
  - 2025-12-16 — `011010d` — update v-1.3.3-minor
  - 2025-12-16 — `6c4ddbe` — Update index.html
  - 2025-12-16 — `3ffd1c1` — feat(bullet-forger): overhaul CSS and layout
  - 2025-12-16 — `e608f69` — feat(game): Implement Bullet Forger presets and in-game selection menu
  - 2025-12-15 — `a6045ca` — update v-adps0d0f0as9d9d984923789432589763440q1bc
  - 2025-12-13 — `a874644` — Update v1.3.3-minor
  - 2025-12-11 — `412db30` — update 1.3.2.1b ***(need)***
  - 2025-12-11 — `8ee689f` — Update static.yml
  - 2025-12-11 — `df026b0` — Update static.yml_errorfixed:x1b2c0000xizx0232ab000
  - 2025-12-11 — `5c3e2f3` — Update static.yml
  - 2025-12-11 — `19f781c` — bug fix
  - 2025-12-11 — `07c0820` — Create jekyll-gh-pages.yml & sync update v1.3.2-beta
  - 2025-12-11 — `eabb428` — Create static.yml & add marketplace addons
  - 2025-12-10 — `a4d888e` — Update README.md
  - 2025-12-10 — `7965b1c` — Create Privacy-Policy.md & sync update 1.3.2 1b 24a c7b
  - 2025-12-08 — `3194937` — Create Overview.md
  - 2025-12-08 — `20cf7b2` — fix: Apply gradient background to start screen
  - 2025-12-07 — `430e0d8` — Update LICENSE
  - 2025-12-07 — `562b261` — codeql.yml (created) sync update 1.3.2
  - 2025-12-07 — `baf5daa` — sync update 1.3.2
  - 2025-12-07 — `44149f3` — sync update 1.3.2
  - 2025-12-07 — `32e3be5` — Update 1.3.2
  - 2025-12-01 — `8863e12` — Update 1.3.1
  - 2025-11-30 — `03262b0` — Update 1.3
  - 2025-11-30 — `7de0720` — Update 1.3.%

  > Notes:
  - Several commits are version bumps (e.g., `2.0.1.1`, `2.0.1-minor`, `2.0.0-minor`) and structural changes (project rename, AI-related file moves).
  - New modules and helper scripts were added under `ai_backend/modules` and top-level `modules/` during mid-December.

  ### Summary (high level)
  - Added: multiple new modules and generators (`tester_module`, `agent_module`, `helper_module`, `slides_generator`, `math_solver`, `game_generator`), privacy & policy docs, overview.
  - Changed: project rename to "Multi repository", multiple version bumps and workflow updates (Java setup in CI).
  - Fixed/Other: several game fixes and feature polish (enemy variety, randomized enemy generation, Bullet Forger features), static/site workflow updates.

  ## [1.0.1] - 2025-12-01

  ### Added
  - Multiple weapon types (Normal, Boost, Spreadshot, Laser, Plasma, Missile, Nova, Tempest, Chaos)
  - Enemy destruction particle effects
  - Score tracking and leaderboard
  - Power-ups (damage boost, speed boost, fire rate boost, invincibility, shield)
  - Game levels and wave progression
  - Boss battles
  - Sound effects and visual effects

  ### Changed
  - Improved game physics calculations
  - Enhanced canvas rendering performance

  ### Fixed
  - Bullet collision detection
  - Enemy spawning logic

  ## [1.0.0] - 2025-11-01

  ### Added
  - Initial Game Center release
  - Multiple game modes:
    - Space Invaders Classic
    - Space Invaders But Better (with weapons)
    - Space Shooter
    - Calculator
    - Flappy Clone
    - Snake
    - Breakout
    - Asteroids Shooter
    - Tower Defense
    - Bubble Shooter
    - Fireworks
  - Dynamic HTML content loading
  - Settings and configuration system
  - Jest testing framework setup
  - Developer documentation

  [Unreleased]: https://github.com/user/game-center/compare/v1.0.1...HEAD
  [1.0.1]: https://github.com/user/game-center/compare/v1.0.0...v1.0.1
  [1.0.0]: https://github.com/user/game-center/releases/tag/v1.0.0
