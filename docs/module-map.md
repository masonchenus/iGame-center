# 🗺️ Code Structure Module Map

**Purpose:** Since our core logic resides in monolithic files (`GameCenter.js` and `Workshop.js`), this map is mandatory for developer navigation and code maintenance. **DO NOT** add code outside of these established sections.

---

## 1. `src/core/index.html` (Approx. 12,943 Lines)

| Section Header (Comment)                           | Line Range (Approx.) | Feature/Purpose                                                                     |
| :------------------------------------------------- | :------------------- | :---------------------------------------------------------------------------------- |
| `// --- SECTION: Core Initialization ---`          | 1 - 500              | Global constants, external imports, and the `initializeGameCenter` function.        |
| `// --- SECTION: UI Rendering ---`                 | 501 - 3000           | All functions for dynamic UI generation (e.g., menu(), renderProfile()).            |
| `// --- SECTION: Networking/API Client ---`        | 3001 - 7000          | All functions making backend API calls (e.g., `fetchGameList()`, `postUserData()`). |
| `// --- SECTION: User Profile Management ---`      | 7001 - 9500          | Logic for stats calculation, inventory storage, and achievement tracking.           |
| `// --- SECTION: Utility Functions (Internal) ---` | 9501 - 12943         | Helper functions used only internally (e.g., data validation, internal sorting).    |

---

## 2. `src/core/Workshop.html` (Approx. 2,000 Lines)

| Section Header (Comment)                   | Line Range (Approx.) | Feature/Purpose                                              |
| :----------------------------------------- | :------------------- | :----------------------------------------------------------- |
| `// --- SECTION: Editor Setup & State ---` | 1 - 100              | Initializing the canvas, global variables, and module state. |
| `// --- SECTION: Asset Importer Logic ---` | 101 - 500            | Handling file uploads and validating models/textures.        |
| `// --- SECTION: Level Editor Core ---`    | 501 - 1500           | Mouse/keyboard input handling and object manipulation logic. |

## 3. `src/core/calculator.html` (Approx. 2,000 Lines)

| Section Header (Comment)                                | Line Range (Approx.) | Feature/Purpose                                                                                |
| :------------------------------------------------------ | :------------------- | :--------------------------------------------------------------------------------------------- |
| `// --- SECTION: Calculator UI & Layout ---`            | 1 - 300              | Markup and DOM wiring for the calculator panel, buttons, and display area.                     |
| `// --- SECTION: Input Handling & Keybindings ---`      | 301 - 600            | Keyboard shortcuts, button debouncing, and input sanitization.                                 |
| `// --- SECTION: Expression Parser & Evaluator ---`     | 601 - 1100           | Tokenizer, parser (shunting-yard or recursive descent), and evaluation engine for expressions. |
| `// --- SECTION: Scientific Functions & Extensions ---` | 1101 - 1500          | Trig, log, constants, unit conversions, and plugin hooks for extra functions.                  |
| `// --- SECTION: History, Memory & Persistence ---`     | 1501 - 1800          | Store/retrieve past calculations, memory slots, and localStorage persistence.                  |
| `// --- SECTION: Utilities & Formatting ---`            | 1801 - 2000          | Numeric formatting, rounding rules, localization helpers, and error formatting.                |



## 4. `src/core/BulletForger/` (Approx. NaN lines)

| File / Section | Line Range (Approx.) | Feature/Purpose                                                                   |
| :------------- | :------------------- | :-------------------------------------------------------------------------------- |
| `index.html`   | 1 - 300              | Main UI container for Bullet Forger editor (canvas, toolbar, preview).            |
| `presets.js`   | 301 - 900            | Preset definitions, import/export presets, and preset metadata.                   |
| `editor-ui.js` | 901 - 1600           | Toolbar actions, property panels, live preview bindings, and event handlers.      |
| `renderer.js`  | 1601 - 2600          | Particle and bullet rendering pipeline used by the preview and exported runtime.  |
| `physics.js`   | 2601 - 3400          | Motion equations, collision helpers (for preview), and path generation utilities. |
| `export.js`    | 3401 - 3800          | Serialization, codegen for runtime use, and file download helpers.                |
| `import.js`    | 3801 - 4100          | Parsing imported presets, backwards compatibility handlers, and validation.       |
| `utils/`       | 4101 - 4500          | Small helpers: color utilities, random seed management, and math helpers.         |

## 5. `src/core/AI.html` (Approx. 0 lines, planned for later updates) 

> Planned integration surface for in-browser AI features. This file is intentionally empty now and will be scaffolded when AI features are onboarded.

| Section Header (Planned)                           | Line Range (Est.) | Feature/Purpose                                                                 |
| :------------------------------------------------- | :---------------: | :------------------------------------------------------------------------------ |
| `// --- SECTION: Model Loader & Adapters ---`      |      1 - 400      | Load local/lightweight models or remote inference adapters; model selection UI. |
| `// --- SECTION: Prompt Templates & Helpers ---`   |     401 - 800     | Prebuilt prompts, prompt merging utilities, and templating helpers.             |
| `// --- SECTION: Conversation / Context Store ---` |    801 - 1200     | Short-term context buffer, conversation state, and retention policies.          |
| `// --- SECTION: Tools & Plugins Bridge ---`       |    1201 - 1600    | Integration hooks for tools (code execution, search, knowledge connectors).     |
| `// --- SECTION: Safety & Usage Policies ---`      |    1601 - 1800    | Sanitization, user consent UI, rate-limiting and telemetry stubs.               |
| `// --- SECTION: Telemetry & Diagnostics ---`      |    1801 - 2000    | Logging, performance metrics, and debug helpers for model calls.                |
