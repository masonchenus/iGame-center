---
description: Repository Information Overview
alwaysApply: true
---

# Game Center Application Information

## Summary

Game Center Application is a web-based Node.js application that dynamically loads and manages different modes (Game Center and Workshop). It uses ES modules for modern JavaScript development and includes comprehensive testing with Jest. The application supports dynamic HTML content loading, configuration-driven settings, and logging utilities for debugging.

## Structure

- **src/main.js** - Primary application entry point; manages mode loading and initialization
- **src/Core/** - HTML files for different application modes (index.html, workshop.html, scoreboard.html)
- **src/Shared/utils.js** - Shared utility functions (logging, clamping, deep copy)
- **src/Config/AppSettings.json** - Application configuration with settings for networking, UI, and game parameters
- **src/Testing Files/** - Jest test suite files (main.test.js, utils.test.js)
- **Documentation/** - Developer guides and API documentation
- **Assets/images/** - Application images and static assets

## Language & Runtime

**Language**: JavaScript (ES Modules)  
**Runtime**: Node.js (requires experimental VM modules flag for testing)  
**Module Type**: ES Module (`"type": "module"` in package.json)  
**Package Manager**: npm

## Dependencies

**Main Dependencies**:
- **python3** (^0.0.1) - Python runtime integration
- **stylus** (^0.64.0) - CSS preprocessor

**Development Dependencies**:
- **jest** (^29.7.0) - Testing framework
- **jest-environment-jsdom** (^29.7.0) - DOM environment for Jest tests

## Build & Installation

```bash
# Install dependencies
npm install

# Start the application
npm start

# Run tests
npm test
```

**Entry Point**: `src/main.js`
**Start Command**: `node src/main.js`

## Testing

**Framework**: Jest (v29.7.0)  
**Environment**: jsdom (for DOM testing)  
**Test Location**: `src/Testing Files/`  
**Naming Convention**: `*.test.js`  
**Configuration**: Jest config embedded in package.json

**Test Files**:
- `src/Testing Files/main.test.js` - Tests for main application logic
- `src/Testing Files/utils.test.js` - Tests for utility functions

**Run Command**:
```bash
npm test
# Internally runs: node --experimental-vm-modules node_modules/jest/bin/jest.js
```

## Configuration

**Application Settings File**: `src/Config/AppSettings.json`

Key configuration sections:
- **app** - Version, language, debug mode
- **networking** - API base URL, socket URL, timeout settings
- **ui** - Theme, resolution settings
- **game** - Max players, default region

## Main Files & Resources

- **src/main.js** - Exports `loadModeHTML()` function for dynamic content loading
- **src/Shared/utils.js** - Exports `log()`, `clamp()`, `deepCopy()` utility functions
- **src/Core/index.html** - Default Game Center mode HTML
- **src/Core/workshop.html** - Workshop mode HTML
- **src/Core/scoreboard.html** - Scoreboard mode HTML

## Version Information

**Application Version**: 1.0.1  
**API Version** (configured): v1  
**License**: Apache License 2.0
