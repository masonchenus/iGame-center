# Developer Guide

This guide provides information for developers & new contributors working on the Game Center Application.

## Project Overview

The Game Center Application is a web-based app that dynamically loads HTML content for different modes: Game Center and Workshop.

## Code Structure

- `src/main.js`: Contains the main application logic, including mode loading and startup.
- `src/Shared/utils.js`: Provides utility functions like logging, clamping, and deep copying.
- `src/Config/AppSettings.json`: Holds application configuration settings.
- `src/Core/`: Directory with HTML files for different modes.
- `src/Testing Files/`: Contains test files to ensure code functionality.

## Testing

Tests are written using Jest and are located in `src/Testing Files/`.

To run tests:

```bash
npm test
```

The tests cover:
- Loading HTML for different modes.
- Error handling for fetch failures.
- Utility functions like log, clamp, and deepCopy.

## Development Workflow

1. Make changes to the code.
2. Run tests to ensure functionality.
3. Update documentation as needed.

## Best Practices

- Use ES6 modules for imports.
- Follow consistent naming conventions.
- Write tests for new functionality.
