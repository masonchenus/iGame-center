# Game Center Application

This is the Game Center Application, a web-based app that supports game center and workshop modes.

## Features

- Dynamic loading of HTML content for different modes (Game Center or Workshop).
- Configuration via JSON settings.
- Logging utilities for debugging.

## Installation

1. Clone the repository using the following terminal command:
   ```bash
   git clone https://github.com/masonchenus/iGame-center.git
   ```
2. Run `npm install` to install dependencies.

## Usage

- To start the application: `npm start`
- To run tests: `npm test`

## Testing

Testing files are located in `src/Testing Files/`.

The tests ensure all code functionality works correctly, covering:

- Main application logic in `src/main.js`
- Utility functions in `src/Shared/utils.js`

Run tests with:
```bash
npm test
```

## Project Structure

- `src/main.js`: Main application entry point.
- `src/Shared/utils.js`: Utility functions.
- `src/Config/AppSettings.json`: Application settings.
- `src/Core/`: HTML files for different modes.
- `src/Testing Files/`: Test files.

## Contributing

We welcome contributions! Please see our [Contributing Guidelines](CONTRIBUTING.md) for more details.

## Security

If you find a security vulnerability, please refer to our [Security Policy](SECURITY.md).

## Errors

If you encounter any errors during installation or usage, please check your Node.js version and try reinstalling dependencies. If the issue persists, feel free to open an issue.

## Contact

If you have any questions about this project, please contact us at masonchenus@gmail.com.
