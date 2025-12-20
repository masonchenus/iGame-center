// Import the AppSettings.json file with correct case-sensitive path
import APP_SETTINGS from './Config/AppSettings.json';

// Import utility functions
import { log } from './Shared/utils.js';

/**
 * Dynamically loads and displays HTML content for the specified mode.
 * @param {string} mode - The mode to load ('gamecenter' or 'workshop').
 * @param {object} settings - Application settings.
 * @param {function} logFunction - Logging function.
 */
export async function loadModeHTML(mode, settings, logFunction) {
    let htmlPath;
    if (mode === 'gamecenter') {
        htmlPath = './gamecenter.html';
    } else {
        htmlPath = `./Core/${mode}.html`;
    }
    try {
        const response = await fetch(htmlPath);
        if (!response.ok) {
            throw new Error(`Failed to load ${htmlPath}: ${response.statusText}`);
        }
        const htmlContent = await response.text();
        document.body.innerHTML = htmlContent;
        logFunction(`${mode.charAt(0).toUpperCase() + mode.slice(1)} loaded successfully.`, "LAUNCH");
    } catch (error) {
        logFunction(`Error loading ${mode}: ${error.message}`, "ERROR");
        // Fallback: display error message
        document.body.innerHTML = `<h1>Error</h1><p>Failed to load ${mode}. Please check the console for details.</p><code>${error.message}</code>`;
    }
}

/**
 * Main application logic.
 */
export async function startApplication() {
    log("Application starting up...", 'STARTUP');

    const debugMode = APP_SETTINGS.app.debugMode;
    const apiUrl = APP_SETTINGS.networking.apiBaseUrl;
    log(`Debug Mode: ${debugMode}. API: ${apiUrl}`, 'CONFIG');

    log("Initialization complete.", "STARTUP");
}

// Execute the main function when the script runs
startApplication();
