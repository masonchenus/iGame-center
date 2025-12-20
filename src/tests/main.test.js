import { jest } from '@jest/globals';
import { log } from '../Shared/utils.js';

// Mock the fetch function
global.fetch = jest.fn();

// Mock the document object
global.document = {
    body: {
        innerHTML: ''
    }
};

// Mock URLSearchParams
global.URLSearchParams = jest.fn().mockImplementation((search) => ({
    get: jest.fn((key) => {
        if (key === 'mode') {
            return search.includes('mode=workshop') ? 'workshop' : null;
        }
        return null;
    })
}));

// Mock window.location
global.window = {
    location: {
        search: ''
    }
};

// Import the functions to test
import { loadModeHTML, startApplication } from '../main.js';

describe('loadModeHTML', () => {
    beforeEach(() => {
        jest.clearAllMocks();
        document.body.innerHTML = '';
    });

    test('should load HTML content successfully for gamecenter mode', async () => {
        const mockHtml = '<div>Game Center Content</div>';
        fetch.mockResolvedValueOnce({
            ok: true,
            text: jest.fn().mockResolvedValue(mockHtml)
        });

        const mockLog = jest.fn();

        await loadModeHTML('gamecenter', {}, mockLog);

        expect(fetch).toHaveBeenCalledWith('./gamecenter.html');
        expect(document.body.innerHTML).toBe(mockHtml);
        expect(mockLog).toHaveBeenCalledWith('Gamecenter loaded successfully.', 'LAUNCH');
    });

    test('should load HTML content successfully for workshop mode', async () => {
        const mockHtml = "<div>Workshop Content</div>";
        fetch.mockResolvedValueOnce({
            ok: true,
            text: jest.fn().mockResolvedValue(mockHtml)
        });

        const mockLog = jest.fn();

        await loadModeHTML('workshop', {}, mockLog);

        expect(fetch).toHaveBeenCalledWith('./Core/workshop.html');
        expect(document.body.innerHTML).toBe(mockHtml);
        expect(mockLog).toHaveBeenCalledWith('Workshop loaded successfully.', 'LAUNCH');
    });

    test('should handle fetch failure', async () => {
        const errorMessage = 'Failed to load ./gamecenter.html: 404 Not Found';
        fetch.mockResolvedValueOnce({
            ok: false,
            statusText: '404 Not Found'
        });

        const mockLog = jest.fn();

        await loadModeHTML('gamecenter', {}, mockLog);

        expect(fetch).toHaveBeenCalledWith('./gamecenter.html');
        expect(mockLog).toHaveBeenCalledWith(`Error loading gamecenter: ${errorMessage}`, 'ERROR');
        expect(document.body.innerHTML).toBe(`<h1>Error</h1><p>Failed to load gamecenter. Please check the console for details.</p><code>${errorMessage}</code>`);
    });

    test('should handle network error', async () => {
        const errorMessage = 'Network error';
        fetch.mockRejectedValueOnce(new Error(errorMessage));

        const mockLog = jest.fn();

        await loadModeHTML('gamecenter', {}, mockLog);

        expect(fetch).toHaveBeenCalledWith('./gamecenter.html');
        expect(mockLog).toHaveBeenCalledWith(`Error loading gamecenter: ${errorMessage}`, 'ERROR');
        expect(document.body.innerHTML).toBe(`<h1>Error</h1><p>Failed to load gamecenter. Please check the console for details.</p><code>${errorMessage}</code>`);
    });
});
