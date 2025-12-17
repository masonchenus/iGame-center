import { jest } from '@jest/globals';
import { log, clamp, deepCopy } from '../Shared/utils.js';

describe('utils.js', () => {
    beforeEach(() => {
        jest.clearAllMocks();
        console.log = jest.fn();
    });

    test('log function should format and log messages correctly', () => {
        log('Test message', 'INFO');
        expect(console.log).toHaveBeenCalledWith(expect.stringMatching(/^\[\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z\] \[INFO\]: Test message$/));
    });

    test('clamp function should clamp values within range', () => {
        expect(clamp(5, 0, 10)).toBe(5);
        expect(clamp(-5, 0, 10)).toBe(0);
        expect(clamp(15, 0, 10)).toBe(10);
    });

    test('deepCopy function should create a deep copy of objects', () => {
        const original = { a: 1, b: { c: 2 } };
        const copy = deepCopy(original);
        expect(copy).toEqual(original);
        expect(copy).not.toBe(original);
        expect(copy.b).not.toBe(original.b);
    });

    test('deepCopy function should handle arrays', () => {
        const original = [1, 2, { a: 3 }];
        const copy = deepCopy(original);
        expect(copy).toEqual(original);
        expect(copy).not.toBe(original);
        expect(copy[2]).not.toBe(original[2]);
    });

    test('deepCopy function should handle primitives', () => {
        expect(deepCopy(42)).toBe(42);
        expect(deepCopy('string')).toBe('string');
        expect(deepCopy(null)).toBe(null);
    });
});
