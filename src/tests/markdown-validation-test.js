import { jest } from '@jest/globals';
import fs from 'fs';
import path from 'path';

describe('Markdown Files Validation', () => {
    describe('CONTRIBUTING.md', () => {
        let content;

        beforeAll(() => {
            const filePath = path.join(process.cwd(), 'CONTRIBUTING.md');
            content = fs.readFileSync(filePath, 'utf-8');
        });

        test('should exist and be readable', () => {
            expect(content).toBeDefined();
            expect(content.length).toBeGreaterThan(0);
        });

        test('should have main heading', () => {
            expect(content).toMatch(/^#\s+Contributing/m);
        });

        test('should contain Bug Reports section', () => {
            expect(content).toMatch(/##\s+Bug Reports/);
        });

        test('should contain Feature Requests section', () => {
            expect(content).toMatch(/##\s+Feature Requests/);
        });

        test('should contain Pull Requests section', () => {
            expect(content).toMatch(/##\s+Pull Requests/);
        });

        test('should provide bug reporting guidelines', () => {
            expect(content).toContain('Check if the bug has already been reported');
            expect(content).toContain('Provide a clear and descriptive title');
            expect(content).toContain('Include steps to reproduce');
        });

        test('should provide feature request guidelines', () => {
            expect(content).toContain("Explain the problem you're trying to solve");
            expect(content).toContain('Describe your proposed solution');
        });

        test('should provide PR guidelines', () => {
            expect(content).toContain('Keep your changes focused and atomic');
            expect(content).toContain('Write a clear and descriptive commit message');
            expect(content).toContain('Ensure all tests pass');
        });

        test('should not contain broken markdown syntax', () => {
            // Check for common markdown errors
            expect(content).not.toMatch(/#{7,}/); // Too many hash marks
            expect(content).not.toMatch(/\[.*\]\(\s*\)/); // Empty links
        });

        test('should have proper line breaks', () => {
            const lines = content.split('\n');
            expect(lines.length).toBeGreaterThan(5);
        });
    });

    describe('SECURITY.md', () => {
        let content;

        beforeAll(() => {
            const filePath = path.join(process.cwd(), 'SECURITY.md');
            content = fs.readFileSync(filePath, 'utf-8');
        });

        test('should exist and be readable', () => {
            expect(content).toBeDefined();
            expect(content.length).toBeGreaterThan(0);
        });

        test('should have main heading', () => {
            expect(content).toMatch(/^#\s+Security Policy/m);
        });

        test('should contain Reporting a Vulnerability section', () => {
            expect(content).toMatch(/##\s+Reporting a Vulnerability/);
        });

        test('should provide email contact', () => {
            expect(content).toContain('masonchenus@gmail.com');
            expect(content).toMatch(/mailto:masonchenus@gmail\.com/);
        });

        test('should mention response time', () => {
            expect(content).toContain('48 hours');
        });

        test('should request vulnerability details', () => {
            expect(content).toContain('detailed description');
            expect(content).toContain('how to reproduce');
        });

        test('should mention version information requirement', () => {
            expect(content).toContain('version of the software');
        });

        test('should mention logs or screenshots', () => {
            expect(content).toContain('logs or screenshots');
        });

        test('should express appreciation for reports', () => {
            expect(content.toLowerCase()).toMatch(/appreciate|thank/);
        });

        test('should not contain broken markdown links', () => {
            const linkPattern = /\[([^\]]+)\]\(([^)]+)\)/g;
            const links = [...content.matchAll(linkPattern)];

            links.forEach(link => {
                expect(link[2]).not.toBe('');
                expect(link[1]).not.toBe('');
            });
        });
    });

    describe('README.md Updates', () => {
        let content;

        beforeAll(() => {
            const filePath = path.join(process.cwd(), 'README.md');
            content = fs.readFileSync(filePath, 'utf-8');
        });

        test('should exist and be readable', () => {
            expect(content).toBeDefined();
            expect(content.length).toBeGreaterThan(0);
        });

        test('should have proper code blocks for installation', () => {
            expect(content).toMatch(/\`\`\`bash\s*\n\s*git clone/);
        });

        test('should contain contributing section', () => {
            expect(content).toMatch(/##\s+Contributing/);
            expect(content).toContain('CONTRIBUTING.md');
        });

        test('should contain security section', () => {
            expect(content).toMatch(/##\s+Security/);
            expect(content).toContain('SECURITY.md');
        });

        test('should link to contributing guidelines', () => {
            expect(content).toMatch(/\[Contributing Guidelines\]\(CONTRIBUTING\.md\)/);
        });

        test('should link to security policy', () => {
            expect(content).toMatch(/\[Security Policy\]\(SECURITY\.md\)/);
        });

        test('should have contact section', () => {
            expect(content).toMatch(/##\s+Contact/);
            expect(content).toContain('masonchenus@gmail.com');
        });

        test('should have improved errors section', () => {
            expect(content).toMatch(/##\s+Errors/);
            expect(content).not.toContain('Issues.txt');
            expect(content).toContain('open an issue');
        });

        test('should have proper npm test command', () => {
            expect(content).toMatch(/\`\`\`bash\s*\n\s*npm test/);
        });

        test('should not have old formatting issues', () => {
            expect(content).not.toMatch(/`\s*\n\s*git clone/); // Old inline code block
            expect(content).not.toContain('</br>'); // HTML breaks
        });

        test('should have all required sections', () => {
            const requiredSections = [
                'Installation',
                'Usage',
                'Testing',
                'Project Structure',
                'Contributing',
                'Security',
                'Errors',
                'Contact'
            ];

            requiredSections.forEach(section => {
                expect(content).toMatch(new RegExp(`##\\s+${section}`, 'i'));
            });
        });
    });

    describe('.vercelignore', () => {
        let content;

        beforeAll(() => {
            const filePath = path.join(process.cwd(), '.vercelignore');
            content = fs.readFileSync(filePath, 'utf-8');
        });

        test('should exist and be readable', () => {
            expect(content).toBeDefined();
            expect(content.length).toBeGreaterThan(0);
        });

        test('should ignore .git directory', () => {
            expect(content).toMatch(/^\.git$/m);
        });

        test('should ignore .vscode directory', () => {
            expect(content).toMatch(/^\.vscode$/m);
        });

        test('should ignore node_modules', () => {
            expect(content).toMatch(/^node_modules$/m);
        });

        test('should ignore .DS_Store', () => {
            expect(content).toMatch(/^\.DS_Store$/m);
        });

        test('should ignore npm log files', () => {
            expect(content).toMatch(/npm-debug\.log\*/);
        });

        test('should ignore yarn log files', () => {
            expect(content).toMatch(/yarn-debug\.log\*/);
            expect(content).toMatch(/yarn-error\.log\*/);
        });

        test('should ignore .npm directory', () => {
            expect(content).toMatch(/^\.npm$/m);
        });

        test('should have proper line breaks', () => {
            const lines = content.trim().split('\n');
            expect(lines.length).toBe(8);
        });

        test('should not have empty lines in middle', () => {
            const lines = content.trim().split('\n');
            lines.forEach(line => {
                expect(line.length).toBeGreaterThan(0);
            });
        });
    });
});
