/**
 * Content Renderer Module
 * Handles markdown, LaTeX, and syntax highlighting for AI responses
 */

/* global MarkdownIt, hljs, katex, DOMPurify */

class ContentRenderer {
    constructor() {
        this.initializeMarkdownIt();
        this.initializeHighlightJS();
        this.setupKatexOptions();
        this.setupCopyButtons();
    }

    /**
     * Initialize Markdown-it with custom configurations
     */
    initializeMarkdownIt() {
        if (typeof window.MarkdownIt === 'undefined') {
            console.warn('MarkdownIt not loaded, falling back to basic text');
            this.md = null;
            return;
        }

        this.md = new window.MarkdownIt({
            html: true,
            linkify: true,
            typographer: true,
            highlight: (str, lang) => {
                if (lang && this.hljs && this.hljs.getLanguage(lang)) {
                    try {
                        return `<pre class="hljs"><code>${this.hljs.highlight(str, { language: lang, ignoreIllegals: true }).value}</code></pre>`;
                    } catch (error) {
                        console.warn('Highlight.js error:', error);
                    }
                }

                return `<pre class="hljs"><code>${this.escapeHtml(str)}</code></pre>`;
            }
        });

        // Custom markdown-it plugins for enhanced features
        this.setupMarkdownExtensions();
    }

    /**
     * Setup Markdown extensions and custom rules
     */
    setupMarkdownExtensions() {
        if (!this.md) {
            return;
        }

        // Add LaTeX support to markdown
        const defaultRender = this.md.renderer.rules.text || function (tokens, options, env, self) {
            return self.renderToken(tokens, options, env, self);
        };

        this.md.renderer.rules.text = (tokens, idx, options, env, self) => {
            let content = defaultRender(tokens, idx, options, env, self);

            // Process inline LaTeX
            content = this.processInlineLatex(content);

            return content;
        };

        // Handle code blocks with copy buttons
        this.md.renderer.rules.fence = (tokens, idx, options, env, self) => {
            const token = tokens[idx];
            const lang = token.info ? this.md.utils.escapeHtml(token.info.trim()) : '';
            const code = token.content;

            const highlighted = this.highlightCode(code, lang);
            const copyButton = this.createCopyButton(code);

            return `<div class="code-block-container">
                        ${copyButton}
                        <div class="code-language">${lang}</div>
                        ${highlighted}
                    </div>`;
        };

        // Handle inline code
        this.md.renderer.rules.code_inline = (tokens, idx, options, env, self) => {
            const token = tokens[idx];
            const code = token.content;
            return `<code class="inline-code">${this.md.utils.escapeHtml(code)}</code>`;
        };
    }

    /**
     * Initialize Highlight.js configuration
     */
    initializeHighlightJS() {
        if (typeof window.hljs === 'undefined') {
            console.warn('Highlight.js not loaded, syntax highlighting disabled');
            this.hljs = null;
            return;
        }

        this.hljs = window.hljs;

        // Configure highlight.js
        this.hljs.configure({
            classPrefix: 'hljs-',
            languages: [
                'javascript', 'typescript', 'python', 'java', 'go', 'rust',
                'cpp', 'c', 'csharp', 'php', 'ruby', 'swift', 'kotlin',
                'scala', 'r', 'sql', 'html', 'css', 'scss', 'sass',
                'json', 'yaml', 'xml', 'markdown', 'bash', 'shell',
                'dockerfile', 'nginx', 'apache', 'ini', 'properties'
            ]
        });
    }

    /**
     * Setup KaTeX options for LaTeX rendering
     */
    setupKatexOptions() {
        if (typeof window.katex === 'undefined') {
            console.warn('KaTeX not loaded, LaTeX rendering disabled');
            this.katex = null;
            return;
        }

        this.katex = window.katex;

        this.katexOptions = {
            displayMode: false,
            throwOnError: false,
            errorColor: '#cc0000',
            macros: {
                "\\f": "#1f(#2)"
            }
        };

        this.katexDisplayOptions = {
            displayMode: true,
            throwOnError: false,
            errorColor: '#cc0000',
            macros: {
                "\\f": "#1f(#2)"
            }
        };
    }

    /**
     * Setup copy button functionality
     */
    setupCopyButtons() {
        document.addEventListener('click', (e) => {
            if (e.target.classList.contains('copy-button')) {
                this.copyToClipboard(e.target.dataset.code);
                this.updateCopyButton(e.target);
            }
        });
    }

    /**
     * Process inline LaTeX expressions
     */
    processInlineLatex(text) {
        if (!this.katex) {
            return text;
        }

        // Handle inline LaTeX: $...$
        return text.replace(/\$([^$]+)\$/g, (match, latex) => {
            try {
                return this.katex.renderToString(latex.trim(), this.katexOptions);
            } catch (error) {
                return `<span class="latex-error">${match}</span>`;
            }
        });
    }

    /**
     * Process display LaTeX expressions
     */
    processDisplayLatex(text) {
        if (!this.katex) {
            return text;
        }

        // Handle display LaTeX: $$...$$
        return text.replace(/\$\$([^$]+)\$\$/g, (match, latex) => {
            try {
                return this.katex.renderToString(latex.trim(), this.katexDisplayOptions);
            } catch (error) {
                return `<div class="latex-error">${match}</div>`;
            }
        });
    }

    /**
     * Highlight code with syntax highlighting
     */
    highlightCode(code, language) {
        if (this.hljs && language && this.hljs.getLanguage(language)) {
            try {
                const result = this.hljs.highlight(code, { language, ignoreIllegals: true });
                return `<pre class="hljs"><code>${result.value}</code></pre>`;
            } catch (error) {
                console.warn('Highlight.js error:', error);
            }
        }

        // Fallback to auto-detection or plain text
        if (this.hljs) {
            try {
                const result = this.hljs.highlightAuto(code);
                return `<pre class="hljs"><code>${result.value}</code></pre>`;
            } catch (error) {
                console.warn('Highlight.js auto-detection error:', error);
            }
        }

        return `<pre class="hljs"><code>${this.escapeHtml(code)}</code></pre>`;
    }

    /**
     * Create copy button for code blocks
     */
    createCopyButton(code) {
        const encodedCode = btoa(unescape(encodeURIComponent(code)));
        return `<button class="copy-button" data-code="${encodedCode}" title="Copy code">
                    <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                        <rect x="9" y="9" width="13" height="13" rx="2" ry="2"></rect>
                        <path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"></path>
                    </svg>
                    Copy
                </button>`;
    }

    /**
     * Update copy button appearance after copying
     */
    updateCopyButton(button) {
        const originalText = button.innerHTML;
        button.innerHTML = '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><polyline points="20,6 9,17 4,12"></polyline></svg> Copied!';
        button.classList.add('copied');

        setTimeout(() => {
            button.innerHTML = originalText;
            button.classList.remove('copied');
        }, 2000);
    }

    /**
     * Copy text to clipboard
     */
    async copyToClipboard(text) {
        try {
            const decodedCode = decodeURIComponent(escape(atob(text)));
            await navigator.clipboard.writeText(decodedCode);
        } catch (error) {
            console.error('Failed to copy to clipboard:', error);
            // Fallback for older browsers
            const textArea = document.createElement('textarea');
            textArea.value = text;
            document.body.appendChild(textArea);
            textArea.select();
            document.execCommand('copy');
            document.body.removeChild(textArea);
        }
    }

    /**
     * Escape HTML characters
     */
    escapeHtml(text) {
        const div = document.createElement('div');
        div.textContent = text;
        return div.innerHTML;
    }

    /**
     * Render markdown content with LaTeX and code highlighting
     */
    renderContent(content) {
        if (!content || typeof content !== 'string') {
            return '<p>No content to render</p>';
        }

        try {
            // If MarkdownIt is not available, return plain text
            if (!this.md) {
                return this.escapeHtml(content);
            }

            // First, process display LaTeX ($$...$$)
            content = this.processDisplayLatex(content);

            // Then render markdown
            let rendered = this.md.render(content);

            // Sanitize the final HTML if DOMPurify is available
            if (typeof DOMPurify !== 'undefined') {
                rendered = DOMPurify.sanitize(rendered, {
                    ALLOWED_TAGS: [
                        'p', 'br', 'strong', 'em', 'u', 's', 'del', 'blockquote',
                        'h1', 'h2', 'h3', 'h4', 'h5', 'h6',
                        'ul', 'ol', 'li', 'code', 'pre',
                        'a', 'img', 'table', 'thead', 'tbody', 'tr', 'th', 'td',
                        'hr', 'span', 'div'
                    ],
                    ALLOWED_ATTR: ['href', 'src', 'alt', 'title', 'class', 'id', 'data-*'],
                    ALLOW_DATA_ATTR: true
                });
            }

            return rendered;
        } catch (error) {
            console.error('Content rendering error:', error);
            return `<div class="render-error">Error rendering content: ${this.escapeHtml(content)}</div>`;
        }
    }

    /**
     * Render just LaTeX expressions
     */
    renderLatex(latex) {
        if (!this.katex) {
            return this.escapeHtml(latex);
        }

        try {
            return this.katex.renderToString(latex, this.katexOptions);
        } catch (error) {
            return `<span class="latex-error">${this.escapeHtml(latex)}</span>`;
        }
    }

    /**
     * Render just code with syntax highlighting
     */
    renderCode(code, language = '') {
        return this.highlightCode(code, language);
    }

    /**
     * Check if content contains markdown, LaTeX, or code
     */
    analyzeContent(content) {
        return {
            hasMarkdown: /[*_#\-[]()`]/.test(content),
            hasLatex: /\$[^$]*\$|\$\$[^$]*\$\$/.test(content),
            hasCode: /```[\s\S]*?```|`[^`]+`/.test(content),
            hasTables: /\|[\s\S]*\|/.test(content),
            hasLists: /^[\s]*[-*+]|^[\s]*\d+\./m.test(content)
        };
    }

    /**
     * Get supported languages for syntax highlighting
     */
    getSupportedLanguages() {
        if (this.hljs) {
            return this.hljs.listLanguages();
        }
        return [];
    }

    /**
     * Initialize renderer for a specific container
     */
    initializeContainer(container) {
        if (typeof container === 'string') {
            container = document.querySelector(container);
        }

        if (container) {
            // Add event listeners for interactive elements
            container.addEventListener('click', (e) => {
                if (e.target.tagName === 'A' && e.target.href) {
                    // Handle external links
                    if (e.target.href.startsWith('http')) {
                        e.target.target = '_blank';
                        e.target.rel = 'noopener noreferrer';
                    }
                }
            });

            // Add copy functionality to code blocks
            container.querySelectorAll('pre').forEach(pre => {
                if (!pre.querySelector('.copy-button')) {
                    const code = pre.textContent;
                    const copyButton = this.createCopyButton(btoa(unescape(encodeURIComponent(code))));
                    pre.parentNode.insertAdjacentHTML('beforebegin', copyButton);
                }
            });
        }
    }

    /**
     * Load external dependencies dynamically
     */
    async loadDependencies() {
        const dependencies = [
            'https://cdn.jsdelivr.net/npm/markdown-it@14.1.0/dist/markdown-it.min.js',
            'https://cdn.jsdelivr.net/npm/highlight.js@11.10.0/highlight.min.js',
            'https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js',
            'https://cdn.jsdelivr.net/npm/dompurify@3.1.6/dist/purify.min.js'
        ];

        const scripts = [];

        for (const src of dependencies) {
            try {
                await this.loadScript(src);
                scripts.push(src);
            } catch (error) {
                console.warn(`Failed to load ${src}:`, error);
            }
        }

        // Re-initialize if dependencies loaded
        if (scripts.length > 0) {
            this.initializeMarkdownIt();
            this.initializeHighlightJS();
            this.setupKatexOptions();
        }

        return scripts.length > 0;
    }

    /**
     * Load a script dynamically
     */
    loadScript(src) {
        return new Promise((resolve, reject) => {
            const script = document.createElement('script');
            script.src = src;
            script.onload = resolve;
            script.onerror = reject;
            document.head.appendChild(script);
        });
    }
}

// Export for use in other modules
export default ContentRenderer;

// Create global instance for use in the interface
if (typeof window !== 'undefined') {
    window.ContentRenderer = ContentRenderer;
}
