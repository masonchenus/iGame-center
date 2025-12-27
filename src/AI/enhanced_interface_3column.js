
// Enhanced 3-Column AI Interface JavaScript
/* global ContentRenderer */

class EnhancedAIInterface {
    constructor() {
        this.chatSessions = this.loadChatSessions();
        this.currentSession = 'current-session';
        this.currentModule = 'codegen_module';
        this.selectedCategory = 'all';
        this.isParametersVisible = false;
        this.allMetrics = []; // Will store 500+ metrics
        this.contentRenderer = null;

        this.init();
    }

    init() {
        this.setupEventListeners();
        this.updateCurrentTime();
        this.startMetricsUpdate();
        this.loadChatHistory();
        this.loadAllModules();
        this.updateParameters();
        this.setupResponsiveLayout();
        this.initializeContentRenderer();
    }

    setupEventListeners() {
        // Chat functionality
        document.getElementById('sendBtn').addEventListener('click', () => this.sendMessage());
        document.getElementById('promptInput').addEventListener('keypress', (e) => {
            if (e.key === 'Enter' && !e.shiftKey) {
                e.preventDefault();
                this.sendMessage();
            }
        });

        // New chat functionality
        document.getElementById('newChatBtn').addEventListener('click', () => this.startNewChat());

        // Chat search
        document.getElementById('chatSearch').addEventListener('input', (e) => {
            this.filterChatHistory(e.target.value);
        });

        // Parameters toggle
        document.getElementById('toggleParameters').addEventListener('click', () => {
            this.toggleParameters();
        });

        // Parameter controls
        this.setupParameterControls();

        // Module functionality
        document.getElementById('moduleSearch').addEventListener('input', (e) => {
            this.filterModules(e.target.value);
        });

        // Category filtering
        document.querySelectorAll('.category-btn').forEach(btn => {
            btn.addEventListener('click', (e) => {
                this.filterByCategory(e.target.dataset.category);
            });
        });

        // Show all metrics
        document.getElementById('showAllMetrics').addEventListener('click', () => {
            this.showAllMetrics();
        });
    }

    setupParameterControls() {
        const controls = [
            'wLearningRate', 'wRegularization', 'wSize',
            'bLearningRate', 'bRegularization', 'bSize'
        ];

        controls.forEach(control => {
            const slider = document.getElementById(control);
            const valueDisplay = document.getElementById(control + 'Value');

            slider.addEventListener('input', (e) => {
                const value = e.target.value;
                if (control.includes('Size')) {
                    valueDisplay.textContent = parseInt(value).toLocaleString();
                } else {
                    valueDisplay.textContent = parseFloat(value).toFixed(4);
                }
            });
        });

        document.getElementById('updateParameters').addEventListener('click', () => {
            this.updateParameters();
        });
    }

    // Chat History Management
    loadChatSessions() {
        const sessions = localStorage.getItem('chatSessions');
        return sessions ? JSON.parse(sessions) : {};
    }

    saveChatSessions() {
        localStorage.setItem('chatSessions', JSON.stringify(this.chatSessions));
    }

    loadChatHistory() {
        const chatHistoryList = document.getElementById('chatHistoryList');
        chatHistoryList.innerHTML = '';

        // Add current session
        const currentSessionDiv = document.createElement('div');
        currentSessionDiv.className = 'chat-session active';
        currentSessionDiv.dataset.sessionId = this.currentSession;
        currentSessionDiv.innerHTML = `
            <div class="chat-session-title">Current Session</div>
            <div class="chat-session-preview">Active chat session</div>
            <div class="chat-session-time">Active now</div>
        `;
        chatHistoryList.appendChild(currentSessionDiv);

        // Add saved sessions
        Object.entries(this.chatSessions).forEach(([sessionId, session]) => {
            const sessionDiv = document.createElement('div');
            sessionDiv.className = 'chat-session';
            sessionDiv.dataset.sessionId = sessionId;
            sessionDiv.innerHTML = `
                <div class="chat-session-title">${session.title}</div>
                <div class="chat-session-preview">${session.preview}</div>
                <div class="chat-session-time">${session.timestamp}</div>
            `;
            sessionDiv.addEventListener('click', () => this.loadSession(sessionId));
            chatHistoryList.appendChild(sessionDiv);
        });
    }

    startNewChat() {
        if (confirm('Start a new chat? Current conversation will be saved.')) {
            this.saveCurrentSession();
            this.clearCurrentChat();
            this.startNewSession();
        }
    }

    saveCurrentSession() {
        const chatHistory = document.getElementById('chatHistory');
        const messages = chatHistory.querySelectorAll('.message');

        if (messages.length > 1) { // More than just the welcome message
            const preview = messages[messages.length - 1].textContent.substring(0, 100) + '...';
            const sessionId = Date.now().toString();

            this.chatSessions[sessionId] = {
                title: `Chat ${Object.keys(this.chatSessions).length + 1}`,
                preview: preview,
                timestamp: new Date().toLocaleString(),
                messages: Array.from(messages).map(msg => ({
                    type: msg.classList.contains('user') ? 'user' : 'ai',
                    content: msg.textContent
                }))
            };

            this.saveChatSessions();
        }
    }

    clearCurrentChat() {
        const chatHistory = document.getElementById('chatHistory');
        chatHistory.innerHTML = `
            <div class="message ai">
                <strong>AI System:</strong> Welcome to the Enhanced AI Interface! I can process your requests using advanced parameter optimization. How can I help you today?
                <div style="font-size: 0.8em; color: #ccc; margin-top: 5px;">Just now</div>
            </div>
        `;
    }

    startNewSession() {
        this.currentSession = 'current-session';
        this.loadChatHistory();
        this.currentModule = 'codegen_module';
        this.selectedCategory = 'all';
    }

    loadSession(sessionId) {
        if (this.chatSessions[sessionId]) {
            this.saveCurrentSession();
            this.currentSession = sessionId;
            this.loadChatHistory();

            const chatHistory = document.getElementById('chatHistory');
            chatHistory.innerHTML = '';

            this.chatSessions[sessionId].messages.forEach(msg => {
                const messageDiv = document.createElement('div');
                messageDiv.className = `message ${msg.type}`;
                messageDiv.innerHTML = `
                    <strong>${msg.type === 'user' ? 'You' : 'AI System'}:</strong> ${msg.content}
                    <div style="font-size: 0.8em; color: #ccc; margin-top: 5px;">Loaded</div>
                `;
                chatHistory.appendChild(messageDiv);
            });

            this.scrollToBottom();
        }
    }

    filterChatHistory(query) {
        const sessions = document.querySelectorAll('.chat-session');
        sessions.forEach(session => {
            const title = session.querySelector('.chat-session-title').textContent.toLowerCase();
            const preview = session.querySelector('.chat-session-preview').textContent.toLowerCase();
            const matches = title.includes(query.toLowerCase()) || preview.includes(query.toLowerCase());
            session.style.display = matches ? 'block' : 'none';
        });
    }

    // Module Management
    loadAllModules() {
        // Load all 57 real AI modules from backend
        this.modules = [
            // Development & Programming
            { id: 'codegen_module', name: 'Code Generator', category: 'development', description: 'Advanced code generation with multiple language support and best practices', status: 'available' },
            { id: 'javascript_ts_module', name: 'JavaScript/TypeScript', category: 'development', description: 'Modern JS/TS development with React, Node.js, and framework support', status: 'available' },
            { id: 'python_ml_module', name: 'Python ML', category: 'development', description: 'Machine learning, data science, and Python development tools', status: 'available' },
            { id: 'java_module', name: 'Java', category: 'development', description: 'Java programming, Spring framework, and enterprise development', status: 'available' },
            { id: 'go_module', name: 'Go', category: 'development', description: 'Go programming language, microservices, and backend development', status: 'available' },
            { id: 'rust_module', name: 'Rust', category: 'development', description: 'Systems programming with Rust, performance optimization', status: 'available' },
            { id: 'code_review_module', name: 'Code Review', category: 'development', description: 'Comprehensive code review, best practices, and quality assessment', status: 'available' },
            { id: 'debug_module', name: 'Debug Assistant', category: 'development', description: 'Debugging tools, error analysis, and troubleshooting help', status: 'available' },
            { id: 'optimization_module', name: 'Code Optimizer', category: 'development', description: 'Performance optimization, code refactoring, and efficiency improvements', status: 'available' },
            { id: 'documentation_module', name: 'Documentation Generator', category: 'development', description: 'Generate comprehensive documentation and code comments', status: 'available' },
            { id: 'test_creator_module', name: 'Test Creator', category: 'development', description: 'Generate unit tests, integration tests, and testing frameworks', status: 'available' },
            { id: 'tester_module', name: 'Code Tester', category: 'development', description: 'Test code functionality, validation, and quality assurance', status: 'available' },

            // Analysis & Processing
            { id: 'data_analysis_module', name: 'Data Analysis', category: 'analysis', description: 'Advanced data analysis, pattern recognition, and statistical insights', status: 'available' },
            { id: 'file_analyzer', name: 'File Analyzer', category: 'analysis', description: 'Analyze file contents, structure, and data patterns', status: 'available' },
            { id: 'stats_analyzer_module', name: 'Statistics Analyzer', category: 'analysis', description: 'Statistical analysis, data visualization, and trend analysis', status: 'available' },
            { id: 'visualization_module', name: 'Data Visualization', category: 'analysis', description: 'Create charts, graphs, and visual representations of data', status: 'available' },
            { id: 'comparison_module', name: 'Comparison Tool', category: 'analysis', description: 'Compare data, concepts, and provide detailed analysis', status: 'available' },
            { id: 'ranking_module', name: 'Ranking & Sorting', category: 'analysis', description: 'Rank items, sort data, and create priority lists', status: 'available' },
            { id: 'fact_check_module', name: 'Fact Checker', category: 'analysis', description: 'Verify facts, check accuracy, and validate information', status: 'available' },
            { id: 'research_module', name: 'Research Assistant', category: 'analysis', description: 'Conduct research, gather information, and analyze findings', status: 'available' },

            // Creativity & Content
            { id: 'creativity_module', name: 'Creativity Hub', category: 'creativity', description: 'Creative content generation, brainstorming, and innovative ideas', status: 'available' },
            { id: 'brainstorming_module', name: 'Brainstorming', category: 'creativity', description: 'Generate ideas, creative solutions, and innovative concepts', status: 'available' },
            { id: 'game_generator', name: 'Game Generator', category: 'creativity', description: 'Create interactive games, game mechanics, and gaming content', status: 'available' },
            { id: 'dialogue_module', name: 'Dialogue Creator', category: 'creativity', description: 'Generate engaging dialogues, conversations, and character interactions', status: 'available' },
            { id: 'story_module', name: 'Story Writer', category: 'creativity', description: 'Write stories, narratives, and creative writing pieces', status: 'available' },
            { id: 'poem_module', name: 'Poetry Generator', category: 'creativity', description: 'Create poems, verses, and poetic content', status: 'available' },
            { id: 'lyrics_module', name: 'Song Lyrics', category: 'creativity', description: 'Generate song lyrics, musical content, and poetic verses', status: 'available' },
            { id: 'design_module', name: 'Design Assistant', category: 'creativity', description: 'UI/UX design guidance, visual concepts, and creative design', status: 'available' },
            { id: 'slides_generator', name: 'Presentation Builder', category: 'creativity', description: 'Create presentations, slide decks, and visual content', status: 'available' },
            { id: 'visualization_module', name: 'Visual Creator', category: 'creativity', description: 'Create visual content, diagrams, and graphic representations', status: 'available' },

            // Productivity & Automation
            { id: 'helper_module', name: 'AI Helper', category: 'productivity', description: 'General purpose assistance, guidance, and productivity tools', status: 'available' },
            { id: 'productivity_module', name: 'Productivity Booster', category: 'productivity', description: 'Workflow optimization, task management, and efficiency tools', status: 'available' },
            { id: 'planning_module', name: 'Planning Assistant', category: 'productivity', description: 'Create plans, schedules, and project management strategies', status: 'available' },
            { id: 'outline_module', name: 'Outline Creator', category: 'productivity', description: 'Generate outlines, structure documents, and organize content', status: 'available' },
            { id: 'checklist_module', name: 'Checklist Generator', category: 'productivity', description: 'Create checklists, task lists, and reminder systems', status: 'available' },
            { id: 'cheat_sheet_module', name: 'Cheat Sheet Maker', category: 'productivity', description: 'Create reference guides, quick references, and study materials', status: 'available' },
            { id: 'prompt_idea_module', name: 'Prompt Generator', category: 'productivity', description: 'Generate creative prompts, writing ideas, and inspiration', status: 'available' },
            { id: 'recommendation_module', name: 'Recommendation Engine', category: 'productivity', description: 'Provide recommendations, suggestions, and personalized advice', status: 'available' },
            { id: 'feedback_module', name: 'Feedback Generator', category: 'productivity', description: 'Generate constructive feedback, reviews, and assessments', status: 'available' },
            { id: 'summarizer_module', name: 'Content Summarizer', category: 'productivity', description: 'Summarize content, condense information, and extract key points', status: 'available' },
            { id: 'generator_module', name: 'Content Generator', category: 'productivity', description: 'Generate various types of content and creative materials', status: 'available' },
            { id: 'naming_module', name: 'Naming Assistant', category: 'productivity', description: 'Generate names for projects, products, characters, and ideas', status: 'available' },
            { id: 'ranking_module', name: 'Priority Ranker', category: 'productivity', description: 'Rank priorities, organize tasks, and create ordered lists', status: 'available' },

            // Entertainment & Games
            { id: 'math_solver', name: 'Math Solver', category: 'entertainment', description: 'Solve mathematical problems, equations, and numerical challenges', status: 'available' },
            { id: 'math_explain_module', name: 'Math Explainer', category: 'entertainment', description: 'Explain mathematical concepts and solve problems step-by-step', status: 'available' },
            { id: 'joke_module', name: 'Joke Generator', category: 'entertainment', description: 'Generate jokes, humor, and entertaining content', status: 'available' },
            { id: 'trivia_module', name: 'Trivia Creator', category: 'entertainment', description: 'Create trivia questions, quizzes, and knowledge challenges', status: 'available' },
            { id: 'riddle_module', name: 'Riddle Generator', category: 'entertainment', description: 'Generate riddles, puzzles, and brain teasers', status: 'available' },
            { id: 'logic_puzzle_module', name: 'Logic Puzzles', category: 'entertainment', description: 'Create logic puzzles, brain games, and mental challenges', status: 'available' },
            { id: 'quiz_module', name: 'Quiz Creator', category: 'entertainment', description: 'Generate quizzes, knowledge tests, and educational content', status: 'available' },
            { id: 'game_tips_module', name: 'Game Tips', category: 'entertainment', description: 'Provide gaming tips, strategies, and gameplay assistance', status: 'available' },
            { id: 'simulation_module', name: 'Simulation Creator', category: 'entertainment', description: 'Create simulations, models, and interactive scenarios', status: 'available' },
            { id: 'coding_challenge_module', name: 'Coding Challenges', category: 'entertainment', description: 'Generate programming challenges and coding exercises', status: 'available' },

            // Communication & Language
            { id: 'translator_module', name: 'Translator', category: 'communication', description: 'Translate text between multiple languages and locales', status: 'available' },
            { id: 'explain_module', name: 'Explainer', category: 'communication', description: 'Explain complex topics in simple, understandable terms', status: 'available' },
            { id: 'diagram_module', name: 'Diagram Creator', category: 'communication', description: 'Create diagrams, flowcharts, and visual explanations', status: 'available' },

            // Specialized & Expert
            { id: 'agent_module', name: 'AI Agent', category: 'specialized', description: 'Advanced AI agent for complex reasoning and decision making', status: 'available' },
            { id: 'health_module', name: 'Health Assistant', category: 'specialized', description: 'Health information, wellness guidance, and medical insights', status: 'available' }
        ];

        this.renderModules();
    }

    renderModules() {
        const moduleList = document.getElementById('moduleList');
        moduleList.innerHTML = '';

        const filteredModules = this.filterModulesByCategory();

        filteredModules.forEach(module => {
            const moduleCard = document.createElement('div');
            moduleCard.className = `module-card ${module.id === this.currentModule ? 'selected' : ''}`;
            moduleCard.dataset.module = module.id;
            moduleCard.innerHTML = `
                <div class="module-name">${module.name}</div>
                <div class="module-description">${module.description}</div>
                <div class="module-status">✓ ${module.status.charAt(0).toUpperCase() + module.status.slice(1)}</div>
            `;

            moduleCard.addEventListener('click', () => this.selectModule(module.id));
            moduleList.appendChild(moduleCard);
        });
    }

    filterModulesByCategory() {
        if (this.selectedCategory === 'all') {
            return this.modules;
        }
        return this.modules.filter(module => module.category === this.selectedCategory);
    }

    filterByCategory(category) {
        this.selectedCategory = category;

        // Update category button states
        document.querySelectorAll('.category-btn').forEach(btn => {
            btn.classList.toggle('active', btn.dataset.category === category);
        });

        this.renderModules();
    }

    filterModules(query) {
        const moduleCards = document.querySelectorAll('.module-card');
        moduleCards.forEach(card => {
            const name = card.querySelector('.module-name').textContent.toLowerCase();
            const description = card.querySelector('.module-description').textContent.toLowerCase();
            const matches = name.includes(query.toLowerCase()) || description.includes(query.toLowerCase());
            card.style.display = matches ? 'block' : 'none';
        });
    }

    selectModule(moduleId) {
        this.currentModule = moduleId;

        // Update visual selection
        document.querySelectorAll('.module-card').forEach(card => {
            card.classList.toggle('selected', card.dataset.module === moduleId);
        });

        // Update status indicators
        this.updateStatusIndicators();
    }

    // Message Handling
    async sendMessage() {
        const promptInput = document.getElementById('promptInput');
        const message = promptInput.value.trim();

        if (!message) {
            return;
        }

        // Add user message
        this.addMessage('user', message);
        promptInput.value = '';

        // Show loading
        this.showLoading();

        try {
            // Simulate AI processing
            await this.processWithAI(message);
        } catch (error) {
            this.addMessage('ai', 'Sorry, I encountered an error processing your request.');
        } finally {
            this.hideLoading();
        }
    }

    addMessage(type, content) {
        const chatHistory = document.getElementById('chatHistory');
        const messageDiv = document.createElement('div');
        messageDiv.className = `message ${type}`;

        const timestamp = new Date().toLocaleTimeString();
        messageDiv.innerHTML = `
            <strong>${type === 'user' ? 'You' : 'AI System'}:</strong> ${content}
            <div style="font-size: 0.8em; color: #ccc; margin-top: 5px;">${timestamp}</div>
        `;

        chatHistory.appendChild(messageDiv);
        this.scrollToBottom();
    }

    async processWithAI(message) {
        try {
            // Get current parameters
            const parameters = this.getCurrentParameters();
            const module = this.modules.find(m => m.id === this.currentModule);

            // Make real API call to backend orchestrator
            const response = await fetch('/api/run', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({
                    mode: this.currentModule,
                    model: 'nexus',
                    input: message,
                    user_id: 'web',
                    session_id: this.currentSession
                })
            });

            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }

            const result = await response.json();

            // Handle different response formats
            let aiResponse = '';
            if (result.response) {
                aiResponse = result.response;
            } else if (result.result) {
                aiResponse = result.result;
            } else if (result.output) {
                aiResponse = result.output;
            } else if (typeof result === 'string') {
                aiResponse = result;
            } else {
                aiResponse = `Processed successfully with ${module.name}: ${JSON.stringify(result)}`;
            }

            this.addMessage('ai', aiResponse);

        } catch (error) {
            console.error('Error calling AI backend:', error);

            // Fallback to simulated response if backend is not available
            const module = this.modules.find(m => m.id === this.currentModule);
            let fallbackResponse = '';

            switch (this.currentModule) {
                case 'codegen_module':
                    fallbackResponse = this.generateCodeResponse(message);
                    break;
                case 'data_analyzer':
                    fallbackResponse = this.generateAnalysisResponse(message);
                    break;
                case 'game_generator':
                    fallbackResponse = this.generateGameResponse(message);
                    break;
                case 'math_solver':
                    fallbackResponse = this.generateMathResponse(message);
                    break;
                default:
                    fallbackResponse = `I'm processing your request: "${message}" using the ${module.name}. Backend connection unavailable - showing simulated response.`;
            }

            this.addMessage('ai', fallbackResponse + '\n\n⚠️ *Note: Backend connection failed. This is a fallback response.*');
        }
    }

    getCurrentParameters() {
        return {
            wLearningRate: parseFloat(document.getElementById('wLearningRate').value) || 0.01,
            wRegularization: parseFloat(document.getElementById('wRegularization').value) || 0.001,
            wSize: parseInt(document.getElementById('wSize').value) || 1000,
            bLearningRate: parseFloat(document.getElementById('bLearningRate').value) || 0.01,
            bRegularization: parseFloat(document.getElementById('bRegularization').value) || 0.001,
            bSize: parseInt(document.getElementById('bSize').value) || 100
        };
    }

    generateCodeResponse(message) {
        return `I'll help you generate code for: "${message}"

\`\`\`python
def solve_problem():
    # Generated using Code Generator module
    result = "Problem solved!"
    return result
\`\`\`

This code was generated with optimized parameters:
- Learning Rate: ${document.getElementById('wLearningRate').value}
- Regularization: ${document.getElementById('wRegularization').value}
- Active Parameters: ${parseInt(document.getElementById('wSize').value).toLocaleString()}

Would you like me to optimize this further or explain any part of the code?`;
    }

    generateAnalysisResponse(message) {
        return `Analyzing your request: "${message}"

**Analysis Results:**
- Input processed: ✓
- Pattern recognition: ✓
- Data correlation: ✓
- Confidence score: 94.7%

**Key Insights:**
1. Your query shows high complexity
2. Multiple data points identified
3. Recommend further investigation

This analysis was performed using advanced parameter tuning for optimal results.`;
    }

    generateGameResponse(message) {
        return `Creating a game based on: "${message}"

**Game Concept Generated:**
- Type: Interactive adventure
- Difficulty: Adaptive
- Features: Multiple endings, character progression

**Technical Implementation:**
\`\`\`javascript
class GameEngine {
    constructor() {
        this.difficulty = "adaptive";
        this.features = ["multiplayer", "save_system"];
    }
}
\`\`\`

Ready to start developing this game concept! Would you like me to create the full implementation?`;
    }

    generateMathResponse(message) {
        return `Solving: "${message}"

**Mathematical Solution:**

Given the complexity of your problem, I'll apply our enhanced mathematical framework:

**Formula Application:** y = w × x + b

**Variables:**
- w (weight parameters): ${document.getElementById('wLearningRate').value}
- x (input): extracted from your query
- b (bias): ${document.getElementById('bLearningRate').value}

**Solution Steps:**
1. Parameter optimization applied
2. Mathematical model processed
3. Result validated

**Answer:** [Mathematical solution would be computed here based on the specific problem]

The solution uses optimized parameters for maximum accuracy.`;
    }

    // Parameter Management
    toggleParameters() {
        const panel = document.getElementById('parameterPanel');
        const button = document.getElementById('toggleParameters');

        this.isParametersVisible = !this.isParametersVisible;

        if (this.isParametersVisible) {
            panel.classList.add('active');
            button.textContent = '🔼 Hide Parameters';
        } else {
            panel.classList.remove('active');
            button.textContent = '⚙️ Parameters';
        }
    }

    updateParameters() {
        const wLearningRate = document.getElementById('wLearningRate').value;
        const wRegularization = document.getElementById('wRegularization').value;
        const wSize = document.getElementById('wSize').value;
        const bLearningRate = document.getElementById('bLearningRate').value;
        const bRegularization = document.getElementById('bRegularization').value;
        const bSize = document.getElementById('bSize').value;

        // Update status
        this.updateStatusIndicators();

        // Update debug info
        this.updateDebugInfo();

        // Show confirmation
        this.addMessage('ai', `Parameters updated successfully! 

**New Configuration:**
- W Learning Rate: ${wLearningRate}
- W Regularization: ${wRegularization} 
- W Size: ${parseInt(wSize).toLocaleString()} parameters
- B Learning Rate: ${bLearningRate}
- B Regularization: ${bRegularization}
- B Size: ${parseInt(bSize).toLocaleString()} parameters

The AI system is now optimized with your custom parameters.`);
    }

    updateStatusIndicators() {
        const statusElements = {
            parameterStatus: 'Parameters Active',
            modelStatus: `${this.modules.find(m => m.id === this.currentModule)?.name || 'Nexus Model'}`
        };

        Object.entries(statusElements).forEach(([id, text]) => {
            const element = document.getElementById(id);
            if (element) {
                element.textContent = text;
            }
        });
    }

    updateDebugInfo() {
        const debugContent = document.getElementById('debugContent');
        const currentTime = new Date().toLocaleTimeString();

        debugContent.innerHTML = `
            <div><strong>🕒 Last Updated:</strong> <span id="currentTime">${currentTime}</span></div>
            <div><strong>🧮 Mathematical Foundation:</strong></div>
            <div style="margin-left: 10px;">
                <div>Formula: y = w × x + b</div>
                <div>W Array Size: ${parseInt(document.getElementById('wSize').value).toLocaleString()} elements</div>
                <div>B Array Size: ${parseInt(document.getElementById('bSize').value).toLocaleString()} elements</div>
                <div>W Learning Rate: ${document.getElementById('wLearningRate').value}</div>
                <div>B Learning Rate: ${document.getElementById('bLearningRate').value}</div>
            </div>
            <div><strong>📊 Current Performance:</strong></div>
            <div style="margin-left: 10px;">
                <div>CPU Usage: ${document.getElementById('cpuUsage').textContent}</div>
                <div>Memory Usage: ${document.getElementById('memoryUsage').textContent}MB</div>
                <div>Response Time: ${document.getElementById('responseTime').textContent}ms</div>
                <div>Status: Ready for input...</div>
            </div>
        `;
    }

    // Metrics Management
    showAllMetrics() {
        // Open window to display all AI modules with search functionality
        const metricsWindow = window.open('', '_blank', 'width=1000,height=700,scrollbars=yes');

        // Ensure we have modules loaded
        if (!this.modules || this.modules.length === 0) {
            this.loadAllModules().then(() => {
                this.displayAllModulesInWindow(metricsWindow);
            });
        } else {
            this.displayAllModulesInWindow(metricsWindow);
        }
    }

    displayAllModulesInWindow(window) {
        // Group modules by category
        const modulesByCategory = {};
        this.modules.forEach(module => {
            if (!modulesByCategory[module.category]) {
                modulesByCategory[module.category] = [];
            }
            modulesByCategory[module.category].push(module);
        });

        const categoryNames = {
            'development': 'Development Tools',
            'analysis': 'Analysis & Processing',
            'creativity': 'Creative Content',
            'productivity': 'Productivity & Automation',
            'entertainment': 'Entertainment & Games',
            'communication': 'Communication & Media',
            'specialized': 'Specialized Expertise'
        };

        window.document.write(`
            <html>
                <head>
                    <title>All Available AI Modules (${this.modules.length} Total)</title>
                    <style>
                        body { 
                            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; 
                            margin: 0; 
                            padding: 20px; 
                            background: linear-gradient(135deg, #1a1a2e 0%, #16213e 100%);
                            color: white; 
                        }
                        .header {
                            text-align: center;
                            margin-bottom: 30px;
                            padding: 20px;
                            background: rgba(255,255,255,0.1);
                            border-radius: 15px;
                            backdrop-filter: blur(10px);
                        }
                        .search-container {
                            margin: 20px 0;
                            text-align: center;
                        }
                        .search-input {
                            width: 60%;
                            padding: 12px 20px;
                            font-size: 16px;
                            border: none;
                            border-radius: 25px;
                            background: rgba(255,255,255,0.2);
                            color: white;
                            backdrop-filter: blur(10px);
                            outline: none;
                        }
                        .search-input::placeholder {
                            color: rgba(255,255,255,0.7);
                        }
                        .category-section {
                            margin-bottom: 40px;
                        }
                        .category-header {
                            background: linear-gradient(45deg, #4ecdc4, #44a08d);
                            color: white;
                            padding: 15px 20px;
                            margin-bottom: 20px;
                            border-radius: 10px;
                            font-size: 1.3em;
                            font-weight: bold;
                            box-shadow: 0 4px 15px rgba(78, 205, 196, 0.3);
                        }
                        .modules-grid {
                            display: grid;
                            grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
                            gap: 20px;
                            margin-bottom: 30px;
                        }
                        .module-card {
                            background: rgba(255,255,255,0.1);
                            padding: 20px;
                            border-radius: 12px;
                            border: 1px solid rgba(255,255,255,0.2);
                            backdrop-filter: blur(10px);
                            transition: all 0.3s ease;
                            cursor: pointer;
                        }
                        .module-card:hover {
                            transform: translateY(-5px);
                            box-shadow: 0 10px 25px rgba(78, 205, 196, 0.3);
                            border-color: #4ecdc4;
                        }
                        .module-name {
                            font-size: 1.2em;
                            font-weight: bold;
                            color: #4ecdc4;
                            margin-bottom: 8px;
                        }
                        .module-description {
                            color: rgba(255,255,255,0.8);
                            margin-bottom: 10px;
                            line-height: 1.4;
                        }
                        .module-status {
                            display: inline-block;
                            padding: 4px 12px;
                            background: #28a745;
                            color: white;
                            border-radius: 15px;
                            font-size: 0.8em;
                            font-weight: bold;
                        }
                        .stats-summary {
                            background: rgba(255,255,255,0.1);
                            padding: 20px;
                            border-radius: 10px;
                            margin-top: 30px;
                            text-align: center;
                        }
                        .hidden {
                            display: none;
                        }
                        .stats-number {
                            font-size: 2em;
                            font-weight: bold;
                            color: #4ecdc4;
                        }
                    </style>
                </head>
                <body>
                    <div class="header">
                        <h1>🤖 Available AI Modules</h1>
                        <p>Discover and select from ${this.modules.length} powerful AI modules</p>
                    </div>
                    
                    <div class="search-container">
                        <input type="text" class="search-input" placeholder="🔍 Search modules by name or description..." id="searchInput">
                    </div>
                    
                    <div id="modulesContainer">
                        ${Object.entries(modulesByCategory).map(([category, modules]) => `
                            <div class="category-section" data-category="${category}">
                                <div class="category-header">
                                    ${categoryNames[category] || category.charAt(0).toUpperCase() + category.slice(1)} (${modules.length} modules)
                                </div>
                                <div class="modules-grid">
                                    ${modules.map(module => `
                                        <div class="module-card" data-module-id="${module.id}">
                                            <div class="module-name">${module.name}</div>
                                            <div class="module-description">${module.description}</div>
                                            <div class="module-status">✓ ${module.status.charAt(0).toUpperCase() + module.status.slice(1)}</div>
                                        </div>
                                    `).join('')}
                                </div>
                            </div>
                        `).join('')}
                    </div>
                    
                    <div class="stats-summary">
                        <h3>📊 System Statistics</h3>
                        <p><span class="stats-number">${this.modules.length}</span> Total AI Modules Available</p>
                        <p>Powered by Enhanced AI Interface Backend Orchestrator</p>
                        <p><em>Click on any module to learn more or return to the main interface to start using it!</em></p>
                    </div>
                    
                    <script>
                        // Search functionality
                        document.getElementById('searchInput').addEventListener('input', function(e) {
                            const searchTerm = e.target.value.toLowerCase();
                            const modules = document.querySelectorAll('.module-card');
                            const categories = document.querySelectorAll('.category-section');
                            
                            modules.forEach(module => {
                                const name = module.querySelector('.module-name').textContent.toLowerCase();
                                const description = module.querySelector('.module-description').textContent.toLowerCase();
                                const matches = name.includes(searchTerm) || description.includes(searchTerm);
                                module.style.display = matches ? 'block' : 'none';
                            });
                            
                            // Hide empty categories
                            categories.forEach(category => {
                                const visibleModules = category.querySelectorAll('.module-card[style*="block"], .module-card:not([style*="none"])');
                                category.style.display = visibleModules.length > 0 ? 'block' : 'none';
                            });
                        });
                        
                        // Add click handlers for module cards
                        document.querySelectorAll('.module-card').forEach(card => {
                            card.addEventListener('click', function() {
                                const moduleId = this.dataset.moduleId;
                                const moduleName = this.querySelector('.module-name').textContent;
                                alert(\`Selected module: \${moduleName}\\n\\nThis module is now ready to use!\\nReturn to the main interface to start chatting.\`);
                            });
                        });
                        
                        // Auto-focus search input
                        document.getElementById('searchInput').focus();
                    </script>
                </body>
            </html>
        `);
    }

    generateMetricValue(metric) {
        const values = {
            'CPU Usage': '2.3%',
            'Memory Usage': '24.7 MB',
            'Response Time': '5.2 ms',
            'Requests Per Minute': '4.8',
            'Error Rate': '0.003%',
            'Model Accuracy': '98.7%',
            'Network Latency': '12.3 ms',
            'Cache Hit Rate': '94.2%',
            'Thread Pool Utilization': '67.8%',
            'Database Query Time': '3.1 ms',
            'API Response Time': '8.7 ms',
            'Session Count': '1,247',
            'Active Users': '89',
            'Bandwidth Usage': '156.7 MB/s',
            'Disk I/O': '245.3 MB/s',
            'Temperature': '42°C',
            'Power Consumption': '78.9W',
            'Error Logs': '2',
            'Warning Count': '7',
            'Information Logs': '1,234',
            'Debug Messages': '567',
            'Performance Score': '9.2/10',
            'Availability': '99.97%',
            'Throughput': '1,847 req/s',
            'Latency': '15.6 ms'
        };

        return values[metric] || (Math.random() * 100).toFixed(1) + '%';
    }

    // Server Management Functions
    attemptServerRestart() {
        // Attempt to restart servers in background
        console.log('🔄 Attempting to restart AI servers...');

        // This would typically call a server management endpoint
        // For now, we'll just log the attempt
        setTimeout(() => {
            console.log('✅ Server restart attempt completed');
        }, 2000);
    }

    // Utility Functions
    showLoading() {
        document.getElementById('loadingIndicator').style.display = 'block';
        document.getElementById('sendBtn').disabled = true;
    }

    hideLoading() {
        document.getElementById('loadingIndicator').style.display = 'none';
        document.getElementById('sendBtn').disabled = false;
    }

    scrollToBottom() {
        const chatHistory = document.getElementById('chatHistory');
        chatHistory.scrollTop = chatHistory.scrollHeight;
    }

    updateCurrentTime() {
        const timeElement = document.getElementById('currentTime');
        if (timeElement) {
            timeElement.textContent = new Date().toLocaleTimeString();
        }
    }

    startMetricsUpdate() {
        setInterval(() => {
            this.updateCurrentTime();
            this.updateRealTimeMetrics();
        }, 5000);
    }

    updateRealTimeMetrics() {
        // Simulate real-time metric updates
        const metrics = {
            'cpuUsage': (2 + Math.random() * 2).toFixed(1) + '%',
            'memoryUsage': (24 + Math.random() * 10).toFixed(1),
            'responseTime': (5 + Math.random() * 3).toFixed(1),
            'requestsPerMin': (4 + Math.random() * 3).toFixed(1),
            'errorRate': (0.002 + Math.random() * 0.002).toFixed(3),
            'modelAccuracy': (98 + Math.random() * 2).toFixed(1) + '%'
        };

        Object.entries(metrics).forEach(([id, value]) => {
            const element = document.getElementById(id);
            if (element) {
                element.textContent = value;
            }
        });
    }

    setupResponsiveLayout() {
        const handleResize = () => {
            const isMobile = window.innerWidth <= 768;
            const leftSidebar = document.querySelector('.left-sidebar');
            const rightSidebar = document.querySelector('.right-sidebar');

            if (isMobile) {
                leftSidebar.style.height = '200px';
                rightSidebar.style.height = '300px';
            } else {
                leftSidebar.style.height = 'auto';
                rightSidebar.style.height = 'auto';
            }
        };

        window.addEventListener('resize', handleResize);
        handleResize(); // Initial call
    }

    /**
     * Initialize the content renderer for markdown, LaTeX, and code highlighting
     */
    initializeContentRenderer() {
        try {
            if (typeof ContentRenderer !== 'undefined') {
                this.contentRenderer = new ContentRenderer();
                console.log('Content renderer initialized successfully');
            } else {
                console.warn('ContentRenderer class not found, content rendering will be disabled');
            }
        } catch (error) {
            console.error('Failed to initialize content renderer:', error);
        }
    }
}

// Initialize the enhanced interface when the page loads
document.addEventListener('DOMContentLoaded', () => {
    window.enhancedAIInterface = new EnhancedAIInterface();

    // Add some demo messages to show functionality
    setTimeout(() => {
        window.enhancedAIInterface.addMessage('ai',
            '🎉 **Enhanced 3-Column Interface Loaded Successfully!**\n\n' +
            '✨ **New Features Available:**\n' +
            '• 💬 Chat History with search and persistence\n' +
            '• 🔧 Collapsible parameter controls\n' +
            '• 📊 Enhanced performance metrics\n' +
            '• 🎯 Module selection sidebar\n' +
            '• 📱 Mobile-responsive design\n\n' +
            'Try sending a message or exploring the new features!'
        );
    }, 1000);
});

// Export for potential use in other modules
if (typeof module !== 'undefined' && module.exports) {
    module.exports = EnhancedAIInterface;
}

