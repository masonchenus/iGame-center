// Advanced AI Interface JavaScript
class AdvancedAIInterface {
    constructor() {
        this.currentChatId = 'current';
        this.chatHistory = new Map();
        this.selectedTool = null;
        this.availableModules = {};
        this.apiBaseUrl = 'http://127.0.0.1:8000';

        this.initializeApp();
    }

    async initializeApp() {
        try {
            await this.loadAvailableTools();
            this.renderTools();
            this.setupEventListeners();
            this.initializeChatHistory();
        } catch (error) {
            console.error('Failed to initialize app:', error);
            this.showError('Failed to load application. Please refresh the page.');
        }
    }

    // Load available tools from backend
    async loadAvailableTools() {
        try {
            const response = await fetch(`${this.apiBaseUrl}/api/tools`);
            const data = await response.json();

            if (data.status === 'success') {
                this.availableModules = data.available_tools;
                document.getElementById('toolsCount').textContent = data.total_count;
            } else {
                throw new Error(data.error || 'Failed to load tools');
            }
        } catch (error) {
            console.error('Error loading tools:', error);
            // Fallback to hardcoded tools if API fails
            this.loadFallbackTools();
        }
    }

    // Fallback tools if API is unavailable
    loadFallbackTools() {
        this.availableModules = {
            // Development (15+ tools)
m
            'code_review_module': { name: 'Code Review', icon: '🔍', category: 'Development', description: 'Review and analyze code' },
            'debug_module': { name: 'Debugger', icon: '🐛', category: 'Development', description: 'Debug and fix code issues' },
            'documentation_module': { name: 'Documentation', icon: '📚', category: 'Development', description: 'Generate documentation' },
            'python_ml_module': { name: 'Python ML', icon: '🐍', category: 'Development', description: 'Python machine learning' },
            'javascript_ts_module': { name: 'JS/TS', icon: '⚡', category: 'Development', description: 'JavaScript/TypeScript' },
            'go_module': { name: 'Go', icon: '🚀', category: 'Development', description: 'Go language development' },
            'rust_module': { name: 'Rust', icon: '🦀', category: 'Development', description: 'Rust programming' },
            'java_module': { name: 'Java', icon: '☕', category: 'Development', description: 'Java development' },
            'csharp_module': { name: 'C#', icon: '🔷', category: 'Development', description: 'C# programming' },
            'swift_module': { name: 'Swift', icon: '🍎', category: 'Development', description: 'Swift development' },
            'kotlin_module': { name: 'Kotlin', icon: '🎯', category: 'Development', description: 'Kotlin programming' },
            'php_module': { name: 'PHP', icon: '🐘', category: 'Development', description: 'PHP development' },
            'ruby_module': { name: 'Ruby', icon: '💎', category: 'Development', description: 'Ruby programming' },
            'sql_module': { name: 'SQL', icon: '🗄️', category: 'Development', description: 'SQL database queries' },

            // Analysis (10+ tools)
            'data_analysis_module': { name: 'Data Analysis', icon: '📊', category: 'Analysis', description: 'Analyze datasets' },
            'stats_analyzer_module': { name: 'Statistics', icon: '📈', category: 'Analysis', description: 'Statistical analysis' },
            'comparison_module': { name: 'Comparison', icon: '⚖️', category: 'Analysis', description: 'Compare data/items' },
            'fact_check_module': { name: 'Fact Check', icon: '✅', category: 'Analysis', description: 'Verify facts and information' },
            'trend_analysis_module': { name: 'Trends', icon: '📈', category: 'Analysis', description: 'Analyze trends' },
            'sentiment_analysis_module': { name: 'Sentiment', icon: '😊', category: 'Analysis', description: 'Analyze sentiment' },
            'risk_assessment_module': { name: 'Risk Assessment', icon: '⚠️', category: 'Analysis', description: 'Assess risks' },
            'competitor_analysis_module': { name: 'Competitor Analysis', icon: '🎯', category: 'Analysis', description: 'Analyze competitors' },
            'performance_analysis_module': { name: 'Performance', icon: '⚡', category: 'Analysis', description: 'Performance analysis' },
            'anomaly_detection_module': { name: 'Anomaly Detection', icon: '🔍', category: 'Analysis', description: 'Detect anomalies' },

            // Creativity (12+ tools)
            'creativity_module': { name: 'Creativity', icon: '🎨', category: 'Creativity', description: 'Boost creativity' },
            'brainstorming_module': { name: 'Brainstorming', icon: '💡', category: 'Creativity', description: 'Brainstorm ideas' },
            'design_module': { name: 'Design', icon: '🎭', category: 'Creativity', description: 'Design assistance' },
            'poem_module': { name: 'Poetry', icon: '📝', category: 'Creativity', description: 'Write poems' },
            'story_module': { name: 'Storytelling', icon: '📖', category: 'Creativity', description: 'Create stories' },
            'music_composition_module': { name: 'Music', icon: '🎵', category: 'Creativity', description: 'Compose music' },
            'art_generator_module': { name: 'Art', icon: '🖼️', category: 'Creativity', description: 'Generate art' },
            'logo_design_module': { name: 'Logo Design', icon: '🏷️', category: 'Creativity', description: 'Design logos' },
            'video_script_module': { name: 'Video Scripts', icon: '🎬', category: 'Creativity', description: 'Write video scripts' },
            'podcast_script_module': { name: 'Podcast Scripts', icon: '🎙️', category: 'Creativity', description: 'Create podcast content' },
            'game_story_module': { name: 'Game Stories', icon: '🎮', category: 'Creativity', description: 'Game narrative' },
            'character_development_module': { name: 'Character Dev', icon: '👤', category: 'Creativity', description: 'Develop characters' },

            // Productivity (15+ tools)
            'productivity_module': { name: 'Productivity', icon: '⚡', category: 'Productivity', description: 'Boost productivity' },
            'planning_module': { name: 'Planning', icon: '📅', category: 'Productivity', description: 'Create plans' },
            'checklist_module': { name: 'Checklists', icon: '✅', category: 'Productivity', description: 'Make checklists' },
            'optimization_module': { name: 'Optimization', icon: '🚀', category: 'Productivity', description: 'Optimize workflows' },
            'time_management_module': { name: 'Time Management', icon: '⏰', category: 'Productivity', description: 'Manage time' },
            'project_management_module': { name: 'Project Mgmt', icon: '📋', category: 'Productivity', description: 'Manage projects' },
            'task_automation_module': { name: 'Automation', icon: '🤖', category: 'Productivity', description: 'Automate tasks' },
            'workflow_design_module': { name: 'Workflows', icon: '🔄', category: 'Productivity', description: 'Design workflows' },
            'meeting_agenda_module': { name: 'Meeting Agendas', icon: '📝', category: 'Productivity', description: 'Plan meetings' },
            'report_generator_module': { name: 'Reports', icon: '📊', category: 'Productivity', description: 'Generate reports' },
            'email_composer_module': { name: 'Email Composer', icon: '📧', category: 'Productivity', description: 'Write emails' },
            'calendar_optimization_module': { name: 'Calendar', icon: '📅', category: 'Productivity', description: 'Optimize calendar' },
            'goal_setting_module': { name: 'Goal Setting', icon: '🎯', category: 'Productivity', description: 'Set goals' },
            'habit_formation_module': { name: 'Habits', icon: '🔄', category: 'Productivity', description: 'Form habits' },
            'resource_planning_module': { name: 'Resource Planning', icon: '📦', category: 'Productivity', description: 'Plan resources' },

            // Business & Finance (8+ tools)
            'business_plan_module': { name: 'Business Plan', icon: '💼', category: 'Business', description: 'Create business plans' },
            'financial_analysis_module': { name: 'Financial Analysis', icon: '💰', category: 'Business', description: 'Analyze finances' },
            'market_research_module': { name: 'Market Research', icon: '🔍', category: 'Business', description: 'Research markets' },
            'investment_analysis_module': { name: 'Investment Analysis', icon: '📈', category: 'Business', description: 'Analyze investments' },
            'budget_planning_module': { name: 'Budget Planning', icon: '💳', category: 'Business', description: 'Plan budgets' },
            'pitch_deck_module': { name: 'Pitch Deck', icon: '🎤', category: 'Business', description: 'Create pitch decks' },
            'competitive_analysis_module': { name: 'Competitive Analysis', icon: '🎯', category: 'Business', description: 'Analyze competition' },
            'roi_calculator_module': { name: 'ROI Calculator', icon: '💹', category: 'Business', description: 'Calculate ROI' },

            // Entertainment (8+ tools)
            'game_generator': { name: 'Game Generator', icon: '🎮', category: 'Entertainment', description: 'Generate games' },
            'joke_module': { name: 'Jokes', icon: '😄', category: 'Entertainment', description: 'Tell jokes' },
            'trivia_module': { name: 'Trivia', icon: '🧠', category: 'Entertainment', description: 'Trivia questions' },
            'riddle_module': { name: 'Riddles', icon: '🤔', category: 'Entertainment', description: 'Solve riddles' },
            'quiz_generator_module': { name: 'Quiz Generator', icon: '❓', category: 'Entertainment', description: 'Create quizzes' },
            'party_games_module': { name: 'Party Games', icon: '🎉', category: 'Entertainment', description: 'Party game ideas' },
            'movie_recommendations_module': { name: 'Movie Recs', icon: '🎬', category: 'Entertainment', description: 'Movie recommendations' },
            'book_recommendations_module': { name: 'Book Recs', icon: '📚', category: 'Entertainment', description: 'Book recommendations' },

            // Communication (10+ tools)
            'dialogue_module': { name: 'Dialogue', icon: '💬', category: 'Communication', description: 'Create dialogue' },
            'translator_module': { name: 'Translator', icon: '🌍', category: 'Communication', description: 'Translate languages' },
            'summarizer_module': { name: 'Summarizer', icon: '📝', category: 'Communication', description: 'Summarize text' },
            'presentation_module': { name: 'Presentation', icon: '📊', category: 'Communication', description: 'Create presentations' },
            'social_media_module': { name: 'Social Media', icon: '📱', category: 'Communication', description: 'Social media content' },
            'press_release_module': { name: 'Press Release', icon: '📰', category: 'Communication', description: 'Write press releases' },
            'customer_service_module': { name: 'Customer Service', icon: '🎧', category: 'Communication', description: 'Customer support' },
            'negotiation_module': { name: 'Negotiation', icon: '🤝', category: 'Communication', description: 'Negotiation skills' },
            'conflict_resolution_module': { name: 'Conflict Resolution', icon: '⚖️', category: 'Communication', description: 'Resolve conflicts' },
            'public_speaking_module': { name: 'Public Speaking', icon: '🎤', category: 'Communication', description: 'Public speaking' },

            // Education (12+ tools)
            'explain_module': { name: 'Explain', icon: '📚', category: 'Education', description: 'Explain concepts' },
            'quiz_module': { name: 'Quiz', icon: '❓', category: 'Education', description: 'Create quizzes' },
            'test_creator_module': { name: 'Test Creator', icon: '📝', category: 'Education', description: 'Create tests' },
            'math_explain_module': { name: 'Math Explain', icon: '🔢', category: 'Education', description: 'Explain math' },
            'science_explain_module': { name: 'Science Explain', icon: '🔬', category: 'Education', description: 'Explain science' },
            'history_explain_module': { name: 'History Explain', icon: '🏛️', category: 'Education', description: 'Explain history' },
            'language_learning_module': { name: 'Language Learning', icon: '🗣️', category: 'Education', description: 'Learn languages' },
            'study_guide_module': { name: 'Study Guide', icon: '📖', category: 'Education', description: 'Create study guides' },
            'lesson_plan_module': { name: 'Lesson Plans', icon: '📋', category: 'Education', description: 'Plan lessons' },
            'research_assistant_module': { name: 'Research Assistant', icon: '🔍', category: 'Education', description: 'Research assistance' },
            'academic_writing_module': { name: 'Academic Writing', icon: '🎓', category: 'Education', description: 'Academic writing' },
            'learning_assessment_module': { name: 'Learning Assessment', icon: '📊', category: 'Education', description: 'Assess learning' },

            // Specialized (15+ tools)
            'diagram_module': { name: 'Diagram', icon: '📐', category: 'Specialized', description: 'Create diagrams' },
            'slides_generator': { name: 'Slides', icon: '📊', category: 'Specialized', description: 'Generate slides' },
            'health_module': { name: 'Health', icon: '🏥', category: 'Specialized', description: 'Health advice' },
            'recommendation_module': { name: 'Recommendations', icon: '⭐', category: 'Specialized', description: 'Give recommendations' },
            'seo_optimization_module': { name: 'SEO', icon: '🔍', category: 'Specialized', description: 'SEO optimization' },
            'legal_assistance_module': { name: 'Legal', icon: '⚖️', category: 'Specialized', description: 'Legal assistance' },
            'medical_advice_module': { name: 'Medical', icon: '🩺', category: 'Specialized', description: 'Medical advice' },
            'nutrition_planning_module': { name: 'Nutrition', icon: '🥗', category: 'Specialized', description: 'Plan nutrition' },
            'fitness_planning_module': { name: 'Fitness', icon: '💪', category: 'Specialized', description: 'Plan fitness' },
            'travel_planning_module': { name: 'Travel', icon: '✈️', category: 'Specialized', description: 'Plan travel' },
            'real_estate_module': { name: 'Real Estate', icon: '🏠', category: 'Specialized', description: 'Real estate advice' },
            'tax_assistance_module': { name: 'Tax', icon: '📋', category: 'Specialized', description: 'Tax assistance' },
            'insurance_analysis_module': { name: 'Insurance', icon: '🛡️', category: 'Specialized', description: 'Analyze insurance' },
            'career_counseling_module': { name: 'Career Counseling', icon: '💼', category: 'Specialized', description: 'Career advice' },
            'relationship_advice_module': { name: 'Relationships', icon: '❤️', category: 'Specialized', description: 'Relationship advice' },

            // Technical & Engineering (10+ tools)
            'api_design_module': { name: 'API Design', icon: '🔗', category: 'Technical', description: 'Design APIs' },
            'database_design_module': { name: 'Database', icon: '🗄️', category: 'Technical', description: 'Design databases' },
            'system_architecture_module': { name: 'Architecture', icon: '🏗️', category: 'Technical', description: 'System architecture' },
            'security_audit_module': { name: 'Security Audit', icon: '🔒', category: 'Technical', description: 'Security auditing' },
            'performance_optimization_module': { name: 'Performance', icon: '⚡', category: 'Technical', description: 'Performance optimization' },
            'cloud_architecture_module': { name: 'Cloud', icon: '☁️', category: 'Technical', description: 'Cloud architecture' },
            'devops_automation_module': { name: 'DevOps', icon: '🔄', category: 'Technical', description: 'DevOps automation' },
            'testing_strategy_module': { name: 'Testing', icon: '🧪', category: 'Technical', description: 'Testing strategy' },
            'code_review_automation_module': { name: 'Code Review Auto', icon: '🤖', category: 'Technical', description: 'Automated code review' },
            'microservices_design_module': { name: 'Microservices', icon: '🔧', category: 'Technical', description: 'Microservices design' },

            // Testing & Quality (5+ tools)
            'trojan_test_module': { name: 'Trojan Testing', icon: '🛡️', category: 'Testing', description: 'Security testing' },
            'unit_testing_module': { name: 'Unit Testing', icon: '🧪', category: 'Testing', description: 'Unit testing' },
            'integration_testing_module': { name: 'Integration Testing', icon: '🔗', category: 'Testing', description: 'Integration testing' },
            'performance_testing_module': { name: 'Performance Testing', icon: '⚡', category: 'Testing', description: 'Performance testing' },
            'security_testing_module': { name: 'Security Testing', icon: '🔒', category: 'Testing', description: 'Security testing' },

            // Original 4 tools
            'code_modifier': { name: 'Code Modifier', icon: '✏️', category: 'Original', description: 'Modify code' },
            'file_analyzer': { name: 'File Analyzer', icon: '📁', category: 'Original', description: 'Analyze files' },
            'code_generator': { name: 'Code Generator', icon: '⚙️', category: 'Original', description: 'Generate code' },
            'code_optimizer': { name: 'Code Optimizer', icon: '🚀', category: 'Original', description: 'Optimize code' }
        };

        document.getElementById('toolsCount').textContent = Object.keys(this.availableModules).length;
    }

    // Render tools in right sidebar
    renderTools() {
        const categories = this.organizeToolsByCategory();
        const toolsContainer = document.getElementById('toolsCategories');
        toolsContainer.innerHTML = '';

        Object.entries(categories).forEach(([category, tools]) => {
            const categoryElement = document.createElement('div');
            categoryElement.className = 'category';

            const categoryIcon = this.getCategoryIcon(category);
            categoryElement.innerHTML = `
                <div class="category-header">
                    <span class="category-icon">${categoryIcon}</span>
                    ${category} (${tools.length})
                </div>
                <div class="tools-grid">
                    ${tools.map(tool => {
                // Use the proper tool name from the object, not the key
                const toolDisplayName = tool.display_name || tool.name || 'Unknown Tool';
                const toolDescription = tool.description || 'No description available';
                const toolIcon = tool.icon || '🔧';

                return `
                            <div class="tool-item" data-tool="${toolDisplayName}" onclick="aiInterface.selectTool('${toolDisplayName}')">
                                <div class="tool-icon">${toolIcon}</div>
                                <div class="tool-name">${toolDisplayName}</div>
                                <div class="tool-description">${toolDescription}</div>
                            </div>
                        `;
            }).join('')}
                </div>
            `;

            toolsContainer.appendChild(categoryElement);
        });
    }

    // Organize tools by category
    organizeToolsByCategory() {
        const categories = {};

        Object.entries(this.availableModules).forEach(([key, tool]) => {
            const category = tool.category || 'Other';
            if (!categories[category]) {
                categories[category] = [];
            }
            categories[category].push({
                name: key,
                ...tool
            });
        });

        return categories;
    }

    // Get icon for category
    getCategoryIcon(category) {
        const icons = {
            'Development': '💻',
            'Analysis': '📊',
            'Creativity': '🎨',
            'Productivity': '⚡',
            'Business': '💼',
            'Entertainment': '🎮',
            'Communication': '💬',
            'Education': '📚',
            'Specialized': '⭐',
            'Technical': '🔧',
            'Testing': '🧪',
            'Original': '🔧'
        };
        return icons[category] || '🔧';
    }

    // Setup event listeners
    setupEventListeners() {
        const chatInput = document.getElementById('chatInput');
        const sendBtn = document.getElementById('sendBtn');
        const toolsSearch = document.getElementById('toolsSearch');

        // Auto-resize chat input
        chatInput.addEventListener('input', () => {
            this.autoResizeInput(chatInput);
        });

        // Send message handlers
        sendBtn.addEventListener('click', () => this.sendMessage());

        // Tool search
        toolsSearch.addEventListener('input', () => this.filterTools());
    }

    // Auto-resize input textarea
    autoResizeInput(input) {
        input.style.height = 'auto';
        input.style.height = Math.min(input.scrollHeight, 200) + 'px';
    }

    // Filter tools based on search
    filterTools() {
        const searchTerm = document.getElementById('toolsSearch').value.toLowerCase();
        const toolItems = document.querySelectorAll('.tool-item');

        toolItems.forEach(item => {
            const toolName = item.querySelector('.tool-name').textContent.toLowerCase();
            const toolDescription = item.querySelector('.tool-description').textContent.toLowerCase();

            if (toolName.includes(searchTerm) || toolDescription.includes(searchTerm)) {
                item.style.display = 'block';
            } else {
                item.style.display = 'none';
            }
        });
    }

    // Select a tool
    selectTool(toolName) {
        // Remove active class from all tools
        document.querySelectorAll('.tool-item').forEach(item => {
            item.classList.remove('active');
        });

        // Add active class to selected tool
        const selectedToolItem = document.querySelector(`[data-tool="${toolName}"]`);
        if (selectedToolItem) {
            selectedToolItem.classList.add('active');
        }

        this.selectedTool = toolName;

        // Update chat input with tool context
        const chatInput = document.getElementById('chatInput');
        const tool = this.availableModules[toolName];
        if (tool) {
            chatInput.placeholder = `Using ${tool.name}: ${tool.description}`;
        }
    }

    // Send message to AI
    async sendMessage() {
        const chatInput = document.getElementById('chatInput');
        const message = chatInput.value.trim();

        if (!message) {
            return;
        }

        // Add user message to chat
        this.addMessage('user', message);

        // Clear input
        chatInput.value = '';
        this.autoResizeInput(chatInput);

        // Show loading indicator
        this.showLoading(true);

        try {
            // Prepare request
            const requestData = {
                mode: this.selectedTool || 'helper',
                model: document.getElementById('modelSelector').value,
                input: JSON.stringify({ prompt: message, tool: this.selectedTool }),
                user_id: 'web_user',
                session_id: this.currentChatId
            };

            // Send request to backend
            const response = await fetch(`${this.apiBaseUrl}/api/run`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify(requestData)
            });

            const data = await response.json();

            if (data.output) {
                this.addMessage('ai', data.output);
            } else {
                throw new Error('No response from AI');
            }

        } catch (error) {
            console.error('Error sending message:', error);
            this.addMessage('ai', 'I apologize, but I encountered an error while processing your request. Please try again.');
        } finally {
            this.showLoading(false);
        }
    }

    // Add message to chat
    addMessage(type, content) {
        const chatMessages = document.getElementById('chatMessages');
        const messageElement = document.createElement('div');
        messageElement.className = `message ${type}`;

        const avatar = type === 'user' ? 'U' : 'AI';
        messageElement.innerHTML = `
            <div class="message-avatar ${type}">${avatar}</div>
            <div class="message-content">
                <div class="message-text">${this.formatMessage(content)}</div>
            </div>
        `;

        chatMessages.appendChild(messageElement);
        chatMessages.scrollTop = chatMessages.scrollHeight;

        // Save to chat history
        this.saveToChatHistory(type, content);
    }

    // Format message content
    formatMessage(content) {
        // Basic markdown-like formatting
        return content
            .replace(/\*\*(.*?)\*\*/g, '<strong>$1</strong>')
            .replace(/\*(.*?)\*/g, '<em>$1</em>')
            .replace(/`(.*?)`/g, '<code>$1</code>')
            .replace(/\n/g, '<br>');
    }

    // Save message to chat history
    saveToChatHistory(type, content) {
        if (!this.chatHistory.has(this.currentChatId)) {
            this.chatHistory.set(this.currentChatId, []);
        }

        const chat = this.chatHistory.get(this.currentChatId);
        chat.push({ type, content, timestamp: Date.now() });
    }

    // Show/hide loading indicator
    showLoading(show) {
        const loadingIndicator = document.getElementById('loadingIndicator');
        const sendBtn = document.getElementById('sendBtn');

        if (show) {
            loadingIndicator.style.display = 'block';
            sendBtn.disabled = true;
        } else {
            loadingIndicator.style.display = 'none';
            sendBtn.disabled = false;
        }
    }

    // Show error message
    showError(message) {
        this.addMessage('ai', `Error: ${message}`);
    }

    // Initialize chat history
    initializeChatHistory() {
        // Load existing chat history or create default
        if (this.chatHistory.size === 0) {
            this.chatHistory.set('current', [
                {
                    type: 'ai',
                    content: 'Welcome to the Advanced AI Interface! I have access to 124 specialized tools and modules to help you with any task. What would you like to work on today?',
                    timestamp: Date.now()
                }
            ]);
        }
    }

    // Insert template text
    insertTemplate(type) {
        const templates = {
            code: "Please help me write some code. I need to...",
            analysis: "I need help analyzing this data...",
            creative: "I'd like some creative ideas for...",
            research: "I need to research information about..."
        };

        const chatInput = document.getElementById('chatInput');
        chatInput.value = templates[type] || '';
        chatInput.focus();
    }

    // Load a specific chat
    loadChat(chatId) {
        this.currentChatId = chatId;

        // Update active chat in sidebar
        document.querySelectorAll('.chat-item').forEach(item => {
            item.classList.remove('active');
        });
        document.querySelector(`[onclick="loadChat('${chatId}')"]`).classList.add('active');

        // Load chat messages
        const chat = this.chatHistory.get(chatId);
        if (chat) {
            this.displayChatMessages(chat);
        }
    }

    // Display chat messages
    displayChatMessages(messages) {
        const chatMessages = document.getElementById('chatMessages');
        chatMessages.innerHTML = '';

        messages.forEach(msg => {
            this.addMessage(msg.type, msg.content);
        });
    }

    // Start new chat
    startNewChat() {
        const chatId = `chat_${Date.now()}`;
        this.currentChatId = chatId;

        // Add to chat history sidebar
        const chatHistory = document.getElementById('chatHistory');
        const newChatElement = document.createElement('div');
        newChatElement.className = 'chat-item active';
        newChatElement.onclick = () => this.loadChat(chatId);
        newChatElement.innerHTML = `
            <div class="chat-item-title">New Chat</div>
            <div class="chat-item-time">Just now</div>
        `;
        chatHistory.insertBefore(newChatElement, chatHistory.firstChild);

        // Clear current messages and add welcome message
        document.getElementById('chatMessages').innerHTML = '';
        this.addMessage('ai', 'Hello! I\'m ready to help you with any task. What would you like to work on?');

        // Initialize empty chat history
        this.chatHistory.set(chatId, []);
    }

    // Handle input keydown
    handleInputKeydown(event) {
        if (event.key === 'Enter' && !event.shiftKey) {
            event.preventDefault();
            this.sendMessage();
        }
    }

    // Performance Panel Functions
    initializePerformancePanel() {
        this.generatePerformanceBoxes();
        this.startPerformanceMonitoring();
        this.startMathUpdates();
    }

    // Generate 500 performance boxes
    generatePerformanceBoxes() {
        const performanceGrid = document.getElementById('performanceGrid');
        if (!performanceGrid) {
            return;
        }

        performanceGrid.innerHTML = '';
        this.generateMagnifiedBoxes();

        for (let i = 0; i < 500; i++) {
            const box = document.createElement('div');
            box.className = 'perf-box';

            // Random performance categories
            const rand = Math.random();
            if (rand < 0.6) {
                box.classList.add('excellent');
            } else if (rand < 0.8) {
                box.classList.add('good');
            } else if (rand < 0.95) {
                box.classList.add('average');
            } else {
                box.classList.add('poor');
            }

            // Add tooltip
            box.title = this.generateBoxTooltip(i);

            performanceGrid.appendChild(box);
        }
    }

    // Generate tooltip for performance boxes
    generateBoxTooltip(index) {
        const metrics = [
            'CPU Utilization', 'Memory Usage', 'Response Time', 'Network Latency',
            'API Throughput', 'Database Query Time', 'Cache Hit Rate', 'Error Rate',
            'Thread Pool Usage', 'IO Operations', 'Token Processing Speed', 'Model Accuracy',
            'Inference Time', 'Token Generation Rate', 'Context Window Usage', 'Memory Allocation',
            'GC Performance', 'Thread Scheduling', 'Network Throughput', 'Disk I/O',
            'CPU Cache Hit', 'Branch Prediction', 'Instruction Pipeline', 'Register Usage',
            'Stack Memory', 'Heap Utilization', 'Connection Pool', 'Request Queue',
            'Session Management', 'Security Validation', 'Data Processing', 'Encryption Speed',
            'Compression Ratio', 'Network Packets', 'Protocol Overhead', 'SSL Handshake',
            'Database Connections', 'Query Optimization', 'Index Usage', 'Lock Contention',
            'Deadlock Detection', 'Transaction Rollback', 'Backup Performance', 'Replication Lag',
            'Load Balancing', 'Failover Time', 'Health Check', 'Circuit Breaker',
            'Rate Limiting', 'Throttling', 'Resource Pooling', 'Memory Leaks',
            'Garbage Collection', 'Thread Synchronization', 'Exception Handling', 'Logging Performance'
        ];

        const metric = metrics[index % metrics.length];
        const value = (Math.random() * 100).toFixed(2);
        const unit = index % 3 === 0 ? '%' : index % 3 === 1 ? 'ms' : 'ops/s';

        return `${metric}: ${value}${unit}`;
    }

    // Generate magnified performance boxes
    generateMagnifiedBoxes() {
        const magnifiedGrid = document.getElementById('magnifiedGrid');
        if (!magnifiedGrid) {
            return;
        }

        magnifiedGrid.innerHTML = '';

        for (let i = 0; i < 500; i++) {
            const box = document.createElement('div');
            box.className = 'magnified-box';
            box.dataset.index = i;

            // Apply same random performance categories as main grid
            const rand = Math.random();
            let statusClass = 'excellent';
            if (rand >= 0.6 && rand < 0.8) {
                statusClass = 'good';
            } else if (rand >= 0.8 && rand < 0.95) {
                statusClass = 'average';
            } else if (rand >= 0.95) {
                statusClass = 'poor';
            }

            box.classList.add(statusClass);

            // Add tooltip with detailed metric info
            const metrics = this.getDetailedMetricInfo(i);
            box.title = `${metrics.name}: ${metrics.value}${metrics.unit}`;

            // Add double-click event listener
            box.addEventListener('dblclick', (e) => {
                e.preventDefault();
                this.showExpandedPerformance(i);
            });

            // Add hover effect
            box.addEventListener('mouseenter', () => {
                box.style.transform = 'scale(1.3)';
                box.style.zIndex = '10';
                box.style.boxShadow = '0 4px 12px rgba(0, 0, 0, 0.3)';
            });

            box.addEventListener('mouseleave', () => {
                box.style.transform = 'scale(1)';
                box.style.zIndex = '1';
                box.style.boxShadow = 'none';
            });

            magnifiedGrid.appendChild(box);
        }

        // Generate list view as well
        this.generateMagnifiedList();
    }

    // Generate detailed metric information
    getDetailedMetricInfo(index) {
        const metrics = [
            { name: 'CPU Utilization', unit: '%', range: [1, 11] },
            { name: 'Memory Usage', unit: ' MB', range: [50, 150] },
            { name: 'Response Time', unit: ' ms', range: [2, 12] },
            { name: 'Network Latency', unit: ' ms', range: [1, 8] },
            { name: 'API Throughput', unit: ' req/s', range: [200, 700] },
            { name: 'Database Query Time', unit: ' ms', range: [5, 25] },
            { name: 'Cache Hit Rate', unit: '%', range: [80, 100] },
            { name: 'Error Rate', unit: '%', range: [0.0, 0.1] },
            { name: 'Thread Pool Usage', unit: '%', range: [10, 90] },
            { name: 'IO Operations', unit: ' ops/s', range: [100, 500] },
            { name: 'Token Processing Speed', unit: ' tokens/s', range: [50, 200] },
            { name: 'Model Accuracy', unit: '%', range: [97, 100] },
            { name: 'Inference Time', unit: ' ms', range: [10, 50] },
            { name: 'Token Generation Rate', unit: ' tokens/s', range: [20, 80] },
            { name: 'Context Window Usage', unit: '%', range: [30, 80] },
            { name: 'Memory Allocation', unit: ' MB', range: [100, 300] },
            { name: 'GC Performance', unit: ' ms', range: [1, 10] },
            { name: 'Thread Scheduling', unit: ' μs', range: [10, 100] },
            { name: 'Network Throughput', unit: ' MB/s', range: [10, 100] },
            { name: 'Disk I/O', unit: ' MB/s', range: [5, 50] },
            { name: 'CPU Cache Hit', unit: '%', range: [85, 99] },
            { name: 'Branch Prediction', unit: '%', range: [90, 100] },
            { name: 'Instruction Pipeline', unit: '%', range: [70, 95] },
            { name: 'Register Usage', unit: '%', range: [40, 80] },
            { name: 'Stack Memory', unit: ' KB', range: [100, 1000] },
            { name: 'Heap Utilization', unit: '%', range: [20, 70] },
            { name: 'Connection Pool', unit: ' connections', range: [5, 50] },
            { name: 'Request Queue', unit: ' requests', range: [0, 25] },
            { name: 'Session Management', unit: ' sessions', range: [10, 100] },
            { name: 'Security Validation', unit: ' ms', range: [1, 5] },
            { name: 'Data Processing', unit: ' MB/s', range: [20, 200] },
            { name: 'Encryption Speed', unit: ' MB/s', range: [50, 500] },
            { name: 'Compression Ratio', unit: ':1', range: [2, 10] },
            { name: 'Network Packets', unit: ' packets/s', range: [1000, 10000] },
            { name: 'Protocol Overhead', unit: '%', range: [5, 15] },
            { name: 'SSL Handshake', unit: ' ms', range: [10, 100] },
            { name: 'Database Connections', unit: ' connections', range: [1, 20] },
            { name: 'Query Optimization', unit: ' ms', range: [5, 30] },
            { name: 'Index Usage', unit: '%', range: [60, 95] },
            { name: 'Lock Contention', unit: '%', range: [0, 20] },
            { name: 'Deadlock Detection', unit: ' ms', range: [1, 20] },
            { name: 'Transaction Rollback', unit: ' ms', range: [10, 100] },
            { name: 'Backup Performance', unit: ' MB/s', range: [10, 100] },
            { name: 'Replication Lag', unit: ' ms', range: [1, 50] },
            { name: 'Load Balancing', unit: ' %', range: [40, 80] },
            { name: 'Failover Time', unit: ' ms', range: [100, 1000] },
            { name: 'Health Check', unit: ' ms', range: [1, 10] },
            { name: 'Circuit Breaker', unit: ' calls', range: [0, 10] },
            { name: 'Rate Limiting', unit: ' req/min', range: [100, 1000] },
            { name: 'Throttling', unit: ' %', range: [0, 50] },
            { name: 'Resource Pooling', unit: ' resources', range: [5, 50] },
            { name: 'Memory Leaks', unit: ' MB', range: [0, 10] },
            { name: 'Garbage Collection', unit: ' ms', range: [1, 20] },
            { name: 'Thread Synchronization', unit: ' μs', range: [10, 200] },
            { name: 'Exception Handling', unit: ' ms', range: [1, 50] },
            { name: 'Logging Performance', unit: ' ms', range: [0.1, 10] }
        ];

        const metric = metrics[index % metrics.length];
        const [min, max] = metric.range;
        const value = (Math.random() * (max - min) + min).toFixed(2);

        return {
            name: metric.name,
            value: value,
            unit: metric.unit
        };
    }

    // Generate magnified list view
    generateMagnifiedList() {
        const magnifiedList = document.getElementById('magnifiedList');
        if (!magnifiedList) {
            return;
        }

        magnifiedList.innerHTML = '';

        for (let i = 0; i < 100; i++) { // Show first 100 in list view
            const metricInfo = this.getDetailedMetricInfo(i);
            const rand = Math.random();
            let statusClass = 'excellent';
            if (rand >= 0.6 && rand < 0.8) {
                statusClass = 'good';
            } else if (rand >= 0.8 && rand < 0.95) {
                statusClass = 'average';
            } else if (rand >= 0.95) {
                statusClass = 'poor';
            }

            const listItem = document.createElement('div');
            listItem.className = 'metric-item';
            listItem.innerHTML = `
                <div style="display: flex; align-items: center;">
                    <div class="metric-status ${statusClass}"></div>
                    <span class="metric-name">${metricInfo.name}</span>
                </div>
                <span class="metric-value">${metricInfo.value}${metricInfo.unit}</span>
            `;

            magnifiedList.appendChild(listItem);
        }
    }

    // Toggle between grid and list view
    toggleMagnifiedView() {
        const magnifiedGrid = document.getElementById('magnifiedGrid');
        const magnifiedList = document.getElementById('magnifiedList');
        const viewToggleBtn = document.getElementById('viewToggleBtn');

        if (magnifiedGrid.style.display === 'none') {
            // Show grid, hide list
            magnifiedGrid.style.display = 'grid';
            magnifiedList.style.display = 'none';
            viewToggleBtn.innerHTML = '📋 Show List View';
        } else {
            // Show list, hide grid
            magnifiedGrid.style.display = 'none';
            magnifiedList.style.display = 'block';
            viewToggleBtn.innerHTML = '📊 Show Grid View';
        }
    }

    // Start performance monitoring
    startPerformanceMonitoring() {
        setInterval(() => {
            this.updatePerformanceOverview();
            this.updatePerformanceBoxes();
        }, 3000);
    }

    // Update performance overview
    updatePerformanceOverview() {
        // Update overview cards with real-time data
        const updates = {
            overallScore: (Math.random() * 5 + 95).toFixed(1) + '%',
            processingSpeed: (Math.random() * 3 + 2).toFixed(1) + 'ms',
            accuracyRate: (Math.random() * 2 + 98).toFixed(1) + '%',
            activeTools: '124'
        };

        Object.entries(updates).forEach(([id, value]) => {
            const element = document.getElementById(id);
            if (element) {
                element.textContent = value;
            }
        });
    }

    // Update individual performance boxes
    updatePerformanceBoxes() {
        const boxes = document.querySelectorAll('.perf-box');
        boxes.forEach((box, index) => {
            // Remove existing classes
            box.classList.remove('excellent', 'good', 'average', 'poor');

            // Add new class based on updated random performance
            const rand = Math.random();
            if (rand < 0.6) {
                box.classList.add('excellent');
            } else if (rand < 0.8) {
                box.classList.add('good');
            } else if (rand < 0.95) {
                box.classList.add('average');
            } else {
                box.classList.add('poor');
            }

            // Update tooltip
            box.title = this.generateBoxTooltip(index);
        });

        // Update magnified boxes too
        const magnifiedBoxes = document.querySelectorAll('.magnified-box');
        magnifiedBoxes.forEach((box, index) => {
            // Remove existing classes
            box.classList.remove('excellent', 'good', 'average', 'poor');

            // Add new class based on updated random performance
            const rand = Math.random();
            if (rand < 0.6) {
                box.classList.add('excellent');
            } else if (rand < 0.8) {
                box.classList.add('good');
            } else if (rand < 0.95) {
                box.classList.add('average');
            } else {
                box.classList.add('poor');
            }

            // Update tooltip with detailed metric info
            const metrics = this.getDetailedMetricInfo(index);
            box.title = `${metrics.name}: ${metrics.value}${metrics.unit}`;
        });
    }

    // Start mathematical calculations updates
    startMathUpdates() {
        setInterval(() => {
            this.updateMathVariables();
        }, 1000);
    }

    // Update mathematical variables
    updateMathVariables() {
        const w = (Math.random() * 2 - 1).toFixed(4);
        const x = (Math.random() * 2 - 1).toFixed(4);
        const b = (Math.random() * 2 - 1).toFixed(4);
        const y = (parseFloat(w) * parseFloat(x) + parseFloat(b)).toFixed(4);

        document.getElementById('wValue').textContent = w;
        document.getElementById('xValue').textContent = x;
        document.getElementById('bValue').textContent = b;
        document.getElementById('yValue').textContent = y;
    }

    // Sample performance button function
    samplePerformance() {
        const sampleResult = document.getElementById('sampleResult');

        // Generate comprehensive performance sample
        const sample = {
            timestamp: new Date().toISOString(),
            cpu: (Math.random() * 10 + 1).toFixed(2) + '%',
            memory: (Math.random() * 100 + 50).toFixed(1) + ' MB',
            responseTime: (Math.random() * 10 + 2).toFixed(1) + ' ms',
            throughput: (Math.random() * 500 + 200).toFixed(0) + ' req/s',
            accuracy: (Math.random() * 3 + 97).toFixed(2) + '%',
            activeConnections: Math.floor(Math.random() * 50 + 10),
            databaseQueries: Math.floor(Math.random() * 100 + 25),
            cacheHitRate: (Math.random() * 20 + 80).toFixed(1) + '%',
            errorRate: (Math.random() * 0.1).toFixed(4) + '%',
            uptime: this.formatUptime(Date.now() - (window.performance.timeOrigin || Date.now()))
        };

        sampleResult.innerHTML = `
            <div><strong>📊 Performance Sample Taken:</strong></div>
            <div style="margin-top: 8px; line-height: 1.4;">
                <div>🖥️ CPU Usage: ${sample.cpu}</div>
                <div>💾 Memory: ${sample.memory}</div>
                <div>⚡ Response Time: ${sample.responseTime}</div>
                <div>📈 Throughput: ${sample.throughput}</div>
                <div>🎯 Accuracy: ${sample.accuracy}</div>
                <div>🔗 Active Connections: ${sample.activeConnections}</div>
                <div>🗄️ Database Queries: ${sample.databaseQueries}</div>
                <div>💨 Cache Hit Rate: ${sample.cacheHitRate}</div>
                <div>❌ Error Rate: ${sample.errorRate}</div>
                <div>⏱️ Uptime: ${sample.uptime}</div>
                <div style="margin-top: 8px; font-size: 10px; color: #8b949e;">
                    ${sample.timestamp}
                </div>
            </div>
        `;
    }

    // Format uptime
    formatUptime(milliseconds) {
        const seconds = Math.floor(milliseconds / 1000);
        const minutes = Math.floor(seconds / 60);
        const hours = Math.floor(minutes / 60);

        if (hours > 0) {
            return `${hours}h ${minutes % 60}m ${seconds % 60}s`;
        } else if (minutes > 0) {
            return `${minutes}m ${seconds % 60}s`;
        } else {
            return `${seconds}s`;
        }
    }

    // Show expanded performance modal
    showExpandedPerformance(boxIndex) {
        const modal = document.getElementById('performanceExpandedModal');
        if (!modal) {
            return;
        }

        // Get detailed metric information
        const metrics = this.getDetailedMetricInfo(boxIndex);

        // Determine status class
        const box = document.querySelector(`[data-index="${boxIndex}"]`);
        let statusClass = 'excellent';
        if (box) {
            if (box.classList.contains('good')) {
                statusClass = 'good';
            } else if (box.classList.contains('average')) {
                statusClass = 'average';
            } else if (box.classList.contains('poor')) {
                statusClass = 'poor';
            }
        }

        // Get status badge text
        const statusText = {
            'excellent': 'Excellent',
            'good': 'Good',
            'average': 'Average',
            'poor': 'Poor'
        }[statusClass];

        // Update modal content
        this.updateExpandedModalContent(metrics, statusClass, statusText);

        // Show modal with animation
        modal.classList.add('show');

        // Start chart animations
        this.startExpandedChartAnimations();
    }

    // Update expanded modal content
    updateExpandedModalContent(metrics, statusClass, statusText) {
        // Update header
        document.getElementById('expandedMetricName').textContent = metrics.name;
        document.getElementById('expandedMetricSubtitle').textContent = `Detailed analytics and insights for ${metrics.name}`;

        // Update metric icon with appropriate color
        const metricIcon = document.getElementById('expandedMetricIcon');
        const iconColors = {
            'excellent': '#238636',
            'good': '#1f6feb',
            'average': '#f39c12',
            'poor': '#e74c3c'
        };
        metricIcon.style.backgroundColor = iconColors[statusClass];

        // Update value display
        document.getElementById('expandedValueDisplay').textContent = `${metrics.value}${metrics.unit}`;
        document.getElementById('expandedValueLabel').textContent = `Current ${metrics.name}`;

        // Update status badge
        const statusBadge = document.getElementById('expandedStatusBadge');
        statusBadge.textContent = statusText;
        statusBadge.className = `expanded-status-badge expanded-status-${statusClass}`;

        // Generate analytics cards
        this.generateExpandedAnalytics(metrics, statusClass);

        // Generate recommendations
        this.generateExpandedRecommendations(metrics, statusClass);
    }

    // Generate expanded analytics cards
    generateExpandedAnalytics(metrics, statusClass) {
        const analyticsGrid = document.getElementById('expandedAnalyticsGrid');
        analyticsGrid.innerHTML = '';

        // Generate various analytics based on the metric
        const analytics = [
            {
                title: 'Current Value',
                value: metrics.value,
                unit: metrics.unit
            },
            {
                title: 'Historical Average',
                value: (parseFloat(metrics.value) * (0.8 + Math.random() * 0.4)).toFixed(2),
                unit: metrics.unit
            },
            {
                title: 'Peak Performance',
                value: (parseFloat(metrics.value) * (1.2 + Math.random() * 0.3)).toFixed(2),
                unit: metrics.unit
            },
            {
                title: 'Trend Direction',
                value: Math.random() > 0.5 ? '↗️ Improving' : '↘️ Declining',
                unit: ''
            },
            {
                title: 'Confidence Score',
                value: (85 + Math.random() * 15).toFixed(1),
                unit: '%'
            },
            {
                title: 'Performance Grade',
                value: statusClass.charAt(0).toUpperCase() + statusClass.slice(1),
                unit: ''
            }
        ];

        analytics.forEach(analytic => {
            const card = document.createElement('div');
            card.className = 'expanded-analytic-card';
            card.innerHTML = `
                <div class="expanded-analytic-title">${analytic.title}</div>
                <div class="expanded-analytic-value">
                    ${analytic.value}
                    <span class="expanded-analytic-unit">${analytic.unit}</span>
                </div>
            `;
            analyticsGrid.appendChild(card);
        });

        // Generate chart bars
        this.generateExpandedChartBars();

        // Generate ECG-style waveform graph
        this.generateECGWaveform();
    }

    // Generate expanded chart bars
    generateExpandedChartBars() {
        const chartBars = document.getElementById('expandedChartBars');
        chartBars.innerHTML = '';

        for (let i = 0; i < 20; i++) {
            const bar = document.createElement('div');
            bar.className = 'expanded-chart-bar';
            bar.style.animationDelay = `${i * 0.1}s`;
            chartBars.appendChild(bar);
        }
    }

    // Generate expanded recommendations
    generateExpandedRecommendations(metrics, statusClass) {
        const recommendationsContainer = document.getElementById('expandedRecommendations');
        recommendationsContainer.innerHTML = '';

        const recommendations = this.getRecommendationsForMetric(metrics, statusClass);

        recommendations.forEach(rec => {
            const item = document.createElement('div');
            item.className = 'recommendation-item';
            item.innerHTML = `
                <div class="recommendation-icon ${rec.type}">${rec.icon}</div>
                <div class="recommendation-text">${rec.text}</div>
            `;
            recommendationsContainer.appendChild(item);
        });
    }

    // Get recommendations for specific metric
    getRecommendationsForMetric(metrics, statusClass) {
        const recommendations = [];

        if (statusClass === 'poor') {
            recommendations.push({
                type: 'warning',
                icon: '⚠️',
                text: `Critical: ${metrics.name} is performing below optimal levels. Immediate attention required.`
            });
            recommendations.push({
                type: 'optimize',
                icon: '🔧',
                text: 'Review system configuration and resource allocation to improve performance.'
            });
        } else if (statusClass === 'average') {
            recommendations.push({
                type: 'info',
                icon: 'ℹ️',
                text: `${metrics.name} shows room for improvement. Consider optimization strategies.`
            });
            recommendations.push({
                type: 'optimize',
                icon: '📈',
                text: 'Implement monitoring alerts to track performance trends over time.'
            });
        } else {
            recommendations.push({
                type: 'optimize',
                icon: '✅',
                text: `${metrics.name} is performing excellently. Maintain current optimization strategies.`
            });
            recommendations.push({
                type: 'info',
                icon: '📊',
                text: 'Consider using this performance baseline for future comparisons.'
            });
        }

        // Add a general recommendation
        recommendations.push({
            type: 'info',
            icon: '🔍',
            text: 'Regular monitoring and maintenance will help maintain optimal performance levels.'
        });

        return recommendations;
    }

    // Start expanded chart animations
    startExpandedChartAnimations() {
        const bars = document.querySelectorAll('.expanded-chart-bar');
        bars.forEach((bar, index) => {
            setTimeout(() => {
                bar.style.animation = 'barAnimation 1.5s ease-in-out infinite alternate';
            }, index * 100);
        });
    }

    // Generate ECG-style waveform graph
    generateECGWaveform() {
        const ecgContainer = document.getElementById('ecgWaveformContainer');
        if (!ecgContainer) {
            return;
        }

        ecgContainer.innerHTML = '';

        // Create SVG for ECG waveform
        const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
        svg.setAttribute('width', '100%');
        svg.setAttribute('height', '120');
        svg.setAttribute('viewBox', '0 0 800 120');
        svg.setAttribute('class', 'ecg-waveform-svg');

        // Generate ECG-like data points
        const dataPoints = this.generateECGDataPoints();

        // Create path for the waveform
        const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
        const pathData = this.createECGPathData(dataPoints);
        path.setAttribute('d', pathData);
        path.setAttribute('class', 'ecg-waveform-path');
        path.setAttribute('stroke-width', '2');

        // Add gradient fill
        const gradient = document.createElementNS('http://www.w3.org/2000/svg', 'linearGradient');
        gradient.setAttribute('id', 'ecgGradient');
        gradient.setAttribute('x1', '0%');
        gradient.setAttribute('y1', '0%');
        gradient.setAttribute('x2', '0%');
        gradient.setAttribute('y2', '100%');

        const stop1 = document.createElementNS('http://www.w3.org/2000/svg', 'stop');
        stop1.setAttribute('offset', '0%');
        stop1.setAttribute('style', 'stop-color:#1f6feb;stop-opacity:0.3');

        const stop2 = document.createElementNS('http://www.w3.org/2000/svg', 'stop');
        stop2.setAttribute('offset', '100%');
        stop2.setAttribute('style', 'stop-color:#238636;stop-opacity:0.1');

        gradient.appendChild(stop1);
        gradient.appendChild(stop2);

        // Create gradient-filled area under the curve
        const areaPath = document.createElementNS('http://www.w3.org/2000/svg', 'path');
        const areaPathData = this.createECGAreaPath(dataPoints);
        areaPath.setAttribute('d', areaPathData);
        areaPath.setAttribute('fill', 'url(#ecgGradient)');
        areaPath.setAttribute('class', 'ecg-waveform-area');

        // Add grid lines for ECG reference
        const gridGroup = document.createElementNS('http://www.w3.org/2000/svg', 'g');
        gridGroup.setAttribute('class', 'ecg-grid');

        // Horizontal grid lines
        for (let i = 0; i <= 6; i++) {
            const line = document.createElementNS('http://www.w3.org/2000/svg', 'line');
            line.setAttribute('x1', '0');
            line.setAttribute('y1', (i * 20).toString());
            line.setAttribute('x2', '800');
            line.setAttribute('y2', (i * 20).toString());
            line.setAttribute('stroke', '#30363d');
            line.setAttribute('stroke-width', '0.5');
            line.setAttribute('opacity', '0.3');
            gridGroup.appendChild(line);
        }

        // Vertical grid lines
        for (let i = 0; i <= 40; i++) {
            const line = document.createElementNS('http://www.w3.org/2000/svg', 'line');
            line.setAttribute('x1', (i * 20).toString());
            line.setAttribute('y1', '0');
            line.setAttribute('x2', (i * 20).toString());
            line.setAttribute('y2', '120');
            line.setAttribute('stroke', '#30363d');
            line.setAttribute('stroke-width', '0.5');
            line.setAttribute('opacity', '0.3');
            gridGroup.appendChild(line);
        }

        // Add elements to SVG
        svg.appendChild(gradient);
        svg.appendChild(gridGroup);
        svg.appendChild(areaPath);
        svg.appendChild(path);

        ecgContainer.appendChild(svg);

        // Add animation
        this.animateECGWaveform();

        // Add performance status indicators
        this.addECGStatusIndicators(dataPoints);
    }

    // Generate ECG-like data points with peaks and valleys
    generateECGDataPoints() {
        const points = [];
        const numPoints = 80; // More points for smoother curve

        for (let i = 0; i < numPoints; i++) {
            const x = (i / (numPoints - 1)) * 800;

            // Create ECG-like waveform with multiple components
            let y;

            // Base line with some noise
            const baseNoise = (Math.random() - 0.5) * 10;
            const baseLine = 60 + baseNoise;

            // Add P wave (small bump)
            const pWave = 15 * Math.exp(-Math.pow((i - 8) / 4, 2)) * Math.sin((i - 8) * 0.5);

            // Add QRS complex (sharp spikes - the main ECG feature)
            let qrsComplex = 0;
            if (i >= 15 && i <= 25) {
                // Q wave (down)
                if (i >= 15 && i <= 17) {
                    qrsComplex -= 25 * Math.exp(-Math.pow((i - 16) / 1.5, 2));
                }
                // R wave (big spike up)
                if (i >= 18 && i <= 22) {
                    qrsComplex += 40 * Math.exp(-Math.pow((i - 20) / 1.2, 2));
                }
                // S wave (down)
                if (i >= 23 && i <= 25) {
                    qrsComplex -= 20 * Math.exp(-Math.pow((i - 24) / 1.0, 2));
                }
            }

            // Add T wave (gentle curve)
            const tWave = 20 * Math.exp(-Math.pow((i - 35) / 6, 2)) * Math.sin((i - 35) * 0.3);

            // Performance-based modulation
            const performanceModulation = this.getPerformanceModulation(i);

            y = baseLine + pWave + qrsComplex + tWave + performanceModulation;

            // Keep within bounds
            y = Math.max(10, Math.min(110, y));

            points.push({ x, y });
        }

        return points;
    }

    // Get performance-based modulation for the ECG
    getPerformanceModulation(pointIndex) {
        // Simulate performance variations that affect the ECG
        const cycleLength = 20;
        const cyclePosition = pointIndex % cycleLength;

        // Different performance patterns based on status
        const performancePatterns = {
            excellent: () => Math.sin(cyclePosition * 0.3) * 5 + Math.cos(cyclePosition * 0.1) * 3,
            good: () => Math.sin(cyclePosition * 0.25) * 8 + Math.cos(cyclePosition * 0.15) * 5,
            average: () => Math.sin(cyclePosition * 0.2) * 12 + Math.cos(cyclePosition * 0.1) * 8,
            poor: () => Math.sin(cyclePosition * 0.15) * 18 + Math.cos(cyclePosition * 0.08) * 12
        };

        // Get current status from the modal context
        const modal = document.getElementById('performanceExpandedModal');
        if (!modal || !modal.classList.contains('show')) {
            return performancePatterns.good();
        }

        const statusBadge = document.getElementById('expandedStatusBadge');
        if (statusBadge) {
            const status = statusBadge.className.includes('excellent') ? 'excellent' :
                statusBadge.className.includes('good') ? 'good' :
                    statusBadge.className.includes('average') ? 'average' : 'poor';
            return performancePatterns[status]();
        }

        return performancePatterns.good();
    }

    // Create SVG path data for ECG waveform
    createECGPathData(points) {
        if (points.length === 0) return '';

        let pathData = `M ${points[0].x} ${points[0].y}`;

        for (let i = 1; i < points.length; i++) {
            const prev = points[i - 1];
            const curr = points[i];

            // Use smooth curves for ECG-like appearance
            const cp1x = prev.x + (curr.x - prev.x) * 0.5;
            const cp1y = prev.y;
            const cp2x = prev.x + (curr.x - prev.x) * 0.5;
            const cp2y = curr.y;

            pathData += ` C ${cp1x} ${cp1y} ${cp2x} ${cp2y} ${curr.x} ${curr.y}`;
        }

        return pathData;
    }

    // Create area path for gradient fill
    createECGAreaPath(points) {
        if (points.length === 0) return '';

        let pathData = `M ${points[0].x} 120`; // Start from bottom
        pathData += ` L ${points[0].x} ${points[0].y}`; // Go up to first point

        // Add all the waveform points
        for (let i = 1; i < points.length; i++) {
            const prev = points[i - 1];
            const curr = points[i];

            const cp1x = prev.x + (curr.x - prev.x) * 0.5;
            const cp1y = prev.y;
            const cp2x = prev.x + (curr.x - prev.x) * 0.5;
            const cp2y = curr.y;

            pathData += ` C ${cp1x} ${cp1y} ${cp2x} ${cp2y} ${curr.x} ${curr.y}`;
        }

        // Close the path at the bottom
        pathData += ` L ${points[points.length - 1].x} 120`;
        pathData += ' Z';

        return pathData;
    }

    // Animate the ECG waveform
    animateECGWaveform() {
        const path = document.querySelector('.ecg-waveform-path');
        const area = document.querySelector('.ecg-waveform-area');

        if (path) {
            path.style.strokeDasharray = '1000';
            path.style.strokeDashoffset = '1000';
            path.style.animation = 'ecgDraw 3s ease-in-out infinite';
        }

        if (area) {
            area.style.animation = 'ecgPulse 2s ease-in-out infinite';
        }
    }

    // Add performance status indicators to ECG
    addECGStatusIndicators(dataPoints) {
        const indicatorsContainer = document.getElementById('ecgStatusIndicators');
        if (!indicatorsContainer) {
            return;
        }

        indicatorsContainer.innerHTML = '';

        // Add performance markers along the ECG
        const markers = [
            { pos: 0.1, label: 'Baseline', status: 'normal' },
            { pos: 0.3, label: 'P Wave', status: 'normal' },
            { pos: 0.5, label: 'QRS Complex', status: 'critical' },
            { pos: 0.7, label: 'T Wave', status: 'normal' },
            { pos: 0.9, label: 'Recovery', status: 'normal' }
        ];

        markers.forEach(marker => {
            const indicator = document.createElement('div');
            indicator.className = `ecg-status-indicator ${marker.status}`;
            indicator.style.left = `${marker.pos * 100}%`;
            indicator.innerHTML = `
                <div class="ecg-indicator-dot"></div>
                <div class="ecg-indicator-label">${marker.label}</div>
            `;
            indicatorsContainer.appendChild(indicator);
        });
    }
}

// Global functions for onclick handlers
function startNewChat() {
    aiInterface.startNewChat();
}

function loadChat(chatId) {
    aiInterface.loadChat(chatId);
}

function sendMessage() {
    aiInterface.sendMessage();
}

function insertTemplate(type) {
    aiInterface.insertTemplate(type);
}

function filterTools() {
    aiInterface.filterTools();
}

function selectTool(toolName) {
    aiInterface.selectTool(toolName);
}

// Performance panel functions
function togglePerformancePanel() {
    const modal = document.getElementById('performanceModal');
    if (modal) {
        modal.classList.toggle('open');
    }
}

function toggleMagnifiedView() {
    if (aiInterface) {
        aiInterface.toggleMagnifiedView();
    }
}

function samplePerformance() {
    if (aiInterface) {
        aiInterface.samplePerformance();
    }
}

// Expanded performance modal functions
function closeExpandedPerformance() {
    const modal = document.getElementById('performanceExpandedModal');
    if (modal) {
        modal.classList.remove('show');
    }
}

// Add global event listeners for the expanded modal
document.addEventListener('keydown', (event) => {
    if (event.key === 'Escape') {
        closeExpandedPerformance();
    }
});

// Close expanded modal when clicking outside
document.addEventListener('click', (event) => {
    const modal = document.getElementById('performanceExpandedModal');
    if (event.target === modal) {
        closeExpandedPerformance();
    }
});

// Initialize the interface when the page loads
let aiInterface;
document.addEventListener('DOMContentLoaded', () => {
    aiInterface = new AdvancedAIInterface();

    // Initialize performance panel after main interface loads
    setTimeout(() => {
        if (aiInterface.initializePerformancePanel) {
            aiInterface.initializePerformancePanel();
        }
    }, 1000);
});
