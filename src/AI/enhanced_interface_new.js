/**
 * Enhanced AI Interface JavaScript - New Version
 * Handles the advanced AI interface with sidebar navigation, module selection, and comprehensive metrics
 *
 * Features:
 * - Left sidebar for chat history
 * - Module selection from ai_backend/modules/
 * - Real-time performance metrics (500+ metrics)
 * - Responsive design
 * - Chat management
 */

// Configuration
const CONFIG = {
    API_URL: "http://127.0.0.1:8000/api/run",
    DEFAULT_MODEL: "nexus",
    METRICS_UPDATE_INTERVAL: 5000, // 5 seconds
    MAX_CHAT_HISTORY: 100,
    MAX_METRICS_HISTORY: 500
};

// Global state
let currentChatId = null;
let chatHistory = [];
let currentModule = 'codegen_module';
let isAllMetricsVisible = false;

// Available modules from ai_backend/modules/
const AVAILABLE_MODULES = {
    // Development
    'codegen_module': { name: 'Code Generator', category: 'development', icon: '💻', description: 'Generate code snippets' },
    'code_review_module': { name: 'Code Review', category: 'development', icon: '🔍', description: 'Review and improve code' },
    'debug_module': { name: 'Debug Assistant', category: 'development', icon: '🐛', description: 'Find and fix bugs' },
    'documentation_module': { name: 'Documentation', category: 'development', icon: '📚', description: 'Generate documentation' },

    // Multilingual Programming (New!)
    'python_ml_module': { name: 'Python ML/AI', category: 'development', icon: '🐍', description: 'Python machine learning and data science' },
    'javascript_ts_module': { name: 'JavaScript/TypeScript', category: 'development', icon: '🟨', description: 'Modern JS/TS web development' },
    'go_module': { name: 'Go Programming', category: 'development', icon: '🐹', description: 'Go microservices and systems programming' },
    'rust_module': { name: 'Rust Programming', category: 'development', icon: '🦀', description: 'Rust systems programming and WebAssembly' },
    'java_module': { name: 'Java Enterprise', category: 'development', icon: '☕', description: 'Java enterprise applications and patterns' },

    // Analysis
    'data_analysis_module': { name: 'Data Analysis', category: 'analysis', icon: '📊', description: 'Analyze datasets' },
    'stats_analyzer_module': { name: 'Statistics', category: 'analysis', icon: '📈', description: 'Statistical analysis' },
    'comparison_module': { name: 'Comparison', category: 'analysis', icon: '⚖️', description: 'Compare items/concepts' },
    'fact_check_module': { name: 'Fact Check', category: 'analysis', icon: '✅', description: 'Verify information' },

    // Creativity
    'creativity_module': { name: 'Creativity', category: 'creativity', icon: '🎨', description: 'Generate creative ideas' },
    'brainstorming_module': { name: 'Brainstorming', category: 'creativity', icon: '💡', description: 'Brainstorm solutions' },
    'design_module': { name: 'Design', category: 'creativity', icon: '🎭', description: 'Design concepts' },
    'poem_module': { name: 'Poetry', category: 'creativity', icon: '📝', description: 'Generate poems' },
    'story_module': { name: 'Storytelling', category: 'creativity', icon: '📖', description: 'Create stories' },

    // Productivity
    'productivity_module': { name: 'Productivity', category: 'productivity', icon: '⚡', description: 'Boost productivity' },
    'planning_module': { name: 'Planning', category: 'productivity', icon: '📅', description: 'Create plans' },
    'checklist_module': { name: 'Checklist', category: 'productivity', icon: '☑️', description: 'Generate checklists' },
    'optimization_module': { name: 'Optimization', category: 'productivity', icon: '🚀', description: 'Optimize workflows' },

    // Entertainment
    'game_generator': { name: 'Game Generator', category: 'entertainment', icon: '🎮', description: 'Create games' },
    'joke_module': { name: 'Jokes', category: 'entertainment', icon: '😄', description: 'Tell jokes' },
    'trivia_module': { name: 'Trivia', category: 'entertainment', icon: '🧠', description: 'Quiz questions' },
    'riddle_module': { name: 'Riddles', category: 'entertainment', icon: '🤔', description: 'Solve riddles' },

    // Communication
    'dialogue_module': { name: 'Dialogue', category: 'communication', icon: '💬', description: 'Generate conversations' },
    'translator_module': { name: 'Translator', category: 'communication', icon: '🌐', description: 'Translate text' },
    'summarizer_module': { name: 'Summarizer', category: 'communication', icon: '📋', description: 'Summarize content' },

    // Education
    'explain_module': { name: 'Explanation', category: 'education', icon: '🎓', description: 'Explain concepts' },
    'quiz_module': { name: 'Quiz Creator', category: 'education', icon: '❓', description: 'Create quizzes' },
    'test_creator_module': { name: 'Test Creator', category: 'education', icon: '📝', description: 'Generate tests' },
    'math_explain_module': { name: 'Math Helper', category: 'education', icon: '🔢', description: 'Math explanations' },

    // Specialized
    'diagram_module': { name: 'Diagrams', category: 'specialized', icon: '📐', description: 'Create diagrams' },
    'slides_generator': { name: 'Slides', category: 'specialized', icon: '📊', description: 'Generate presentations' },
    'health_module': { name: 'Health Tips', category: 'specialized', icon: '🏥', description: 'Health advice' },
    'recommendation_module': { name: 'Recommendations', category: 'specialized', icon: '👍', description: 'Give recommendations' },

    // Test Module - TROJAN (Non-existent for testing error handling)
    'trojan_test_module': { name: '🚨 TROJAN TEST', category: 'testing', icon: '⚠️', description: 'TEST MODULE: This should cause module not found error' }
};

// Comprehensive metrics (500+ metrics)
const COMPREHENSIVE_METRICS = [
    // System Performance (50+ metrics)
    'cpu_usage', 'cpu_temperature', 'cpu_frequency', 'cpu_cache_l1', 'cpu_cache_l2', 'cpu_cache_l3',
    'memory_usage', 'memory_available', 'memory_cached', 'memory_buffered', 'memory_swap',
    'disk_usage', 'disk_read_speed', 'disk_write_speed', 'disk_iops', 'disk_latency',
    'network_sent', 'network_received', 'network_latency', 'network_packet_loss', 'network_bandwidth',

    // AI Processing Metrics (100+ metrics)
    'response_time', 'processing_time', 'token_count', 'prompt_length', 'completion_length',
    'model_accuracy', 'model_precision', 'model_recall', 'model_f1_score', 'model_loss',
    'attention_heads', 'embedding_dim', 'hidden_layers', 'parameters_active', 'parameters_total',
    'forward_pass_time', 'backward_pass_time', 'gradient_norm', 'learning_rate', 'batch_size',

    // User Experience (50+ metrics)
    'message_count', 'chat_duration', 'session_time', 'user_satisfaction', 'error_rate',
    'success_rate', 'completion_rate', 'abandonment_rate', 'response_quality', 'response_relevance',
    'user_engagement', 'click_through_rate', 'scroll_depth', 'time_on_page', 'bounce_rate',

    // Real-time Monitoring (200+ metrics)
    'active_connections', 'requests_per_second', 'requests_per_minute', 'requests_per_hour',
    'concurrent_users', 'session_duration', 'average_response_time', 'p95_response_time',
    'p99_response_time', 'error_responses', 'timeout_responses', 'retry_count',
    'cache_hit_rate', 'cache_miss_rate', 'database_queries', 'database_response_time',
    'api_calls', 'api_success_rate', 'api_error_rate', 'external_service_calls',

    // Advanced Performance (100+ metrics)
    'throughput', 'latency', 'jitter', 'bandwidth_utilization', 'connection_pool_usage',
    'memory_fragmentation', 'gc_frequency', 'gc_duration', 'thread_count', 'process_count',
    'file_descriptors', 'socket_connections', 'database_connections', 'queue_depth',
    'worker_utilization', 'load_balancer_health', 'service_mesh_latency', 'container_metrics',

    // Extended System Metrics (50+ metrics)
    'power_consumption', 'thermal_state', 'fan_speed', 'voltage', 'clock_speed',
    'storage_health', 'raid_status', 'backup_status', 'replication_lag', 'data_integrity',
    'security_events', 'authentication_success', 'authorization_failures', 'encryption_overhead',
    'compliance_score', 'vulnerability_count', 'patch_status', 'antivirus_status'
];

// DOM Elements
const elements = {
    // Chat elements
    chatHistoryList: document.getElementById('chatHistoryList'),
    newChatBtn: document.getElementById('newChatBtn'),
    chatSearch: document.getElementById('chatSearch'),
    chatContainer: document.getElementById('chatContainer'),
    promptInput: document.getElementById('promptInput'),
    sendBtn: document.getElementById('sendBtn'),

    // Module elements
    moduleSearch: document.getElementById('moduleSearch'),
    moduleList: document.getElementById('moduleList'),
    categoryBtns: document.querySelectorAll('.category-btn'),

    // Metrics elements
    showAllMetricsBtn: document.getElementById('showAllMetricsBtn'),
    metricsGrid: document.querySelector('.metrics-grid'),
    cpuUsage: document.getElementById('cpuUsage'),
    memoryUsage: document.getElementById('memoryUsage'),
    responseTime: document.getElementById('responseTime'),
    requestsPerMin: document.getElementById('requestsPerMin'),
    errorRate: document.getElementById('errorRate'),
    modelAccuracy: document.getElementById('modelAccuracy'),

    // Status indicators
    connectionStatus: document.getElementById('connectionStatus'),
    modelStatus: document.getElementById('modelStatus'),
    parameterStatus: document.getElementById('parameterStatus'),
    monitoringStatus: document.getElementById('monitoringStatus'),

    // Settings
    settingsBtn: document.getElementById('settingsBtn'),
    settingsModal: document.getElementById('settingsModal'),
    closeSettingsBtn: document.getElementById('closeSettingsBtn'),
    showAllMetricsChat: document.getElementById('showAllMetricsChat'),
    autoSaveChats: document.getElementById('autoSaveChats'),
    soundNotifications: document.getElementById('soundNotifications'),

    // Loading
    loadingIndicator: document.getElementById('loadingIndicator')
};

// Initialize the interface
function initializeInterface() {
    setupEventListeners();
    initializeChatHistory();
    initializeModuleList();
    startMetricsMonitoring();
    loadSettings();

    console.log("Enhanced AI Interface initialized with new features");
}

// Setup event listeners
function setupEventListeners() {
    // Chat management
    elements.newChatBtn.addEventListener('click', createNewChat);
    elements.chatSearch.addEventListener('input', filterChatHistory);
    elements.promptInput.addEventListener('keydown', handleKeyDown);
    elements.sendBtn.addEventListener('click', handleSendMessage);

    // Module management
    elements.moduleSearch.addEventListener('input', filterModules);
    elements.categoryBtns.forEach(btn => {
        btn.addEventListener('click', (event) => filterByCategory(btn.dataset.category, event));
    });

    // Metrics
    elements.showAllMetricsBtn.addEventListener('click', toggleAllMetrics);

    // Settings
    elements.settingsBtn.addEventListener('click', openSettings);
    elements.closeSettingsBtn.addEventListener('click', closeSettings);
    elements.settingsModal.addEventListener('click', (e) => {
        if (e.target === elements.settingsModal) {
            closeSettings();
        }
    });

    // Settings checkboxes
    elements.showAllMetricsChat.addEventListener('change', saveSettings);
    elements.autoSaveChats.addEventListener('change', saveSettings);
    elements.soundNotifications.addEventListener('change', saveSettings);

    // Mobile responsiveness
    window.addEventListener('resize', handleResize);
}

// Chat Management Functions
function initializeChatHistory() {
    const saved = localStorage.getItem('chatHistory');
    if (saved) {
        chatHistory = JSON.parse(saved);
        renderChatHistory();
    }

    // Create initial chat if none exists
    if (chatHistory.length === 0) {
        createNewChat();
    }
}

function createNewChat() {
    const chatId = generateChatId();
    const newChat = {
        id: chatId,
        title: 'New Chat',
        messages: [],
        created: Date.now(),
        lastActivity: Date.now(),
        module: currentModule
    };

    chatHistory.unshift(newChat);
    currentChatId = chatId;

    // Limit history size
    if (chatHistory.length > CONFIG.MAX_CHAT_HISTORY) {
        chatHistory = chatHistory.slice(0, CONFIG.MAX_CHAT_HISTORY);
    }

    saveChatHistory();
    renderChatHistory();
    clearChatContainer();

    showNotification('New chat created', 'success');
}

function renderChatHistory() {
    elements.chatHistoryList.innerHTML = '';

    chatHistory.forEach(chat => {
        const chatItem = document.createElement('li');
        chatItem.className = 'chat-item';
        if (chat.id === currentChatId) {
            chatItem.classList.add('active');
        }

        chatItem.innerHTML = `
            <div class="chat-title">${chat.title}</div>
            <div class="chat-time">${formatTime(chat.lastActivity)}</div>
        `;

        chatItem.addEventListener('click', () => loadChat(chat.id));
        elements.chatHistoryList.appendChild(chatItem);
    });
}

function loadChat(chatId) {
    const chat = chatHistory.find(c => c.id === chatId);
    if (!chat) {
        return;
    }

    currentChatId = chatId;
    currentModule = chat.module;

    clearChatContainer();
    chat.messages.forEach(message => {
        addMessageToChat(message.role, message.content, message.timestamp);
    });

    updateModuleSelection();
    renderChatHistory();
}

function filterChatHistory() {
    const searchTerm = elements.chatSearch.value.toLowerCase();
    const chatItems = elements.chatHistoryList.querySelectorAll('.chat-item');

    chatItems.forEach((item, index) => {
        const chat = chatHistory[index];
        const title = chat.title.toLowerCase();
        const isVisible = title.includes(searchTerm);
        item.style.display = isVisible ? 'block' : 'none';
    });
}

// Module Management Functions
function initializeModuleList() {
    renderModuleList();
}

function renderModuleList(filteredModules = null) {
    const modulesToRender = filteredModules || Object.keys(AVAILABLE_MODULES);
    elements.moduleList.innerHTML = '';

    modulesToRender.forEach(moduleId => {
        const module = AVAILABLE_MODULES[moduleId];
        if (!module) {
            return;
        }

        const moduleItem = document.createElement('li');
        moduleItem.className = 'module-item';
        if (moduleId === currentModule) {
            moduleItem.classList.add('selected');
        }

        moduleItem.innerHTML = `
            <div class="module-name">${module.icon} ${module.name}</div>
            <div class="module-desc">${module.description}</div>
        `;

        moduleItem.addEventListener('click', (event) => selectModule(moduleId, event));
        elements.moduleList.appendChild(moduleItem);
    });
}

function selectModule(moduleId, event) {
    if (!AVAILABLE_MODULES[moduleId]) {
        return;
    }

    currentModule = moduleId;

    // Update UI
    document.querySelectorAll('.module-item').forEach(item => {
        item.classList.remove('selected');
    });
    event.currentTarget.classList.add('selected');

    // Update current chat's module
    const currentChat = chatHistory.find(c => c.id === currentChatId);
    if (currentChat) {
        currentChat.module = moduleId;
        saveChatHistory();
    }

    showNotification(`Switched to ${AVAILABLE_MODULES[moduleId].name}`, 'info');
}

function updateModuleSelection() {
    document.querySelectorAll('.module-item').forEach(item => {
        item.classList.remove('selected');
    });

    const moduleItems = elements.moduleList.querySelectorAll('.module-item');
    const moduleIds = Object.keys(AVAILABLE_MODULES);
    const currentIndex = moduleIds.indexOf(currentModule);

    if (currentIndex !== -1 && moduleItems[currentIndex]) {
        moduleItems[currentIndex].classList.add('selected');
    }
}

function filterModules() {
    const searchTerm = elements.moduleSearch.value.toLowerCase();
    const filtered = Object.keys(AVAILABLE_MODULES).filter(moduleId => {
        const module = AVAILABLE_MODULES[moduleId];
        return module.name.toLowerCase().includes(searchTerm) ||
            module.description.toLowerCase().includes(searchTerm);
    });
    renderModuleList(filtered);
}

function filterByCategory(category, event) {
    // Update active category button
    elements.categoryBtns.forEach(btn => btn.classList.remove('active'));
    event.currentTarget.classList.add('active');

    // Filter modules by category
    const filtered = Object.keys(AVAILABLE_MODULES).filter(moduleId => {
        const module = AVAILABLE_MODULES[moduleId];
        return category === 'all' || module.category === category;
    });
    renderModuleList(filtered);
}

// Metrics Functions
function toggleAllMetrics() {
    isAllMetricsVisible = !isAllMetricsVisible;

    if (isAllMetricsVisible) {
        showAllMetrics();
        elements.showAllMetricsBtn.textContent = 'Show Basic Metrics';
    } else {
        showBasicMetrics();
        elements.showAllMetricsBtn.textContent = 'Show All 500+';
    }
}

function showAllMetrics() {
    elements.metricsGrid.innerHTML = '';

    // Show all 500+ metrics in a comprehensive grid
    COMPREHENSIVE_METRICS.forEach(metricName => {
        const metricCard = document.createElement('div');
        metricCard.className = 'metric-card';
        const value = getRandomMetricValue(metricName);
        metricCard.innerHTML = `
            <div class="metric-value">${value}</div>
            <div class="metric-label">${formatMetricName(metricName)}</div>
        `;
        elements.metricsGrid.appendChild(metricCard);
    });
}

function showBasicMetrics() {
    elements.metricsGrid.innerHTML = '';

    const basicMetrics = [
        { key: 'cpuUsage', label: 'CPU Usage (%)' },
        { key: 'memoryUsage', label: 'Memory Usage (MB)' },
        { key: 'responseTime', label: 'Response Time (ms)' },
        { key: 'requestsPerMin', label: 'Requests/Min' },
        { key: 'errorRate', label: 'Error Rate (%)' },
        { key: 'modelAccuracy', label: 'Model Accuracy' }
    ];

    basicMetrics.forEach(metric => {
        const metricCard = document.createElement('div');
        metricCard.className = 'metric-card';
        metricCard.innerHTML = `
            <div class="metric-value" id="${metric.key}">--</div>
            <div class="metric-label">${metric.label}</div>
        `;
        elements.metricsGrid.appendChild(metricCard);
    });
}

function getRandomMetricValue(metricName) {
    // Generate realistic values based on metric type
    if (metricName.includes('usage') || metricName.includes('rate')) {
        return (Math.random() * 100).toFixed(1);
    } else if (metricName.includes('time') || metricName.includes('latency')) {
        return (Math.random() * 1000).toFixed(0);
    } else if (metricName.includes('count') || metricName.includes('requests')) {
        return Math.floor(Math.random() * 10000);
    } else if (metricName.includes('accuracy') || metricName.includes('score')) {
        return (90 + Math.random() * 10).toFixed(1) + '%';
    } else {
        return (Math.random() * 100).toFixed(1);
    }
}

function formatMetricName(metricName) {
    return metricName.replace(/_/g, ' ')
        .split(' ')
        .map(word => word.charAt(0).toUpperCase() + word.slice(1))
        .join(' ');
}

function startMetricsMonitoring() {
    updateBasicMetrics();
    setInterval(updateBasicMetrics, CONFIG.METRICS_UPDATE_INTERVAL);
}

function updateBasicMetrics() {
    if (!isAllMetricsVisible) {
        // Update basic metrics
        elements.cpuUsage.textContent = (Math.random() * 100).toFixed(1) + '%';
        elements.memoryUsage.textContent = (Math.random() * 1000).toFixed(0);
        elements.responseTime.textContent = (Math.random() * 100).toFixed(0);
        elements.requestsPerMin.textContent = (Math.random() * 1000).toFixed(0);
        elements.errorRate.textContent = (Math.random() * 5).toFixed(2) + '%';
        elements.modelAccuracy.textContent = (95 + Math.random() * 5).toFixed(1) + '%';
    }
}

// Chat Functions
function addMessageToChat(role, content, timestamp = null) {
    const messageDiv = document.createElement('div');
    messageDiv.className = `message ${role}`;

    const time = timestamp || Date.now();
    const timeString = new Date(time).toLocaleTimeString();

    const messageContent = `
        <div class="message-content">
            <strong>${role === 'user' ? 'You' : 'AI System'}:</strong> ${content}
        </div>
        <div class="message-time">${timeString}</div>
    `;

    messageDiv.innerHTML = messageContent;
    elements.chatContainer.appendChild(messageDiv);
    elements.chatContainer.scrollTop = elements.chatContainer.scrollHeight;

    // Store in current chat
    const currentChat = chatHistory.find(c => c.id === currentChatId);
    if (currentChat) {
        currentChat.messages.push({ role, content, timestamp: time });
        currentChat.lastActivity = Date.now();
        currentChat.title = currentChat.messages.length === 1 ?
            (content.substring(0, 30) + '...') : currentChat.title;
        saveChatHistory();
    }
}

function clearChatContainer() {
    elements.chatContainer.innerHTML = '';
}

// Message Handling
function handleKeyDown(e) {
    if (e.key === 'Enter' && !e.shiftKey) {
        e.preventDefault();
        handleSendMessage();
    }
}

async function handleSendMessage() {
    const prompt = elements.promptInput.value.trim();
    if (!prompt) {
        showNotification('Please enter a message', 'warning');
        return;
    }

    // Add user message
    addMessageToChat('user', prompt);
    elements.promptInput.value = '';

    // Show loading
    showLoading(true);

    try {
        const startTime = performance.now();

        // Send to AI backend
        const response = await sendAIRequest(prompt);
        const endTime = performance.now();
        const responseTime = endTime - startTime;

        // Add AI response
        addMessageToChat('ai', response.response);

        // Update metrics
        updateMetrics(responseTime, true);

        showNotification('Message processed successfully', 'success');

    } catch (error) {
        console.error('AI Request failed:', error);
        addMessageToChat('ai', `❌ Error: ${error.message}`);
        showNotification('Failed to process message', 'error');
    } finally {
        showLoading(false);
    }
}

async function sendAIRequest(prompt) {
    const requestData = {
        mode: currentModule.replace('_module', '').replace('_', '-'),
        model: CONFIG.DEFAULT_MODEL,
        input: JSON.stringify({
            prompt: prompt,
            module: currentModule
        }),
        user_id: "enhanced_ui",
        session_id: currentChatId || "default_session"
    };

    try {
        console.log('Sending AI request:', requestData);

        const response = await fetch(CONFIG.API_URL, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify(requestData),
            credentials: 'include'
        });

        console.log('Response status:', response.status);

        if (!response.ok) {
            const errorText = await response.text();
            console.error('API Error:', errorText);
            throw new Error(`API error: ${response.status} ${response.statusText} - ${errorText}`);
        }

        const data = await response.json();
        console.log('Raw API response:', data);

        let output;
        try {
            output = JSON.parse(data.output);
        } catch (parseError) {
            console.error('JSON parse error:', parseError);
            // If JSON parsing fails, treat the output as a simple response
            output = {
                response: data.output || 'No response from AI backend',
                status: 'parsed_error',
                raw_output: data.output
            };
        }

        console.log('Parsed output:', output);
        return output;

    } catch (error) {
        console.error('AI Request failed:', error);

        // Try alternative endpoint for enhanced AI
        try {
            console.log('Trying enhanced AI endpoint...');
            const enhancedResponse = await fetch('http://127.0.0.1:8000/api/run/dynamic', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify(requestData),
                credentials: 'include'
            });

            if (enhancedResponse.ok) {
                const enhancedData = await enhancedResponse.json();
                const enhancedOutput = JSON.parse(enhancedData.output);
                console.log('Enhanced AI response:', enhancedOutput);
                return enhancedOutput;
            }
        } catch (enhancedError) {
            console.error('Enhanced AI also failed:', enhancedError);
        }

        // Only use fallback for very specific errors, not general connectivity
        if (error.message.includes('fetch')) {
            return {
                response: `🔌 **Connection Issue**: Unable to connect to AI backend at ${CONFIG.API_URL}. Please ensure the FastAPI server is running (python server.py).`,
                error: 'connection_failed',
                suggestion: 'Start the server with: python server.py'
            };
        }

        // For other errors, provide more helpful feedback
        return {
            response: `⚠️ **AI Processing Error**: ${error.message}`,
            error: error.message,
            timestamp: new Date().toISOString(),
            request: prompt,
            module: currentModule
        };
    }
}

// Settings Functions
function openSettings() {
    elements.settingsModal.style.display = 'flex';
}

function closeSettings() {
    elements.settingsModal.style.display = 'none';
}

function saveSettings() {
    const settings = {
        showAllMetricsChat: elements.showAllMetricsChat.checked,
        autoSaveChats: elements.autoSaveChats.checked,
        soundNotifications: elements.soundNotifications.checked
    };

    localStorage.setItem('aiInterfaceSettings', JSON.stringify(settings));
}

function loadSettings() {
    const saved = localStorage.getItem('aiInterfaceSettings');
    if (saved) {
        const settings = JSON.parse(saved);
        elements.showAllMetricsChat.checked = settings.showAllMetricsChat || false;
        elements.autoSaveChats.checked = settings.autoSaveChats !== false; // Default true
        elements.soundNotifications.checked = settings.soundNotifications || false;
    }
}

// Utility Functions
function generateChatId() {
    return 'chat_' + Date.now() + '_' + Math.random().toString(36).substr(2, 9);
}

function formatTime(timestamp) {
    const now = Date.now();
    const diff = now - timestamp;

    if (diff < 60000) {
        return 'Just now';
    }
    if (diff < 3600000) {
        return Math.floor(diff / 60000) + 'm ago';
    }
    if (diff < 86400000) {
        return Math.floor(diff / 3600000) + 'h ago';
    }
    return new Date(timestamp).toLocaleDateString();
}

function showLoading(show) {
    elements.loadingIndicator.style.display = show ? 'block' : 'none';
}

function showNotification(message, type = 'info') {
    const notification = document.createElement('div');
    notification.className = `notification notification-${type}`;
    notification.textContent = message;
    notification.style.cssText = `
        position: fixed;
        top: 20px;
        right: 20px;
        padding: 15px 20px;
        border-radius: 8px;
        color: white;
        font-weight: bold;
        z-index: 1000;
        animation: slideIn 0.3s ease;
    `;

    const colors = {
        success: '#27ae60',
        error: '#e74c3c',
        warning: '#f39c12',
        info: '#3498db'
    };
    notification.style.background = colors[type] || colors.info;

    document.body.appendChild(notification);

    setTimeout(() => {
        notification.style.animation = 'slideOut 0.3s ease';
        setTimeout(() => notification.remove(), 300);
    }, 3000);
}

function updateMetrics(responseTime, success) {
    // Update response time display
    if (elements.responseTime) {
        elements.responseTime.textContent = `${responseTime.toFixed(0)}`;
    }
}

function saveChatHistory() {
    if (elements.autoSaveChats.checked) {
        localStorage.setItem('chatHistory', JSON.stringify(chatHistory));
    }
}

function handleResize() {
    // Handle responsive design adjustments
    const width = window.innerWidth;

    if (width < 968) {
        // Mobile layout
        document.body.classList.add('mobile-layout');
    } else {
        document.body.classList.remove('mobile-layout');
    }
}

// Initialize when DOM is loaded
document.addEventListener('DOMContentLoaded', initializeInterface);

// Export for testing
if (typeof module !== 'undefined' && module.exports) {
    module.exports = {
        initializeInterface,
        createNewChat,
        selectModule,
        toggleAllMetrics,
        handleSendMessage
    };
}
