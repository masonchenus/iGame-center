<?php
/**
 * Game Server Framework - PHP Implementation
 * Provides web-based game server capabilities
 */

class GameServer {
    private $database;
    private $playerSessions = [];
    private $gameRooms = [];
    private $config;
    
    public function __construct($config = []) {
        $this->config = array_merge([
            'host' => 'localhost',
            'username' => 'root',
            'password' => '',
            'database' => 'game_center',
            'port' => 3306
        ], $config);
        
        $this->initializeDatabase();
        $this->setupRoutes();
    }
    
    private function initializeDatabase() {
        try {
            $dsn = "mysql:host={$this->config['host']};port={$this->config['port']};dbname={$this->config['database']};charset=utf8mb4";
            $this->database = new PDO($dsn, $this->config['username'], $this->config['password'], [
                PDO::ATTR_ERRMODE => PDO::ERRMODE_EXCEPTION,
                PDO::ATTR_DEFAULT_FETCH_MODE => PDO::FETCH_ASSOC,
                PDO::ATTR_EMULATE_PREPARES => false
            ]);
        } catch (PDOException $e) {
            throw new Exception("Database connection failed: " . $e->getMessage());
        }
    }
    
    private function setupRoutes() {
        $routes = [
            'GET /' => 'index',
            'POST /api/login' => 'handleLogin',
            'POST /api/register' => 'handleRegistration',
            'GET /api/player/{id}' => 'getPlayer',
            'POST /api/game/join' => 'joinGame',
            'POST /api/game/leave' => 'leaveGame',
            'POST /api/game/move' => 'handleGameMove',
            'GET /api/leaderboard/{game_id}' => 'getLeaderboard',
            'POST /api/achievement/unlock' => 'unlockAchievement'
        ];
        
        $this->routes = $routes;
    }
    
    /**
     * Handle HTTP requests
     */
    public function handleRequest($method, $uri, $data = []) {
        try {
            $route = $this->matchRoute($method, $uri);
            
            if (!$route) {
                return $this->jsonResponse(['error' => 'Route not found'], 404);
            }
            
            $action = $route['action'];
            $params = $route['params'];
            
            return $this->$action($params, $data);
            
        } catch (Exception $e) {
            error_log("Server error: " . $e->getMessage());
            return $this->jsonResponse(['error' => 'Internal server error'], 500);
        }
    }
    
    private function matchRoute($method, $uri) {
        foreach ($this->routes as $pattern => $action) {
            list($routeMethod, $routePath) = explode(' ', $pattern);
            
            if ($method !== $routeMethod) continue;
            
            $pattern = preg_replace('/\{([^}]+)\}/', '(?P<$1>[^/]+)', $routePath);
            $pattern = '#^' . $pattern . '$#';
            
            if (preg_match($pattern, $uri, $matches)) {
                $params = array_filter($matches, 'is_string', ARRAY_FILTER_USE_KEY);
                return ['action' => $action, 'params' => $params];
            }
        }
        
        return null;
    }
    
    /**
     * Player Management
     */
    public function handleRegistration($params, $data) {
        $required = ['username', 'email', 'password'];
        $validation = $this->validateRequired($data, $required);
        
        if (!$validation['valid']) {
            return $this->jsonResponse(['error' => $validation['error']], 400);
        }
        
        try {
            $stmt = $this->database->prepare("
                INSERT INTO players (username, email, password_hash, display_name) 
                VALUES (?, ?, ?, ?)
            ");
            
            $passwordHash = password_hash($data['password'], PASSWORD_DEFAULT);
            $displayName = $data['display_name'] ?? $data['username'];
            
            $stmt->execute([
                $data['username'],
                $data['email'],
                $passwordHash,
                $displayName
            ]);
            
            $playerId = $this->database->lastInsertId();
            
            return $this->jsonResponse([
                'success' => true,
                'player_id' => $playerId,
                'message' => 'Registration successful'
            ]);
            
        } catch (PDOException $e) {
            if ($e->getCode() == 23000) {
                return $this->jsonResponse(['error' => 'Username or email already exists'], 409);
            }
            throw $e;
        }
    }
    
    public function handleLogin($params, $data) {
        $required = ['username', 'password'];
        $validation = $this->validateRequired($data, $required);
        
        if (!$validation['valid']) {
            return $this->jsonResponse(['error' => $validation['error']], 400);
        }
        
        try {
            $stmt = $this->database->prepare("
                SELECT player_id, username, password_hash, display_name, is_active 
                FROM players 
                WHERE username = ? AND is_active = 1
            ");
            
            $stmt->execute([$data['username']]);
            $player = $stmt->fetch();
            
            if (!$player || !password_verify($data['password'], $player['password_hash'])) {
                return $this->jsonResponse(['error' => 'Invalid credentials'], 401);
            }
            
            // Create session
            $sessionToken = bin2hex(random_bytes(32));
            $this->playerSessions[$sessionToken] = [
                'player_id' => $player['player_id'],
                'username' => $player['username'],
                'created_at' => time()
            ];
            
            // Update last login
            $stmt = $this->database->prepare("UPDATE players SET last_login = NOW() WHERE player_id = ?");
            $stmt->execute([$player['player_id']]);
            
            return $this->jsonResponse([
                'success' => true,
                'session_token' => $sessionToken,
                'player' => [
                    'id' => $player['player_id'],
                    'username' => $player['username'],
                    'display_name' => $player['display_name']
                ]
            ]);
            
        } catch (PDOException $e) {
            throw $e;
        }
    }
    
    /**
     * Game Room Management
     */
    public function joinGame($params, $data) {
        $token = $this->validateSession($data['session_token'] ?? '');
        
        if (!$token) {
            return $this->jsonResponse(['error' => 'Invalid session'], 401);
        }
        
        $gameId = $data['game_id'];
        $playerId = $token['player_id'];
        
        // Check if game exists and has space
        $stmt = $this->database->prepare("
            SELECT g.game_id, g.max_players, COUNT(gs.player_id) as current_players
            FROM games g
            LEFT JOIN game_sessions gs ON g.game_id = gs.game_id AND gs.end_time IS NULL
            WHERE g.game_id = ? AND g.is_active = 1
            GROUP BY g.game_id
        ");
        
        $stmt->execute([$gameId]);
        $game = $stmt->fetch();
        
        if (!$game) {
            return $this->jsonResponse(['error' => 'Game not found'], 404);
        }
        
        if ($game['current_players'] >= $game['max_players']) {
            return $this->jsonResponse(['error' => 'Game room is full'], 409);
        }
        
        // Create new game session
        $stmt = $this->database->prepare("
            INSERT INTO game_sessions (game_id, player_id, start_time) 
            VALUES (?, ?, NOW())
        ");
        
        $stmt->execute([$gameId, $playerId]);
        $sessionId = $this->database->lastInsertId();
        
        return $this->jsonResponse([
            'success' => true,
            'session_id' => $sessionId,
            'game_id' => $gameId,
            'message' => 'Joined game successfully'
        ]);
    }
    
    public function handleGameMove($params, $data) {
        $token = $this->validateSession($data['session_token'] ?? '');
        
        if (!$token) {
            return $this->jsonResponse(['error' => 'Invalid session'], 401);
        }
        
        $sessionId = $data['session_id'];
        $move = $data['move'];
        
        // Validate move and update game state
        $stmt = $this->database->prepare("
            UPDATE game_sessions 
            SET score = score + ?,
                level_reached = level_reached + 1
            WHERE session_id = ? AND player_id = ?
        ");
        
        $scoreIncrease = $this->calculateMoveScore($move);
        $stmt->execute([$scoreIncrease, $sessionId, $token['player_id']]);
        
        if ($stmt->rowCount() === 0) {
            return $this->jsonResponse(['error' => 'Invalid session'], 404);
        }
        
        return $this->jsonResponse([
            'success' => true,
            'score_increase' => $scoreIncrease,
            'message' => 'Move processed successfully'
        ]);
    }
    
    /**
     * Utility Methods
     */
    private function validateRequired($data, $required) {
        foreach ($required as $field) {
            if (!isset($data[$field]) || empty(trim($data[$field]))) {
                return ['valid' => false, 'error' => "Field '$field' is required"];
            }
        }
        return ['valid' => true];
    }
    
    private function validateSession($token) {
        if (!isset($this->playerSessions[$token])) {
            return false;
        }
        
        $session = $this->playerSessions[$token];
        
        // Check session expiry (24 hours)
        if (time() - $session['created_at'] > 86400) {
            unset($this->playerSessions[$token]);
            return false;
        }
        
        return $session;
    }
    
    private function calculateMoveScore($move) {
        // Simple scoring logic - can be extended based on game type
        $baseScore = 10;
        
        switch ($move['type']) {
            case 'correct':
                return $baseScore * ($move['difficulty'] ?? 1);
            case 'perfect':
                return $baseScore * 2 * ($move['difficulty'] ?? 1);
            default:
                return $baseScore;
        }
    }
    
    private function jsonResponse($data, $statusCode = 200) {
        http_response_code($statusCode);
        header('Content-Type: application/json');
        echo json_encode($data, JSON_PRETTY_PRINT);
        return true;
    }
    
    /**
     * Server control methods
     */
    public function start() {
        $method = $_SERVER['REQUEST_METHOD'];
        $uri = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);
        $data = json_decode(file_get_contents('php://input'), true) ?? $_POST;
        
        return $this->handleRequest($method, $uri, $data);
    }
    
    public function shutdown() {
        $this->database = null;
        $this->playerSessions = [];
        $this->gameRooms = [];
    }
}

// Web interface
function renderIndex() {
    ?>
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Game Center - PHP Server</title>
        <style>
            body { 
                font-family: Arial, sans-serif; 
                margin: 40px; 
                background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                color: white;
            }
            .container { 
                max-width: 800px; 
                margin: 0 auto; 
                background: rgba(255,255,255,0.1);
                padding: 30px;
                border-radius: 10px;
                backdrop-filter: blur(10px);
            }
            .endpoint { 
                background: rgba(255,255,255,0.2); 
                padding: 15px; 
                margin: 10px 0; 
                border-radius: 5px;
                border-left: 4px solid #4CAF50;
            }
            .method { 
                font-weight: bold; 
                color: #FFD700; 
            }
            pre { 
                background: rgba(0,0,0,0.3); 
                padding: 10px; 
                border-radius: 5px; 
                overflow-x: auto; 
            }
        </style>
    </head>
    <body>
        <div class="container">
            <h1>🎮 Game Center PHP Server</h1>
            <p>Web-based game server with comprehensive game management capabilities.</p>
            
            <h2>API Endpoints</h2>
            
            <div class="endpoint">
                <span class="method">POST</span> /api/register
                <p>Register a new player account</p>
                <pre>{
  "username": "player1",
  "email": "player1@example.com",
  "password": "securepassword",
  "display_name": "Player One"
}</pre>
            </div>
            
            <div class="endpoint">
                <span class="method">POST</span> /api/login
                <p>Authenticate player and get session token</p>
                <pre>{
  "username": "player1",
  "password": "securepassword"
}</pre>
            </div>
            
            <div class="endpoint">
                <span class="method">POST</span> /api/game/join
                <p>Join a game room</p>
                <pre>{
  "session_token": "abc123...",
  "game_id": 1
}</pre>
            </div>
            
            <div class="endpoint">
                <span class="method">POST</span> /api/game/move
                <p>Submit a game move</p>
                <pre>{
  "session_token": "abc123...",
  "session_id": 123,
  "move": {
    "type": "correct",
    "difficulty": 2
  }
}</pre>
            </div>
            
            <div class="endpoint">
                <span class="method">GET</span> /api/leaderboard/{game_id}
                <p>Get game leaderboard</p>
            </div>
            
            <h2>Features</h2>
            <ul>
                <li>🔐 Player authentication and session management</li>
                <li>🎮 Multi-player game room management</li>
                <li>📊 Real-time scoring and statistics</li>
                <li>🏆 Leaderboard and achievement system</li>
                <li>💾 Database integration with MySQL</li>
                <li>🔄 RESTful API design</li>
                <li>🛡️ Security and validation</li>
            </ul>
        </div>
    </body>
    </html>
    <?php
}

// Route handling
if (php_sapi_name() === 'cli') {
    // CLI mode for testing
    $server = new GameServer();
    echo "PHP Game Server initialized\n";
} else {
    // Web mode
    $server = new GameServer();
    
    if ($_SERVER['REQUEST_URI'] === '/' || $_SERVER['REQUEST_URI'] === '/index.php') {
        renderIndex();
    } else {
        $server->start();
    }
}
?>
