<?php
/**
 * Web-based Tic-Tac-Toe Game
 * PHP implementation with HTML/CSS/JavaScript
 */

class TicTacToeGame {
    private $board;
    private $currentPlayer;
    private $gameEnded;
    private $winner;
    private $scores;
    
    public function __construct() {
        $this->initializeGame();
    }
    
    private function initializeGame() {
        $this->board = array_fill(0, 9, '');
        $this->currentPlayer = 'X';
        $this->gameEnded = false;
        $this->winner = '';
        $this->scores = ['X' => 0, 'O' => 0, 'Draws' => 0];
    }
    
    /**
     * Make a move on the board
     */
    public function makeMove($position) {
        if ($this->gameEnded || !is_numeric($position) || $position < 0 || $position > 8) {
            return ['valid' => false, 'message' => 'Invalid move'];
        }
        
        if ($this->board[$position] !== '') {
            return ['valid' => false, 'message' => 'Position already taken'];
        }
        
        $this->board[$position] = $this->currentPlayer;
        
        // Check for winner
        if ($this->checkWinner()) {
            $this->gameEnded = true;
            $this->winner = $this->currentPlayer;
            $this->scores[$this->winner]++;
            return [
                'valid' => true, 
                'gameEnded' => true, 
                'winner' => $this->winner,
                'message' => "Player {$this->winner} wins!"
            ];
        }
        
        // Check for draw
        if ($this->isBoardFull()) {
            $this->gameEnded = true;
            $this->scores['Draws']++;
            return [
                'valid' => true, 
                'gameEnded' => true, 
                'winner' => 'Draw',
                'message' => "It's a draw!"
            ];
        }
        
        // Switch player
        $this->currentPlayer = ($this->currentPlayer === 'X') ? 'O' : 'X';
        
        return ['valid' => true, 'gameEnded' => false, 'currentPlayer' => $this->currentPlayer];
    }
    
    /**
     * Check if there's a winner
     */
    private function checkWinner() {
        $lines = [
            [0, 1, 2], [3, 4, 5], [6, 7, 8], // Rows
            [0, 3, 6], [1, 4, 7], [2, 5, 8], // Columns
            [0, 4, 8], [2, 4, 6] // Diagonals
        ];
        
        foreach ($lines as $line) {
            if ($this->board[$line[0]] !== '' && 
                $this->board[$line[0]] === $this->board[$line[1]] && 
                $this->board[$line[1]] === $this->board[$line[2]]) {
                return true;
            }
        }
        
        return false;
    }
    
    /**
     * Check if board is full
     */
    private function isBoardFull() {
        return !in_array('', $this->board);
    }
    
    /**
     * Reset the game
     */
    public function resetGame() {
        $this->initializeGame();
        return ['message' => 'Game reset', 'currentPlayer' => $this->currentPlayer];
    }
    
    /**
     * Get current game state
     */
    public function getGameState() {
        return [
            'board' => $this->board,
            'currentPlayer' => $this->currentPlayer,
            'gameEnded' => $this->gameEnded,
            'winner' => $this->winner,
            'scores' => $this->scores
        ];
    }
    
    /**
     * Get best move for AI (simple AI using minimax)
     */
    public function getBestMove() {
        $bestScore = -1000;
        $bestMove = -1;
        
        for ($i = 0; $i < 9; $i++) {
            if ($this->board[$i] === '') {
                $this->board[$i] = 'O'; // AI is O
                $score = $this->minimax($this->board, false, 0);
                $this->board[$i] = '';
                
                if ($score > $bestScore) {
                    $bestScore = $score;
                    $bestMove = $i;
                }
            }
        }
        
        return $bestMove;
    }
    
    /**
     * Minimax algorithm for AI
     */
    private function minimax($board, $isMaximizing, $depth) {
        if ($this->checkWinForPlayer($board, 'O')) return 10 - $depth;
        if ($this->checkWinForPlayer($board, 'X')) return $depth - 10;
        if (!$this->hasEmptyCells($board)) return 0;
        
        if ($isMaximizing) {
            $bestScore = -1000;
            for ($i = 0; $i < 9; $i++) {
                if ($board[$i] === '') {
                    $board[$i] = 'O';
                    $score = $this->minimax($board, false, $depth + 1);
                    $board[$i] = '';
                    $bestScore = max($score, $bestScore);
                }
            }
            return $bestScore;
        } else {
            $bestScore = 1000;
            for ($i = 0; $i < 9; $i++) {
                if ($board[$i] === '') {
                    $board[$i] = 'X';
                    $score = $this->minimax($board, true, $depth + 1);
                    $board[$i] = '';
                    $bestScore = min($score, $bestScore);
                }
            }
            return $bestScore;
        }
    }
    
    private function checkWinForPlayer($board, $player) {
        $lines = [
            [0, 1, 2], [3, 4, 5], [6, 7, 8],
            [0, 3, 6], [1, 4, 7], [2, 5, 8],
            [0, 4, 8], [2, 4, 6]
        ];
        
        foreach ($lines as $line) {
            if ($board[$line[0]] === $player && 
                $board[$line[1]] === $player && 
                $board[$line[2]] === $player) {
                return true;
            }
        }
        
        return false;
    }
    
    private function hasEmptyCells($board) {
        return in_array('', $board);
    }
}

// Handle game logic
$game = new TicTacToeGame();
$message = '';
$gameState = $game->getGameState();

// Handle AJAX requests
if ($_SERVER['REQUEST_METHOD'] === 'POST') {
    $input = json_decode(file_get_contents('php://input'), true);
    
    if (isset($input['action'])) {
        switch ($input['action']) {
            case 'move':
                $position = $input['position'];
                $result = $game->makeMove($position);
                echo json_encode($result);
                exit;
                
            case 'reset':
                $result = $game->resetGame();
                echo json_encode($result);
                exit;
                
            case 'ai_move':
                $bestMove = $game->getBestMove();
                $result = $game->makeMove($bestMove);
                echo json_encode($result);
                exit;
        }
    }
}

$gameState = $game->getGameState();
?>

<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Tic-Tac-Toe - PHP Game</title>
    <style>
        body {
            font-family: 'Arial', sans-serif;
            margin: 0;
            padding: 20px;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            min-height: 100vh;
            display: flex;
            flex-direction: column;
            align-items: center;
            justify-content: center;
        }
        
        .container {
            background: rgba(255, 255, 255, 0.1);
            backdrop-filter: blur(10px);
            border-radius: 20px;
            padding: 30px;
            box-shadow: 0 8px 32px 0 rgba(31, 38, 135, 0.37);
            text-align: center;
        }
        
        h1 {
            color: white;
            margin-bottom: 20px;
            text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
        }
        
        .game-board {
            display: grid;
            grid-template-columns: repeat(3, 100px);
            grid-template-rows: repeat(3, 100px);
            gap: 2px;
            background: #333;
            border: 3px solid #555;
            border-radius: 10px;
            margin: 20px auto;
            box-shadow: 0 4px 8px rgba(0,0,0,0.2);
        }
        
        .cell {
            background: linear-gradient(145deg, #f0f0f0, #cacaca);
            border: none;
            font-size: 2em;
            font-weight: bold;
            color: #333;
            cursor: pointer;
            display: flex;
            align-items: center;
            justify-content: center;
            transition: all 0.3s ease;
            border-radius: 8px;
        }
        
        .cell:hover {
            background: linear-gradient(145deg, #cacaca, #f0f0f0);
            transform: scale(1.05);
        }
        
        .cell:disabled {
            cursor: not-allowed;
            opacity: 0.7;
        }
        
        .cell.x { color: #e74c3c; }
        .cell.o { color: #3498db; }
        
        .game-info {
            color: white;
            margin: 20px 0;
        }
        
        .current-player {
            font-size: 1.2em;
            font-weight: bold;
            margin: 10px 0;
        }
        
        .message {
            font-size: 1.1em;
            margin: 15px 0;
            padding: 10px;
            border-radius: 5px;
            background: rgba(255, 255, 255, 0.2);
        }
        
        .scores {
            display: flex;
            justify-content: space-around;
            margin: 20px 0;
            background: rgba(255, 255, 255, 0.1);
            padding: 15px;
            border-radius: 10px;
        }
        
        .score-item {
            color: white;
            font-weight: bold;
        }
        
        .controls {
            margin: 20px 0;
        }
        
        .btn {
            background: linear-gradient(145deg, #4CAF50, #45a049);
            border: none;
            color: white;
            padding: 12px 24px;
            font-size: 1em;
            font-weight: bold;
            border-radius: 8px;
            cursor: pointer;
            margin: 0 10px;
            transition: all 0.3s ease;
            box-shadow: 0 4px 8px rgba(0,0,0,0.2);
        }
        
        .btn:hover {
            transform: translateY(-2px);
            box-shadow: 0 6px 12px rgba(0,0,0,0.3);
        }
        
        .btn:active {
            transform: translateY(0);
        }
        
        .game-mode {
            margin: 20px 0;
        }
        
        .mode-btn {
            background: linear-gradient(145deg, #9b59b6, #8e44ad);
            border: none;
            color: white;
            padding: 10px 20px;
            font-size: 0.9em;
            border-radius: 6px;
            cursor: pointer;
            margin: 0 5px;
            transition: all 0.3s ease;
        }
        
        .mode-btn.active {
            background: linear-gradient(145deg, #e67e22, #d35400);
        }
        
        .winner {
            color: #f1c40f;
            font-size: 1.3em;
            font-weight: bold;
            text-shadow: 2px 2px 4px rgba(0,0,0,0.5);
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>🎮 Tic-Tac-Toe</h1>
        
        <div class="game-mode">
            <button class="mode-btn active" onclick="setGameMode('pvp')">Player vs Player</button>
            <button class="mode-btn" onclick="setGameMode('ai')">Player vs AI</button>
        </div>
        
        <div class="scores">
            <div class="score-item">X Wins: <span id="scoreX">0</span></div>
            <div class="score-item">O Wins: <span id="scoreO">0</span></div>
            <div class="score-item">Draws: <span id="scoreDraws">0</span></div>
        </div>
        
        <div class="game-info">
            <div class="current-player" id="currentPlayer">Current Player: X</div>
            <div class="message" id="message"></div>
        </div>
        
        <div class="game-board">
            <?php for ($i = 0; $i < 9; $i++): ?>
                <button class="cell" onclick="makeMove(<?php echo $i; ?>)" id="cell<?php echo $i; ?>">
                    <?php echo $gameState['board'][$i]; ?>
                </button>
            <?php endfor; ?>
        </div>
        
        <div class="controls">
            <button class="btn" onclick="resetGame()">New Game</button>
            <button class="btn" onclick="makeAIMove()">AI Move</button>
        </div>
    </div>

    <script>
        let gameMode = 'pvp';
        let gameEnded = <?php echo json_encode($gameState['gameEnded']); ?>;
        let currentPlayer = '<?php echo $gameState['currentPlayer']; ?>';
        
        function updateGameState(state) {
            gameEnded = state.gameEnded;
            currentPlayer = state.currentPlayer || currentPlayer;
            
            // Update UI
            document.getElementById('currentPlayer').textContent = 
                gameEnded && state.winner !== 'Draw' ? 
                `Winner: ${state.winner}` : 
                `Current Player: ${currentPlayer}`;
            
            document.getElementById('message').textContent = state.message || '';
            
            if (state.winner) {
                document.getElementById('message').className = 'message winner';
            } else {
                document.getElementById('message').className = 'message';
            }
            
            // Update scores
            if (state.scores) {
                document.getElementById('scoreX').textContent = state.scores.X || 0;
                document.getElementById('scoreO').textContent = state.scores.O || 0;
                document.getElementById('scoreDraws').textContent = state.scores.Draws || 0;
            }
            
            // Disable cells if game ended
            if (gameEnded) {
                for (let i = 0; i < 9; i++) {
                    document.getElementById('cell' + i).disabled = true;
                }
            }
        }
        
        function makeMove(position) {
            if (gameEnded) return;
            
            fetch('', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({
                    action: 'move',
                    position: position
                })
            })
            .then(response => response.json())
            .then(data => {
                if (data.valid) {
                    // Update UI
                    const cell = document.getElementById('cell' + position);
                    cell.textContent = currentPlayer;
                    cell.classList.add(currentPlayer.toLowerCase());
                    cell.disabled = true;
                    
                    updateGameState(data);
                    
                    // Make AI move if in AI mode and game not ended
                    if (gameMode === 'ai' && !gameEnded && currentPlayer === 'O') {
                        setTimeout(makeAIMove, 500);
                    }
                } else {
                    alert(data.message);
                }
            })
            .catch(error => {
                console.error('Error:', error);
            });
        }
        
        function makeAIMove() {
            if (gameEnded || gameMode !== 'ai' || currentPlayer !== 'O') return;
            
            fetch('', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({
                    action: 'ai_move'
                })
            })
            .then(response => response.json())
            .then(data => {
                if (data.valid) {
                    // AI makes the best move (already handled by makeMove logic)
                    console.log('AI move completed');
                }
            })
            .catch(error => {
                console.error('Error:', error);
            });
        }
        
        function resetGame() {
            fetch('', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({
                    action: 'reset'
                })
            })
            .then(response => response.json())
            .then(data => {
                // Reset UI
                for (let i = 0; i < 9; i++) {
                    const cell = document.getElementById('cell' + i);
                    cell.textContent = '';
                    cell.className = 'cell';
                    cell.disabled = false;
                }
                
                updateGameState(data);
            })
            .catch(error => {
                console.error('Error:', error);
            });
        }
        
        function setGameMode(mode) {
            gameMode = mode;
            
            // Update button styles
            document.querySelectorAll('.mode-btn').forEach(btn => {
                btn.classList.remove('active');
            });
            event.target.classList.add('active');
            
            // Reset game when changing mode
            resetGame();
        }
        
        // Initialize game state
        updateGameState(<?php echo json_encode($gameState); ?>);
    </script>
</body>
</html>
