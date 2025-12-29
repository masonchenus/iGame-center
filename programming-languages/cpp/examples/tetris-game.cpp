/*
 * Tetris Game - C++ Implementation
 * Classic falling blocks puzzle game
 */

#include <iostream>
#include <vector>
#include <chrono>
#include <thread>
#include <random>
#include <algorithm>

class TetrisGame {
private:
    static const int BOARD_WIDTH = 10;
    static const int BOARD_HEIGHT = 20;
    static const int BLOCK_SIZE = 1;
    
    enum class PieceType {
        I, O, T, S, Z, J, L
    };
    
    struct Piece {
        std::vector<std::vector<int>> shape;
        int x, y;
        PieceType type;
    };
    
    std::vector<std::vector<int>> board;
    Piece currentPiece;
    Piece nextPiece;
    int score;
    int level;
    int linesCleared;
    bool gameOver;
    std::chrono::steady_clock::time_point lastUpdate;
    int dropTimer;
    
public:
    TetrisGame() : score(0), level(1), linesCleared(0), gameOver(false), dropTimer(0) {
        initializeBoard();
        generateNewPiece();
        lastUpdate = std::chrono::steady_clock::now();
    }
    
    void initializeBoard() {
        board = std::vector<std::vector<int>>(BOARD_HEIGHT, std::vector<int>(BOARD_WIDTH, 0));
    }
    
    void generateNewPiece() {
        std::mt19937 rng(std::random_device{}());
        std::uniform_int_distribution<int> dist(0, 6);
        
        int pieceType = dist(rng);
        currentPiece = createPiece(static_cast<PieceType>(pieceType));
        nextPiece = createPiece(static_cast<PieceType>(dist(rng)));
    }
    
    Piece createPiece(PieceType type) {
        Piece piece;
        piece.type = type;
        
        switch (type) {
            case PieceType::I:
                piece.shape = {{1, 1, 1, 1}};
                break;
            case PieceType::O:
                piece.shape = {{1, 1}, {1, 1}};
                break;
            case PieceType::T:
                piece.shape = {{0, 1, 0}, {1, 1, 1}};
                break;
            case PieceType::S:
                piece.shape = {{0, 1, 1}, {1, 1, 0}};
                break;
            case PieceType::Z:
                piece.shape = {{1, 1, 0}, {0, 1, 1}};
                break;
            case PieceType::J:
                piece.shape = {{1, 0, 0}, {1, 1, 1}};
                break;
            case PieceType::L:
                piece.shape = {{0, 0, 1}, {1, 1, 1}};
                break;
        }
        
        piece.x = BOARD_WIDTH / 2 - piece.shape[0].size() / 2;
        piece.y = 0;
        
        return piece;
    }
    
    void update() {
        auto now = std::chrono::steady_clock::now();
        auto deltaTime = std::chrono::duration_cast<std::chrono::milliseconds>(now - lastUpdate);
        
        if (deltaTime.count() < 100) return; // 10 FPS
        lastUpdate = now;
        
        if (gameOver) return;
        
        dropTimer += deltaTime.count();
        int dropSpeed = std::max(1000 - (level - 1) * 100, 100); // Speed increases with level
        
        if (dropTimer >= dropSpeed) {
            movePieceDown();
            dropTimer = 0;
        }
    }
    
    void movePieceDown() {
        if (canMovePiece(currentPiece.x, currentPiece.y + 1, currentPiece.shape)) {
            currentPiece.y++;
        } else {
            // Piece can't move down, place it on board
            placePiece();
            clearLines();
            generateNewPiece();
            
            // Check game over
            if (!canMovePiece(currentPiece.x, currentPiece.y, currentPiece.shape)) {
                gameOver = true;
            }
        }
    }
    
    bool canMovePiece(int x, int y, const std::vector<std::vector<int>>& shape) {
        for (size_t i = 0; i < shape.size(); i++) {
            for (size_t j = 0; j < shape[i].size(); j++) {
                if (shape[i][j]) {
                    int newX = x + j;
                    int newY = y + i;
                    
                    if (newX < 0 || newX >= BOARD_WIDTH || 
                        newY >= BOARD_HEIGHT || 
                        (newY >= 0 && board[newY][newX])) {
                        return false;
                    }
                }
            }
        }
        return true;
    }
    
    void placePiece() {
        for (size_t i = 0; i < currentPiece.shape.size(); i++) {
            for (size_t j = 0; j < currentPiece.shape[i].size(); j++) {
                if (currentPiece.shape[i][j]) {
                    int x = currentPiece.x + j;
                    int y = currentPiece.y + i;
                    if (y >= 0 && y < BOARD_HEIGHT && x >= 0 && x < BOARD_WIDTH) {
                        board[y][x] = 1;
                    }
                }
            }
        }
    }
    
    void rotatePiece() {
        std::vector<std::vector<int>> rotated = currentPiece.shape;
        // Simple rotation (clockwise 90 degrees)
        int rows = rotated.size();
        int cols = rotated[0].size();
        
        std::vector<std::vector<int>> newShape(cols, std::vector<int>(rows));
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                newShape[j][rows - 1 - i] = rotated[i][j];
            }
        }
        
        if (canMovePiece(currentPiece.x, currentPiece.y, newShape)) {
            currentPiece.shape = newShape;
        }
    }
    
    void clearLines() {
        int linesClearedThisMove = 0;
        
        for (int i = BOARD_HEIGHT - 1; i >= 0; i--) {
            bool lineFull = true;
            for (int j = 0; j < BOARD_WIDTH; j++) {
                if (!board[i][j]) {
                    lineFull = false;
                    break;
                }
            }
            
            if (lineFull) {
                // Remove the line and add empty line at top
                board.erase(board.begin() + i);
                board.insert(board.begin(), std::vector<int>(BOARD_WIDTH, 0));
                linesClearedThisMove++;
                i++; // Check same line again
            }
        }
        
        if (linesClearedThisMove > 0) {
            linesCleared += linesClearedThisMove;
            
            // Calculate score
            int lineScore[] = {0, 100, 300, 500, 800};
            score += lineScore[linesClearedThisMove] * level;
            
            // Increase level every 10 lines
            level = linesCleared / 10 + 1;
        }
    }
    
    void movePiece(int direction) {
        if (gameOver) return;
        
        int newX = currentPiece.x + direction;
        if (canMovePiece(newX, currentPiece.y, currentPiece.shape)) {
            currentPiece.x = newX;
        }
    }
    
    void dropPiece() {
        while (canMovePiece(currentPiece.x, currentPiece.y + 1, currentPiece.shape)) {
            currentPiece.y++;
        }
        placePiece();
        clearLines();
        generateNewPiece();
        
        if (!canMovePiece(currentPiece.x, currentPiece.y, currentPiece.shape)) {
            gameOver = true;
        }
    }
    
    void render() {
        // Clear screen
        std::cout << "\033[2J\033[H";
        
        // Print score and level
        std::cout << "TETRIS" << std::endl;
        std::cout << "Score: " << score << " | Level: " << level << " | Lines: " << linesCleared << std::endl;
        std::cout << std::string(BOARD_WIDTH + 2, '=') << std::endl;
        
        // Create display board
        std::vector<std::string> display(BOARD_HEIGHT, std::string(BOARD_WIDTH, ' '));
        
        // Add placed pieces
        for (int i = 0; i < BOARD_HEIGHT; i++) {
            for (int j = 0; j < BOARD_WIDTH; j++) {
                if (board[i][j]) {
                    display[i][j] = '#';
                }
            }
        }
        
        // Add current piece
        for (size_t i = 0; i < currentPiece.shape.size(); i++) {
            for (size_t j = 0; j < currentPiece.shape[i].size(); j++) {
                if (currentPiece.shape[i][j]) {
                    int x = currentPiece.x + j;
                    int y = currentPiece.y + i;
                    if (y >= 0 && y < BOARD_HEIGHT && x >= 0 && x < BOARD_WIDTH) {
                        display[y][x] = '*';
                    }
                }
            }
        }
        
        // Print board
        for (const auto& line : display) {
            std::cout << "|" << line << "|" << std::endl;
        }
        
        std::cout << std::string(BOARD_WIDTH + 2, '=') << std::endl;
        
        if (gameOver) {
            std::cout << "GAME OVER!" << std::endl;
            std::cout << "Final Score: " << score << std::endl;
        }
        
        std::cout << "Controls: A/D to move, S to drop faster, W to rotate, Q to quit" << std::endl;
    }
    
    bool isGameOver() const {
        return gameOver;
    }
};

int main() {
    TetrisGame game;
    char input;
    
    std::cout << "=== TETRIS ===" << std::endl;
    std::cout << "Classic falling blocks puzzle game" << std::endl;
    std::cout << "Controls: A/D to move, S to drop faster, W to rotate" << std::endl;
    std::cout << "Press ENTER to start..." << std::endl;
    
    std::cin.get();
    
    while (!game.isGameOver()) {
        game.render();
        
        // Non-blocking input check
        if (std::cin.peek() != EOF) {
            input = std::cin.get();
            
            switch (input) {
                case 'a':
                case 'A':
                    game.movePiece(-1);
                    break;
                case 'd':
                case 'D':
                    game.movePiece(1);
                    break;
                case 's':
                case 'S':
                    game.movePieceDown();
                    break;
                case 'w':
                case 'W':
                    game.rotatePiece();
                    break;
                case 'q':
                case 'Q':
                    return 0;
            }
        }
        
        // Update game logic
        game.update();
        
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }
    
    std::cout << "Thanks for playing!" << std::endl;
    
    return 0;
}
