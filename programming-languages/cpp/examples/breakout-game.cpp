/*
 * Breakout Game - C++ Implementation
 * Classic brick-breaking arcade game
 */

#include <iostream>
#include <vector>
#include <chrono>
#include <thread>
#include <random>
#include <cmath>

class BreakoutGame {
private:
    static const int SCREEN_WIDTH = 80;
    static const int SCREEN_HEIGHT = 30;
    static const int PADDLE_WIDTH = 10;
    static const int BALL_SPEED = 3;
    
    struct Paddle {
        int x, y;
        int width;
        int height;
    };
    
    struct Ball {
        int x, y;
        int velocity_x, velocity_y;
        int radius;
    };
    
    struct Brick {
        int x, y;
        int width, height;
        int points;
        bool active;
    };
    
    Paddle paddle;
    Ball ball;
    std::vector<Brick> bricks;
    int score;
    int lives;
    int level;
    bool gameRunning;
    std::chrono::steady_clock::time_point lastUpdate;
    
public:
    BreakoutGame() : score(0), lives(3), level(1), gameRunning(true) {
        initializeGame();
    }
    
    void initializeGame() {
        // Initialize paddle
        paddle.x = SCREEN_WIDTH / 2 - PADDLE_WIDTH / 2;
        paddle.y = SCREEN_HEIGHT - 5;
        paddle.width = PADDLE_WIDTH;
        paddle.height = 2;
        
        // Initialize ball
        resetBall();
        
        // Initialize bricks
        initializeBricks();
        
        lastUpdate = std::chrono::steady_clock::now();
    }
    
    void resetBall() {
        ball.x = SCREEN_WIDTH / 2;
        ball.y = SCREEN_HEIGHT - 8;
        ball.velocity_x = BALL_SPEED;
        ball.velocity_y = -BALL_SPEED;
        ball.radius = 1;
    }
    
    void initializeBricks() {
        bricks.clear();
        
        int brickWidth = 6;
        int brickHeight = 2;
        int numCols = SCREEN_WIDTH / (brickWidth + 1);
        int numRows = 5;
        
        for (int row = 0; row < numRows; row++) {
            for (int col = 0; col < numCols; col++) {
                Brick brick;
                brick.x = col * (brickWidth + 1) + 2;
                brick.y = row * (brickHeight + 1) + 3;
                brick.width = brickWidth;
                brick.height = brickHeight;
                brick.points = (numRows - row) * 10;
                brick.active = true;
                bricks.push_back(brick);
            }
        }
    }
    
    void update() {
        auto now = std::chrono::steady_clock::now();
        auto deltaTime = std::chrono::duration_cast<std::chrono::milliseconds>(now - lastUpdate);
        
        if (deltaTime.count() < 50) return; // 20 FPS
        lastUpdate = now;
        
        if (!gameRunning) return;
        
        updateBall();
        checkCollisions();
        checkLevelComplete();
    }
    
    void updateBall() {
        ball.x += ball.velocity_x;
        ball.y += ball.velocity_y;
        
        // Bounce off walls
        if (ball.x <= 1 || ball.x >= SCREEN_WIDTH - 1) {
            ball.velocity_x *= -1;
        }
        
        if (ball.y <= 2) { // Don't bounce off top border (score area)
            ball.velocity_y *= -1;
        }
        
        // Ball fell below paddle
        if (ball.y >= SCREEN_HEIGHT) {
            lives--;
            if (lives <= 0) {
                gameRunning = false;
            } else {
                resetBall();
            }
        }
    }
    
    void checkCollisions() {
        // Ball vs paddle
        if (ball.x >= paddle.x && ball.x < paddle.x + paddle.width &&
            ball.y >= paddle.y && ball.y < paddle.y + paddle.height) {
            
            // Calculate bounce angle based on where ball hit paddle
            float hitPos = (ball.x - paddle.x) / static_cast<float>(paddle.width);
            float angle = (hitPos - 0.5f) * M_PI / 3; // Max 60 degree angle
            
            ball.velocity_x = static_cast<int>(BALL_SPEED * std::sin(angle));
            ball.velocity_y = -static_cast<int>(BALL_SPEED * std::cos(angle));
        }
        
        // Ball vs bricks
        for (auto& brick : bricks) {
            if (!brick.active) continue;
            
            // Simple AABB collision detection
            if (ball.x >= brick.x && ball.x < brick.x + brick.width &&
                ball.y >= brick.y && ball.y < brick.y + brick.height) {
                
                brick.active = false;
                score += brick.points;
                
                // Determine bounce direction
                float ballCenterX = ball.x + 0.5f;
                float ballCenterY = ball.y + 0.5f;
                float brickCenterX = brick.x + brick.width / 2.0f;
                float brickCenterY = brick.y + brick.height / 2.0f;
                
                float dx = ballCenterX - brickCenterX;
                float dy = ballCenterY - brickCenterY;
                
                if (std::abs(dx) > std::abs(dy)) {
                    ball.velocity_x *= -1;
                } else {
                    ball.velocity_y *= -1;
                }
                
                break; // Only hit one brick per frame
            }
        }
    }
    
    void checkLevelComplete() {
        bool anyActive = false;
        for (const auto& brick : bricks) {
            if (brick.active) {
                anyActive = true;
                break;
            }
        }
        
        if (!anyActive) {
            level++;
            score += 100 * level;
            
            // Increase difficulty
            ball.velocity_x *= 1.2f;
            ball.velocity_y *= 1.2f;
            
            initializeBricks();
            resetBall();
        }
    }
    
    void movePaddle(int direction) {
        int newX = paddle.x + direction;
        if (newX >= 1 && newX + paddle.width < SCREEN_WIDTH - 1) {
            paddle.x = newX;
        }
    }
    
    void render() {
        // Clear screen
        std::cout << "\033[2J\033[H";
        
        // Print score and info
        std::cout << "BREAKOUT" << std::endl;
        std::cout << "Score: " << score << " | Lives: " << lives << " | Level: " << level << std::endl;
        std::cout << std::string(SCREEN_WIDTH, '=') << std::endl;
        
        // Create game screen
        std::vector<std::string> screen(SCREEN_HEIGHT, std::string(SCREEN_WIDTH, ' '));
        
        // Add bricks
        for (const auto& brick : bricks) {
            if (!brick.active) continue;
            
            for (int y = 0; y < brick.height && brick.y + y < SCREEN_HEIGHT; y++) {
                for (int x = 0; x < brick.width && brick.x + x < SCREEN_WIDTH; x++) {
                    screen[brick.y + y][brick.x + x] = '=';
                }
            }
        }
        
        // Add paddle
        for (int x = 0; x < paddle.width && paddle.x + x < SCREEN_WIDTH; x++) {
            for (int y = 0; y < paddle.height && paddle.y + y < SCREEN_HEIGHT; y++) {
                screen[paddle.y + y][paddle.x + x] = '-';
            }
        }
        
        // Add ball
        screen[ball.y][ball.x] = 'O';
        
        // Print screen
        for (const auto& line : screen) {
            std::cout << line << std::endl;
        }
        
        std::cout << std::string(SCREEN_WIDTH, '=') << std::endl;
        std::cout << "Controls: A/D to move paddle, Q to quit" << std::endl;
        
        if (!gameRunning) {
            std::cout << "GAME OVER! Final Score: " << score << std::endl;
        }
    }
    
    bool isGameRunning() const {
        return gameRunning;
    }
    
    void quitGame() {
        gameRunning = false;
    }
};

int main() {
    BreakoutGame game;
    char input;
    
    std::cout << "=== BREAKOUT ===" << std::endl;
    std::cout << "Classic brick-breaking arcade game" << std::endl;
    std::cout << "Controls: A/D to move paddle" << std::endl;
    std::cout << "Press ENTER to start..." << std::endl;
    
    std::cin.get();
    
    while (game.isGameRunning()) {
        game.render();
        
        // Non-blocking input check
        if (std::cin.peek() != EOF) {
            input = std::cin.get();
            
            switch (input) {
                case 'a':
                case 'A':
                    game.movePaddle(-2);
                    break;
                case 'd':
                case 'D':
                    game.movePaddle(2);
                    break;
                case 'q':
                case 'Q':
                    game.quitGame();
                    break;
            }
        }
        
        // Update game logic
        game.update();
        
        std::this_thread::sleep_for(std::chrono::milliseconds(50));
    }
    
    std::cout << "Thanks for playing!" << std::endl;
    
    return 0;
}
