/*
 * Pong Game - C++ Implementation
 * Classic Pong game demonstrating 2D graphics and collision detection
 */

#include <iostream>
#include <vector>
#include <chrono>
#include <thread>
#include <random>

class PongGame {
private:
    static const int SCREEN_WIDTH = 80;
    static const int SCREEN_HEIGHT = 25;
    static const int PADDLE_HEIGHT = 6;
    static const int BALL_SIZE = 1;
    
    struct Paddle {
        int x, y;
        int height;
        int score;
    };
    
    struct Ball {
        int x, y;
        int velocity_x, velocity_y;
        int size;
    };
    
    Paddle leftPaddle, rightPaddle;
    Ball ball;
    bool gameRunning;
    std::chrono::steady_clock::time_point lastUpdate;
    
public:
    PongGame() : gameRunning(true) {
        initializeGame();
    }
    
    void initializeGame() {
        // Initialize paddles
        leftPaddle = {2, SCREEN_HEIGHT / 2 - PADDLE_HEIGHT / 2, PADDLE_HEIGHT, 0};
        rightPaddle = {SCREEN_WIDTH - 4, SCREEN_HEIGHT / 2 - PADDLE_HEIGHT / 2, PADDLE_HEIGHT, 0};
        
        // Initialize ball
        resetBall();
        
        lastUpdate = std::chrono::steady_clock::now();
    }
    
    void resetBall() {
        ball.x = SCREEN_WIDTH / 2;
        ball.y = SCREEN_HEIGHT / 2;
        
        // Random initial direction
        std::mt19937 rng(std::random_device{}());
        std::uniform_int_distribution<int> dist(-1, 1);
        ball.velocity_x = dist(rng);
        if (ball.velocity_x == 0) ball.velocity_x = 1;
        ball.velocity_y = dist(rng);
        if (ball.velocity_y == 0) ball.velocity_y = -1;
    }
    
    void update() {
        auto now = std::chrono::steady_clock::now();
        auto deltaTime = std::chrono::duration_cast<std::chrono::milliseconds>(now - lastUpdate);
        
        if (deltaTime.count() < 50) return; // 20 FPS
        lastUpdate = now;
        
        if (!gameRunning) return;
        
        updateBall();
        checkCollisions();
        checkScoring();
    }
    
    void updateBall() {
        ball.x += ball.velocity_x;
        ball.y += ball.velocity_y;
        
        // Bounce off top and bottom walls
        if (ball.y <= 1 || ball.y >= SCREEN_HEIGHT - 2) {
            ball.velocity_y *= -1;
        }
    }
    
    void checkCollisions() {
        // Left paddle collision
        if (ball.x <= leftPaddle.x + 2 && 
            ball.x >= leftPaddle.x && 
            ball.y >= leftPaddle.y && 
            ball.y < leftPaddle.y + leftPaddle.height) {
            
            ball.velocity_x = abs(ball.velocity_x);
            ball.velocity_y = calculatePaddleBounce(leftPaddle);
        }
        
        // Right paddle collision
        if (ball.x >= rightPaddle.x - 2 && 
            ball.x <= rightPaddle.x + 2 && 
            ball.y >= rightPaddle.y && 
            ball.y < rightPaddle.y + rightPaddle.height) {
            
            ball.velocity_x = -abs(ball.velocity_x);
            ball.velocity_y = calculatePaddleBounce(rightPaddle);
        }
    }
    
    int calculatePaddleBounce(const Paddle& paddle) {
        int paddleCenter = paddle.y + paddle.height / 2;
        int distanceFromCenter = ball.y - paddleCenter;
        int maxBounceAngle = 3;
        
        if (distanceFromCenter > paddle.height / 4) return maxBounceAngle;
        if (distanceFromCenter < -paddle.height / 4) return -maxBounceAngle;
        return 0;
    }
    
    void checkScoring() {
        // Left player scores
        if (ball.x < 0) {
            rightPaddle.score++;
            resetBall();
        }
        
        // Right player scores
        if (ball.x > SCREEN_WIDTH) {
            leftPaddle.score++;
            resetBall();
        }
    }
    
    void render() {
        // Clear screen
        std::cout << "\033[2J\033[H";
        
        // Print score
        std::cout << "PONG GAME" << std::endl;
        std::cout << "Player 1: " << leftPaddle.score << " | Player 2: " << rightPaddle.score << std::endl;
        std::cout << std::string(SCREEN_WIDTH, '=') << std::endl;
        
        // Create game screen
        std::vector<std::string> screen(SCREEN_HEIGHT, std::string(SCREEN_WIDTH, ' '));
        
        // Add paddles
        for (int i = 0; i < leftPaddle.height; i++) {
            screen[leftPaddle.y + i][leftPaddle.x] = '|';
            screen[leftPaddle.y + i][leftPaddle.x + 1] = '|';
        }
        
        for (int i = 0; i < rightPaddle.height; i++) {
            screen[rightPaddle.y + i][rightPaddle.x] = '|';
            screen[rightPaddle.y + i][rightPaddle.x + 1] = '|';
        }
        
        // Add ball
        screen[ball.y][ball.x] = 'O';
        
        // Print screen
        for (const auto& line : screen) {
            std::cout << line << std::endl;
        }
        
        std::cout << "Controls: W/S for left paddle, Arrow Up/Down for right paddle" << std::endl;
        std::cout << "Press Q to quit" << std::endl;
    }
    
    void moveLeftPaddleUp() {
        if (leftPaddle.y > 1) leftPaddle.y--;
    }
    
    void moveLeftPaddleDown() {
        if (leftPaddle.y + leftPaddle.height < SCREEN_HEIGHT - 1) leftPaddle.y++;
    }
    
    void moveRightPaddleUp() {
        if (rightPaddle.y > 1) rightPaddle.y--;
    }
    
    void moveRightPaddleDown() {
        if (rightPaddle.y + rightPaddle.height < SCREEN_HEIGHT - 1) rightPaddle.y++;
    }
    
    bool isGameRunning() const {
        return gameRunning;
    }
    
    void quitGame() {
        gameRunning = false;
    }
};

int main() {
    PongGame game;
    char input;
    
    std::cout << "=== PONG GAME ===" << std::endl;
    std::cout << "Classic two-player Pong game" << std::endl;
    std::cout << "Controls: W/S for left paddle, Arrow keys for right paddle" << std::endl;
    std::cout << "Press ENTER to start..." << std::endl;
    
    std::cin.get();
    
    while (game.isGameRunning()) {
        game.render();
        
        // Non-blocking input check
        if (std::cin.peek() != EOF) {
            input = std::cin.get();
            
            switch (input) {
                case 'w':
                case 'W':
                    game.moveLeftPaddleUp();
                    break;
                case 's':
                case 'S':
                    game.moveLeftPaddleDown();
                    break;
                case 72: // Up arrow
                    game.moveRightPaddleUp();
                    break;
                case 80: // Down arrow
                    game.moveRightPaddleDown();
                    break;
                case 'q':
                case 'Q':
                    game.quitGame();
                    break;
            }
        }
        
        // Update game logic
        game.update();
        
        // Small delay
        std::this_thread::sleep_for(std::chrono::milliseconds(50));
    }
    
    std::cout << "Game Over! Thanks for playing!" << std::endl;
    
    return 0;
}
