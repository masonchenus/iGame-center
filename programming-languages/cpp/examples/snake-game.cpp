/*
 * Snake Game - C++ Implementation
 * Classic snake game with modern C++ features
 */

#include <iostream>
#include <vector>
#include <deque>
#include <chrono>
#include <random>
#include <algorithm>

class SnakeGame {
private:
    static const int GRID_WIDTH = 40;
    static const int GRID_HEIGHT = 20;
    static const int INITIAL_LENGTH = 3;
    
    struct Position {
        int x, y;
        
        Position() : x(0), y(0) {}
        Position(int x, int y) : x(x), y(y) {}
        
        bool operator==(const Position& other) const {
            return x == other.x && y == other.y;
        }
        
        bool operator<(const Position& other) const {
            return x < other.x || (x == other.x && y < other.y);
        }
    };
    
    enum class Direction {
        Up, Down, Left, Right
    };
    
    std::deque<Position> snake;
    Position food;
    Direction currentDirection;
    Direction nextDirection;
    int score;
    int speed;
    bool gameOver;
    bool paused;
    std::chrono::steady_clock::time_point lastUpdate;
    std::mt19937 rng;
    
public:
    SnakeGame() : 
        currentDirection(Direction::Right), 
        nextDirection(Direction::Right),
        score(0), 
        speed(150),
        gameOver(false), 
        paused(false),
        rng(std::random_device{}()) {
        initializeGame();
    }
    
    void initializeGame() {
        snake.clear();
        
        // Initialize snake in center
        for (int i = 0; i < INITIAL_LENGTH; i++) {
            snake.push_front(Position(GRID_WIDTH / 2 - i, GRID_HEIGHT / 2));
        }
        
        currentDirection = Direction::Right;
        nextDirection = Direction::Right;
        score = 0;
        speed = 150;
        gameOver = false;
        paused = false;
        
        generateFood();
        lastUpdate = std::chrono::steady_clock::now();
    }
    
    void generateFood() {
        std::uniform_int_distribution<int> xDist(0, GRID_WIDTH - 1);
        std::uniform_int_distribution<int> yDist(0, GRID_HEIGHT - 1);
        
        Position newFood;
        do {
            newFood = Position(xDist(rng), yDist(rng));
        } while (std::find(snake.begin(), snake.end(), newFood) != snake.end());
        
        food = newFood;
    }
    
    void update() {
        auto now = std::chrono::steady_clock::now();
        auto deltaTime = std::chrono::duration_cast<std::chrono::milliseconds>(now - lastUpdate);
        
        if (deltaTime.count() < speed) return;
        lastUpdate = now;
        
        if (gameOver || paused) return;
        
        // Update direction
        currentDirection = nextDirection;
        
        // Calculate new head position
        Position newHead = snake.front();
        switch (currentDirection) {
            case Direction::Up:
                newHead.y--;
                break;
            case Direction::Down:
                newHead.y++;
                break;
            case Direction::Left:
                newHead.x--;
                break;
            case Direction::Right:
                newHead.x++;
                break;
        }
        
        // Check wall collision
        if (newHead.x < 0 || newHead.x >= GRID_WIDTH || 
            newHead.y < 0 || newHead.y >= GRID_HEIGHT) {
            gameOver = true;
            return;
        }
        
        // Check self collision
        if (std::find(snake.begin(), snake.end(), newHead) != snake.end()) {
            gameOver = true;
            return;
        }
        
        // Add new head
        snake.push_front(newHead);
        
        // Check food collision
        if (newHead == food) {
            score += 10;
            
            // Increase speed every 50 points
            if (score % 50 == 0 && speed > 50) {
                speed -= 10;
            }
            
            generateFood();
        } else {
            // Remove tail if no food eaten
            snake.pop_back();
        }
    }
    
    void setDirection(Direction direction) {
        // Prevent reverse direction
        if (snake.size() > 1) {
            Direction opposite;
            switch (direction) {
                case Direction::Up: opposite = Direction::Down; break;
                case Direction::Down: opposite = Direction::Up; break;
                case Direction::Left: opposite = Direction::Right; break;
                case Direction::Right: opposite = Direction::Left; break;
            }
            
            if (currentDirection == opposite) return;
        }
        
        nextDirection = direction;
    }
    
    void togglePause() {
        paused = !paused;
    }
    
    void render() {
        // Clear screen
        std::cout << "\033[2J\033[H";
        
        // Print score and info
        std::cout << "SNAKE GAME - C++ Edition" << std::endl;
        std::cout << "Score: " << score << " | Length: " << snake.size() 
                  << " | Speed: " << (200 - speed) / 10 << std::endl;
        std::cout << std::string(GRID_WIDTH + 2, '=') << std::endl;
        
        // Create grid
        std::vector<std::string> grid(GRID_HEIGHT, std::string(GRID_WIDTH, ' '));
        
        // Add food
        grid[food.y][food.x] = '@';
        
        // Add snake
        for (size_t i = 0; i < snake.size(); i++) {
            const auto& segment = snake[i];
            char snakeChar = (i == 0) ? 'O' : 'o';
            grid[segment.y][segment.x] = snakeChar;
        }
        
        // Print grid with borders
        for (const auto& row : grid) {
            std::cout << "|" << row << "|" << std::endl;
        }
        
        std::cout << std::string(GRID_WIDTH + 2, '=') << std::endl;
        std::cout << "Controls: W/A/S/D to move, P to pause, R to restart, Q to quit" << std::endl;
        
        if (paused) {
            std::cout << "PAUSED - Press P to resume" << std::endl;
        }
        
        if (gameOver) {
            std::cout << "GAME OVER! Final Score: " << score << std::endl;
            std::cout << "Press R to restart or Q to quit" << std::endl;
        }
    }
    
    bool isGameOver() const {
        return gameOver;
    }
    
    bool isPaused() const {
        return paused;
    }
    
    void restart() {
        initializeGame();
    }
    
    void quitGame() {
        gameOver = true;
    }
};

// Advanced Snake with power-ups and obstacles
class AdvancedSnakeGame : public SnakeGame {
private:
    struct PowerUp {
        enum Type { SpeedBoost, DoublePoints, Shrink, Obstacle };
        Type type;
        Position position;
        bool active;
        int lifetime;
    };
    
    struct Obstacle {
        Position position;
        bool active;
    };
    
    std::vector<PowerUp> powerUps;
    std::vector<Obstacle> obstacles;
    int doublePointsTimer;
    int speedBoostTimer;
    std::uniform_int_distribution<int> powerUpDist;
    
public:
    AdvancedSnakeGame() : 
        SnakeGame(),
        doublePointsTimer(0),
        speedBoostTimer(0),
        powerUpDist(0, 3) {
        initializeObstacles();
    }
    
    void initializeObstacles() {
        obstacles.clear();
        
        // Add some obstacles
        for (int i = 5; i < GRID_WIDTH - 5; i += 8) {
            for (int j = 5; j < GRID_HEIGHT - 5; j += 4) {
                Obstacle obstacle;
                obstacle.position = Position(i, j);
                obstacle.active = true;
                obstacles.push_back(obstacle);
            }
        }
    }
    
    void generatePowerUp() {
        std::uniform_int_distribution<int> xDist(1, GRID_WIDTH - 2);
        std::uniform_int_distribution<int> yDist(1, GRID_HEIGHT - 2);
        
        PowerUp powerUp;
        powerUp.type = static_cast<PowerUp::Type>(powerUpDist(rng));
        powerUp.active = true;
        powerUp.lifetime = 300; // 5 seconds
        
        do {
            powerUp.position = Position(xDist(rng), yDist(rng));
        } while (std::find(snake.begin(), snake.end(), powerUp.position) != snake.end() ||
                 isObstacleAt(powerUp.position));
        
        powerUps.push_back(powerUp);
    }
    
    bool isObstacleAt(const Position& pos) {
        return std::find_if(obstacles.begin(), obstacles.end(),
            [&pos](const Obstacle& obs) { return obs.active && obs.position == pos; }) != obstacles.end();
    }
    
    void checkPowerUpCollision() {
        Position head = snake.front();
        
        for (auto& powerUp : powerUps) {
            if (!powerUp.active || powerUp.position != head) continue;
            
            switch (powerUp.type) {
                case PowerUp::SpeedBoost:
                    speedBoostTimer = 150; // 2.5 seconds
                    break;
                case PowerUp::DoublePoints:
                    doublePointsTimer = 300; // 5 seconds
                    break;
                case PowerUp::Shrink:
                    // Remove 2 segments from tail
                    for (int i = 0; i < 2 && snake.size() > 1; i++) {
                        snake.pop_back();
                    }
                    break;
                case PowerUp::Obstacle:
                    // Add permanent obstacle
                    obstacles.push_back({head, true});
                    break;
            }
            
            powerUp.active = false;
            score += 20; // Bonus for power-up
        }
        
        // Remove inactive power-ups
        powerUps.erase(
            std::remove_if(powerUps.begin(), powerUps.end(),
                [](const PowerUp& pu) { return !pu.active || pu.lifetime <= 0; }),
            powerUps.end()
        );
    }
    
    void updatePowerUps() {
        // Update power-up timers
        if (doublePointsTimer > 0) doublePointsTimer--;
        if (speedBoostTimer > 0) speedBoostTimer--;
        
        // Update power-up lifetimes
        for (auto& powerUp : powerUps) {
            if (powerUp.active) {
                powerUp.lifetime--;
                if (powerUp.lifetime <= 0) {
                    powerUp.active = false;
                }
            }
        }
        
        // Generate new power-ups occasionally
        static int powerUpTimer = 0;
        powerUpTimer++;
        if (powerUpTimer > 600 && powerUps.size() < 3) { // Every 10 seconds
            generatePowerUp();
            powerUpTimer = 0;
        }
    }
    
    void update() override {
        SnakeGame::update();
        
        if (gameOver) return;
        
        checkPowerUpCollision();
        updatePowerUps();
    }
    
    void render() override {
        SnakeGame::render();
        
        // Render power-ups
        std::cout << "Power-ups active: ";
        if (doublePointsTimer > 0) std::cout << "2xPOINTS ";
        if (speedBoostTimer > 0) std::cout << "SPEED+ ";
        std::cout << std::endl;
        
        // Render obstacles and power-ups on grid
        std::cout << "\x1B[" << GRID_HEIGHT + 6 << ";0H"; // Move cursor
        
        for (const auto& obstacle : obstacles) {
            if (obstacle.active) {
                std::cout << "█";
            }
        }
        
        for (const auto& powerUp : powerUps) {
            if (powerUp.active) {
                char symbol = '#';
                switch (powerUp.type) {
                    case PowerUp::SpeedBoost: symbol = '>'; break;
                    case PowerUp::DoublePoints: symbol = '$'; break;
                    case PowerUp::Shrink: symbol = '<'; break;
                    case PowerUp::Obstacle: symbol = '!'; break;
                }
                std::cout << "Power-up at (" << powerUp.position.x << "," << powerUp.position.y << "): " << symbol << std::endl;
            }
        }
    }
};

int main() {
    std::cout << "=== SNAKE GAME - C++ ADVANCED ===" << std::endl;
    std::cout << "1. Classic Snake" << std::endl;
    std::cout << "2. Advanced Snake (with power-ups)" << std::endl;
    std::cout << "Choose game mode (1 or 2): ";
    
    int choice;
    std::cin >> choice;
    std::cin.ignore(); // Clear input buffer
    
    if (choice == 2) {
        AdvancedSnakeGame game;
        char input;
        
        std::cout << "Advanced Snake Game with power-ups!" << std::endl;
        std::cout << "Controls: W/A/S/D to move, P to pause, R to restart, Q to quit" << std::endl;
        std::cout << "Power-ups: > speed boost, $ double points, < shrink snake, ! add obstacle" << std::endl;
        std::cout << "Press ENTER to start..." << std::endl;
        std::cin.get();
        
        while (!game.isGameOver()) {
            game.render();
            
            if (std::cin.peek() != EOF) {
                input = std::cin.get();
                
                switch (input) {
                    case 'w':
                    case 'W':
                        game.setDirection(SnakeGame::Direction::Up);
                        break;
                    case 's':
                    case 'S':
                        game.setDirection(SnakeGame::Direction::Down);
                        break;
                    case 'a':
                    case 'A':
                        game.setDirection(SnakeGame::Direction::Left);
                        break;
                    case 'd':
                    case 'D':
                        game.setDirection(SnakeGame::Direction::Right);
                        break;
                    case 'p':
                    case 'P':
                        game.togglePause();
                        break;
                    case 'r':
                    case 'R':
                        game.restart();
                        break;
                    case 'q':
                    case 'Q':
                        game.quitGame();
                        break;
                }
            }
            
            game.update();
            std::this_thread::sleep_for(std::chrono::milliseconds(16));
        }
    } else {
        SnakeGame game;
        char input;
        
        std::cout << "Classic Snake Game!" << std::endl;
        std::cout << "Controls: W/A/S/D to move, P to pause, R to restart, Q to quit" << std::endl;
        std::cout << "Press ENTER to start..." << std::endl;
        std::cin.get();
        
        while (!game.isGameOver()) {
            game.render();
            
            if (std::cin.peek() != EOF) {
                input = std::cin.get();
                
                switch (input) {
                    case 'w':
                    case 'W':
                        game.setDirection(SnakeGame::Direction::Up);
                        break;
                    case 's':
                    case 'S':
                        game.setDirection(SnakeGame::Direction::Down);
                        break;
                    case 'a':
                    case 'A':
                        game.setDirection(SnakeGame::Direction::Left);
                        break;
                    case 'd':
                    case 'D':
                        game.setDirection(SnakeGame::Direction::Right);
                        break;
                    case 'p':
                    case 'P':
                        game.togglePause();
                        break;
                    case 'r':
                    case 'R':
                        game.restart();
                        break;
                    case 'q':
                    case 'Q':
                        game.quitGame();
                        break;
                }
            }
            
            game.update();
            std::this_thread::sleep_for(std::chrono::milliseconds(16));
        }
    }
    
    std::cout << "Thanks for playing!" << std::endl;
    
    return 0;
}
