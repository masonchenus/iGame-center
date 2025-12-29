/*
 * Space Invaders Game - C++ Implementation
 * Classic arcade game demonstrating C++ game development
 */

#include <iostream>
#include <vector>
#include <chrono>
#include <thread>
#include <random>
#include <algorithm>

class SpaceInvaders {
private:
    static const int SCREEN_WIDTH = 80;
    static const int SCREEN_HEIGHT = 30;
    static const int PLAYER_WIDTH = 3;
    static const int ENEMY_WIDTH = 2;
    static const int ENEMY_HEIGHT = 1;
    
    struct Position {
        int x, y;
    };
    
    struct GameObject {
        Position pos;
        int width;
        int height;
        char symbol;
        bool active;
    };
    
    GameObject player;
    std::vector<GameObject> enemies;
    std::vector<GameObject> bullets;
    std::vector<GameObject> enemyBullets;
    
    int score;
    int lives;
    bool gameOver;
    bool paused;
    int enemyDirection; // 1 for right, -1 for left
    std::chrono::steady_clock::time_point lastUpdate;
    
public:
    SpaceInvaders() : score(0), lives(3), gameOver(false), paused(false), enemyDirection(1) {
        initializeGame();
    }
    
    void initializeGame() {
        // Initialize player
        player.pos = {SCREEN_WIDTH / 2 - PLAYER_WIDTH / 2, SCREEN_HEIGHT - 2};
        player.width = PLAYER_WIDTH;
        player.height = 1;
        player.symbol = '^';
        player.active = true;
        
        // Initialize enemies
        enemies.clear();
        for (int row = 0; row < 5; row++) {
            for (int col = 0; col < 10; col++) {
                GameObject enemy;
                enemy.pos = {10 + col * 6, 5 + row * 3};
                enemy.width = ENEMY_WIDTH;
                enemy.height = ENEMY_HEIGHT;
                enemy.symbol = (row < 2) ? 'E' : 'e'; // Different symbols for different rows
                enemy.active = true;
                enemies.push_back(enemy);
            }
        }
        
        bullets.clear();
        enemyBullets.clear();
        lastUpdate = std::chrono::steady_clock::now();
    }
    
    void update() {
        auto now = std::chrono::steady_clock::now();
        auto deltaTime = std::chrono::duration_cast<std::chrono::milliseconds>(now - lastUpdate);
        
        if (deltaTime.count() < 100) return; // 10 FPS
        lastUpdate = now;
        
        if (gameOver || paused) return;
        
        updateEnemies();
        updateBullets();
        checkCollisions();
        checkWinCondition();
        checkLoseCondition();
    }
    
    void updateEnemies() {
        bool shouldMoveDown = false;
        
        // Check if any enemy hit the screen edge
        for (const auto& enemy : enemies) {
            if (!enemy.active) continue;
            
            if ((enemyDirection > 0 && enemy.pos.x + enemy.width >= SCREEN_WIDTH - 1) ||
                (enemyDirection < 0 && enemy.pos.x <= 0)) {
                shouldMoveDown = true;
                break;
            }
        }
        
        if (shouldMoveDown) {
            enemyDirection *= -1; // Reverse direction
            // Move all enemies down
            for (auto& enemy : enemies) {
                if (enemy.active) {
                    enemy.pos.y += 1;
                }
            }
        } else {
            // Move enemies horizontally
            for (auto& enemy : enemies) {
                if (enemy.active) {
                    enemy.pos.x += enemyDirection;
                }
            }
        }
        
        // Enemy shooting (random)
        static std::mt19937 rng(std::random_device{}());
        static std::uniform_int_distribution<int> dist(0, 100);
        
        if (dist(rng) < 5) { // 5% chance per update
            shootEnemyBullet();
        }
    }
    
    void updateBullets() {
        // Update player bullets
        for (auto& bullet : bullets) {
            if (bullet.active) {
                bullet.pos.y -= 1;
                if (bullet.pos.y < 0) {
                    bullet.active = false;
                }
            }
        }
        
        // Update enemy bullets
        for (auto& bullet : enemyBullets) {
            if (bullet.active) {
                bullet.pos.y += 1;
                if (bullet.pos.y >= SCREEN_HEIGHT) {
                    bullet.active = false;
                }
            }
        }
        
        // Remove inactive bullets
        bullets.erase(std::remove_if(bullets.begin(), bullets.end(), 
            [](const GameObject& b) { return !b.active; }), bullets.end());
        enemyBullets.erase(std::remove_if(enemyBullets.begin(), enemyBullets.end(),
            [](const GameObject& b) { return !b.active; }), enemyBullets.end());
    }
    
    void checkCollisions() {
        // Player bullets vs enemies
        for (auto& bullet : bullets) {
            if (!bullet.active) continue;
            
            for (auto& enemy : enemies) {
                if (!enemy.active) continue;
                
                if (checkCollision(bullet, enemy)) {
                    bullet.active = false;
                    enemy.active = false;
                    score += enemy.symbol == 'E' ? 30 : 10; // Different points for different enemies
                    break;
                }
            }
        }
        
        // Enemy bullets vs player
        for (auto& bullet : enemyBullets) {
            if (!bullet.active) continue;
            
            if (checkCollision(bullet, player)) {
                bullet.active = false;
                lives--;
                if (lives <= 0) {
                    gameOver = true;
                }
            }
        }
        
        // Enemies reaching player level
        for (const auto& enemy : enemies) {
            if (!enemy.active) continue;
            
            if (enemy.pos.y + enemy.height >= player.pos.y) {
                gameOver = true;
                break;
            }
        }
    }
    
    bool checkCollision(const GameObject& a, const GameObject& b) {
        return !(a.pos.x + a.width <= b.pos.x || 
                 a.pos.x >= b.pos.x + b.width ||
                 a.pos.y + a.height <= b.pos.y || 
                 a.pos.y >= b.pos.y + b.height);
    }
    
    void checkWinCondition() {
        bool allEnemiesDead = true;
        for (const auto& enemy : enemies) {
            if (enemy.active) {
                allEnemiesDead = false;
                break;
            }
        }
        if (allEnemiesDead) {
            gameOver = true;
            score += 100; // Bonus for clearing all enemies
        }
    }
    
    void checkLoseCondition() {
        if (lives <= 0) {
            gameOver = true;
        }
    }
    
    void shootBullet() {
        if (bullets.size() < 3) { // Limit player bullets
            GameObject bullet;
            bullet.pos = {player.pos.x + player.width / 2, player.pos.y - 1};
            bullet.width = 1;
            bullet.height = 1;
            bullet.symbol = '|';
            bullet.active = true;
            bullets.push_back(bullet);
        }
    }
    
    void shootEnemyBullet() {
        // Find lowest active enemy in each column and shoot from one
        static std::mt19937 rng(std::random_device{}());
        std::uniform_int_distribution<int> dist(0, enemies.size() - 1);
        
        int attempts = 0;
        while (attempts < 10) {
            int enemyIndex = dist(rng);
            if (enemies[enemyIndex].active) {
                GameObject bullet;
                bullet.pos = {enemies[enemyIndex].pos.x + enemies[enemyIndex].width / 2, 
                             enemies[enemyIndex].pos.y + enemies[enemyIndex].height};
                bullet.width = 1;
                bullet.height = 1;
                bullet.symbol = '!';
                bullet.active = true;
                enemyBullets.push_back(bullet);
                break;
            }
            attempts++;
        }
    }
    
    void movePlayer(int direction) {
        if (gameOver || paused) return;
        
        int newX = player.pos.x + direction;
        if (newX >= 0 && newX + player.width < SCREEN_WIDTH) {
            player.pos.x = newX;
        }
    }
    
    void render() {
        // Clear screen
        std::cout << "\033[2J\033[H";
        
        // Print score and lives
        std::cout << "Score: " << score << " | Lives: ";
        for (int i = 0; i < lives; i++) {
            std::cout << "♥ ";
        }
        std::cout << "\n";
        
        // Print top border
        for (int i = 0; i < SCREEN_WIDTH; i++) {
            std::cout << "-";
        }
        std::cout << "\n";
        
        // Create game screen
        std::vector<std::string> screen(SCREEN_HEIGHT, std::string(SCREEN_WIDTH, ' '));
        
        // Add player
        if (player.active) {
            for (int x = 0; x < player.width; x++) {
                if (player.pos.x + x < SCREEN_WIDTH && player.pos.y < SCREEN_HEIGHT) {
                    screen[player.pos.y][player.pos.x + x] = player.symbol;
                }
            }
        }
        
        // Add enemies
        for (const auto& enemy : enemies) {
            if (!enemy.active) continue;
            
            for (int x = 0; x < enemy.width; x++) {
                for (int y = 0; y < enemy.height; y++) {
                    int screenX = enemy.pos.x + x;
                    int screenY = enemy.pos.y + y;
                    if (screenX >= 0 && screenX < SCREEN_WIDTH && 
                        screenY >= 0 && screenY < SCREEN_HEIGHT) {
                        screen[screenY][screenX] = enemy.symbol;
                    }
                }
            }
        }
        
        // Add bullets
        for (const auto& bullet : bullets) {
            if (bullet.active && bullet.pos.y >= 0 && bullet.pos.y < SCREEN_HEIGHT &&
                bullet.pos.x >= 0 && bullet.pos.x < SCREEN_WIDTH) {
                screen[bullet.pos.y][bullet.pos.x] = bullet.symbol;
            }
        }
        
        for (const auto& bullet : enemyBullets) {
            if (bullet.active && bullet.pos.y >= 0 && bullet.pos.y < SCREEN_HEIGHT &&
                bullet.pos.x >= 0 && bullet.pos.x < SCREEN_WIDTH) {
                screen[bullet.pos.y][bullet.pos.x] = bullet.symbol;
            }
        }
        
        // Print screen
        for (const auto& line : screen) {
            std::cout << line << "\n";
        }
        
        // Print bottom border
        for (int i = 0; i < SCREEN_WIDTH; i++) {
            std::cout << "-";
        }
        std::cout << "\n";
        
        // Print controls
        std::cout << "Controls: A/D to move, SPACE to shoot, P to pause, Q to quit\n";
        
        if (gameOver) {
            std::cout << "\n=== GAME OVER ===\n";
            std::cout << "Final Score: " << score << "\n";
            if (lives <= 0) {
                std::cout << "You ran out of lives!\n";
            } else {
                std::cout << "Congratulations! You cleared all enemies!\n";
            }
            std::cout << "Press R to restart or Q to quit.\n";
        } else if (paused) {
            std::cout << "\n=== PAUSED ===\n";
            std::cout << "Press P to resume.\n";
        }
    }
    
    void handleInput(char input) {
        switch (input) {
            case 'a':
            case 'A':
                movePlayer(-1);
                break;
            case 'd':
            case 'D':
                movePlayer(1);
                break;
            case ' ':
                shootBullet();
                break;
            case 'p':
            case 'P':
                paused = !paused;
                break;
            case 'q':
            case 'Q':
                gameOver = true;
                break;
            case 'r':
            case 'R':
                if (gameOver) {
                    initializeGame();
                    gameOver = false;
                }
                break;
        }
    }
    
    bool isGameOver() const {
        return gameOver;
    }
};

int main() {
    SpaceInvaders game;
    char input;
    
    std::cout << "=== SPACE INVADERS ===\n";
    std::cout << "Classic arcade game in C++\n";
    std::cout << "Controls: A/D to move, SPACE to shoot, P to pause, Q to quit\n";
    std::cout << "Press ENTER to start...\n";
    
    // Wait for user to start
    std::cin.get();
    
    while (!game.isGameOver()) {
        game.render();
        
        // Non-blocking input check
        if (std::cin.peek() != EOF) {
            input = std::cin.get();
            game.handleInput(input);
        }
        
        // Update game logic
        game.update();
        
        // Small delay to prevent excessive CPU usage
        std::this_thread::sleep_for(std::chrono::milliseconds(50));
    }
    
    game.render();
    std::cout << "Thanks for playing!\n";
    
    return 0;
}
