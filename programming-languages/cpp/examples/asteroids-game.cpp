/*
 * Asteroids Game - C++ Implementation
 * Classic vector-based space shooter game
 */

#include <iostream>
#include <vector>
#include <cmath>
#include <chrono>
#include <random>

class AsteroidsGame {
private:
    static const int SCREEN_WIDTH = 80;
    static const int SCREEN_HEIGHT = 30;
    
    struct Point {
        float x, y;
    };
    
    struct Asteroid {
        std::vector<Point> points;
        Point position;
        Point velocity;
        float rotation;
        float rotationSpeed;
        int size; // 3 = large, 2 = medium, 1 = small
        bool active;
    };
    
    struct Bullet {
        Point position;
        Point velocity;
        int lifetime;
        bool active;
    };
    
    struct Ship {
        std::vector<Point> points;
        Point position;
        Point velocity;
        float angle;
        float rotationSpeed;
        bool thrust;
        int lives;
        int invincibility;
        bool active;
    };
    
    Ship ship;
    std::vector<Asteroid> asteroids;
    std::vector<Bullet> bullets;
    int score;
    int level;
    std::chrono::steady_clock::time_point lastUpdate;
    bool gameRunning;
    
public:
    AsteroidsGame() : score(0), level(1), gameRunning(true) {
        initializeGame();
    }
    
    void initializeGame() {
        // Initialize ship
        ship.position = {SCREEN_WIDTH / 2.0f, SCREEN_HEIGHT / 2.0f};
        ship.velocity = {0, 0};
        ship.angle = -M_PI / 2; // Point up
        ship.rotationSpeed = 0.15f;
        ship.thrust = false;
        ship.lives = 3;
        ship.invincibility = 180; // 3 seconds at 60 FPS
        ship.active = true;
        
        // Ship points (triangle)
        ship.points = {
            {0, -15},
            {-10, 10},
            {-5, 5},
            {5, 5},
            {10, 10}
        };
        
        // Initialize asteroids
        asteroids.clear();
        spawnAsteroids(4);
        
        bullets.clear();
        lastUpdate = std::chrono::steady_clock::now();
    }
    
    void spawnAsteroids(int count) {
        std::mt19937 rng(std::random_device{}());
        std::uniform_real_distribution<float> posDist(10.0f, SCREEN_WIDTH - 10.0f);
        std::uniform_real_distribution<float> velDist(-2.0f, 2.0f);
        
        for (int i = 0; i < count; i++) {
            Asteroid asteroid;
            
            // Generate random asteroid shape
            int numPoints = rng() % 8 + 8;
            for (int j = 0; j < numPoints; j++) {
                float angle = (2.0f * M_PI * j) / numPoints;
                float radius = rng() % 20 + 10;
                asteroid.points.push_back({
                    std::cos(angle) * radius,
                    std::sin(angle) * radius
                });
            }
            
            // Position away from ship
            do {
                asteroid.position = {posDist(rng), posDist(rng)};
            } while (std::abs(asteroid.position.x - ship.position.x) < 30 &&
                     std::abs(asteroid.position.y - ship.position.y) < 30);
            
            asteroid.velocity = {velDist(rng), velDist(rng)};
            asteroid.rotation = 0;
            asteroid.rotationSpeed = (rng() % 3 - 1) * 0.02f;
            asteroid.size = 3; // Start as large
            asteroid.active = true;
            
            asteroids.push_back(asteroid);
        }
    }
    
    void update() {
        auto now = std::chrono::steady_clock::now();
        auto deltaTime = std::chrono::duration_cast<std::chrono::milliseconds>(now - lastUpdate);
        
        if (deltaTime.count() < 16) return; // ~60 FPS
        lastUpdate = now;
        
        if (!gameRunning) return;
        
        updateShip();
        updateBullets();
        updateAsteroids();
        checkCollisions();
        checkLevelComplete();
    }
    
    void updateShip() {
        if (!ship.active) return;
        
        // Handle rotation
        if (ship.angle > 2 * M_PI) ship.angle -= 2 * M_PI;
        if (ship.angle < 0) ship.angle += 2 * M_PI;
        
        // Handle thrust
        if (ship.thrust) {
            ship.velocity.x += std::cos(ship.angle) * 0.1f;
            ship.velocity.y += std::sin(ship.angle) * 0.1f;
        }
        
        // Apply friction
        ship.velocity.x *= 0.99f;
        ship.velocity.y *= 0.99f;
        
        // Update position
        ship.position.x += ship.velocity.x;
        ship.position.y += ship.velocity.y;
        
        // Wrap around screen
        if (ship.position.x < 0) ship.position.x = SCREEN_WIDTH;
        if (ship.position.x > SCREEN_WIDTH) ship.position.x = 0;
        if (ship.position.y < 0) ship.position.y = SCREEN_HEIGHT;
        if (ship.position.y > SCREEN_HEIGHT) ship.position.y = 0;
        
        // Decrease invincibility
        if (ship.invincibility > 0) {
            ship.invincibility--;
        }
    }
    
    void updateBullets() {
        for (auto& bullet : bullets) {
            if (!bullet.active) continue;
            
            bullet.position.x += bullet.velocity.x;
            bullet.position.y += bullet.velocity.y;
            bullet.lifetime--;
            
            // Remove if out of bounds or lifetime expired
            if (bullet.position.x < 0 || bullet.position.x > SCREEN_WIDTH ||
                bullet.position.y < 0 || bullet.position.y > SCREEN_HEIGHT ||
                bullet.lifetime <= 0) {
                bullet.active = false;
            }
        }
        
        // Remove inactive bullets
        bullets.erase(
            std::remove_if(bullets.begin(), bullets.end(), 
                [](const Bullet& b) { return !b.active; }),
            bullets.end()
        );
    }
    
    void updateAsteroids() {
        for (auto& asteroid : asteroids) {
            if (!asteroid.active) continue;
            
            asteroid.position.x += asteroid.velocity.x;
            asteroid.position.y += asteroid.velocity.y;
            asteroid.rotation += asteroid.rotationSpeed;
            
            // Wrap around screen
            if (asteroid.position.x < 0) asteroid.position.x = SCREEN_WIDTH;
            if (asteroid.position.x > SCREEN_WIDTH) asteroid.position.x = 0;
            if (asteroid.position.y < 0) asteroid.position.y = SCREEN_HEIGHT;
            if (asteroid.position.y > SCREEN_HEIGHT) asteroid.position.y = 0;
        }
    }
    
    void checkCollisions() {
        // Ship vs asteroids
        if (ship.active && ship.invincibility <= 0) {
            for (auto& asteroid : asteroids) {
                if (!asteroid.active) continue;
                
                float distance = std::sqrt(
                    std::pow(ship.position.x - asteroid.position.x, 2) +
                    std::pow(ship.position.y - asteroid.position.y, 2)
                );
                
                if (distance < 20) { // Collision radius
                    ship.lives--;
                    if (ship.lives <= 0) {
                        ship.active = false;
                        gameRunning = false;
                    } else {
                        // Reset ship position
                        ship.position = {SCREEN_WIDTH / 2.0f, SCREEN_HEIGHT / 2.0f};
                        ship.velocity = {0, 0};
                        ship.invincibility = 180;
                    }
                    break;
                }
            }
        }
        
        // Bullets vs asteroids
        for (auto& bullet : bullets) {
            if (!bullet.active) continue;
            
            for (auto& asteroid : asteroids) {
                if (!asteroid.active) continue;
                
                float distance = std::sqrt(
                    std::pow(bullet.position.x - asteroid.position.x, 2) +
                    std::pow(bullet.position.y - asteroid.position.y, 2)
                );
                
                if (distance < 15) { // Collision radius
                    bullet.active = false;
                    destroyAsteroid(asteroid);
                    break;
                }
            }
        }
    }
    
    void destroyAsteroid(Asteroid& asteroid) {
        asteroid.active = false;
        
        // Award points based on size
        int points = 0;
        switch (asteroid.size) {
            case 3: points = 20; break; // Large
            case 2: points = 50; break; // Medium
            case 1: points = 100; break; // Small
        }
        score += points;
        
        // Split into smaller asteroids
        if (asteroid.size > 1) {
            spawnSmallerAsteroids(asteroid.position, asteroid.size - 1);
        }
    }
    
    void spawnSmallerAsteroids(Point position, int size) {
        std::mt19937 rng(std::random_device{}());
        std::uniform_real_distribution<float> velDist(-1.0f, 1.0f);
        
        for (int i = 0; i < 2; i++) {
            Asteroid asteroid;
            
            // Generate smaller asteroid shape
            int numPoints = rng() % 6 + 6;
            for (int j = 0; j < numPoints; j++) {
                float angle = (2.0f * M_PI * j) / numPoints;
                float radius = rng() % 10 + 5;
                asteroid.points.push_back({
                    std::cos(angle) * radius,
                    std::sin(angle) * radius
                });
            }
            
            asteroid.position = position;
            asteroid.velocity = {velDist(rng), velDist(rng)};
            asteroid.rotation = 0;
            asteroid.rotationSpeed = (rng() % 3 - 1) * 0.05f;
            asteroid.size = size;
            asteroid.active = true;
            
            asteroids.push_back(asteroid);
        }
    }
    
    void checkLevelComplete() {
        bool anyActive = false;
        for (const auto& asteroid : asteroids) {
            if (asteroid.active) {
                anyActive = true;
                break;
            }
        }
        
        if (!anyActive) {
            level++;
            spawnAsteroids(3 + level); // Increase difficulty
        }
    }
    
    void shoot() {
        if (!ship.active) return;
        
        Bullet bullet;
        bullet.position = ship.position;
        bullet.velocity = {
            std::cos(ship.angle) * 5.0f + ship.velocity.x,
            std::sin(ship.angle) * 5.0f + ship.velocity.y
        };
        bullet.lifetime = 60; // 1 second at 60 FPS
        bullet.active = true;
        
        bullets.push_back(bullet);
    }
    
    void rotateShip(bool left) {
        if (!ship.active) return;
        
        if (left) {
            ship.angle -= ship.rotationSpeed;
        } else {
            ship.angle += ship.rotationSpeed;
        }
    }
    
    void setThrust(bool thrust) {
        ship.thrust = thrust;
    }
    
    void render() {
        // Clear screen
        std::cout << "\033[2J\033[H";
        
        // Print score and info
        std::cout << "ASTEROIDS" << std::endl;
        std::cout << "Score: " << score << " | Level: " << level << " | Lives: " << ship.lives << std::endl;
        std::cout << std::string(SCREEN_WIDTH, '=') << std::endl;
        
        // Create game screen
        std::vector<std::string> screen(SCREEN_HEIGHT, std::string(SCREEN_WIDTH, ' '));
        
        // Render asteroids
        for (const auto& asteroid : asteroids) {
            if (!asteroid.active) continue;
            
            for (const auto& point : asteroid.points) {
                float x = asteroid.position.x + 
                         point.x * std::cos(asteroid.rotation) - 
                         point.y * std::sin(asteroid.rotation);
                float y = asteroid.position.y + 
                         point.x * std::sin(asteroid.rotation) + 
                         point.y * std::cos(asteroid.rotation);
                
                int screenX = static_cast<int>(x);
                int screenY = static_cast<int>(y);
                
                if (screenX >= 0 && screenX < SCREEN_WIDTH && 
                    screenY >= 0 && screenY < SCREEN_HEIGHT) {
                    screen[screenY][screenX] = '*';
                }
            }
        }
        
        // Render bullets
        for (const auto& bullet : bullets) {
            if (!bullet.active) continue;
            
            int screenX = static_cast<int>(bullet.position.x);
            int screenY = static_cast<int>(bullet.position.y);
            
            if (screenX >= 0 && screenX < SCREEN_WIDTH && 
                screenY >= 0 && screenY < SCREEN_HEIGHT) {
                screen[screenY][screenX] = '.';
            }
        }
        
        // Render ship
        if (ship.active) {
            char shipChar = (ship.invincibility > 0 && ship.invincibility % 10 < 5) ? ' ' : 'A';
            
            for (const auto& point : ship.points) {
                float x = ship.position.x + 
                         point.x * std::cos(ship.angle) - 
                         point.y * std::sin(ship.angle);
                float y = ship.position.y + 
                         point.x * std::sin(ship.angle) + 
                         point.y * std::cos(ship.angle);
                
                int screenX = static_cast<int>(x);
                int screenY = static_cast<int>(y);
                
                if (screenX >= 0 && screenX < SCREEN_WIDTH && 
                    screenY >= 0 && screenY < SCREEN_HEIGHT) {
                    screen[screenY][screenX] = shipChar;
                }
            }
            
            // Render thrust flame
            if (ship.thrust) {
                float flameX = ship.position.x + 
                              (-15) * std::cos(ship.angle) - 
                              (0) * std::sin(ship.angle);
                float flameY = ship.position.y + 
                              (-15) * std::sin(ship.angle) + 
                              (0) * std::cos(ship.angle);
                
                int screenX = static_cast<int>(flameX);
                int screenY = static_cast<int>(flameY);
                
                if (screenX >= 0 && screenX < SCREEN_WIDTH && 
                    screenY >= 0 && screenY < SCREEN_HEIGHT) {
                    screen[screenY][screenX] = '~';
                }
            }
        }
        
        // Print screen
        for (const auto& line : screen) {
            std::cout << line << std::endl;
        }
        
        std::cout << std::string(SCREEN_WIDTH, '=') << std::endl;
        std::cout << "Controls: A/D to rotate, W for thrust, SPACE to shoot, Q to quit" << std::endl;
        
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
    AsteroidsGame game;
    char input;
    bool thrust = false;
    
    std::cout << "=== ASTEROIDS ===" << std::endl;
    std::cout << "Classic vector-based space shooter" << std::endl;
    std::cout << "Controls: A/D to rotate, W for thrust, SPACE to shoot" << std::endl;
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
                    game.rotateShip(true);
                    break;
                case 'd':
                case 'D':
                    game.rotateShip(false);
                    break;
                case 'w':
                case 'W':
                    thrust = true;
                    game.setThrust(true);
                    break;
                case ' ':
                    game.shoot();
                    break;
                case 'q':
                case 'Q':
                    game.quitGame();
                    break;
            }
        } else {
            // Release thrust when no input
            if (thrust) {
                thrust = false;
                game.setThrust(false);
            }
        }
        
        // Update game logic
        game.update();
        
        std::this_thread::sleep_for(std::chrono::milliseconds(16));
    }
    
    std::cout << "Thanks for playing!" << std::endl;
    
    return 0;
}
