/*
 * Pac-Man Game - C++ Implementation
 * Classic maze game with ghosts and power pellets
 */

#include <iostream>
#include <vector>
#include <chrono>
#include <thread>
#include <random>
#include <algorithm>

class PacManGame {
private:
    static const int MAZE_WIDTH = 19;
    static const int MAZE_HEIGHT = 21;
    
    enum CellType {
        WALL = '#',
        PELLET = '.',
        POWER_PELLET = 'O',
        EMPTY = ' ',
        PACMAN = 'P',
        GHOST = 'G',
        SCARED_GHOST = 'S'
    };
    
    struct Position {
        int x, y;
        
        Position() : x(0), y(0) {}
        Position(int x, int y) : x(x), y(y) {}
        
        bool operator==(const Position& other) const {
            return x == other.x && y == other.y;
        }
        
        bool operator!=(const Position& other) const {
            return !(*this == other);
        }
    };
    
    struct Ghost {
        Position position;
        Position direction;
        bool scared;
        int scaredTimer;
        char color; // 'R' red, 'P' pink, 'B' blue, 'O' orange
    };
    
    std::vector<std::string> maze;
    Position pacmanPos;
    Position pacmanDir;
    std::vector<Ghost> ghosts;
    int score;
    int lives;
    int level;
    bool gameRunning;
    bool powerMode;
    int powerTimer;
    std::chrono::steady_clock::time_point lastUpdate;
    
public:
    PacManGame() : score(0), lives(3), level(1), gameRunning(true), powerMode(false), powerTimer(0) {
        initializeGame();
    }
    
    void initializeGame() {
        // Initialize maze
        maze = {
            "###################",
            "#........#........#",
            "#.##.###.#.###.##.#",
            "#O.................#",
            "#.##.#.#####.#.##.#",
            "#....#...#...#....#",
            "####.###.#.###.####",
            "   #.#.......#.#   ",
            "####.#.## ##.#.####",
            "      .#    #.      ",
            "####.#.#####.#.####",
            "   #.#.........#   ",
            "####.#.#####.#.####",
            "#........#........#",
            "#.##.###.#.###.##.#",
            "#..#.............#",
            "##.#.#.#####.#.#.##",
            "#....#...#...#....#",
            "#.######.#.######.#",
            "#.................#",
            "###################"
        };
        
        // Initialize Pac-Man
        pacmanPos = Position(9, 15);
        pacmanDir = Position(0, 0);
        
        // Initialize ghosts
        initializeGhosts();
        
        lastUpdate = std::chrono::steady_clock::now();
    }
    
    void initializeGhosts() {
        ghosts.clear();
        
        // Red ghost (Blinky) - chases Pac-Man directly
        Ghost redGhost;
        redGhost.position = Position(9, 9);
        redGhost.direction = Position(0, -1);
        redGhost.scared = false;
        redGhost.scaredTimer = 0;
        redGhost.color = 'R';
        ghosts.push_back(redGhost);
        
        // Pink ghost (Pinky) - ambushes Pac-Man
        Ghost pinkGhost;
        pinkGhost.position = Position(9, 10);
        pinkGhost.direction = Position(0, 1);
        pinkGhost.scared = false;
        pinkGhost.scaredTimer = 0;
        pinkGhost.color = 'P';
        ghosts.push_back(pinkGhost);
        
        // Blue ghost (Inky) - unpredictable
        Ghost blueGhost;
        blueGhost.position = Position(8, 10);
        blueGhost.direction = Position(-1, 0);
        blueGhost.scared = false;
        blueGhost.scaredTimer = 0;
        blueGhost.color = 'B';
        ghosts.push_back(blueGhost);
        
        // Orange ghost (Clyde) - random movement
        Ghost orangeGhost;
        orangeGhost.position = Position(10, 10);
        orangeGhost.direction = Position(1, 0);
        orangeGhost.scared = false;
        orangeGhost.scaredTimer = 0;
        orangeGhost.color = 'O';
        ghosts.push_back(orangeGhost);
    }
    
    void update() {
        auto now = std::chrono::steady_clock::now();
        auto deltaTime = std::chrono::duration_cast<std::chrono::milliseconds>(now - lastUpdate);
        
        if (deltaTime.count() < 200) return; // Slower pace for Pac-Man
        lastUpdate = now;
        
        if (!gameRunning) return;
        
        updatePacman();
        updateGhosts();
        updatePowerMode();
        checkCollisions();
        checkLevelComplete();
    }
    
    void updatePacman() {
        // Try to move in current direction
        Position newPos = pacmanPos;
        newPos.x += pacmanDir.x;
        newPos.y += pacmanDir.y;
        
        // Wrap around horizontally
        if (newPos.x < 0) newPos.x = MAZE_WIDTH - 1;
        if (newPos.x >= MAZE_WIDTH) newPos.x = 0;
        
        // Check collision with walls
        if (maze[newPos.y][newPos.x] != WALL) {
            pacmanPos = newPos;
            
            // Eat pellet
            if (maze[pacmanPos.y][pacmanPos.x] == PELLET) {
                maze[pacmanPos.y][pacmanPos.x] = EMPTY;
                score += 10;
            }
            
            // Eat power pellet
            if (maze[pacmanPos.y][pacmanPos.x] == POWER_PELLET) {
                maze[pacmanPos.y][pacmanPos.x] = EMPTY;
                activatePowerMode();
                score += 50;
            }
        }
    }
    
    void updateGhosts() {
        for (auto& ghost : ghosts) {
            if (ghost.scared && ghost.scaredTimer > 0) {
                ghost.scaredTimer--;
                if (ghost.scaredTimer <= 0) {
                    ghost.scared = false;
                }
            }
            
            // Move ghost
            Position newPos = ghost.position;
            
            // Simple AI based on ghost type
            if (ghost.color == 'R') {
                // Red ghost: chase Pac-Man
                newPos = moveTowardsTarget(ghost, pacmanPos);
            } else if (ghost.color == 'P') {
                // Pink ghost: ambush (4 tiles ahead of Pac-Man)
                Position target = pacmanPos;
                target.x += pacmanDir.x * 4;
                target.y += pacmanDir.y * 4;
                newPos = moveTowardsTarget(ghost, target);
            } else if (ghost.color == 'O') {
                // Orange ghost: random movement
                std::mt19937 rng(std::random_device{}());
                std::uniform_int_distribution<int> dirDist(0, 3);
                int dir = dirDist(rng);
                switch (dir) {
                    case 0: newPos.x--; break; // Left
                    case 1: newPos.x++; break; // Right
                    case 2: newPos.y--; break; // Up
                    case 3: newPos.y++; break; // Down
                }
            } else {
                // Blue ghost: scatter to corners
                Position corner;
                switch (ghost.color) {
                    case 'B': corner = Position(1, 1); break; // Top-left
                    default: corner = Position(MAZE_WIDTH - 2, MAZE_HEIGHT - 2); break; // Bottom-right
                }
                newPos = moveTowardsTarget(ghost, corner);
            }
            
            // Wrap around horizontally
            if (newPos.x < 0) newPos.x = MAZE_WIDTH - 1;
            if (newPos.x >= MAZE_WIDTH) newPos.x = 0;
            
            // Check if move is valid
            if (newPos.y >= 0 && newPos.y < MAZE_HEIGHT && 
                maze[newPos.y][newPos.x] != WALL) {
                ghost.position = newPos;
            }
        }
    }
    
    Position moveTowardsTarget(Ghost& ghost, Position target) {
        // Simple pathfinding: try to move towards target
        Position current = ghost.position;
        Position bestMove = current;
        int bestDistance = INT_MAX;
        
        // Try all 4 directions
        std::vector<Position> directions = {
            Position(0, -1), Position(0, 1), Position(-1, 0), Position(1, 0)
        };
        
        for (const auto& dir : directions) {
            Position newPos = current;
            newPos.x += dir.x;
            newPos.y += dir.y;
            
            // Wrap around horizontally
            if (newPos.x < 0) newPos.x = MAZE_WIDTH - 1;
            if (newPos.x >= MAZE_WIDTH) newPos.x = 0;
            
            if (newPos.y >= 0 && newPos.y < MAZE_HEIGHT && 
                maze[newPos.y][newPos.x] != WALL) {
                
                int distance = abs(newPos.x - target.x) + abs(newPos.y - target.y);
                if (distance < bestDistance) {
                    bestDistance = distance;
                    bestMove = newPos;
                }
            }
        }
        
        return bestMove;
    }
    
    void activatePowerMode() {
        powerMode = true;
        powerTimer = 300; // 10 seconds at 30 FPS
        
        // Make all ghosts scared
        for (auto& ghost : ghosts) {
            ghost.scared = true;
            ghost.scaredTimer = powerTimer;
        }
    }
    
    void updatePowerMode() {
        if (powerMode) {
            powerTimer--;
            if (powerTimer <= 0) {
                powerMode = false;
            }
        }
    }
    
    void checkCollisions() {
        // Pac-Man vs ghosts
        for (auto& ghost : ghosts) {
            if (pacmanPos == ghost.position) {
                if (ghost.scared) {
                    // Eat ghost
                    ghost.position = Position(9, 9); // Reset ghost position
                    ghost.scared = false;
                    score += 200;
                } else {
                    // Pac-Man dies
                    lives--;
                    if (lives <= 0) {
                        gameRunning = false;
                    } else {
                        // Reset positions
                        pacmanPos = Position(9, 15);
                        pacmanDir = Position(0, 0);
                        initializeGhosts();
                    }
                }
                break;
            }
        }
    }
    
    void checkLevelComplete() {
        bool pelletsLeft = false;
        for (const auto& row : maze) {
            if (row.find(PELLET) != std::string::npos || 
                row.find(POWER_PELLET) != std::string::npos) {
                pelletsLeft = true;
                break;
            }
        }
        
        if (!pelletsLeft) {
            level++;
            score += 1000 * level;
            initializeGame(); // Start next level
        }
    }
    
    void setPacmanDirection(int dx, int dy) {
        pacmanDir.x = dx;
        pacmanDir.y = dy;
    }
    
    void render() {
        // Clear screen
        std::cout << "\033[2J\033[H";
        
        // Print score and info
        std::cout << "PAC-MAN" << std::endl;
        std::cout << "Score: " << score << " | Lives: " << lives << " | Level: " << level;
        if (powerMode) {
            std::cout << " | POWER MODE: " << powerTimer / 30 << "s";
        }
        std::cout << std::endl;
        std::cout << std::string(MAZE_WIDTH, '=') << std::endl;
        
        // Create display maze
        std::vector<std::string> display = maze;
        
        // Add Pac-Man
        if (pacmanPos.y >= 0 && pacmanPos.y < MAZE_HEIGHT && 
            pacmanPos.x >= 0 && pacmanPos.x < MAZE_WIDTH) {
            display[pacmanPos.y][pacmanPos.x] = PACMAN;
        }
        
        // Add ghosts
        for (const auto& ghost : ghosts) {
            if (ghost.position.y >= 0 && ghost.position.y < MAZE_HEIGHT && 
                ghost.position.x >= 0 && ghost.position.x < MAZE_WIDTH) {
                
                char ghostChar = ghost.scared ? SCARED_GHOST : GHOST;
                display[ghost.position.y][ghost.position.x] = ghostChar;
            }
        }
        
        // Print maze
        for (const auto& row : display) {
            std::cout << row << std::endl;
        }
        
        std::cout << std::string(MAZE_WIDTH, '=') << std::endl;
        std::cout << "Controls: W/A/S/D to move, Q to quit" << std::endl;
        std::cout << "Eat all pellets to advance to next level!" << std::endl;
        
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
    PacManGame game;
    char input;
    
    std::cout << "=== PAC-MAN ===" << std::endl;
    std::cout << "Classic maze game with ghosts and power pellets" << std::endl;
    std::cout << "Controls: W/A/S/D to move Pac-Man" << std::endl;
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
                    game.setPacmanDirection(0, -1);
                    break;
                case 's':
                case 'S':
                    game.setPacmanDirection(0, 1);
                    break;
                case 'a':
                case 'A':
                    game.setPacmanDirection(-1, 0);
                    break;
                case 'd':
                case 'D':
                    game.setPacmanDirection(1, 0);
                    break;
                case 'q':
                case 'Q':
                    game.quitGame();
                    break;
            }
        }
        
        // Update game logic
        game.update();
        
        std::this_thread::sleep_for(std::chrono::milliseconds(200));
    }
    
    std::cout << "Thanks for playing!" << std::endl;
    
    return 0;
}
