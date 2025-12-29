/*
 * Simple Platformer Game - C Implementation
 * Classic side-scrolling platformer demonstrating C game development
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>

#define SCREEN_WIDTH 80
#define SCREEN_HEIGHT 25
#define GRAVITY 0.5f
#define JUMP_FORCE 8.0f
#define PLAYER_WIDTH 3
#define PLAYER_HEIGHT 4
#define ENEMY_WIDTH 2
#define ENEMY_HEIGHT 2

// Game structures
typedef struct {
    float x, y;
    float vx, vy;
    int width, height;
    int onGround;
    int lives;
    int score;
} Player;

typedef struct {
    float x, y;
    float vx, vy;
    int width, height;
    int direction; // 1 = right, -1 = left
    int active;
} Enemy;

typedef struct {
    float x, y;
    int width, height;
    int type; // 1 = solid, 2 = collectible, 3 = power-up
    int active;
    int value;
} Platform;

typedef struct {
    float x, y;
    int active;
    int points;
} Collectible;

// Global game state
Player player;
Enemy enemies[5];
Platform platforms[20];
Collectible collectibles[10];
int numEnemies = 3;
int numPlatforms = 15;
int numCollectibles = 5;
int gameRunning = 1;
int level = 1;
int cameraX = 0;

// Input handling
char input() {
    if (kbhit()) {
        return getch();
    }
    return 0;
}

int kbhit() {
    return _kbhit();
}

// Initialize game level
void initializeLevel() {
    // Initialize player
    player.x = 50;
    player.y = 10;
    player.vx = 0;
    player.vy = 0;
    player.width = PLAYER_WIDTH;
    player.height = PLAYER_HEIGHT;
    player.onGround = 0;
    player.lives = 3;
    player.score = 0;
    
    // Initialize platforms
    memset(platforms, 0, sizeof(platforms));
    
    // Ground platforms
    for (int i = 0; i < 30; i++) {
        platforms[i].x = i * 30;
        platforms[i].y = 20;
        platforms[i].width = 25;
        platforms[i].height = 3;
        platforms[i].type = 1; // Solid
        platforms[i].active = 1;
    }
    
    // Floating platforms
    platforms[30].x = 100;
    platforms[30].y = 15;
    platforms[30].width = 15;
    platforms[30].height = 2;
    platforms[30].type = 1;
    platforms[30].active = 1;
    
    platforms[31].x = 200;
    platforms[31].y = 12;
    platforms[31].width = 20;
    platforms[31].height = 2;
    platforms[31].type = 1;
    platforms[31].active = 1;
    
    platforms[32].x = 350;
    platforms[32].y = 8;
    platforms[32].width = 15;
    platforms[32].height = 2;
    platforms[32].type = 1;
    platforms[32].active = 1;
    
    // Collectibles
    memset(collectibles, 0, sizeof(collectibles));
    for (int i = 0; i < numCollectibles; i++) {
        collectibles[i].x = 80 + i * 60;
        collectibles[i].y = 18;
        collectibles[i].active = 1;
        collectibles[i].points = 100;
    }
    
    // Enemies
    memset(enemies, 0, sizeof(enemies));
    enemies[0].x = 120;
    enemies[0].y = 16;
    enemies[0].vx = 0.5f;
    enemies[0].direction = 1;
    enemies[0].active = 1;
    
    enemies[1].x = 220;
    enemies[1].y = 13;
    enemies[1].vx = -0.3f;
    enemies[1].direction = -1;
    enemies[1].active = 1;
    
    enemies[2].x = 380;
    enemies[2].y = 9;
    enemies[2].vx = 0.4f;
    enemies[2].direction = 1;
    enemies[2].active = 1;
}

// Physics update
void updatePlayerPhysics() {
    // Apply gravity
    if (!player.onGround) {
        player.vy += GRAVITY;
    }
    
    // Update position
    player.x += player.vx;
    player.y += player.vy;
    
    // Ground collision
    player.onGround = 0;
    for (int i = 0; i < numPlatforms; i++) {
        if (!platforms[i].active || platforms[i].type != 1) continue;
        
        // Simple AABB collision
        if (player.x < platforms[i].x + platforms[i].width &&
            player.x + player.width > platforms[i].x &&
            player.y < platforms[i].y + platforms[i].height &&
            player.y + player.height > platforms[i].y) {
            
            // Landing on platform
            if (player.vy > 0 && player.y < platforms[i].y) {
                player.y = platforms[i].y - player.height;
                player.vy = 0;
                player.onGround = 1;
            }
        }
    }
    
    // World boundaries
    if (player.y > 25) {
        // Player fell - lose life
        player.lives--;
        if (player.lives <= 0) {
            gameRunning = 0;
        } else {
            player.x = 50;
            player.y = 10;
            player.vx = 0;
            player.vy = 0;
        }
    }
}

// Update enemies
void updateEnemies() {
    for (int i = 0; i < numEnemies; i++) {
        if (!enemies[i].active) continue;
        
        // Simple AI - move back and forth
        enemies[i].x += enemies[i].vx * enemies[i].direction;
        
        // Check platform edges
        int onPlatform = 0;
        for (int j = 0; j < numPlatforms; j++) {
            if (!platforms[j].active || platforms[j].type != 1) continue;
            
            if (enemies[i].y >= platforms[j].y - 1 &&
                enemies[i].y <= platforms[j].y + platforms[j].height &&
                enemies[i].x >= platforms[j].x &&
                enemies[i].x <= platforms[j].x + platforms[j].width) {
                onPlatform = 1;
                break;
            }
        }
        
        if (!onPlatform) {
            enemies[i].direction *= -1;
        }
        
        // Player collision
        if (player.x < enemies[i].x + enemies[i].width &&
            player.x + player.width > enemies[i].x &&
            player.y < enemies[i].y + enemies[i].height &&
            player.y + player.height > enemies[i].y) {
            
            // Simple collision - player takes damage
            player.lives--;
            if (player.lives <= 0) {
                gameRunning = 0;
            } else {
                player.x -= 20;
                player.vy = -JUMP_FORCE; // Knockback
            }
        }
    }
}

// Update collectibles
void updateCollectibles() {
    for (int i = 0; i < numCollectibles; i++) {
        if (!collectibles[i].active) continue;
        
        // Player collision
        if (player.x < collectibles[i].x + 2 &&
            player.x + player.width > collectibles[i].x &&
            player.y < collectibles[i].y + 2 &&
            player.y + player.height > collectibles[i].y) {
            
            player.score += collectibles[i].points;
            collectibles[i].active = 0;
        }
    }
}

// Check level completion
int checkLevelComplete() {
    for (int i = 0; i < numCollectibles; i++) {
        if (collectibles[i].active) return 0;
    }
    return 1;
}

// Render game
void renderGame() {
    // Clear screen
    system("cls");
    
    // Print game info
    printf("PLATFORMER GAME - Level %d\n", level);
    printf("Lives: %d | Score: %d | Position: (%.0f, %.0f)\n", 
           player.lives, player.score, player.x, player.y);
    printf("Controls: A/D to move, W or SPACE to jump, Q to quit\n");
    printf("================================================================================\n");
    
    // Create screen buffer
    char screen[SCREEN_HEIGHT][SCREEN_WIDTH];
    memset(screen, ' ', sizeof(screen));
    
    // Render platforms
    for (int i = 0; i < numPlatforms; i++) {
        if (!platforms[i].active) continue;
        
        int screenX = (int)platforms[i].x - cameraX;
        int screenY = (int)platforms[i].y;
        
        for (int y = 0; y < platforms[i].height && screenY + y < SCREEN_HEIGHT; y++) {
            for (int x = 0; x < platforms[i].width && screenX + x < SCREEN_WIDTH; x++) {
                if (screenX + x >= 0 && screenY + y >= 0) {
                    screen[screenY + y][screenX + x] = '=';
                }
            }
        }
    }
    
    // Render collectibles
    for (int i = 0; i < numCollectibles; i++) {
        if (!collectibles[i].active) continue;
        
        int screenX = (int)collectibles[i].x - cameraX;
        int screenY = (int)collectibles[i].y;
        
        if (screenX >= 0 && screenX < SCREEN_WIDTH && 
            screenY >= 0 && screenY < SCREEN_HEIGHT) {
            screen[screenY][screenX] = '$';
        }
    }
    
    // Render enemies
    for (int i = 0; i < numEnemies; i++) {
        if (!enemies[i].active) continue;
        
        int screenX = (int)enemies[i].x - cameraX;
        int screenY = (int)enemies[i].y;
        
        for (int y = 0; y < enemies[i].height && screenY + y < SCREEN_HEIGHT; y++) {
            for (int x = 0; x < enemies[i].width && screenX + x < SCREEN_WIDTH; x++) {
                if (screenX + x >= 0 && screenY + y >= 0) {
                    screen[screenY + y][screenX + x] = 'E';
                }
            }
        }
    }
    
    // Render player
    int playerScreenX = (int)player.x - cameraX;
    int playerScreenY = (int)player.y;
    
    for (int y = 0; y < player.height && playerScreenY + y < SCREEN_HEIGHT; y++) {
        for (int x = 0; x < player.width && playerScreenX + x < SCREEN_WIDTH; x++) {
            if (playerScreenX + x >= 0 && playerScreenY + y >= 0) {
                screen[playerScreenY + y][playerScreenX + x] = '#';
            }
        }
    }
    
    // Print screen
    for (int y = 0; y < SCREEN_HEIGHT - 3; y++) {
        for (int x = 0; x < SCREEN_WIDTH; x++) {
            putchar(screen[y][x]);
        }
        putchar('\n');
    }
    
    printf("================================================================================\n");
    
    if (!gameRunning) {
        printf("GAME OVER! Final Score: %d\n", player.score);
    }
}

// Handle input
void handleInput() {
    char key = input();
    
    switch (key) {
        case 'a':
        case 'A':
            player.vx = -2.0f;
            break;
        case 'd':
        case 'D':
            player.vx = 2.0f;
            break;
        case 'w':
        case 'W':
        case ' ':
            if (player.onGround) {
                player.vy = -JUMP_FORCE;
                player.onGround = 0;
            }
            break;
        case 'q':
        case 'Q':
            gameRunning = 0;
            break;
    }
    
    // Apply friction
    if (key != 'a' && key != 'A' && key != 'd' && key != 'D') {
        player.vx *= 0.8f;
        if (fabs(player.vx) < 0.1f) player.vx = 0;
    }
}

// Update camera
void updateCamera() {
    // Follow player with some offset
    cameraX = (int)(player.x - SCREEN_WIDTH / 2);
    if (cameraX < 0) cameraX = 0;
}

// Main game loop
void gameLoop() {
    while (gameRunning) {
        handleInput();
        updatePlayerPhysics();
        updateEnemies();
        updateCollectibles();
        updateCamera();
        
        renderGame();
        
        if (checkLevelComplete()) {
            level++;
            player.score += 1000 * level;
            initializeLevel();
            
            // Increase difficulty
            for (int i = 0; i < numEnemies; i++) {
                enemies[i].vx *= 1.2f;
            }
        }
        
        // Simple delay
        for (volatile int i = 0; i < 100000; i++);
    }
}

int main() {
    printf("=== PLATFORMER GAME ===\n");
    printf("Classic side-scrolling platformer in C\n");
    printf("Controls: A/D to move, W or SPACE to jump, Q to quit\n");
    printf("Collect all $ symbols to complete the level!\n");
    printf("Press ENTER to start...\n");
    getchar();
    
    initializeLevel();
    gameLoop();
    
    printf("Thanks for playing!\n");
    return 0;
}
