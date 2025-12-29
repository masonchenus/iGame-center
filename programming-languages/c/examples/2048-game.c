/*
 * 2048 Game - C Implementation
 * Classic sliding puzzle game with numbers
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdbool.h>

#define GRID_SIZE 4
#define WIN_VALUE 2048

// Game state
int grid[GRID_SIZE][GRID_SIZE];
int score = 0;
bool gameWon = false;
bool gameOver = false;

// Function prototypes
void initializeGame();
void addRandomTile();
void printGrid();
bool canMove();
void moveLeft();
void moveRight();
void moveUp();
void moveDown();
bool mergeRowLeft(int row[]);
bool canMerge(int a, int b);
void shiftRowLeft(int row[]);
bool hasWon();
void resetGame();

// Initialize the game
void initializeGame() {
    // Clear grid
    for (int i = 0; i < GRID_SIZE; i++) {
        for (int j = 0; j < GRID_SIZE; j++) {
            grid[i][j] = 0;
        }
    }
    
    score = 0;
    gameWon = false;
    gameOver = false;
    
    // Add two initial tiles
    addRandomTile();
    addRandomTile();
}

// Add a random tile (2 or 4)
void addRandomTile() {
    int emptyCells[GRID_SIZE * GRID_SIZE][2];
    int emptyCount = 0;
    
    // Find all empty cells
    for (int i = 0; i < GRID_SIZE; i++) {
        for (int j = 0; j < GRID_SIZE; j++) {
            if (grid[i][j] == 0) {
                emptyCells[emptyCount][0] = i;
                emptyCells[emptyCount][1] = j;
                emptyCount++;
            }
        }
    }
    
    if (emptyCount > 0) {
        // Randomly select an empty cell
        int randomIndex = rand() % emptyCount;
        int row = emptyCells[randomIndex][0];
        int col = emptyCells[randomIndex][1];
        
        // Add tile (90% chance of 2, 10% chance of 4)
        grid[row][col] = (rand() % 10 == 0) ? 4 : 2;
    }
}

// Print the game grid
void printGrid() {
    system("cls"); // Clear screen
    
    printf("2048 GAME - C Edition\n");
    printf("Score: %d\n", score);
    
    if (gameWon) {
        printf("*** YOU WIN! ***\n");
    }
    
    if (gameOver) {
        printf("*** GAME OVER ***\n");
    }
    
    printf("Controls: WASD to move, R to restart, Q to quit\n");
    printf("===========================================\n");
    
    // Print grid
    for (int i = 0; i < GRID_SIZE; i++) {
        printf("|");
        for (int j = 0; j < GRID_SIZE; j++) {
            if (grid[i][j] == 0) {
                printf("    |");
            } else {
                printf("%4d|", grid[i][j]);
            }
        }
        printf("\n");
    }
    
    printf("===========================================\n");
}

// Check if the game can still move
bool canMove() {
    // Check for empty cells
    for (int i = 0; i < GRID_SIZE; i++) {
        for (int j = 0; j < GRID_SIZE; j++) {
            if (grid[i][j] == 0) return true;
        }
    }
    
    // Check for possible merges
    for (int i = 0; i < GRID_SIZE; i++) {
        for (int j = 0; j < GRID_SIZE; j++) {
            if (j < GRID_SIZE - 1 && grid[i][j] == grid[i][j + 1]) return true;
            if (i < GRID_SIZE - 1 && grid[i][j] == grid[i + 1][j]) return true;
        }
    }
    
    return false;
}

// Move all tiles to the left
void moveLeft() {
    bool moved = false;
    
    for (int i = 0; i < GRID_SIZE; i++) {
        int row[GRID_SIZE];
        for (int j = 0; j < GRID_SIZE; j++) {
            row[j] = grid[i][j];
        }
        
        shiftRowLeft(row);
        if (mergeRowLeft(row)) {
            moved = true;
        }
        
        for (int j = 0; j < GRID_SIZE; j++) {
            grid[i][j] = row[j];
        }
    }
    
    if (moved) addRandomTile();
}

// Move all tiles to the right
void moveRight() {
    bool moved = false;
    
    for (int i = 0; i < GRID_SIZE; i++) {
        int row[GRID_SIZE];
        for (int j = 0; j < GRID_SIZE; j++) {
            row[j] = grid[i][GRID_SIZE - 1 - j];
        }
        
        shiftRowLeft(row);
        if (mergeRowLeft(row)) {
            moved = true;
        }
        
        for (int j = 0; j < GRID_SIZE; j++) {
            grid[i][GRID_SIZE - 1 - j] = row[j];
        }
    }
    
    if (moved) addRandomTile();
}

// Move all tiles up
void moveUp() {
    bool moved = false;
    
    for (int j = 0; j < GRID_SIZE; j++) {
        int col[GRID_SIZE];
        for (int i = 0; i < GRID_SIZE; i++) {
            col[i] = grid[i][j];
        }
        
        shiftRowLeft(col);
        if (mergeRowLeft(col)) {
            moved = true;
        }
        
        for (int i = 0; i < GRID_SIZE; i++) {
            grid[i][j] = col[i];
        }
    }
    
    if (moved) addRandomTile();
}

// Move all tiles down
void moveDown() {
    bool moved = false;
    
    for (int j = 0; j < GRID_SIZE; j++) {
        int col[GRID_SIZE];
        for (int i = 0; i < GRID_SIZE; i++) {
            col[i] = grid[GRID_SIZE - 1 - i][j];
        }
        
        shiftRowLeft(col);
        if (mergeRowLeft(col)) {
            moved = true;
        }
        
        for (int i = 0; i < GRID_SIZE; i++) {
            grid[GRID_SIZE - 1 - i][j] = col[i];
        }
    }
    
    if (moved) addRandomTile();
}

// Shift a row to the left (remove zeros)
void shiftRowLeft(int row[]) {
    int temp[GRID_SIZE];
    int index = 0;
    
    for (int i = 0; i < GRID_SIZE; i++) {
        if (row[i] != 0) {
            temp[index++] = row[i];
        }
    }
    
    for (int i = index; i < GRID_SIZE; i++) {
        temp[i] = 0;
    }
    
    for (int i = 0; i < GRID_SIZE; i++) {
        row[i] = temp[i];
    }
}

// Merge adjacent tiles in a row
bool mergeRowLeft(int row[]) {
    bool merged = false;
    
    for (int i = 0; i < GRID_SIZE - 1; i++) {
        if (row[i] != 0 && row[i] == row[i + 1]) {
            row[i] *= 2;
            row[i + 1] = 0;
            score += row[i];
            merged = true;
        }
    }
    
    if (merged) {
        shiftRowLeft(row);
    }
    
    return merged;
}

// Check if the player has won
bool hasWon() {
    for (int i = 0; i < GRID_SIZE; i++) {
        for (int j = 0; j < GRID_SIZE; j++) {
            if (grid[i][j] >= WIN_VALUE) {
                return true;
            }
        }
    }
    return false;
}

// Reset the game
void resetGame() {
    initializeGame();
}

// Main game loop
void gameLoop() {
    char input;
    
    while (!gameOver) {
        printGrid();
        
        if (gameWon && !hasWon()) {
            gameWon = false; // Continue playing after winning
        }
        
        if (!gameWon && hasWon()) {
            gameWon = true;
        }
        
        if (!canMove()) {
            gameOver = true;
            break;
        }
        
        printf("Enter move (WASD, R for restart, Q to quit): ");
        scanf(" %c", &input);
        
        switch (input) {
            case 'w':
            case 'W':
                moveUp();
                break;
            case 's':
            case 'S':
                moveDown();
                break;
            case 'a':
            case 'A':
                moveLeft();
                break;
            case 'd':
            case 'D':
                moveRight();
                break;
            case 'r':
            case 'R':
                resetGame();
                break;
            case 'q':
            case 'Q':
                gameOver = true;
                break;
            default:
                printf("Invalid input! Please use WASD keys.\n");
                printf("Press Enter to continue...");
                getchar();
                break;
        }
    }
    
    printGrid();
    printf("Final Score: %d\n", score);
    
    if (gameWon) {
        printf("Congratulations! You reached %d!\n", WIN_VALUE);
    }
}

int main() {
    printf("=== 2048 GAME ===\n");
    printf("Classic sliding puzzle game\n");
    printf("Goal: Reach the 2048 tile!\n");
    printf("Controls: WASD to move tiles, R to restart, Q to quit\n");
    printf("Press ENTER to start...\n");
    getchar();
    
    srand(time(NULL)); // Seed random number generator
    initializeGame();
    gameLoop();
    
    printf("Thanks for playing!\n");
    return 0;
}
