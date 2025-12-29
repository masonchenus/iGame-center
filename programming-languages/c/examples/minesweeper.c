
/*
 * Minesweeper Game - C Implementation
 * Classic puzzle game with mines and numbers
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdbool.h>
#include <string.h>

#define ROWS 9
#define COLS 9
#define MINES 10

typedef struct {
    bool hasMine;
    bool revealed;
    bool flagged;
    int adjacentMines;
} Cell;

Cell grid[ROWS][COLS];
int score = 0;
int moves = 0;
bool gameOver = false;
bool gameWon = false;

// Function prototypes
void initializeGrid();
void placeMines();
void calculateAdjacentMines();
void printGrid();
void revealCell(int row, int col);
void toggleFlag(int row, int col);
bool isValidMove(int row, int col);
void revealAllMines();
bool checkWinCondition();
void floodFill(int row, int col);

// Initialize the game grid
void initializeGrid() {
    for (int i = 0; i < ROWS; i++) {
        for (int j = 0; j < COLS; j++) {
            grid[i][j].hasMine = false;
            grid[i][j].revealed = false;
            grid[i][j].flagged = false;
            grid[i][j].adjacentMines = 0;
        }
    }
    
    score = 0;
    moves = 0;
    gameOver = false;
    gameWon = false;
}

// Place mines randomly on the grid
void placeMines() {
    srand(time(NULL));
    int minesPlaced = 0;
    
    while (minesPlaced < MINES) {
        int row = rand() % ROWS;
        int col = rand() % COLS;
        
        if (!grid[row][col].hasMine) {
            grid[row][col].hasMine = true;
            minesPlaced++;
        }
    }
}

// Calculate number of adjacent mines for each cell
void calculateAdjacentMines() {
    for (int i = 0; i < ROWS; i++) {
        for (int j = 0; j < COLS; j++) {
            if (grid[i][j].hasMine) {
                grid[i][j].adjacentMines = -1; // Mark as mine
                continue;
            }
            
            int count = 0;
            for (int di = -1; di <= 1; di++) {
                for (int dj = -1; dj <= 1; dj++) {
                    if (di == 0 && dj == 0) continue;
                    
                    int ni = i + di;
                    int nj = j + dj;
                    
                    if (ni >= 0 && ni < ROWS && nj >= 0 && nj < COLS) {
                        if (grid[ni][nj].hasMine) {
                            count++;
                        }
                    }
                }
            }
            
            grid[i][j].adjacentMines = count;
        }
    }
}

// Print the current game state
void printGrid() {
    system("cls");
    
    printf("MINESWEEPER\n");
    printf("Score: %d | Moves: %d | Mines: %d\n", score, moves, MINES);
    printf("Controls: R row col (reveal), F row col (flag), Q to quit\n");
    printf("================================================\n");
    
    // Print column headers
    printf("    ");
    for (int j = 0; j < COLS; j++) {
        printf("%2d ", j + 1);
    }
    printf("\n");
    
    // Print grid
    for (int i = 0; i < ROWS; i++) {
        printf("%2d ", i + 1);
        for (int j = 0; j < COLS; j++) {
            if (!grid[i][j].revealed) {
                if (grid[i][j].flagged) {
                    printf(" F ");
                } else {
                    printf(" # ");
                }
            } else {
                if (grid[i][j].hasMine) {
                    printf(" * ");
                } else if (grid[i][j].adjacentMines == 0) {
                    printf(" . ");
                } else {
                    printf("%2d ", grid[i][j].adjacentMines);
                }
            }
        }
        printf("\n");
    }
    
    printf("================================================\n");
    
    if (gameOver) {
        printf("*** GAME OVER ***\n");
        printf("You hit a mine!\n");
    } else if (gameWon) {
        printf("*** CONGRATULATIONS! ***\n");
        printf("You cleared all mines!\n");
    }
}

// Reveal a cell
void revealCell(int row, int col) {
    if (!isValidMove(row, col) || grid[row][col].revealed || grid[row][col].flagged) {
        return;
    }
    
    grid[row][col].revealed = true;
    
    if (grid[row][col].hasMine) {
        gameOver = true;
        revealAllMines();
        return;
    }
    
    // If cell has no adjacent mines, reveal neighboring cells
    if (grid[row][col].adjacentMines == 0) {
        floodFill(row, col);
    }
    
    score += 10;
    moves++;
}

// Flood fill to reveal connected empty cells
void floodFill(int row, int col) {
    if (!isValidMove(row, col) || grid[row][col].revealed || grid[row][col].flagged) {
        return;
    }
    
    grid[row][col].revealed = true;
    
    if (grid[row][col].adjacentMines > 0) {
        return; // Don't flood if cell has adjacent mines
    }
    
    // Recursively reveal neighbors
    for (int di = -1; di <= 1; di++) {
        for (int dj = -1; dj <= 1; dj++) {
            if (di == 0 && dj == 0) continue;
            
            int ni = row + di;
            int nj = col + dj;
            
            if (isValidMove(ni, nj) && !grid[ni][nj].revealed && !grid[ni][nj].flagged) {
                floodFill(ni, nj);
            }
        }
    }
}

// Toggle flag on a cell
void toggleFlag(int row, int col) {
    if (!isValidMove(row, col) || grid[row][col].revealed) {
        return;
    }
    
    grid[row][col].flagged = !grid[row][col].flagged;
    moves++;
    
    if (grid[row][col].flagged) {
        score -= 5; // Penalty for flagging
    } else {
        score += 5; // Reward for unflagging
    }
}

// Check if coordinates are valid
bool isValidMove(int row, int col) {
    return row >= 0 && row < ROWS && col >= 0 && col < COLS;
}

// Reveal all mines when game is over
void revealAllMines() {
    for (int i = 0; i < ROWS; i++) {
        for (int j = 0; j < COLS; j++) {
            if (grid[i][j].hasMine) {
                grid[i][j].revealed = true;
            }
        }
    }
}

// Check if the game is won
bool checkWinCondition() {
    for (int i = 0; i < ROWS; i++) {
        for (int j = 0; j < COLS; j++) {
            if (!grid[i][j].hasMine && !grid[i][j].revealed) {
                return false;
            }
        }
    }
    return true;
}

// Process player input
void processInput() {
    char action;
    int row, col;
    
    printf("Enter command (R row col / F row col / Q): ");
    
    if (scanf(" %c %d %d", &action, &row, &col) == 3) {
        // Convert to 0-based indexing
        row--;
        col--;
        
        if (!isValidMove(row, col)) {
            printf("Invalid coordinates! Use values 1-%d for rows and columns.\n", ROWS);
            getchar();
            return;
        }
        
        switch (action) {
            case 'r':
            case 'R':
                revealCell(row, col);
                break;
            case 'f':
            case 'F':
                toggleFlag(row, col);
                break;
            default:
                printf("Invalid action! Use 'R' to reveal or 'F' to flag.\n");
                break;
        }
    } else {
        // Check for quit command
        char quit[10];
        fgets(quit, sizeof(quit), stdin);
        if (quit[0] == 'q' || quit[0] == 'Q') {
            gameOver = true;
        } else {
            printf("Invalid input! Use format: R row col or F row col or Q to quit.\n");
        }
    }
}

// Main game loop
void gameLoop() {
    while (!gameOver && !gameWon) {
        printGrid();
        processInput();
        
        if (!gameOver && checkWinCondition()) {
            gameWon = true;
            score += 1000; // Bonus for winning
        }
    }
    
    // Final display
    printGrid();
    printf("Final Score: %d\n", score);
    printf("Total Moves: %d\n", moves);
    
    if (gameWon) {
        printf("Excellent work! You cleared the minefield!\n");
    }
}

int main() {
    printf("=== MINESWEEPER ===\n");
    printf("Classic puzzle game with mines and numbers\n");
    printf("Grid size: %dx%d with %d mines\n", ROWS, COLS, MINES);
    printf("Reveal all cells except mines to win!\n");
    printf("Press ENTER to start...\n");
    getchar();
    
    initializeGame();
    gameLoop();
    
    printf("Thanks for playing!\n");
    return 0;
}

// Helper function to initialize the game
void initializeGame() {
    initializeGrid();
    placeMines();
    calculateAdjacentMines();
}
