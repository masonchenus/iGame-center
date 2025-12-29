/*
 * Sudoku Solver - C Implementation
 * Classic number puzzle with backtracking solver
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define GRID_SIZE 9
#define BOX_SIZE 3

// Sudoku grid (0 = empty)
int grid[GRID_SIZE][GRID_SIZE];

// Function prototypes
void printGrid();
bool isValid(int row, int col, int num);
bool solveSudoku();
bool findEmpty(int* row, int* col);
bool isSafe(int row, int col, int num);
void clearGrid();
void loadPuzzle();
bool checkSolution();
void printRules();
void playInteractive();

// Print the sudoku grid
void printGrid() {
    printf("\n    1 2 3   4 5 6   7 8 9\n");
    printf("  +-------+-------+-------+\n");
    
    for (int i = 0; i < GRID_SIZE; i++) {
        printf("%d |", i + 1);
        
        for (int j = 0; j < GRID_SIZE; j++) {
            if (grid[i][j] == 0) {
                printf(" .");
            } else {
                printf(" %d", grid[i][j]);
            }
            
            if ((j + 1) % 3 == 0 && j < GRID_SIZE - 1) {
                printf(" |");
            } else {
                printf(" ");
            }
        }
        printf("|\n");
        
        if ((i + 1) % 3 == 0 && i < GRID_SIZE - 1) {
            printf("  +-------+-------+-------+\n");
        }
    }
    
    printf("  +-------+-------+-------+\n");
}

// Check if a number is valid in a given position
bool isValid(int row, int col, int num) {
    // Check row
    for (int j = 0; j < GRID_SIZE; j++) {
        if (grid[row][j] == num) {
            return false;
        }
    }
    
    // Check column
    for (int i = 0; i < GRID_SIZE; i++) {
        if (grid[i][col] == num) {
            return false;
        }
    }
    
    // Check 3x3 box
    int boxRow = row - (row % BOX_SIZE);
    int boxCol = col - (col % BOX_SIZE);
    
    for (int i = boxRow; i < boxRow + BOX_SIZE; i++) {
        for (int j = boxCol; j < boxCol + BOX_SIZE; j++) {
            if (grid[i][j] == num) {
                return false;
            }
        }
    }
    
    return true;
}

// Main Sudoku solver using backtracking
bool solveSudoku() {
    int row, col;
    
    // Find empty cell
    if (!findEmpty(&row, &col)) {
        return true; // No empty cells - solved!
    }
    
    // Try numbers 1-9
    for (int num = 1; num <= GRID_SIZE; num++) {
        if (isValid(row, col, num)) {
            grid[row][col] = num;
            
            // Recursively solve
            if (solveSudoku()) {
                return true;
            }
            
            // Backtrack
            grid[row][col] = 0;
        }
    }
    
    return false; // No solution found
}

// Find empty cell
bool findEmpty(int* row, int* col) {
    for (int i = 0; i < GRID_SIZE; i++) {
        for (int j = 0; j < GRID_SIZE; j++) {
            if (grid[i][j] == 0) {
                *row = i;
                *col = j;
                return true;
            }
        }
    }
    return false;
}

// Alternative safety check for solver
bool isSafe(int row, int col, int num) {
    // Check if num is already in this row
    for (int j = 0; j < GRID_SIZE; j++) {
        if (grid[row][j] == num) {
            return false;
        }
    }
    
    // Check if num is already in this column
    for (int i = 0; i < GRID_SIZE; i++) {
        if (grid[i][col] == num) {
            return false;
        }
    }
    
    // Check if num is already in this 3x3 box
    int boxRow = row - (row % BOX_SIZE);
    int boxCol = col - (col % BOX_SIZE);
    
    for (int i = boxRow; i < boxRow + BOX_SIZE; i++) {
        for (int j = boxCol; j < boxCol + BOX_SIZE; j++) {
            if (grid[i][j] == num) {
                return false;
            }
        }
    }
    
    return true;
}

// Clear the grid
void clearGrid() {
    for (int i = 0; i < GRID_SIZE; i++) {
        for (int j = 0; j < GRID_SIZE; j++) {
            grid[i][j] = 0;
        }
    }
}

// Load a sample puzzle
void loadPuzzle() {
    clearGrid();
    
    // Easy puzzle
    int puzzle[GRID_SIZE][GRID_SIZE] = {
        {5, 3, 0, 0, 7, 0, 0, 0, 0},
        {6, 0, 0, 1, 9, 5, 0, 0, 0},
        {0, 9, 8, 0, 0, 0, 0, 6, 0},
        {8, 0, 0, 0, 6, 0, 0, 0, 3},
        {4, 0, 0, 8, 0, 3, 0, 0, 1},
        {7, 0, 0, 0, 2, 0, 0, 0, 6},
        {0, 6, 0, 0, 0, 0, 2, 8, 0},
        {0, 0, 0, 4, 1, 9, 0, 0, 5},
        {0, 0, 0, 0, 8, 0, 0, 7, 9}
    };
    
    for (int i = 0; i < GRID_SIZE; i++) {
        for (int j = 0; j < GRID_SIZE; j++) {
            grid[i][j] = puzzle[i][j];
        }
    }
}

// Check if the current solution is valid
bool checkSolution() {
    for (int i = 0; i < GRID_SIZE; i++) {
        for (int j = 0; j < GRID_SIZE; j++) {
            if (grid[i][j] == 0) {
                return false; // Grid not completely filled
            }
            if (!isValid(i, j, grid[i][j])) {
                return false; // Invalid number in position
            }
        }
    }
    return true;
}

// Print game rules
void printRules() {
    printf("\n=== SUDOKU RULES ===\n");
    printf("1. Fill the 9x9 grid so that each row contains digits 1-9\n");
    printf("2. Each column must contain digits 1-9\n");
    printf("3. Each 3x3 box must contain digits 1-9\n");
    printf("4. No digit can repeat in any row, column, or box\n");
    printf("5. Use the solver to automatically solve the puzzle\n");
    printf("6. Use the interactive mode to try solving yourself\n");
    printf("===================\n");
}

// Interactive play mode
void playInteractive() {
    int row, col, num;
    bool playing = true;
    
    printf("\n=== INTERACTIVE MODE ===\n");
    printf("Enter coordinates and number to place.\n");
    printf("Format: row col number (1-9 each, 0 0 0 to quit)\n");
    
    while (playing) {
        printGrid();
        
        printf("Enter your move: ");
        if (scanf("%d %d %d", &row, &col, &num) == 3) {
            if (row == 0 && col == 0 && num == 0) {
                break;
            }
            
            // Convert to 0-based indexing
            row--;
            col--;
            
            if (row < 0 || row >= GRID_SIZE || col < 0 || col >= GRID_SIZE) {
                printf("Invalid coordinates! Use values 1-9.\n");
                continue;
            }
            
            if (num < 0 || num > 9) {
                printf("Invalid number! Use values 1-9 or 0 to clear.\n");
                continue;
            }
            
            if (num == 0) {
                grid[row][col] = 0;
                printf("Cell cleared.\n");
            } else if (isValid(row, col, num)) {
                grid[row][col] = num;
                printf("Number placed.\n");
            } else {
                printf("Invalid move! Number violates Sudoku rules.\n");
            }
            
            if (checkSolution()) {
                printf("\n*** CONGRATULATIONS! You solved the puzzle! ***\n");
                playing = false;
            }
        } else {
            printf("Invalid input! Please enter three numbers.\n");
            break;
        }
    }
}

// Main program
int main() {
    int choice;
    
    printf("=== SUDOKU SOLVER ===\n");
    printf("Classic number puzzle with automatic solver\n");
    
    while (true) {
        printf("\n1. Solve puzzle\n");
        printf("2. Interactive play\n");
        printf("3. View rules\n");
        printf("4. Quit\n");
        printf("Choose an option: ");
        
        scanf("%d", &choice);
        
        switch (choice) {
            case 1:
                printf("\nLoading puzzle...\n");
                loadPuzzle();
                printGrid();
                
                printf("\nSolving puzzle...\n");
                if (solveSudoku()) {
                    printf("*** PUZZLE SOLVED! ***\n");
                    printGrid();
                } else {
                    printf("*** NO SOLUTION FOUND ***\n");
                }
                break;
                
            case 2:
                printf("\nStarting interactive mode...\n");
                loadPuzzle();
                playInteractive();
                break;
                
            case 3:
                printRules();
                break;
                
            case 4:
                printf("Thanks for playing Sudoku!\n");
                return 0;
                
            default:
                printf("Invalid choice! Please try again.\n");
                break;
        }
        
        printf("\nPress Enter to continue...");
        getchar();
        getchar(); // Clear input buffer
    }
    
    return 0;
}
