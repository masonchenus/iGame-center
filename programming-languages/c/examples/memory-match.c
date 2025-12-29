/*
 * Memory Match Game - C Implementation
 * Classic card matching memory game
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdbool.h>
#include <string.h>

#define ROWS 4
#define COLS 4
#define TOTAL_CARDS ROWS * COLS
#define MAX_PAIRS TOTAL_CARDS / 2

// Card structure
typedef struct {
    int value;      // 0 means face down, 1-8 are pairs
    bool revealed;  // Currently visible
    bool matched;   // Successfully matched
} Card;

// Game state
Card cards[TOTAL_CARDS];
int score = 0;
int moves = 0;
int pairsFound = 0;
bool gameOver = false;

// Function prototypes
void initializeGame();
void shuffleCards();
void printBoard();
void printCard(int index);
void revealCard(int index);
void hideCard(int index);
bool isGameComplete();
void processMove();
int getPlayerChoice();
bool isValidMove(int choice);

// Initialize the game
void initializeGame() {
    // Initialize all cards
    for (int i = 0; i < TOTAL_CARDS; i++) {
        cards[i].value = 0;
        cards[i].revealed = false;
        cards[i].matched = false;
    }
    
    score = 0;
    moves = 0;
    pairsFound = 0;
    gameOver = false;
    
    // Set up pairs
    for (int pair = 1; pair <= MAX_PAIRS; pair++) {
        int positions[2];
        // Find two random positions
        do {
            positions[0] = rand() % TOTAL_CARDS;
            positions[1] = rand() % TOTAL_CARDS;
        } while (positions[0] == positions[1] || cards[positions[0]].value != 0 || cards[positions[1]].value != 0);
        
        cards[positions[0]].value = pair;
        cards[positions[1]].value = pair;
    }
}

// Print the game board
void printBoard() {
    system("cls"); // Clear screen
    
    printf("MEMORY MATCH GAME\n");
    printf("Score: %d | Moves: %d | Pairs Found: %d/%d\n", 
           score, moves, pairsFound, MAX_PAIRS);
    printf("==============================================\n");
    
    // Print column headers
    printf("    ");
    for (int j = 0; j < COLS; j++) {
        printf("%2d  ", j + 1);
    }
    printf("\n");
    
    // Print rows with cards
    for (int i = 0; i < ROWS; i++) {
        printf("%2d ", i + 1);
        for (int j = 0; j < COLS; j++) {
            int index = i * COLS + j;
            printf(" ");
            if (cards[index].revealed || cards[index].matched) {
                printf("%2d ", cards[index].value);
            } else {
                printf("?? ");
            }
            printf(" ");
        }
        printf("\n");
    }
    
    printf("==============================================\n");
    printf("Enter two card positions (e.g., '1 1 2 3' for row1,col1 and row2,col3)\n");
    printf("Or 'q' to quit\n");
    
    if (gameOver) {
        printf("*** GAME COMPLETE! ***\n");
        printf("Final Score: %d in %d moves\n", score, moves);
    }
}

// Process a player move
void processMove() {
    if (gameOver) return;
    
    printf("Enter your move: ");
    
    int r1, c1, r2, c2;
    if (scanf("%d %d %d %d", &r1, &c1, &r2, &c2) == 4) {
        // Convert to 0-based indexing
        r1--; c1--; r2--; c2--;
        
        // Validate input
        if (r1 < 0 || r1 >= ROWS || c1 < 0 || c1 >= COLS ||
            r2 < 0 || r2 >= ROWS || c2 < 0 || c2 >= COLS) {
            printf("Invalid positions! Please enter row and column numbers between 1 and %d.\n", ROWS);
            getchar();
            return;
        }
        
        int index1 = r1 * COLS + c1;
        int index2 = r2 * COLS + c2;
        
        if (index1 == index2) {
            printf("You can't select the same card twice!\n");
            getchar();
            return;
        }
        
        if (cards[index1].matched || cards[index1].revealed ||
            cards[index2].matched || cards[index2].revealed) {
            printf("One or both cards are already revealed or matched!\n");
            getchar();
            return;
        }
        
        // Reveal both cards
        cards[index1].revealed = true;
        cards[index2].revealed = true;
        
        printBoard();
        
        // Check for match
        moves++;
        
        if (cards[index1].value == cards[index2].value) {
            // Match found!
            cards[index1].matched = true;
            cards[index2].matched = true;
            pairsFound++;
            
            // Calculate score based on moves taken
            int pairScore = 1000 - (moves * 10);
            if (pairScore < 100) pairScore = 100;
            score += pairScore;
            
            printf("*** MATCH! ***\n");
            printf("Found pair %d! +%d points\n", cards[index1].value, pairScore);
        } else {
            // No match - hide after delay
            printf("*** NO MATCH ***\n");
            printf("Cards %d and %d don't match. Hiding in 2 seconds...\n", 
                   cards[index1].value, cards[index2].value);
            
            // Simple delay (in real implementation, would use sleep)
            for (volatile long i = 0; i < 200000000; i++);
            
            cards[index1].revealed = false;
            cards[index2].revealed = false;
        }
        
        getchar(); // Clear input buffer
        
        // Check if game is complete
        if (isGameComplete()) {
            gameOver = true;
            score += 500; // Bonus for completing game
        }
    } else {
        // Check for quit command
        char quit[10];
        fgets(quit, sizeof(quit), stdin);
        if (quit[0] == 'q' || quit[0] == 'Q') {
            gameOver = true;
        } else {
            printf("Invalid input! Please enter four numbers or 'q' to quit.\n");
        }
    }
}

// Check if the game is complete
bool isGameComplete() {
    for (int i = 0; i < TOTAL_CARDS; i++) {
        if (!cards[i].matched) {
            return false;
        }
    }
    return true;
}

// Main game loop
void gameLoop() {
    while (!gameOver) {
        printBoard();
        processMove();
    }
    
    // Final score display
    printf("\n");
    printBoard();
    printf("*** GAME OVER ***\n");
    printf("Final Score: %d\n", score);
    printf("Total Moves: %d\n", moves);
    printf("Average moves per pair: %.1f\n", (float)moves / MAX_PAIRS);
    
    if (moves <= MAX_PAIRS * 2) {
        printf("Excellent memory!\n");
    } else if (moves <= MAX_PAIRS * 3) {
        printf("Good job!\n");
    } else {
        printf("Keep practicing to improve your memory!\n");
    }
}

int main() {
    printf("=== MEMORY MATCH GAME ===\n");
    printf("Classic card matching memory game\n");
    printf("Find all pairs of matching cards!\n");
    printf("Grid size: %dx%d (%d pairs total)\n", ROWS, COLS, MAX_PAIRS);
    printf("Controls: Enter row and column numbers for two cards\n");
    printf("Press ENTER to start...\n");
    getchar();
    
    srand(time(NULL)); // Seed random number generator
    initializeGame();
    gameLoop();
    
    printf("Thanks for playing!\n");
    return 0;
}
