/*
 * Tic-Tac-Toe - C Implementation
 * Classic game with AI opponent
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <time.h>

#define BOARD_SIZE 3
#define EMPTY ' '
#define PLAYER 'X'
#define AI 'O'

char board[BOARD_SIZE][BOARD_SIZE];
int score = 0;
bool gameOver = false;
bool playerTurn = true;

// Function prototypes
void initializeBoard();
void printBoard();
bool isValidMove(int row, int col);
void makeMove(int row, int col, char player);
bool checkWin(char player);
bool checkTie();
int minimax(int depth, bool isMaximizing);
void aiMove();
bool isBoardFull();
void resetGame();

// Initialize the game board
void initializeBoard() {
    for (int i = 0; i < BOARD_SIZE; i++) {
        for (int j = 0; j < BOARD_SIZE; j++) {
            board[i][j] = EMPTY;
        }
    }
    
    score = 0;
    gameOver = false;
    playerTurn = true;
}

// Print the current board state
void printBoard() {
    system("cls");
    
    printf("TIC-TAC-TOE\n");
    printf("Score: %d\n", score);
    printf("You are X, Computer is O\n");
    if (playerTurn && !gameOver) {
        printf("Your turn! Enter row and column (1-3): ");
    } else if (!gameOver) {
        printf("Computer is thinking...\n");
    }
    printf("================================\n");
    
    for (int i = 0; i < BOARD_SIZE; i++) {
        for (int j = 0; j < BOARD_SIZE; j++) {
            printf(" %c ", board[i][j]);
            if (j < BOARD_SIZE - 1) printf("|");
        }
        printf("\n");
        if (i < BOARD_SIZE - 1) {
            printf("---+---+---\n");
        }
    }
    printf("================================\n");
}

// Check if a move is valid
bool isValidMove(int row, int col) {
    return row >= 0 && row < BOARD_SIZE && 
           col >= 0 && col < BOARD_SIZE && 
           board[row][col] == EMPTY;
}

// Make a move on the board
void makeMove(int row, int col, char player) {
    if (isValidMove(row, col)) {
        board[row][col] = player;
    }
}

// Check if a player has won
bool checkWin(char player) {
    // Check rows
    for (int i = 0; i < BOARD_SIZE; i++) {
        if (board[i][0] == player && 
            board[i][1] == player && 
            board[i][2] == player) {
            return true;
        }
    }
    
    // Check columns
    for (int j = 0; j < BOARD_SIZE; j++) {
        if (board[0][j] == player && 
            board[1][j] == player && 
            board[2][j] == player) {
            return true;
        }
    }
    
    // Check diagonals
    if (board[0][0] == player && 
        board[1][1] == player && 
        board[2][2] == player) {
        return true;
    }
    
    if (board[0][2] == player && 
        board[1][1] == player && 
        board[2][0] == player) {
        return true;
    }
    
    return false;
}

// Check if the game is a tie
bool checkTie() {
    return isBoardFull() && !checkWin(PLAYER) && !checkWin(AI);
}

// Minimax algorithm for AI
int minimax(int depth, bool isMaximizing) {
    if (checkWin(AI)) return 10 - depth;
    if (checkWin(PLAYER)) return depth - 10;
    if (isBoardFull()) return 0;
    
    if (isMaximizing) {
        int bestScore = -1000;
        for (int i = 0; i < BOARD_SIZE; i++) {
            for (int j = 0; j < BOARD_SIZE; j++) {
                if (board[i][j] == EMPTY) {
                    board[i][j] = AI;
                    int score = minimax(depth + 1, false);
                    board[i][j] = EMPTY;
                    if (score > bestScore) {
                        bestScore = score;
                    }
                }
            }
        }
        return bestScore;
    } else {
        int bestScore = 1000;
        for (int i = 0; i < BOARD_SIZE; i++) {
            for (int j = 0; j < BOARD_SIZE; j++) {
                if (board[i][j] == EMPTY) {
                    board[i][j] = PLAYER;
                    int score = minimax(depth + 1, true);
                    board[i][j] = EMPTY;
                    if (score < bestScore) {
                        bestScore = score;
                    }
                }
            }
        }
        return bestScore;
    }
}

// AI makes its move using minimax
void aiMove() {
    int bestScore = -1000;
    int bestRow = -1, bestCol = -1;
    
    for (int i = 0; i < BOARD_SIZE; i++) {
        for (int j = 0; j < BOARD_SIZE; j++) {
            if (board[i][j] == EMPTY) {
                board[i][j] = AI;
                int score = minimax(0, false);
                board[i][j] = EMPTY;
                
                if (score > bestScore) {
                    bestScore = score;
                    bestRow = i;
                    bestCol = j;
                }
            }
        }
    }
    
    if (bestRow != -1) {
        makeMove(bestRow, bestCol, AI);
    }
}

// Check if the board is full
bool isBoardFull() {
    for (int i = 0; i < BOARD_SIZE; i++) {
        for (int j = 0; j < BOARD_SIZE; j++) {
            if (board[i][j] == EMPTY) {
                return false;
            }
        }
    }
    return true;
}

// Reset the game
void resetGame() {
    initializeBoard();
}

// Main game loop
void gameLoop() {
    int row, col;
    
    while (!gameOver) {
        printBoard();
        
        if (playerTurn) {
            if (scanf("%d %d", &row, &col) == 2) {
                row--; col--; // Convert to 0-based indexing
                
                if (isValidMove(row, col)) {
                    makeMove(row, col, PLAYER);
                    playerTurn = false;
                    
                    if (checkWin(PLAYER)) {
                        gameOver = true;
                        score += 10;
                        printf("\n*** YOU WIN! ***\n");
                    } else if (checkTie()) {
                        gameOver = true;
                        printf("\n*** TIE GAME! ***\n");
                    }
                } else {
                    printf("Invalid move! Try again.\n");
                    getchar();
                }
            } else {
                printf("Invalid input! Please enter two numbers.\n");
                getchar();
            }
        } else {
            // AI's turn
            printf("Computer is making its move...\n");
            for (volatile long i = 0; i < 100000000; i++); // Simulate thinking
            
            aiMove();
            playerTurn = true;
            
            if (checkWin(AI)) {
                gameOver = true;
                printf("\n*** COMPUTER WINS! ***\n");
            } else if (checkTie()) {
                gameOver = true;
                printf("\n*** TIE GAME! ***\n");
            }
        }
    }
    
    printBoard();
    printf("Final Score: %d\n", score);
    printf("Play again? (1 = yes, 0 = no): ");
}

int main() {
    int playAgain;
    
    printf("=== TIC-TAC-TOE ===\n");
    printf("Classic game with AI opponent\n");
    printf("You are X, Computer is O\n");
    printf("Press ENTER to start...\n");
    getchar();
    
    do {
        initializeBoard();
        gameLoop();
        
        scanf("%d", &playAgain);
        getchar(); // Clear input buffer
        
        if (playAgain != 1) {
            printf("Thanks for playing!\n");
            break;
        }
    } while (true);
    
    return 0;
}
