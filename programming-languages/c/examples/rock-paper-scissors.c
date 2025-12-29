/*
 * Rock Paper Scissors - C Implementation
 * Classic hand game with computer AI
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdbool.h>

#define MAX_ROUNDS 10

typedef enum {
    ROCK = 1,
    PAPER = 2,
    SCISSORS = 3
} Choice;

typedef enum {
    PLAYER_WIN,
    COMPUTER_WIN,
    TIE
} Result;

typedef struct {
    int playerScore;
    int computerScore;
    int roundsPlayed;
    int totalWins;
    int totalLosses;
    int totalTies;
    bool gameOver;
} GameState;

// Function prototypes
void printMenu();
Choice getPlayerChoice();
Choice getComputerChoice();
Result determineWinner(Choice player, Choice computer);
void printChoice(Choice choice);
void updateScore(Result result, GameState* state);
void printScore(GameState* state);
void playRound(GameState* state);
void printRules();
void resetGame(GameState* state);
void saveStats(GameState* state);
void loadStats(GameState* state);

// Print the main menu
void printMenu() {
    printf("\n=== ROCK PAPER SCISSORS ===\n");
    printf("1. Play Game\n");
    printf("2. View Rules\n");
    printf("3. Statistics\n");
    printf("4. Quit\n");
    printf("Choose an option: ");
}

// Get player's choice
Choice getPlayerChoice() {
    int choice;
    
    printf("\nYour move:\n");
    printf("1. Rock\n");
    printf("2. Paper\n");
    printf("3. Scissors\n");
    printf("Enter your choice (1-3): ");
    
    scanf("%d", &choice);
    
    while (choice < 1 || choice > 3) {
        printf("Invalid choice! Please enter 1, 2, or 3: ");
        scanf("%d", &choice);
    }
    
    return (Choice)choice;
}

// Get computer's random choice
Choice getComputerChoice() {
    return (Choice)(rand() % 3 + 1);
}

// Determine the winner
Result determineWinner(Choice player, Choice computer) {
    if (player == computer) {
        return TIE;
    }
    
    switch (player) {
        case ROCK:
            return (computer == SCISSORS) ? PLAYER_WIN : COMPUTER_WIN;
        case PAPER:
            return (computer == ROCK) ? PLAYER_WIN : COMPUTER_WIN;
        case SCISSORS:
            return (computer == PAPER) ? PLAYER_WIN : COMPUTER_WIN;
        default:
            return TIE;
    }
}

// Print choice as string
void printChoice(Choice choice) {
    switch (choice) {
        case ROCK:
            printf("ROCK");
            break;
        case PAPER:
            printf("PAPER");
            break;
        case SCISSORS:
            printf("SCISSORS");
            break;
    }
}

// Update game state based on result
void updateScore(Result result, GameState* state) {
    state->roundsPlayed++;
    
    switch (result) {
        case PLAYER_WIN:
            state->playerScore++;
            state->totalWins++;
            printf("*** YOU WIN! ***\n");
            break;
        case COMPUTER_WIN:
            state->computerScore++;
            state->totalLosses++;
            printf("*** COMPUTER WINS! ***\n");
            break;
        case TIE:
            state->totalTies++;
            printf("*** TIE! ***\n");
            break;
    }
}

// Print current score
void printScore(GameState* state) {
    printf("\n--- ROUND %d SCORE ---\n", state->roundsPlayed + 1);
    printf("Player: %d\n", state->playerScore);
    printf("Computer: %d\n", state->computerScore);
    printf("Ties: %d\n", state->totalTies);
    printf("---------------------\n");
}

// Play a single round
void playRound(GameState* state) {
    Choice playerChoice, computerChoice;
    Result result;
    
    printf("\n*** ROUND %d ***\n", state->roundsPlayed + 1);
    
    playerChoice = getPlayerChoice();
    computerChoice = getComputerChoice();
    
    printf("You chose: ");
    printChoice(playerChoice);
    printf("\nComputer chose: ");
    printChoice(computerChoice);
    printf("\n");
    
    result = determineWinner(playerChoice, computerChoice);
    updateScore(result, state);
    
    printScore(state);
}

// Print game rules
void printRules() {
    printf("\n=== GAME RULES ===\n");
    printf("Rock beats Scissors\n");
    printf("Scissors beats Paper\n");
    printf("Paper beats Rock\n");
    printf("\nWin by choosing the hand that beats your opponent's choice.\n");
    printf("First to %d rounds wins!\n", MAX_ROUNDS);
    printf("==================\n");
}

// Reset game state
void resetGame(GameState* state) {
    state->playerScore = 0;
    state->computerScore = 0;
    state->roundsPlayed = 0;
    state->gameOver = false;
}

// Save statistics to file
void saveStats(GameState* state) {
    FILE* file = fopen("rps_stats.txt", "w");
    if (file) {
        fprintf(file, "%d\n%d\n%d\n", state->totalWins, state->totalLosses, state->totalTies);
        fclose(file);
        printf("Statistics saved!\n");
    }
}

// Load statistics from file
void loadStats(GameState* state) {
    FILE* file = fopen("rps_stats.txt", "r");
    if (file) {
        fscanf(file, "%d\n%d\n%d\n", &state->totalWins, &state->totalLosses, &state->totalTies);
        fclose(file);
        printf("Statistics loaded!\n");
    }
}

// Main game loop
void gameLoop() {
    GameState state = {0};
    int choice;
    bool running = true;
    
    loadStats(&state);
    
    while (running) {
        printMenu();
        scanf("%d", &choice);
        
        switch (choice) {
            case 1:
                resetGame(&state);
                
                printf("\n*** STARTING NEW GAME ***\n");
                printf("First to %d wins!\n", MAX_ROUNDS);
                
                while (!state.gameOver && state.roundsPlayed < MAX_ROUNDS) {
                    playRound(&state);
                    
                    // Check if someone has won
                    if (state.playerScore > MAX_ROUNDS / 2) {
                        state.gameOver = true;
                        printf("\n*** CONGRATULATIONS! YOU WIN THE GAME! ***\n");
                    } else if (state.computerScore > MAX_ROUNDS / 2) {
                        state.gameOver = true;
                        printf("\n*** COMPUTER WINS THE GAME! ***\n");
                    }
                }
                
                printScore(&state);
                saveStats(&state);
                printf("Game over! Press Enter to continue...");
                getchar();
                break;
                
            case 2:
                printRules();
                printf("Press Enter to continue...");
                getchar();
                break;
                
            case 3:
                printf("\n=== LIFETIME STATISTICS ===\n");
                printf("Total Wins: %d\n", state.totalWins);
                printf("Total Losses: %d\n", state.totalLosses);
                printf("Total Ties: %d\n", state.totalTies);
                
                if (state.totalWins + state.totalLosses > 0) {
                    float winRate = (float)state.totalWins / (state.totalWins + state.totalLosses) * 100;
                    printf("Win Rate: %.1f%%\n", winRate);
                }
                printf("===========================\n");
                printf("Press Enter to continue...");
                getchar();
                break;
                
            case 4:
                printf("Thanks for playing!\n");
                saveStats(&state);
                running = false;
                break;
                
            default:
                printf("Invalid choice! Please try again.\n");
                break;
        }
    }
}

int main() {
    printf("=== ROCK PAPER SCISSORS ===\n");
    printf("Classic hand game against computer\n");
    printf("Press ENTER to start...");
    getchar();
    
    srand(time(NULL)); // Seed random number generator
    gameLoop();
    
    return 0;
}
