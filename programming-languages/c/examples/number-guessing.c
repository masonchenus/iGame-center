/*
 * Number Guessing Game - C Implementation
 * Classic number guessing game with hints
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdbool.h>

#define MIN_NUMBER 1
#define MAX_NUMBER 100
#define MAX_GUESSES 7

// Game state
int targetNumber;
int currentGuess;
int guessesLeft;
int totalGames = 0;
int gamesWon = 0;
int totalGuesses = 0;
bool gameOver = false;

// Function prototypes
void initializeGame();
void playRound();
void giveHint();
void updateStats();
void printStats();
void resetGame();

// Initialize a new game
void initializeGame() {
    srand(time(NULL));
    targetNumber = MIN_NUMBER + rand() % (MAX_NUMBER - MIN_NUMBER + 1);
    currentGuess = 0;
    guessesLeft = MAX_GUESSES;
    gameOver = false;
}

// Play a single round
void playRound() {
    printf("I'm thinking of a number between %d and %d.\n", MIN_NUMBER, MAX_NUMBER);
    printf("You have %d guesses to find it.\n\n", guessesLeft);
    
    while (guessesLeft > 0 && !gameOver) {
        printf("Enter your guess: ");
        if (scanf("%d", &currentGuess) != 1) {
            printf("Invalid input! Please enter a number.\n");
            getchar();
            continue;
        }
        
        if (currentGuess < MIN_NUMBER || currentGuess > MAX_NUMBER) {
            printf("Please enter a number between %d and %d.\n", MIN_NUMBER, MAX_NUMBER);
            continue;
        }
        
        guessesLeft--;
        totalGuesses++;
        
        if (currentGuess == targetNumber) {
            printf("\n*** CONGRATULATIONS! ***\n");
            printf("You guessed the number %d in %d guesses!\n", 
                   targetNumber, MAX_GUESSES - guessesLeft);
            gamesWon++;
            gameOver = true;
        } else {
            giveHint();
            
            if (guessesLeft > 0) {
                printf("You have %d guesses left.\n\n", guessesLeft);
            } else {
                printf("\n*** GAME OVER ***\n");
                printf("The number was %d.\n", targetNumber);
            }
        }
    }
}

// Give hint based on the guess
void giveHint() {
    int difference = abs(currentGuess - targetNumber);
    
    if (difference <= 5) {
        printf("*** VERY HOT! *** ");
    } else if (difference <= 10) {
        printf("*** HOT! *** ");
    } else if (difference <= 20) {
        printf("*** WARM! *** ");
    } else {
        printf("*** COLD! *** ");
    }
    
    if (currentGuess < targetNumber) {
        printf("Too low!");
    } else {
        printf("Too high!");
    }
    
    // Additional hints based on difference
    if (difference <= 2) {
        printf(" You're almost there!");
    } else if (difference >= 30) {
        printf(" You're way off!");
    }
    
    printf("\n");
}

// Update game statistics
void updateStats() {
    totalGames++;
}

// Print game statistics
void printStats() {
    printf("\n=== GAME STATISTICS ===\n");
    printf("Total games played: %d\n", totalGames);
    printf("Games won: %d\n", gamesWon);
    printf("Win rate: %.1f%%\n", totalGames > 0 ? (float)gamesWon/totalGames*100 : 0);
    printf("Average guesses per game: %.1f\n", totalGames > 0 ? (float)totalGuesses/totalGames : 0);
    
    if (gamesWon > 0) {
        printf("Best performance: %.1f average guesses when won\n", 
               (float)(totalGuesses - (totalGames - gamesWon) * MAX_GUESSES) / gamesWon);
    }
    printf("========================\n");
}

// Reset for new game
void resetGame() {
    initializeGame();
}

// Advanced difficulty levels
void playWithDifficulty() {
    int difficulty;
    
    printf("\nChoose difficulty level:\n");
    printf("1. Easy (1-50, 10 guesses)\n");
    printf("2. Medium (1-100, 7 guesses)\n");
    printf("3. Hard (1-200, 6 guesses)\n");
    printf("4. Expert (1-500, 8 guesses)\n");
    printf("Enter choice (1-4): ");
    
    scanf("%d", &difficulty);
    
    int min, max, maxGuesses;
    
    switch (difficulty) {
        case 1:
            min = 1; max = 50; maxGuesses = 10;
            break;
        case 2:
            min = 1; max = 100; maxGuesses = 7;
            break;
        case 3:
            min = 1; max = 200; maxGuesses = 6;
            break;
        case 4:
            min = 1; max = 500; maxGuesses = 8;
            break;
        default:
            min = 1; max = 100; maxGuesses = 7;
            break;
    }
    
    printf("\nDifficulty: %d range, %d guesses\n", max, maxGuesses);
    printf("I'm thinking of a number between %d and %d.\n", min, max);
    printf("You have %d guesses to find it.\n\n", maxGuesses);
    
    targetNumber = min + rand() % (max - min + 1);
    guessesLeft = maxGuesses;
    gameOver = false;
    
    while (guessesLeft > 0 && !gameOver) {
        printf("Enter your guess (%d-%d): ", min, max);
        if (scanf("%d", &currentGuess) != 1) {
            printf("Invalid input! Please enter a number.\n");
            getchar();
            continue;
        }
        
        if (currentGuess < min || currentGuess > max) {
            printf("Please enter a number between %d and %d.\n", min, max);
            continue;
        }
        
        guessesLeft--;
        totalGuesses++;
        
        if (currentGuess == targetNumber) {
            printf("\n*** EXPERT GUESS! ***\n");
            printf("You found %d in %d guesses!\n", targetNumber, maxGuesses - guessesLeft);
            gamesWon++;
            gameOver = true;
        } else {
            int difference = abs(currentGuess - targetNumber);
            
            if (difference <= max / 20) {
                printf("*** BURNING HOT! *** ");
            } else if (difference <= max / 10) {
                printf("*** VERY HOT! *** ");
            } else if (difference <= max / 5) {
                printf("*** HOT! *** ");
            } else if (difference <= max / 3) {
                printf("*** WARM! *** ");
            } else {
                printf("*** COLD! *** ");
            }
            
            if (currentGuess < targetNumber) {
                printf("Too low!");
            } else {
                printf("Too high!");
            }
            
            printf("\n");
            
            if (guessesLeft > 0) {
                printf("%d guesses remaining.\n\n", guessesLeft);
            } else {
                printf("\n*** OUT OF GUESSES ***\n");
                printf("The number was %d.\n", targetNumber);
            }
        }
    }
}

// Main game loop
void gameLoop() {
    int choice;
    
    while (true) {
        printf("\n=== NUMBER GUESSING GAME ===\n");
        printf("1. Play game (1-100, 7 guesses)\n");
        printf("2. Play with difficulty levels\n");
        printf("3. View statistics\n");
        printf("4. Quit\n");
        printf("Choose an option: ");
        
        scanf("%d", &choice);
        
        switch (choice) {
            case 1:
                resetGame();
                playRound();
                updateStats();
                printf("Play again? (y/n): ");
                getchar(); // Clear buffer
                break;
                
            case 2:
                playWithDifficulty();
                updateStats();
                printf("Play again? (y/n): ");
                getchar(); // Clear buffer
                break;
                
            case 3:
                printStats();
                printf("Press Enter to continue...");
                getchar();
                break;
                
            case 4:
                printf("Thanks for playing!\n");
                printStats();
                return;
                
            default:
                printf("Invalid choice! Please try again.\n");
                break;
        }
        
        // Check if player wants to continue
        if (choice == 1 || choice == 2) {
            char playAgain;
            scanf(" %c", &playAgain);
            
            if (playAgain != 'y' && playAgain != 'Y') {
                printStats();
                break;
            }
        }
    }
}

int main() {
    printf("=== NUMBER GUESSING GAME ===\n");
    printf("Classic number guessing game with hints\n");
    printf("I'll think of a number, you try to guess it!\n");
    printf("Press ENTER to start...\n");
    getchar();
    
    gameLoop();
    
    return 0;
}
