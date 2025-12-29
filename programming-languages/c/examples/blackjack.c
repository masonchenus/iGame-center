/*
 * Blackjack Game - C Implementation
 * Classic casino card game
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdbool.h>
#include <string.h>

#define MAX_CARDS 52
#define HAND_SIZE 21

// Card structure
typedef struct {
    char suit; // 'H', 'D', 'C', 'S'
    char rank; // 'A', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K'
    int value;
} Card;

// Deck of cards
Card deck[MAX_CARDS];
int deckPos = 0;

// Player hands
int playerCards[10];
int dealerCards[10];
int playerCount = 0;
int dealerCount = 0;

// Game state
int playerWins = 0;
int dealerWins = 0;
int pushes = 0;
int totalGames = 0;
bool gameOver = false;

// Function prototypes
void initializeDeck();
void shuffleDeck();
Card drawCard();
void dealCards();
int getCardValue(char rank);
int calculateHand(int hand[], int count);
void printCard(Card card);
void printHand(int hand[], int count, bool hideFirst);
void printGame();
void playerTurn();
void dealerTurn();
bool shouldDealerHit();
bool isBlackjack(int hand[], int count);
bool isBust(int hand[], int count);
void resetGame();

// Initialize and shuffle the deck
void initializeDeck() {
    char suits[] = {'H', 'D', 'C', 'S'};
    char ranks[] = {'A', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K'};
    int cardIndex = 0;
    
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 13; j++) {
            deck[cardIndex].suit = suits[i];
            deck[cardIndex].rank = ranks[j];
            deck[cardIndex].value = getCardValue(ranks[j]);
            cardIndex++;
        }
    }
}

// Shuffle the deck using Fisher-Yates algorithm
void shuffleDeck() {
    for (int i = MAX_CARDS - 1; i > 0; i--) {
        int j = rand() % (i + 1);
        Card temp = deck[i];
        deck[i] = deck[j];
        deck[j] = temp;
    }
    deckPos = 0;
}

// Get the value of a card
int getCardValue(char rank) {
    switch (rank) {
        case 'A': return 11; // Ace can be 1 or 11
        case 'T':
        case 'J':
        case 'Q':
        case 'K': return 10;
        default: return rank - '0';
    }
}

// Draw a card from the deck
Card drawCard() {
    if (deckPos >= MAX_CARDS) {
        initializeDeck();
        shuffleDeck();
    }
    return deck[deckPos++];
}

// Deal initial cards
void dealCards() {
    // Deal two cards to player
    playerCards[playerCount++] = drawCard().value;
    playerCards[playerCount++] = drawCard().value;
    
    // Deal two cards to dealer
    dealerCards[dealerCount++] = drawCard().value;
    dealerCards[dealerCount++] = drawCard().value;
}

// Calculate hand value (handle Aces)
int calculateHand(int hand[], int count) {
    int total = 0;
    int aces = 0;
    
    for (int i = 0; i < count; i++) {
        total += hand[i];
        if (hand[i] == 11) aces++;
    }
    
    // Handle Aces
    while (total > 21 && aces > 0) {
        total -= 10; // Convert Ace from 11 to 1
        aces--;
    }
    
    return total;
}

// Print a card
void printCard(Card card) {
    printf("%c%c", card.rank, card.suit);
}

// Print a hand
void printHand(int hand[], int count, bool hideFirst) {
    printf("[");
    for (int i = 0; i < count; i++) {
        if (i == 0 && hideFirst) {
            printf("XX"); // Hidden card
        } else {
            printf("%d", hand[i]);
        }
        if (i < count - 1) printf(" ");
    }
    printf("]");
}

// Print the current game state
void printGame() {
    system("cls");
    
    printf("BLACKJACK\n");
    printf("Games: %d | Player Wins: %d | Dealer Wins: %d | Pushes: %d\n", 
           totalGames, playerWins, dealerWins, pushes);
    printf("======================================================\n");
    
    printf("Dealer: ");
    printHand(dealerCards, dealerCount, true);
    printf(" (?)\n");
    
    printf("Player: ");
    printHand(playerCards, playerCount, false);
    printf(" (%d)\n", calculateHand(playerCards, playerCount));
    
    printf("======================================================\n");
    
    if (!gameOver) {
        printf("Actions: H (Hit), S (Stand), Q (Quit)\n");
    }
}

// Player's turn
void playerTurn() {
    char action;
    
    while (!gameOver) {
        printGame();
        
        int playerTotal = calculateHand(playerCards, playerCount);
        
        if (playerTotal > 21) {
            printf("*** BUST! You went over 21. ***\n");
            dealerWins++;
            gameOver = true;
            break;
        }
        
        if (playerTotal == 21) {
            printf("*** BLACKJACK! Standing automatically. ***\n");
            break;
        }
        
        printf("Your move: ");
        scanf(" %c", &action);
        
        switch (action) {
            case 'h':
            case 'H':
                playerCards[playerCount++] = drawCard().value;
                printf("You drew a card.\n");
                getchar();
                break;
            case 's':
            case 'S':
                printf("You stand.\n");
                getchar();
                return;
            case 'q':
            case 'Q':
                gameOver = true;
                break;
            default:
                printf("Invalid action! Use H, S, or Q.\n");
                getchar();
                break;
        }
    }
}

// Dealer's turn
void dealerTurn() {
    printf("\nDealer's turn...\n");
    printf("Dealer reveals: ");
    printHand(dealerCards, dealerCount, false);
    printf(" (%d)\n", calculateHand(dealerCards, dealerCount));
    
    int dealerTotal = calculateHand(dealerCards, dealerCount);
    
    while (dealerTotal < 17) {
        dealerCards[dealerCount++] = drawCard().value;
        printf("Dealer hits.\n");
        dealerTotal = calculateHand(dealerCards, dealerCount);
        printf("Dealer has: ");
        printHand(dealerCards, dealerCount, false);
        printf(" (%d)\n", dealerTotal);
    }
    
    if (dealerTotal > 21) {
        printf("*** DEALER BUSTS! You win! ***\n");
        playerWins++;
    } else if (dealerTotal == 21) {
        printf("*** DEALER HAS BLACKJACK! ***\n");
        dealerWins++;
    } else {
        // Compare hands
        int playerTotal = calculateHand(playerCards, playerCount);
        
        printf("\nFinal hands:\n");
        printf("Player: ");
        printHand(playerCards, playerCount, false);
        printf(" (%d)\n", playerTotal);
        
        printf("Dealer: ");
        printHand(dealerCards, dealerCount, false);
        printf(" (%d)\n", dealerTotal);
        
        if (playerTotal > dealerTotal) {
            printf("*** YOU WIN! ***\n");
            playerWins++;
        } else if (playerTotal < dealerTotal) {
            printf("*** DEALER WINS! ***\n");
            dealerWins++;
        } else {
            printf("*** PUSH! It's a tie. ***\n");
            pushes++;
        }
    }
}

// Check for blackjack
bool isBlackjack(int hand[], int count) {
    return count == 2 && calculateHand(hand, count) == 21;
}

// Check if hand is bust
bool isBust(int hand[], int count) {
    return calculateHand(hand, count) > 21;
}

// Reset game for next round
void resetGame() {
    playerCount = 0;
    dealerCount = 0;
    gameOver = false;
    
    // Shuffle deck if we're running low
    if (deckPos > MAX_CARDS - 20) {
        shuffleDeck();
    }
}

// Main game loop
void gameLoop() {
    char playAgain;
    
    while (true) {
        resetGame();
        dealCards();
        
        // Check for initial blackjack
        bool playerBlackjack = isBlackjack(playerCards, playerCount);
        bool dealerBlackjack = isBlackjack(dealerCards, dealerCount);
        
        printGame();
        
        if (playerBlackjack || dealerBlackjack) {
            printf("\n*** CHECKING FOR BLACKJACK... ***\n");
            
            if (playerBlackjack && dealerBlackjack) {
                printf("Both have blackjack! Push!\n");
                pushes++;
            } else if (playerBlackjack) {
                printf("*** BLACKJACK! You win! ***\n");
                playerWins++;
            } else {
                printf("*** Dealer has blackjack! Dealer wins! ***\n");
                dealerWins++;
            }
            
            gameOver = true;
        } else {
            playerTurn();
            
            if (!gameOver) {
                dealerTurn();
            }
        }
        
        totalGames++;
        
        printGame();
        printf("\nPlay again? (y/n): ");
        scanf(" %c", &playAgain);
        
        if (playAgain != 'y' && playAgain != 'Y') {
            break;
        }
    }
    
    printf("\n=== GAME STATISTICS ===\n");
    printf("Total games played: %d\n", totalGames);
    printf("Player wins: %d (%.1f%%)\n", playerWins, 
           totalGames > 0 ? (float)playerWins/totalGames*100 : 0);
    printf("Dealer wins: %d (%.1f%%)\n", dealerWins,
           totalGames > 0 ? (float)dealerWins/totalGames*100 : 0);
    printf("Pushes: %d\n", pushes);
    printf("========================\n");
}

int main() {
    printf("=== BLACKJACK ===\n");
    printf("Classic casino card game\n");
    printf("Goal: Get as close to 21 as possible without going over\n");
    printf("Dealer must hit on 16 and stand on 17\n");
    printf("Press ENTER to start...\n");
    getchar();
    
    srand(time(NULL));
    initializeDeck();
    shuffleDeck();
    gameLoop();
    
    printf("Thanks for playing!\n");
    return 0;
}
