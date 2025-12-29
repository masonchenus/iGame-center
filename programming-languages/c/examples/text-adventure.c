/*
 * Text Adventure Game - C Implementation
 * Classic text-based adventure game demonstrating C programming
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_ROOMS 20
#define MAX_ITEMS 10
#define MAX_DESCRIPTION 200
#define MAX_COMMAND 100

// Game structures
typedef struct {
    char name[30];
    char description[MAX_DESCRIPTION];
    int north, south, east, west;
    int items[MAX_ITEMS];
    int item_count;
    int visited;
} Room;

typedef struct {
    char name[30];
    char description[MAX_DESCRIPTION];
    int portable;
    int effect_value;
} Item;

typedef struct {
    char inventory[MAX_ITEMS][30];
    int item_count;
    int health;
    int score;
    int current_room;
} Player;

// Global game state
Room rooms[MAX_ROOMS];
Item items[MAX_ITEMS];
Player player;

// Function prototypes
void initialize_game();
void display_room();
void process_command(char* command);
void look_command();
void move_command(char* direction);
void take_command(char* item_name);
void use_command(char* item_name);
void inventory_command();
void help_command();
int find_item(char* name);
int find_room(char* name);
void save_game();
void load_game();

// Initialize the game world
void initialize_game() {
    // Initialize rooms
    memset(rooms, 0, sizeof(rooms));
    
    // Room 0: Starting location
    strcpy(rooms[0].name, "Village Square");
    strcpy(rooms[0].description, "You stand in the center of a small village. Cobblestone paths lead in four directions. An old fountain bubbles in the middle of the square. To the north you see a forest path, to the south a blacksmith's shop, to the east a market, and to the west a tavern.");
    rooms[0].north = 1;
    rooms[0].south = 2;
    rooms[0].east = 3;
    rooms[0].west = 4;
    rooms[0].item_count = 0;
    
    // Room 1: Forest
    strcpy(rooms[1].name, "Dark Forest");
    strcpy(rooms[1].description, "Tall trees tower above you, their branches creating a canopy that blocks most sunlight. The air is cool and damp. You hear birds chirping somewhere in the distance. A narrow path continues deeper into the forest.");
    rooms[1].south = 0;
    rooms[1].north = 5;
    rooms[1].item_count = 0;
    
    // Room 2: Blacksmith
    strcpy(rooms[2].name, "Blacksmith's Shop");
    strcpy(rooms[2].description, "The heat of the forge warms the room. Various weapons and tools hang on the walls. The blacksmith, a burly man with soot-stained hands, works on a sword. The smell of hot metal fills the air.");
    rooms[2].north = 0;
    rooms[2].item_count = 0;
    
    // Room 3: Market
    strcpy(rooms[3].name, "Village Market");
    strcpy(rooms[3].description, "Colorful stalls line the market square, selling various goods and food. Merchants call out their prices, and the air is filled with the sounds of bargaining and commerce.");
    rooms[3].west = 0;
    rooms[3].item_count = 0;
    
    // Room 4: Tavern
    strcpy(rooms[4].name, "The Prancing Pony Tavern");
    strcpy(rooms[4].description, "Warm light spills from the tavern windows. Inside, you can hear laughter and the clink of mugs. The smell of ale and roasted meat makes your stomach growl.");
    rooms[4].east = 0;
    rooms[4].item_count = 0;
    
    // Room 5: Ancient Temple
    strcpy(rooms[5].name, "Ancient Temple");
    strcpy(rooms[5].description, "Ruins of an ancient temple rise before you. Carved stones lie scattered about, and mysterious symbols are etched into the remaining walls. A sense of history and mystery pervades the place.");
    rooms[5].south = 1;
    rooms[5].item_count = 0;
    
    // Initialize items
    memset(items, 0, sizeof(items));
    
    // Item 0: Health Potion
    strcpy(items[0].name, "health potion");
    strcpy(items[0].description, "A small red bottle filled with a healing liquid. It glows faintly.");
    items[0].portable = 1;
    items[0].effect_value = 25;
    
    // Item 1: Magic Sword
    strcpy(items[1].name, "magic sword");
    strcpy(items[1].description, "An elegant blade that seems to shimmer with inner light. It feels perfectly balanced in your hand.");
    items[1].portable = 1;
    items[1].effect_value = 50;
    
    // Item 2: Ancient Key
    strcpy(items[2].name, "ancient key");
    strcpy(items[2].description, "A bronze key with intricate engravings. It looks very old and important.");
    items[2].portable = 1;
    items[2].effect_value = 1;
    
    // Place items in rooms
    rooms[0].items[0] = 0; // Health potion in village square
    rooms[0].item_count = 1;
    
    rooms[1].items[0] = 1; // Magic sword in forest
    rooms[1].item_count = 1;
    
    rooms[5].items[0] = 2; // Ancient key in temple
    rooms[5].item_count = 1;
    
    // Initialize player
    player.item_count = 0;
    player.health = 100;
    player.score = 0;
    player.current_room = 0;
    rooms[0].visited = 1;
}

// Display current room
void display_room() {
    Room* current = &rooms[player.current_room];
    
    printf("\n=== %s ===\n", current->name);
    printf("%s\n", current->description);
    
    if (current->item_count > 0) {
        printf("You can see: ");
        for (int i = 0; i < current->item_count; i++) {
            printf("%s", items[current->items[i]].name);
            if (i < current->item_count - 1) printf(", ");
        }
        printf("\n");
    }
    
    printf("Exits: ");
    if (current->north >= 0) printf("north ");
    if (current->south >= 0) printf("south ");
    if (current->east >= 0) printf("east ");
    if (current->west >= 0) printf("west ");
    printf("\n");
}

// Process player commands
void process_command(char* command) {
    char* cmd = strtok(command, " \n");
    if (!cmd) return;
    
    // Convert to lowercase for comparison
    for (char* p = cmd; *p; p++) {
        *p = tolower(*p);
    }
    
    if (strcmp(cmd, "look") == 0 || strcmp(cmd, "l") == 0) {
        look_command();
    } else if (strcmp(cmd, "move") == 0 || strcmp(cmd, "go") == 0 || 
               strcmp(cmd, "north") == 0 || strcmp(cmd, "south") == 0 ||
               strcmp(cmd, "east") == 0 || strcmp(cmd, "west") == 0) {
        if (strcmp(cmd, "move") == 0 || strcmp(cmd, "go") == 0) {
            char* direction = strtok(NULL, " \n");
            if (direction) move_command(direction);
            else printf("Move where?\n");
        } else {
            move_command(cmd);
        }
    } else if (strcmp(cmd, "take") == 0 || strcmp(cmd, "get") == 0 ||
               strcmp(cmd, "pickup") == 0) {
        char* item_name = strtok(NULL, " \n");
        if (item_name) take_command(item_name);
        else printf("Take what?\n");
    } else if (strcmp(cmd, "use") == 0) {
        char* item_name = strtok(NULL, " \n");
        if (item_name) use_command(item_name);
        else printf("Use what?\n");
    } else if (strcmp(cmd, "inventory") == 0 || strcmp(cmd, "inv") == 0) {
        inventory_command();
    } else if (strcmp(cmd, "help") == 0 || strcmp(cmd, "?") == 0) {
        help_command();
    } else if (strcmp(cmd, "save") == 0) {
        save_game();
    } else if (strcmp(cmd, "load") == 0) {
        load_game();
    } else if (strcmp(cmd, "quit") == 0 || strcmp(cmd, "exit") == 0) {
        printf("Thanks for playing! Your final score: %d\n", player.score);
        exit(0);
    } else {
        printf("I don't understand that command. Type 'help' for available commands.\n");
    }
}

// Look around command
void look_command() {
    display_room();
}

// Move in a direction
void move_command(char* direction) {
    Room* current = &rooms[player.current_room];
    int new_room = -1;
    
    // Convert direction to lowercase
    for (char* p = direction; *p; p++) {
        *p = tolower(*p);
    }
    
    if (strcmp(direction, "north") == 0 || strcmp(direction, "n") == 0) {
        new_room = current->north;
    } else if (strcmp(direction, "south") == 0 || strcmp(direction, "s") == 0) {
        new_room = current->south;
    } else if (strcmp(direction, "east") == 0 || strcmp(direction, "e") == 0) {
        new_room = current->east;
    } else if (strcmp(direction, "west") == 0 || strcmp(direction, "w") == 0) {
        new_room = current->west;
    } else {
        printf("I don't recognize that direction.\n");
        return;
    }
    
    if (new_room < 0) {
        printf("You can't go that way.\n");
        return;
    }
    
    player.current_room = new_room;
    if (!rooms[new_room].visited) {
        rooms[new_room].visited = 1;
        player.score += 10; // Points for exploring new areas
    }
    
    display_room();
}

// Take an item
void take_command(char* item_name) {
    Room* current = &rooms[player.current_room];
    int item_index = -1;
    
    // Convert item name to lowercase
    for (char* p = item_name; *p; p++) {
        *p = tolower(*p);
    }
    
    // Find item in current room
    for (int i = 0; i < current->item_count; i++) {
        if (strstr(items[current->items[i]].name, item_name) != NULL) {
            item_index = current->items[i];
            break;
        }
    }
    
    if (item_index < 0) {
        printf("You don't see that item here.\n");
        return;
    }
    
    if (!items[item_index].portable) {
        printf("You can't take that.\n");
        return;
    }
    
    if (player.item_count >= MAX_ITEMS) {
        printf("Your inventory is full.\n");
        return;
    }
    
    // Add item to inventory
    strcpy(player.inventory[player.item_count], items[item_index].name);
    player.item_count++;
    
    // Remove item from room
    for (int i = 0; i < current->item_count; i++) {
        if (current->items[i] == item_index) {
            for (int j = i; j < current->item_count - 1; j++) {
                current->items[j] = current->items[j + 1];
            }
            break;
        }
    }
    current->item_count--;
    
    printf("You take the %s.\n", items[item_index].name);
    player.score += 5;
}

// Use an item
void use_command(char* item_name) {
    int item_index = -1;
    
    // Convert item name to lowercase
    for (char* p = item_name; *p; p++) {
        *p = tolower(*p);
    }
    
    // Find item in inventory
    for (int i = 0; i < player.item_count; i++) {
        if (strstr(player.inventory[i], item_name) != NULL) {
            item_index = i;
            break;
        }
    }
    
    if (item_index < 0) {
        printf("You don't have that item.\n");
        return;
    }
    
    // Use the item (simplified logic)
    if (strstr(item_name, "health potion") != NULL) {
        player.health = MIN(100, player.health + 25);
        printf("You drink the health potion. You feel better! Health: %d\n", player.health);
        
        // Remove item from inventory
        for (int i = item_index; i < player.item_count - 1; i++) {
            strcpy(player.inventory[i], player.inventory[i + 1]);
        }
        player.item_count--;
        player.score += 10;
        
    } else if (strstr(item_name, "magic sword") != NULL) {
        printf("You wield the magic sword. It glows with power!\n");
        player.score += 20;
        
    } else if (strstr(item_name, "ancient key") != NULL) {
        if (player.current_room == 5) {
            printf("You use the ancient key on a hidden door that opens with a grinding sound!\n");
            player.score += 50;
        } else {
            printf("You examine the ancient key, but there's nothing to unlock here.\n");
        }
    } else {
        printf("You can't figure out how to use that.\n");
    }
}

// Show inventory
void inventory_command() {
    if (player.item_count == 0) {
        printf("Your inventory is empty.\n");
    } else {
        printf("Your inventory contains:\n");
        for (int i = 0; i < player.item_count; i++) {
            printf("- %s\n", player.inventory[i]);
        }
    }
    
    printf("Health: %d | Score: %d\n", player.health, player.score);
}

// Show help
void help_command() {
    printf("\n=== TEXT ADVENTURE COMMANDS ===\n");
    printf("look (l) - Look around the current room\n");
    printf("move/go [direction] - Move in a direction (north, south, east, west)\n");
    printf("n, s, e, w - Quick movement commands\n");
    printf("take/get [item] - Pick up an item\n");
    printf("use [item] - Use an item from your inventory\n");
    printf("inventory (inv) - Check your inventory\n");
    printf("save - Save your game\n");
    printf("load - Load a saved game\n");
    printf("help (?) - Show this help\n");
    printf("quit/exit - Quit the game\n\n");
}

// Find item by name
int find_item(char* name) {
    for (int i = 0; i < MAX_ITEMS; i++) {
        if (strstr(items[i].name, name) != NULL) {
            return i;
        }
    }
    return -1;
}

// Save game
void save_game() {
    FILE* file = fopen("savegame.txt", "w");
    if (!file) {
        printf("Failed to save game.\n");
        return;
    }
    
    fprintf(file, "%d %d %d\n", player.health, player.score, player.current_room);
    fprintf(file, "%d\n", player.item_count);
    for (int i = 0; i < player.item_count; i++) {
        fprintf(file, "%s\n", player.inventory[i]);
    }
    
    fclose(file);
    printf("Game saved successfully!\n");
}

// Load game
void load_game() {
    FILE* file = fopen("savegame.txt", "r");
    if (!file) {
        printf("No saved game found.\n");
        return;
    }
    
    fscanf(file, "%d %d %d\n", &player.health, &player.score, &player.current_room);
    fscanf(file, "%d\n", &player.item_count);
    for (int i = 0; i < player.item_count; i++) {
        fgets(player.inventory[i], 30, file);
        player.inventory[i][strcspn(player.inventory[i], "\n")] = 0; // Remove newline
    }
    
    fclose(file);
    printf("Game loaded successfully!\n");
    display_room();
}

int main() {
    char command[MAX_COMMAND];
    
    printf("=== TEXT ADVENTURE GAME ===\n");
    printf("Welcome to the Text Adventure!\n");
    printf("Type 'help' for a list of commands.\n");
    
    initialize_game();
    display_room();
    
    while (1) {
        printf("\n> ");
        if (fgets(command, MAX_COMMAND, stdin)) {
            command[strcspn(command, "\n")] = 0; // Remove newline
            process_command(command);
        }
    }
    
    return 0;
}
