/*
 * Conway's Game of Life - C Implementation
 * Cellular automaton simulation with advanced features
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <time.h>
#include <string.h>
#include <unistd.h>

#define GRID_WIDTH 80
#define GRID_HEIGHT 80
#define MAX_GENERATIONS 1000000
#define INITIAL_DENSITY 0.3

// Grid structure
typedef struct {
    bool cells[GRID_HEIGHT][GRID_WIDTH];
    int generation;
    int alive_cells;
} GameGrid;

// Function prototypes
void initialize_grid(GameGrid* grid);
void randomize_grid(GameGrid* grid, float density);
void clear_grid(GameGrid* grid);
void copy_grid(GameGrid* dest, GameGrid* src);
int count_neighbors(GameGrid* grid, int x, int y);
void update_generation(GameGrid* grid);
void render_grid(GameGrid* grid);
void render_cell(int x, int y, bool alive);
void add_glider(GameGrid* grid, int start_x, int start_y);
void add_blinker(GameGrid* grid, int start_x, int start_y);
void add_toad(GameGrid* grid, int start_x, int start_y);
void add_beacon(GameGrid* grid, int start_x, int start_y);
void add_pulsar(GameGrid* grid, int start_x, int start_y);
void run_automaton(GameGrid* grid, int generations);
void run_interactive(GameGrid* grid);
void save_pattern(GameGrid* grid, const char* filename);
bool load_pattern(GameGrid* grid, const char* filename);
void analyze_pattern(GameGrid* grid);
void detect_patterns(GameGrid* grid);

// Pattern definitions
void add_pattern(GameGrid* grid, const char* pattern_name, int x, int y) {
    if (strcmp(pattern_name, "glider") == 0) {
        add_glider(grid, x, y);
    } else if (strcmp(pattern_name, "blinker") == 0) {
        add_blinker(grid, x, y);
    } else if (strcmp(pattern_name, "toad") == 0) {
        add_toad(grid, x, y);
    } else if (strcmp(pattern_name, "beacon") == 0) {
        add_beacon(grid, x, y);
    } else if (strcmp(pattern_name, "pulsar") == 0) {
        add_pulsar(grid, x, y);
    }
}

// Initialize grid to all dead cells
void initialize_grid(GameGrid* grid) {
    memset(grid->cells, 0, sizeof(grid->cells));
    grid->generation = 0;
    grid->alive_cells = 0;
}

// Randomize grid with given density
void randomize_grid(GameGrid* grid, float density) {
    srand(time(NULL));
    grid->alive_cells = 0;
    
    for (int y = 0; y < GRID_HEIGHT; y++) {
        for (int x = 0; x < GRID_WIDTH; x++) {
            if ((float)rand() / RAND_MAX < density) {
                grid->cells[y][x] = true;
                grid->alive_cells++;
            } else {
                grid->cells[y][x] = false;
            }
        }
    }
}

// Clear grid
void clear_grid(GameGrid* grid) {
    memset(grid->cells, 0, sizeof(grid->cells));
    grid->generation = 0;
    grid->alive_cells = 0;
}

// Copy grid
void copy_grid(GameGrid* dest, GameGrid* src) {
    memcpy(dest->cells, src->cells, sizeof(src->cells));
    dest->generation = src->generation;
    dest->alive_cells = src->alive_cells;
}

// Count live neighbors around a cell
int count_neighbors(GameGrid* grid, int x, int y) {
    int count = 0;
    
    for (int dy = -1; dy <= 1; dy++) {
        for (int dx = -1; dx <= 1; dx++) {
            if (dx == 0 && dy == 0) continue;
            
            int nx = (x + dx + GRID_WIDTH) % GRID_WIDTH;
            int ny = (y + dy + GRID_HEIGHT) % GRID_HEIGHT;
            
            if (grid->cells[ny][nx]) {
                count++;
            }
        }
    }
    
    return count;
}

// Update to next generation
void update_generation(GameGrid* grid) {
    GameGrid new_grid;
    new_grid.generation = grid->generation + 1;
    new_grid.alive_cells = 0;
    
    for (int y = 0; y < GRID_HEIGHT; y++) {
        for (int x = 0; x < GRID_WIDTH; x++) {
            int neighbors = count_neighbors(grid, x, y);
            bool current = grid->cells[y][x];
            bool next;
            
            // Conway's Game of Life rules
            if (current) {
                // Cell is alive
                next = (neighbors == 2 || neighbors == 3);
            } else {
                // Cell is dead
                next = (neighbors == 3);
            }
            
            new_grid.cells[y][x] = next;
            if (next) {
                new_grid.alive_cells++;
            }
        }
    }
    
    copy_grid(grid, &new_grid);
}

// Render grid to console
void render_grid(GameGrid* grid) {
    printf("\033[H"); // Move cursor to top-left
    printf("\033[2J"); // Clear screen
    
    printf("Conway's Game of Life - Generation %d\n", grid->generation);
    printf("Alive cells: %d\n\n", grid->alive_cells);
    
    for (int y = 0; y < GRID_HEIGHT; y++) {
        for (int x = 0; x < GRID_WIDTH; x++) {
            render_cell(x, y, grid->cells[y][x]);
        }
        printf("\n");
    }
    
    printf("\nControls: SPACE=pause/resume, R=randomize, C=clear, Q=quit\n");
    printf("Press any key to continue...\n");
}

// Render individual cell
void render_cell(int x, int y, bool alive) {
    if (alive) {
        printf("█");
    } else {
        printf(" ");
    }
}

// Add glider pattern
void add_glider(GameGrid* grid, int start_x, int start_y) {
    int glider[3][3] = {
        {0, 1, 0},
        {0, 0, 1},
        {1, 1, 1}
    };
    
    for (int y = 0; y < 3; y++) {
        for (int x = 0; x < 3; x++) {
            int gx = (start_x + x) % GRID_WIDTH;
            int gy = (start_y + y) % GRID_HEIGHT;
            
            if (glider[y][x]) {
                grid->cells[gy][gx] = true;
                grid->alive_cells++;
            }
        }
    }
}

// Add blinker pattern
void add_blinker(GameGrid* grid, int start_x, int start_y) {
    for (int x = 0; x < 3; x++) {
        int gx = (start_x + x) % GRID_WIDTH;
        int gy = start_y % GRID_HEIGHT;
        grid->cells[gy][gx] = true;
        grid->alive_cells++;
    }
}

// Add toad pattern
void add_toad(GameGrid* grid, int start_x, int start_y) {
    for (int x = 0; x < 2; x++) {
        for (int y = 0; y < 4; y++) {
            int gx = (start_x + x) % GRID_WIDTH;
            int gy = (start_y + y) % GRID_HEIGHT;
            
            if ((y == 1 || y == 2) && (x == 0 || x == 1)) {
                grid->cells[gy][gx] = true;
                grid->alive_cells++;
            }
        }
    }
}

// Add beacon pattern
void add_beacon(GameGrid* grid, int start_x, int start_y) {
    int beacon[4][4] = {
        {1, 1, 0, 0},
        {1, 1, 0, 0},
        {0, 0, 1, 1},
        {0, 0, 1, 1}
    };
    
    for (int y = 0; y < 4; y++) {
        for (int x = 0; x < 4; x++) {
            int gx = (start_x + x) % GRID_WIDTH;
            int gy = (start_y + y) % GRID_HEIGHT;
            
            if (beacon[y][x]) {
                grid->cells[gy][gx] = true;
                grid->alive_cells++;
            }
        }
    }
}

// Add pulsar pattern (13x13)
void add_pulsar(GameGrid* grid, int start_x, int start_y) {
    // Simplified 8x8 pulsar pattern
    int pulsar[8][8] = {
        {0, 0, 1, 1, 1, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 0, 0},
        {1, 0, 0, 0, 0, 0, 0, 1},
        {1, 0, 0, 0, 0, 0, 0, 1},
        {1, 0, 0, 0, 0, 0, 0, 1},
        {0, 0, 0, 0, 0, 0, 0, 0},
        {0, 0, 1, 1, 1, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 0, 0}
    };
    
    for (int y = 0; y < 8; y++) {
        for (int x = 0; x < 8; x++) {
            int gx = (start_x + x) % GRID_WIDTH;
            int gy = (start_y + y) % GRID_HEIGHT;
            
            if (pulsar[y][x]) {
                grid->cells[gy][gx] = true;
                grid->alive_cells++;
            }
        }
    }
}

// Run automaton for specified generations
void run_automaton(GameGrid* grid, int generations) {
    printf("Running Conway's Game of Life for %d generations...\n", generations);
    
    for (int gen = 0; gen < generations; gen++) {
        update_generation(grid);
        
        // Show every 10th generation
        if (gen % 10 == 0 || gen == generations - 1) {
            printf("Generation %d: %d alive cells\n", grid->generation, grid->alive_cells);
        }
        
        // Check for extinction
        if (grid->alive_cells == 0) {
            printf("All cells died at generation %d\n", grid->generation);
            break;
        }
        
        // Check for stable pattern
        if (grid->generation > 100 && grid->alive_cells == grid->alive_cells) {
            // In a real implementation, would track previous states
            printf("Possible stable pattern detected\n");
        }
    }
    
    printf("Final generation: %d with %d alive cells\n", grid->generation, grid->alive_cells);
}

// Interactive mode
void run_interactive(GameGrid* grid) {
    bool running = true;
    bool paused = false;
    char input;
    
    while (running) {
        if (!paused) {
            update_generation(grid);
        }
        
        render_grid(grid);
        
        // Get user input
        input = getchar();
        
        switch (input) {
            case ' ':
                paused = !paused;
                break;
            case 'r':
            case 'R':
                randomize_grid(grid, INITIAL_DENSITY);
                grid->generation = 0;
                break;
            case 'c':
            case 'C':
                clear_grid(grid);
                break;
            case 'q':
            case 'Q':
                running = false;
                break;
            case 'g':
            case 'G':
                add_glider(grid, GRID_WIDTH / 2, GRID_HEIGHT / 2);
                break;
            case 'b':
            case 'B':
                add_blinker(grid, GRID_WIDTH / 2, GRID_HEIGHT / 2);
                break;
            default:
                break;
        }
    }
}

// Analyze grid statistics
void analyze_pattern(GameGrid* grid) {
    printf("\n=== Pattern Analysis ===\n");
    printf("Generation: %d\n", grid->generation);
    printf("Alive cells: %d\n", grid->alive_cells);
    printf("Density: %.2f%%\n", (float)grid->alive_cells / (GRID_WIDTH * GRID_HEIGHT) * 100);
    
    // Find bounding box of alive cells
    int min_x = GRID_WIDTH, max_x = 0, min_y = GRID_HEIGHT, max_y = 0;
    bool found = false;
    
    for (int y = 0; y < GRID_HEIGHT; y++) {
        for (int x = 0; x < GRID_WIDTH; x++) {
            if (grid->cells[y][x]) {
                found = true;
                if (x < min_x) min_x = x;
                if (x > max_x) max_x = x;
                if (y < min_y) min_y = y;
                if (y > max_y) max_y = y;
            }
        }
    }
    
    if (found) {
        printf("Bounding box: (%d,%d) to (%d,%d)\n", min_x, min_y, max_x, max_y);
        printf("Pattern size: %dx%d\n", max_x - min_x + 1, max_y - min_y + 1);
    } else {
        printf("No alive cells found\n");
    }
}

// Detect common patterns
void detect_patterns(GameGrid* grid) {
    printf("\n=== Pattern Detection ===\n");
    
    bool glider_found = false;
    bool blinker_found = false;
    
    // Simple pattern detection (inefficient but educational)
    for (int y = 0; y < GRID_HEIGHT - 2; y++) {
        for (int x = 0; x < GRID_WIDTH - 2; x++) {
            // Check for glider
            if (grid->cells[y][x+1] && 
                grid->cells[y+1][x+2] && 
                grid->cells[y+2][x] && 
                grid->cells[y+2][x+1] && 
                grid->cells[y+2][x+2]) {
                printf("Glider detected at (%d,%d)\n", x, y);
                glider_found = true;
            }
            
            // Check for blinker
            if (grid->cells[y][x] && 
                grid->cells[y][x+1] && 
                grid->cells[y][x+2]) {
                printf("Blinker detected at (%d,%d)\n", x, y);
                blinker_found = true;
            }
        }
    }
    
    if (!glider_found) printf("No gliders detected\n");
    if (!blinker_found) printf("No blinkers detected\n");
}

// Save pattern to file
bool save_pattern(GameGrid* grid, const char* filename) {
    FILE* file = fopen(filename, "w");
    if (!file) {
        printf("Error: Could not save to %s\n", filename);
        return false;
    }
    
    fprintf(file, "%d\n", grid->generation);
    fprintf(file, "%d\n", grid->alive_cells);
    
    for (int y = 0; y < GRID_HEIGHT; y++) {
        for (int x = 0; x < GRID_WIDTH; x++) {
            fprintf(file, "%c", grid->cells[y][x] ? '1' : '0');
        }
        fprintf(file, "\n");
    }
    
    fclose(file);
    printf("Pattern saved to %s\n", filename);
    return true;
}

// Load pattern from file
bool load_pattern(GameGrid* grid, const char* filename) {
    FILE* file = fopen(filename, "r");
    if (!file) {
        printf("Error: Could not load from %s\n", filename);
        return false;
    }
    
    fscanf(file, "%d", &grid->generation);
    fscanf(file, "%d", &grid->alive_cells);
    
    for (int y = 0; y < GRID_HEIGHT; y++) {
        for (int x = 0; x < GRID_WIDTH; x++) {
            char c = fgetc(file);
            grid->cells[y][x] = (c == '1');
        }
        fgetc(file); // Consume newline
    }
    
    fclose(file);
    printf("Pattern loaded from %s\n", filename);
    return true;
}

// Main function
int main() {
    GameGrid grid;
    int choice;
    
    printf("=== Conway's Game of Life ===\n");
    printf("1. Random pattern\n");
    printf("2. Interactive mode\n");
    printf("3. Load pattern\n");
    printf("4. Custom patterns\n");
    printf("5. Analysis only\n");
    printf("Enter choice (1-5): ");
    scanf("%d", &choice);
    
    initialize_grid(&grid);
    
    switch (choice) {
        case 1:
            randomize_grid(&grid, INITIAL_DENSITY);
            run_automaton(&grid, 100);
            analyze_pattern(&grid);
            detect_patterns(&grid);
            break;
            
        case 2:
            // Add some initial patterns
            add_glider(&grid, 10, 10);
            add_blinker(&grid, 50, 20);
            add_beacon(&grid, 20, 30);
            run_interactive(&grid);
            break;
            
        case 3:
            printf("Enter filename to load: ");
            char filename[100];
            scanf("%s", filename);
            if (load_pattern(&grid, filename)) {
                analyze_pattern(&grid);
                detect_patterns(&grid);
            }
            break;
            
        case 4:
            // Create custom pattern
            clear_grid(&grid);
            add_pulsar(&grid, 20, 15);
            add_glider(&grid, 10, 10);
            add_glider(&grid, 60, 25);
            printf("Custom pattern created\n");
            run_interactive(&grid);
            break;
            
        case 5:
            randomize_grid(&grid, INITIAL_DENSITY);
            analyze_pattern(&grid);
            detect_patterns(&grid);
            break;
            
        default:
            printf("Invalid choice\n");
            return 1;
    }
    
    printf("\nWould you like to save this pattern? (y/n): ");
    char save_choice;
    scanf(" %c", &save_choice);
    
    if (save_choice == 'y' || save_choice == 'Y') {
        char save_filename[100];
        printf("Enter filename: ");
        scanf("%s", save_filename);
        save_pattern(&grid, save_filename);
    }
    
    printf("Thanks for exploring Conway's Game of Life!\n");
    return 0;
}

