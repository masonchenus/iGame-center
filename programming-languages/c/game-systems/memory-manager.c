/*
 * Custom Memory Manager for Game Systems
 * Provides efficient memory allocation and deallocation for game objects
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define MEMORY_POOL_SIZE (1024 * 1024) // 1MB pool
#define MAX_BLOCKS 256

// Memory block structure
typedef struct MemoryBlock {
    size_t size;
    void* memory;
    int is_free;
    struct MemoryBlock* next;
} MemoryBlock;

// Memory pool structure
typedef struct MemoryPool {
    uint8_t* pool;
    size_t total_size;
    size_t used_size;
    MemoryBlock* blocks;
    int block_count;
} MemoryPool;

// Global memory pool
static MemoryPool game_memory_pool;

// Initialize memory pool
void memory_pool_init(size_t size) {
    game_memory_pool.pool = (uint8_t*)malloc(size);
    game_memory_pool.total_size = size;
    game_memory_pool.used_size = 0;
    game_memory_pool.blocks = NULL;
    game_memory_pool.block_count = 0;
    
    if (!game_memory_pool.pool) {
        fprintf(stderr, "Failed to allocate memory pool\n");
        exit(1);
    }
    
    printf("Memory pool initialized: %zu bytes\n", size);
}

// Allocate memory from pool
void* memory_alloc(size_t size) {
    if (size == 0) return NULL;
    
    // Find free block or allocate new one
    MemoryBlock* current = game_memory_pool.blocks;
    MemoryBlock* prev = NULL;
    
    while (current) {
        if (current->is_free && current->size >= size) {
            // Split block if it's significantly larger
            if (current->size > size + 64) {
                MemoryBlock* new_block = (MemoryBlock*)malloc(sizeof(MemoryBlock));
                new_block->size = current->size - size - sizeof(MemoryBlock);
                new_block->memory = (void*)((uint8_t*)current->memory + size);
                new_block->is_free = 1;
                new_block->next = current->next;
                
                current->next = new_block;
                current->size = size;
            }
            
            current->is_free = 0;
            game_memory_pool.used_size += size;
            printf("Allocated %zu bytes at %p\n", size, current->memory);
            return current->memory;
        }
        
        prev = current;
        current = current->next;
    }
    
    // No suitable block found, create new one
    if (game_memory_pool.used_size + size + sizeof(MemoryBlock) > game_memory_pool.total_size) {
        fprintf(stderr, "Memory pool exhausted\n");
        return NULL;
    }
    
    MemoryBlock* new_block = (MemoryBlock*)malloc(sizeof(MemoryBlock));
    if (!new_block) {
        fprintf(stderr, "Failed to allocate memory block\n");
        return NULL;
    }
    
    new_block->size = size;
    new_block->memory = game_memory_pool.pool + game_memory_pool.used_size;
    new_block->is_free = 0;
    new_block->next = NULL;
    
    if (prev) {
        prev->next = new_block;
    } else {
        game_memory_pool.blocks = new_block;
    }
    
    game_memory_pool.used_size += size;
    game_memory_pool.block_count++;
    
    printf("Allocated %zu bytes at %p (new block)\n", size, new_block->memory);
    return new_block->memory;
}

// Free allocated memory
void memory_free(void* ptr) {
    if (!ptr) return;
    
    MemoryBlock* current = game_memory_pool.blocks;
    MemoryBlock* prev = NULL;
    
    while (current) {
        if (current->memory == ptr) {
            current->is_free = 1;
            game_memory_pool.used_size -= current->size;
            
            // Merge with adjacent free blocks
            MemoryBlock* next = current->next;
            if (next && next->is_free) {
                current->size += next->size + sizeof(MemoryBlock);
                current->next = next->next;
                free(next);
            }
            
            if (prev && prev->is_free) {
                prev->size += current->size + sizeof(MemoryBlock);
                prev->next = current->next;
                free(current);
            }
            
            printf("Freed memory at %p\n", ptr);
            return;
        }
        
        prev = current;
        current = current->next;
    }
    
    fprintf(stderr, "Attempted to free unallocated memory at %p\n", ptr);
}

// Print memory statistics
void memory_stats() {
    printf("\n=== Memory Pool Statistics ===\n");
    printf("Total size: %zu bytes\n", game_memory_pool.total_size);
    printf("Used size: %zu bytes\n", game_memory_pool.used_size);
    printf("Free size: %zu bytes\n", game_memory_pool.total_size - game_memory_pool.used_size);
    printf("Block count: %d\n", game_memory_pool.block_count);
    
    MemoryBlock* current = game_memory_pool.blocks;
    int block_num = 0;
    
    while (current) {
        printf("Block %d: %s, size: %zu bytes, address: %p\n", 
               block_num, 
               current->is_free ? "FREE" : "USED",
               current->size,
               current->memory);
        current = current->next;
        block_num++;
    }
}

// Cleanup memory pool
void memory_pool_cleanup() {
    MemoryBlock* current = game_memory_pool.blocks;
    while (current) {
        MemoryBlock* next = current->next;
        free(current);
        current = next;
    }
    
    if (game_memory_pool.pool) {
        free(game_memory_pool.pool);
    }
    
    printf("Memory pool cleaned up\n");
}

// Game object allocation wrapper
typedef struct GameObject {
    char name[32];
    float position[2];
    float velocity[2];
    int health;
    void (*update)(struct GameObject*);
    void (*render)(struct GameObject*);
} GameObject;

// Create new game object
GameObject* game_object_create(const char* name) {
    GameObject* obj = (GameObject*)memory_alloc(sizeof(GameObject));
    if (!obj) return NULL;
    
    strncpy(obj->name, name, 31);
    obj->name[31] = '\0';
    obj->position[0] = 0.0f;
    obj->position[1] = 0.0f;
    obj->velocity[0] = 0.0f;
    obj->velocity[1] = 0.0f;
    obj->health = 100;
    obj->update = NULL;
    obj->render = NULL;
    
    printf("Created game object: %s\n", name);
    return obj;
}

// Destroy game object
void game_object_destroy(GameObject* obj) {
    if (obj) {
        printf("Destroyed game object: %s\n", obj->name);
        memory_free(obj);
    }
}
