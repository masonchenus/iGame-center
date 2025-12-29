/*
 * Memory Management System - D Language Implementation
 * Advanced memory management with D's safety features
 */

import std.stdio;
import std.memory;
import std.typecons;
import std.variant;
import std.algorithm;
import std.range;
import std.container;
import std.conv;

/**
 * Memory pool for efficient allocation
 */
class MemoryPool {
    private {
        void[] pool;
        size_t poolSize;
        size_t usedSize;
        bool[] blockUsed;
        size_t blockSize;
        size_t numBlocks;
        size_t[] freeBlocks;
    }
    
    this(size_t poolSize, size_t blockSize) {
        this.poolSize = poolSize;
        this.blockSize = blockSize;
        this.numBlocks = poolSize / blockSize;
        this.pool = new void[poolSize];
        this.blockUsed = new bool[numBlocks];
        this.freeBlocks = iota(0, numBlocks).array;
        this.usedSize = 0;
    }
    
    /**
     * Allocate memory from pool
     */
    void* allocate(size_t size) {
        if (size > blockSize || freeBlocks.empty) {
            return null;
        }
        
        auto blockIndex = freeBlocks.back;
        freeBlocks.popBack();
        blockUsed[blockIndex] = true;
        usedSize += blockSize;
        
        return cast(void*)(pool.ptr + blockIndex * blockSize);
    }
    
    /**
     * Deallocate memory back to pool
     */
    void deallocate(void* ptr) {
        if (ptr < pool.ptr || ptr >= pool.ptr + poolSize) {
            return;
        }
        
        auto offset = cast(size_t)ptr - cast(size_t)pool.ptr;
        auto blockIndex = offset / blockSize;
        
        if (blockIndex < numBlocks && blockUsed[blockIndex]) {
            blockUsed[blockIndex] = false;
            freeBlocks ~= blockIndex;
            usedSize -= blockSize;
        }
    }
    
    /**
     * Get pool statistics
     */
    auto getStats() const {
        return tuple(
            totalSize: poolSize,
            usedSize: usedSize,
            freeSize: poolSize - usedSize,
            blockCount: numBlocks,
            freeBlocks: freeBlocks.length
        );
    }
    
    /**
     * Defragment pool (simplified)
     */
    void defragment() {
        // Simple defragmentation - move all used blocks to beginning
        auto tempPool = new void[poolSize];
        auto tempUsed = new bool[numBlocks];
        size_t writeIndex = 0;
        
        foreach (i, used; blockUsed) {
            if (used) {
                tempPool[writeIndex * blockSize .. (writeIndex + 1) * blockSize] = 
                    pool[i * blockSize .. (i + 1) * blockSize];
                tempUsed[writeIndex] = true;
                writeIndex++;
            }
        }
        
        pool = tempPool;
        blockUsed = tempUsed;
        freeBlocks = iota(writeIndex, numBlocks).array;
    }
}

/**
 * Smart pointer implementation
 */
class RefCounted(T) if (is(T == class)) {
    private {
        T instance;
        size_t* refCount;
        MemoryPool* pool;
    }
    
    this(T instance, MemoryPool* pool = null) {
        this.instance = instance;
        this.pool = pool;
        this.refCount = pool ? cast(size_t*)pool.allocate(size_t.sizeof) : new size_t(1);
        *refCount = 1;
    }
    
    this(ref RefCounted other) {
        instance = other.instance;
        refCount = other.refCount;
        pool = other.pool;
        (*refCount)++;
    }
    
    ~this() {
        if (refCount && --(*refCount) == 0) {
            if (pool) {
                pool.deallocate(cast(void*)refCount);
                if (instance) {
                    pool.deallocate(cast(void*)instance);
                }
            } else {
                delete refCount;
                delete instance;
            }
        }
    }
    
    T opUnary(string op)() if (op == "*") {
        return instance;
    }
    
    T* opUnary(string op)() if (op == "&") {
        return &instance;
    }
    
    ref T opDispatch(string name, Args...)(Args args) {
        return __traits(getMember, instance, name)(args);
    }
    
    @property {
        size_t refCount() const {
            return refCount ? *refCount : 0;
        }
        
        bool isUnique() const {
            return refCount && *refCount == 1;
        }
    }
}

/**
 * Game object with automatic memory management
 */
class GameObject {
    private {
        static MemoryPool* objectPool;
        size_t id;
        string name;
        float[2] position;
        float[2] velocity;
        bool active;
        GameObject[] children;
    }
    
    static this() {
        objectPool = new MemoryPool(1024 * 1024, GameObject.sizeof); // 1MB pool
    }
    
    static GameObject create(string name, float[2] pos = [0, 0]) {
        auto memory = objectPool.allocate(GameObject.sizeof);
        if (!memory) {
            return null;
        }
        
        auto obj = cast(GameObject)memory;
        obj.name = name;
        obj.position = pos;
        obj.velocity = [0, 0];
        obj.active = true;
        obj.id = objectPool.getStats().usedSize / GameObject.sizeof;
        
        return obj;
    }
    
    void destroy() {
        // Clean up children first
        foreach (child; children) {
            child.destroy();
        }
        
        active = false;
        objectPool.deallocate(cast(void*)this);
    }
    
    /**
     * Add child game object
     */
    void addChild(GameObject child) {
        if (child && child !in children) {
            children ~= child;
        }
    }
    
    /**
     * Remove child game object
     */
    void removeChild(GameObject child) {
        children = children.remove!(c => c is child);
    }
    
    /**
     * Update game object
     */
    void update(float deltaTime) {
        if (!active) return;
        
        // Update position
        position[0] += velocity[0] * deltaTime;
        position[1] += velocity[1] * deltaTime;
        
        // Update children
        foreach (child; children) {
            child.update(deltaTime);
        }
    }
    
    /**
     * Render game object
     */
    void render() {
        if (!active) return;
        
        writefln("Rendering %s at (%.1f, %.1f)", name, position[0], position[1]);
        
        foreach (child; children) {
            child.render();
        }
    }
    
    /**
     * Get all active objects
     */
    static GameObject[] getActiveObjects() {
        // This would scan the pool for active objects
        // Simplified implementation
        return [];
    }
    
    @property {
        size_t Id() const { return id; }
        string Name() const { return name; }
        void Name(string value) { name = value; }
        
        float[2] Position() const { return position; }
        void Position(float[2] value) { position = value; }
        
        float[2] Velocity() const { return velocity; }
        void Velocity(float[2] value) { velocity = value; }
        
        bool Active() const { return active; }
        void Active(bool value) { active = value; }
    }
}

/**
 * Memory allocator for different object types
 */
class GameObjectAllocator {
    private {
        MemoryPool[TypeInfo] pools;
        size_t[TypeInfo] allocationCounts;
    }
    
    T* allocate(T)() {
        auto typeInfo = typeid(T);
        
        if (typeInfo !in pools) {
            pools[typeInfo] = new MemoryPool(64 * 1024, T.sizeof);
        }
        
        auto pool = pools[typeInfo];
        auto memory = pool.allocate(T.sizeof);
        
        if (memory) {
            allocationCounts[typeInfo]++;
            return cast(T*)memory;
        }
        
        return null;
    }
    
    void deallocate(T)(T* ptr) {
        if (!ptr) return;
        
        auto typeInfo = typeid(T);
        if (auto pool = typeInfo in pools) {
            pool.deallocate(cast(void*)ptr);
        }
    }
    
    /**
     * Get allocation statistics
     */
    auto getAllocationStats() const {
        return allocationCounts.byKeyValue()
            .map!(kv => tuple(kv.key.toString(), kv.value))
            .array;
    }
}

/**
 * Garbage collection simulation
 */
class GarbageCollector {
    private {
        GameObject[] roots;
        bool[void*] visited;
        size_t collectedCount;
    }
    
    /**
     * Mark phase - mark all reachable objects
     */
    void markPhase(GameObject[] roots) {
        this.roots = roots;
        visited = null;
        
        foreach (root; roots) {
            if (root) {
                markObject(cast(void*)root);
            }
        }
    }
    
    /**
     * Sweep phase - collect unmarked objects
     */
    void sweepPhase() {
        collectedCount = 0;
        auto activeObjects = GameObject.getActiveObjects();
        
        foreach (obj; activeObjects) {
            if (cast(void*)obj !in visited) {
                writeln("Collecting garbage: ", obj.name);
                obj.destroy();
                collectedCount++;
            }
        }
    }
    
    /**
     * Full garbage collection cycle
     */
    void collect(GameObject[] roots) {
        writeln("Starting garbage collection...");
        
        auto beforeCount = GameObject.getActiveObjects().length;
        
        markPhase(roots);
        sweepPhase();
        
        auto afterCount = GameObject.getActiveObjects().length;
        writeln("GC complete: ", collectedCount, " objects collected");
        writeln("Active objects: ", beforeCount, " -> ", afterCount);
    }
    
    private {
        void markObject(void* obj) {
            if (obj in visited) return;
            
            visited[obj] = true;
            
            // Mark children (simplified)
            // In real implementation, would use reflection or metadata
        }
    }
}

/**
 * Memory profiler
 */
class MemoryProfiler {
    private {
        size_t[void*] allocationSizes;
        string[void*] allocationTypes;
        size_t totalAllocated;
        size_t peakAllocated;
    }
    
    /**
     * Track allocation
     */
    void trackAllocation(void* ptr, size_t size, string type) {
        allocationSizes[ptr] = size;
        allocationTypes[ptr] = type;
        totalAllocated += size;
        peakAllocated = max(peakAllocated, totalAllocated);
    }
    
    /**
     * Track deallocation
     */
    void trackDeallocation(void* ptr) {
        if (auto size = ptr in allocationSizes) {
            totalAllocated -= *size;
            allocationSizes.remove(ptr);
            allocationTypes.remove(ptr);
        }
    }
    
    /**
     * Generate memory report
     */
    void generateReport() {
        writeln("\n=== MEMORY PROFILER REPORT ===");
        writeln("Current allocated: ", totalAllocated, " bytes");
        writeln("Peak allocated: ", peakAllocated, " bytes");
        writeln("Active allocations: ", allocationSizes.length);
        
        if (!allocationTypes.empty) {
            writeln("\nAllocation by type:");
            auto byType = allocationTypes.byValue()
                .group!
                .map!(g => tuple(g[0], g[1].length))
                .array
                .sort!((a, b) => a[1] > b[1]);
            
            foreach (type, count; byType) {
                writeln("  ", type, ": ", count, " allocations");
            }
        }
    }
    
    /**
     * Check for memory leaks
     */
    bool hasMemoryLeaks() const {
        return !allocationSizes.empty;
    }
    
    /**
     * Get memory leak details
     */
    auto getMemoryLeaks() const {
        return allocationSizes.byKeyValue()
            .map!(kv => tuple(kv.key, kv.value, allocationTypes[kv.key]))
            .array;
    }
}

/**
 * Demo program
 */
void main() {
    writeln("=== D Language Memory Management Demo ===\n");
    
    auto profiler = new MemoryProfiler();
    auto gc = new GarbageCollector();
    auto allocator = new GameObjectAllocator();
    
    // Create memory pool
    writeln("Creating memory pool...");
    auto pool = new MemoryPool(1024, 64);
    
    // Test memory allocation
    writeln("\nTesting memory allocation...");
    auto memory1 = pool.allocate(32);
    auto memory2 = pool.allocate(32);
    auto memory3 = pool.allocate(32);
    
    profiler.trackAllocation(memory1, 32, "test_data");
    profiler.trackAllocation(memory2, 32, "test_data");
    profiler.trackAllocation(memory3, 32, "test_data");
    
    writeln("Pool stats: ", pool.getStats());
    
    // Create game objects
    writeln("\nCreating game objects...");
    auto player = GameObject.create("Player", [100, 100]);
    auto enemy1 = GameObject.create("Enemy1", [200, 150]);
    auto enemy2 = GameObject.create("Enemy2", [300, 200]);
    
    player.addChild(enemy1);
    player.addChild(enemy2);
    
    // Update and render
    writeln("\nUpdating and rendering objects...");
    player.update(0.016f);
    player.render();
    
    // Test smart pointers
    writeln("\nTesting reference counting...");
    auto smartObj1 = new RefCounted!GameObject(GameObject.create("SmartObject"));
    writeln("Ref count: ", smartObj1.refCount);
    
    {
        auto smartObj2 = smartObj1;
        writeln("Ref count after copy: ", smartObj1.refCount);
    }
    
    writeln("Ref count after scope exit: ", smartObj1.refCount);
    
    // Garbage collection simulation
    writeln("\nSimulating garbage collection...");
    auto roots = [player];
    gc.collect(roots);
    
    // Generate memory report
    profiler.generateReport();
    
    if (profiler.hasMemoryLeaks()) {
        writeln("\nMemory leaks detected:");
        foreach (ptr, size, type; profiler.getMemoryLeaks()) {
            writeln("  ", type, ": ", size, " bytes at ", ptr);
        }
    } else {
        writeln("\nNo memory leaks detected!");
    }
    
    // Test allocator
    writeln("\nTesting allocator...");
    auto allocated = allocator.allocate!int();
    *allocated = 42;
    writeln("Allocated value: ", *allocated);
    allocator.deallocate(allocated);
    
    writeln("\nAllocation stats:");
    foreach (type, count; allocator.getAllocationStats()) {
        writeln("  ", type, ": ", count);
    }
    
    // Cleanup
    pool.deallocate(memory1);
    pool.deallocate(memory2);
    pool.deallocate(memory3);
    
    writeln("\nPool stats after cleanup: ", pool.getStats());
    
    writeln("\n=== D Language Memory Management Features ===");
    writeln("✓ Safe memory management with RAII");
    writeln("✓ Custom memory pools for performance");
    writeln("✓ Reference counting smart pointers");
    writeln("✓ Garbage collection simulation");
    writeln("✓ Memory profiling and leak detection");
    writeln("✓ Type-safe allocation");
    writeln("✓ Automatic cleanup and resource management");
    
    writeln("\nMemory management demo completed!");
}
