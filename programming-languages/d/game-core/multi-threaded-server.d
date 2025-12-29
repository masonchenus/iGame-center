/*
 * Multi-threaded Game Server - D Language Implementation
 * High-performance concurrent game server with D's concurrency features
 */

import std.stdio;
import std.concurrency;
import std.parallelism;
import std.range;
import std.array;
import std.socket;
import std.thread;
import std.datetime;
import std.typecons;
import std.variant;
import std.format;

/**
 * Player connection state
 */
struct Player {
    Tid threadId;
    string playerId;
    Socket socket;
    Position position;
    int score;
    bool connected;
    SysTime lastActivity;
    
    this(Tid id, string pid, Socket sock) {
        threadId = id;
        playerId = pid;
        socket = sock;
        position = Position(0, 0);
        score = 0;
        connected = true;
        lastActivity = Clock.currTime();
    }
}

/**
 * Game world position
 */
struct Position {
    float x, y;
    
    this(float x, float y) {
        this.x = x;
        this.y = y;
    }
    
    Position opBinary(string op)(Position other) if (op == "+") {
        return Position(x + other.x, y + other.y);
    }
    
    Position opBinary(string op)(Position other) if (op == "-") {
        return Position(x - other.x, y - other.y);
    }
    
    float distance(Position other) const {
        auto dx = x - other.x;
        auto dy = y - other.y;
        return sqrt(dx * dx + dy * dy);
    }
}

/**
 * Game world state
 */
class GameWorld {
    private {
        Player[string] players;
        Position[ulong] gameObjects;
        Mutex worldMutex;
        TickDuration lastUpdate;
        float worldSize;
    }
    
    this(float worldSize = 1000.0f) {
        this.worldSize = worldSize;
        this.worldMutex = new Mutex();
        this.lastUpdate = Clock.currTime().toTickDuration;
    }
    
    /**
     * Add player to world
     */
    bool addPlayer(Player player) {
        synchronized(worldMutex) {
            if (player.playerId in players) {
                return false;
            }
            players[player.playerId] = player;
            return true;
        }
    }
    
    /**
     * Remove player from world
     */
    void removePlayer(string playerId) {
        synchronized(worldMutex) {
            players.remove(playerId);
        }
    }
    
    /**
     * Update player position
     */
    void updatePlayerPosition(string playerId, Position newPos) {
        synchronized(worldMutex) {
            if (auto player = playerId in players) {
                player.position = newPos;
                player.lastActivity = Clock.currTime();
            }
        }
    }
    
    /**
     * Get all players in range
     */
    Player[] getPlayersInRange(Position center, float range) {
        synchronized(worldMutex) {
            return players.byValue()
                .filter!(p => p.position.distance(center) <= range)
                .array;
        }
    }
    
    /**
     * Broadcast message to all players
     */
    void broadcast(string message) {
        synchronized(worldMutex) {
            foreach (player; players.byValue()) {
                if (player.connected) {
                    player.threadId.send(GameMessage("broadcast", message));
                }
            }
        }
    }
    
    /**
     * Update game world (physics, AI, etc.)
     */
    void updateWorld() {
        auto currentTime = Clock.currTime().toTickDuration;
        auto deltaTime = (currentTime - lastUpdate).to!"msecs" / 1000.0f;
        lastUpdate = currentTime;
        
        synchronized(worldMutex) {
            // Update game objects (parallel processing)
            auto objectUpdates = parallel(gameObjects.byKeyValue().map!(kv => updateGameObject(kv.key, kv.value, deltaTime)));
            
            // Update player states
            foreach (player; players.byValue()) {
                updatePlayerState(player, deltaTime);
            }
        }
    }
    
    private {
        Position updateGameObject(ulong objId, Position pos, float deltaTime) {
            // Simple AI movement (could be much more complex)
            auto newX = pos.x + cos(cast(float)Clock.currTime().toUnixTime * 0.1) * 10 * deltaTime;
            auto newY = pos.y + sin(cast(float)Clock.currTime().toUnixTime * 0.1) * 10 * deltaTime;
            
            // Keep within world bounds
            newX = max(-worldSize, min(worldSize, newX));
            newY = max(-worldSize, min(worldSize, newY));
            
            return Position(newX, newY);
        }
        
        void updatePlayerState(ref Player player, float deltaTime) {
            // Check for idle players
            auto idleTime = Clock.currTime() - player.lastActivity;
            if (idleTime.total!"seconds" > 300) { // 5 minutes
                player.connected = false;
            }
        }
    }
}

/**
 * Game message types
 */
alias GameMessage = Variant!(string, Player, Position, int, bool);

/**
 * Player connection handler
 */
void playerConnectionHandler(Socket clientSocket, shared GameWorld world) {
    auto playerId = generatePlayerId();
    auto threadId = thisTid;
    auto player = Player(threadId, playerId, clientSocket);
    
    writeln("New player connected: ", playerId);
    
    // Add player to world
    cast(GameWorld)world.addPlayer(player);
    
    // Send welcome message
    clientSocket.send("Welcome to the game! Your ID: " ~ playerId ~ "\n");
    
    // Send initial world state
    auto playersInRange = world.getPlayersInRange(player.position, 500.0f);
    clientSocket.send("Players in range: " ~ to!string(playersInRange.length) ~ "\n");
    
    // Handle player messages
    bool running = true;
    while (running && player.connected) {
        try {
            char[1024] buffer;
            auto bytesReceived = clientSocket.receive(buffer);
            
            if (bytesReceived > 0) {
                auto message = strip(buffer[0..bytesReceived].idup);
                auto response = processPlayerMessage(player, message);
                
                if (response) {
                    clientSocket.send(response ~ "\n");
                }
                
                // Update player activity
                player.lastActivity = Clock.currTime();
            } else {
                // Client disconnected
                running = false;
            }
        } catch (Exception e) {
            writeln("Error handling player ", playerId, ": ", e.msg);
            running = false;
        }
    }
    
    // Cleanup
    cast(GameWorld)world.removePlayer(playerId);
    clientSocket.close();
    writeln("Player disconnected: ", playerId);
}

/**
 * Generate unique player ID
 */
string generatePlayerId() {
    import std.uuid : randomUUID;
    return "player_" ~ randomUUID().toString()[0..8];
}

/**
 * Process player input messages
 */
string processPlayerMessage(ref Player player, string message) {
    import std.algorithm : canFind;
    import std.conv : to;
    
    if (message.canFind("move")) {
        // Parse movement command: "move x y"
        auto parts = message.split();
        if (parts.length >= 3) {
            try {
                auto x = parts[1].to!float;
                auto y = parts[2].to!float;
                player.position = Position(x, y);
                return "Moved to (" ~ to!string(x) ~ ", " ~ to!string(y) ~ ")";
            } catch (Exception e) {
                return "Invalid move coordinates";
            }
        }
    } else if (message.canFind("score")) {
        return "Current score: " ~ to!string(player.score);
    } else if (message.canFind("where")) {
        return "Your position: (" ~ to!string(player.position.x) ~ ", " ~ to!string(player.position.y) ~ ")";
    } else if (message.canFind("quit")) {
        player.connected = false;
        return "Goodbye!";
    } else if (message.canFind("help")) {
        return "Commands: move x y, score, where, quit, help";
    }
    
    return "Unknown command. Type 'help' for available commands.";
}

/**
 * Game server main loop
 */
void gameServer(ushort port, shared GameWorld world) {
    auto serverSocket = new Socket(AddressFamily.INET, SocketType.STREAM);
    serverSocket.bind(new InternetAddress("localhost", port));
    serverSocket.listen(10);
    
    writeln("Game server listening on port ", port);
    
    // Start world update thread
    auto worldUpdateThread = spawn(&worldUpdateLoop, cast(shared)world);
    
    // Accept client connections
    while (true) {
        auto clientSocket = serverSocket.accept();
        spawn(&playerConnectionHandler, clientSocket, world);
    }
}

/**
 * World update loop (runs in separate thread)
 */
void worldUpdateLoop(shared GameWorld world) {
    while (true) {
        try {
            cast(GameWorld)world.updateWorld();
            Thread.sleep(16.msecs); // ~60 FPS
        } catch (Exception e) {
            writeln("World update error: ", e.msg);
        }
    }
}

/**
 * Performance monitor
 */
class PerformanceMonitor {
    private {
        TickDuration lastTick;
        int frames;
        int tickRate;
        size_t memoryUsage;
        int playerCount;
    }
    
    this() {
        lastTick = Clock.currTime().toTickDuration;
    }
    
    void update(shared GameWorld world) {
        auto currentTick = Clock.currTime().toTickDuration;
        auto deltaTime = (currentTick - lastTick).to!"msecs";
        frames++;
        
        // Calculate tick rate every second
        if (deltaTime >= 1000) {
            tickRate = frames;
            frames = 0;
            lastTick = currentTick;
            
            // Update stats
            playerCount = cast(int)cast(GameWorld)world.getPlayersInRange(Position(0, 0), float.max).length;
            memoryUsage = 0; // Would get actual memory usage in real implementation
            
            // Log performance
            writeln("Performance: ", tickRate, " FPS, ", playerCount, " players, ", 
                   memoryUsage / 1024 / 1024, " MB");
        }
    }
    
    @property {
        int TickRate() const { return tickRate; }
        size_t MemoryUsage() const { return memoryUsage; }
        int PlayerCount() const { return playerCount; }
    }
}

/**
 * Load balancer for distributing players
 */
class LoadBalancer {
    private {
        GameWorld[ulong] gameWorlds;
        ulong nextWorldId;
        Mutex balancerMutex;
        int maxPlayersPerWorld;
    }
    
    this(int maxPlayers = 100) {
        this.maxPlayersPerWorld = maxPlayers;
        this.balancerMutex = new Mutex();
        this.nextWorldId = 1;
        
        // Create initial world
        createNewWorld();
    }
    
    /**
     * Assign player to appropriate world
     */
    GameWorld assignPlayer() {
        synchronized(balancerMutex) {
            foreach (world; gameWorlds.byValue()) {
                auto playerCount = world.getPlayersInRange(Position(0, 0), float.max).length;
                if (playerCount < maxPlayersPerWorld) {
                    return world;
                }
            }
            
            // All worlds full, create new one
            return createNewWorld();
        }
    }
    
    private {
        GameWorld createNewWorld() {
            auto worldId = nextWorldId++;
            auto world = new GameWorld();
            gameWorlds[worldId] = world;
            writeln("Created new game world: ", worldId);
            return world;
        }
    }
}

/**
 * Demo program
 */
void main() {
    writeln("=== D Language Multi-threaded Game Server Demo ===\n");
    
    // Create shared game world
    shared GameWorld world = new shared GameWorld(500.0f);
    
    // Create performance monitor
    auto monitor = new PerformanceMonitor();
    
    // Create load balancer
    auto loadBalancer = new LoadBalancer();
    
    // Start game server in background thread
    auto serverThread = spawn(&gameServer, 8080, world);
    
    writeln("Server started. Testing connections...\n");
    
    // Simulate some player connections
    foreach (i; 0..5) {
        auto testSocket = new Socket(AddressFamily.INET, SocketType.STREAM);
        testSocket.connect(new InternetAddress("localhost", 8080));
        
        // Send some test messages
        testSocket.send("move " ~ to!string(i * 100) ~ " " ~ to!string(i * 50) ~ "\n");
        testSocket.send("score\n");
        testSocket.send("where\n");
        
        char[1024] buffer;
        auto response = testSocket.receive(buffer);
        if (response > 0) {
            writeln("Server response: ", strip(buffer[0..response].idup));
        }
        
        Thread.sleep(100.msecs);
        testSocket.close();
    }
    
    // Run performance monitoring
    writeln("\nMonitoring server performance...");
    foreach (i; 0..60) { // Monitor for 1 minute
        monitor.update(world);
        Thread.sleep(1000.msecs); // Update every second
    }
    
    writeln("\n=== Server Performance Summary ===");
    writeln("Average Tick Rate: ", monitor.TickRate, " FPS");
    writeln("Peak Memory Usage: ", monitor.MemoryUsage / 1024 / 1024, " MB");
    writeln("Load Balancing: ", loadBalancer.gameWorlds.length, " active worlds");
    
    writeln("\n=== D Language Multi-threading Features ===");
    writeln("✓ Actor-based concurrency with std.concurrency");
    writeln("✓ Shared state management with synchronized blocks");
    writeln("✓ Parallel algorithms for game world updates");
    writeln("✓ High-performance socket programming");
    writeln("✓ Real-time performance monitoring");
    writeln("✓ Load balancing across multiple game worlds");
    writeln("✓ Thread-safe player management");
    
    writeln("\nMulti-threaded game server demo completed!");
    
    // Cleanup (in real application, would handle gracefully)
    serverThread.send(0); // Signal shutdown
}
