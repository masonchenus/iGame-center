/*
 * Modern Game Manager using D Language features
 * Demonstrates D's powerful features for game development
 */

import std.stdio;
import std.container;
import std.typecons;
import std.variant;
import std.algorithm;
import std.range;
import std.datetime;

/**
 * Game object with D's powerful features
 */
class GameObject {
    private {
        string name;
        bool isActive;
        float[2] position;
        float[2] velocity;
        float rotation;
        float[2] scale;
        Component[] components;
    }
    
    this(string name) {
        this.name = name;
        this.isActive = true;
        this.position = [0.0f, 0.0f];
        this.velocity = [0.0f, 0.0f];
        this.rotation = 0.0f;
        this.scale = [1.0f, 1.0f];
    }
    
    // D-style property accessors
    @property {
        string Name() const { return name; }
        void Name(string value) { name = value; }
        
        bool Active() const { return isActive; }
        void Active(bool value) { isActive = value; }
        
        float[2] Position() const { return position; }
        void Position(float[2] value) { position = value; }
        
        float[2] Velocity() const { return velocity; }
        void Velocity(float[2] value) { velocity = value; }
    }
    
    // Template method for component addition
    void addComponent(T : Component)(T component) if (is(T : Component)) {
        components ~= component;
        writeln("Added component of type ", T.stringof, " to ", name);
    }
    
    // D's powerful UFCS (Uniform Function Call Syntax)
    void initialize() {
        writeln("Initializing GameObject: ", name);
        foreach (component; components) {
            component.initialize();
        }
    }
    
    void update(float deltaTime) {
        if (!isActive) return;
        
        // Update position based on velocity
        position[0] += velocity[0] * deltaTime;
        position[1] += velocity[1] * deltaTime;
        
        // Update all components
        foreach (component; components) {
            component.update(deltaTime);
        }
    }
    
    void render() {
        if (!isActive) return;
        
        writeln("Rendering ", name, " at position (", position[0], ", ", position[1], ")");
    }
    
    void cleanup() {
        writeln("Cleaning up GameObject: ", name);
        components = null;
    }
}

/**
 * Base Component class with D interfaces
 */
interface Component {
    void initialize();
    void update(float deltaTime);
    void render();
}

/**
 * Transform component with D's features
 */
class TransformComponent : Component {
    private {
        float[2] position;
        float rotation;
        float[2] scale;
    }
    
    this() {
        position = [0.0f, 0.0f];
        rotation = 0.0f;
        scale = [1.0f, 1.0f];
    }
    
    void initialize() {
        writeln("Transform component initialized");
    }
    
    void update(float deltaTime) {
        // Transform updates handled by GameObject
    }
    
    void render() {
        // Transform doesn't render directly
    }
    
    // D-style property accessors
    @property {
        float[2] Position() const { return position; }
        void Position(float[2] value) { position = value; }
        
        float Rotation() const { return rotation; }
        void Rotation(float value) { rotation = value; }
        
        float[2] Scale() const { return scale; }
        void Scale(float[2] value) { scale = value; }
    }
}

/**
 * Physics component using D's ranges and algorithms
 */
class PhysicsComponent : Component {
    private {
        float[2] acceleration;
        float mass;
        bool useGravity;
        float[2] gravity;
    }
    
    this(bool useGravity = true, float mass = 1.0f) {
        this.acceleration = [0.0f, 0.0f];
        this.mass = mass;
        this.useGravity = useGravity;
        this.gravity = [0.0f, -9.81f]; // Earth gravity
    }
    
    void initialize() {
        writeln("Physics component initialized with mass: ", mass);
    }
    
    void update(float deltaTime) {
        // D's powerful algorithm usage for physics calculations
        if (useGravity) {
            acceleration = gravity;
        }
        
        // Apply acceleration
        velocity[0] += acceleration[0] * deltaTime;
        velocity[1] += acceleration[1] * deltaTime;
    }
    
    void render() {
        // Physics doesn't render directly
    }
    
    // D's uniform function call syntax
    @property {
        float[2] Velocity() const { return velocity; }
        void Velocity(float[2] value) { velocity = value; }
        
        float[2] Acceleration() const { return acceleration; }
        void Acceleration(float[2] value) { acceleration = value; }
        
        float Mass() const { return mass; }
        void Mass(float value) { mass = value; }
    }
    
    private float[2] velocity;
}

/**
 * Game Manager with D's powerful features
 */
class GameManager {
    private {
        bool isRunning;
        GameObject[] gameObjects;
        float deltaTime;
        SysTime lastUpdateTime;
    }
    
    this() {
        isRunning = false;
        deltaTime = 0.0f;
        lastUpdateTime = Clock.currTime();
    }
    
    void initialize() {
        writeln("Game Manager initialized with D language features");
        isRunning = true;
    }
    
    void update() {
        auto currentTime = Clock.currTime();
        deltaTime = cast(float)((currentTime - lastUpdateTime).total!"msecs") / 1000.0f;
        lastUpdateTime = currentTime;
        
        // D's powerful range algorithms for game object updates
        gameObjects.filter!(obj => obj.Active)
                   .each!(obj => obj.update(deltaTime));
    }
    
    void render() {
        // Clear screen (console simulation)
        writeln("\x1B[2J\x1B[H");
        
        // D's range algorithms for rendering
        gameObjects.filter!(obj => obj.Active)
                   .each!(obj => obj.render());
    }
    
    void shutdown() {
        writeln("Shutting down Game Manager...");
        
        // D's array operations
        gameObjects.each!(obj => obj.cleanup());
        gameObjects = null;
        isRunning = false;
    }
    
    void addGameObject(GameObject gameObject) {
        gameObjects ~= gameObject;
    }
    
    void removeGameObject(GameObject gameObject) {
        // D's powerful array manipulation
        gameObjects = gameObjects.remove!(obj => obj is gameObject);
    }
    
    GameObject findGameObject(string name) {
        return gameObjects.find!(obj => obj.Name == name).front;
    }
    
    @property {
        bool Running() const { return isRunning; }
        float DeltaTime() const { return deltaTime; }
    }
}

/**
 * Entity-Component System implementation
 */
class ECS {
    private {
        Component[][string] componentPools;
        GameObject[ulong] entities;
        ulong nextEntityId;
    }
    
    this() {
        nextEntityId = 0;
    }
    
    ulong createEntity(string name) {
        auto entityId = nextEntityId++;
        entities[entityId] = new GameObject(name);
        return entityId;
    }
    
    void addComponent(ulong entityId, Component component) {
        auto componentType = typeid(component).toString();
        componentPools[componentType] ~= component;
        writeln("Added component ", componentType, " to entity ", entityId);
    }
    
    // D's powerful template and compile-time features
    Component[] getComponents(T : Component)() {
        auto componentType = T.stringof;
        return componentPools.get(componentType, []);
    }
    
    void update(float deltaTime) {
        // Update all components by type
        foreach (componentType, components; componentPools) {
            foreach (component; components) {
                component.update(deltaTime);
            }
        }
    }
}

/**
 * Main function demonstrating D language features
 */
void main() {
    writeln("=== D Language Game Engine Demo ===");
    
    // Create game manager
    auto gameManager = new GameManager();
    gameManager.initialize();
    
    // Create game objects with D's features
    auto player = new GameObject("Player");
    player.Position = [100.0f, 200.0f];
    player.Velocity = [50.0f, 0.0f];
    player.addComponent(new TransformComponent());
    player.addComponent(new PhysicsComponent());
    
    auto enemy = new GameObject("Enemy");
    enemy.Position = [300.0f, 150.0f];
    enemy.Velocity = [-30.0f, 0.0f];
    enemy.addComponent(new TransformComponent());
    
    auto projectile = new GameObject("Projectile");
    projectile.Position = [150.0f, 180.0f];
    projectile.Velocity = [100.0f, 10.0f];
    projectile.addComponent(new PhysicsComponent());
    
    // Add to game manager
    gameManager.addGameObject(player);
    gameManager.addGameObject(enemy);
    gameManager.addGameObject(projectile);
    
    // Demo ECS
    auto ecs = new ECS();
    auto playerId = ecs.createEntity("PlayerEntity");
    ecs.addComponent(playerId, new PhysicsComponent());
    
    // Game loop simulation
    writeln("\nStarting game loop...");
    foreach (i; 0..10) {
        writeln("\n--- Frame ", i, " ---");
        gameManager.update();
        gameManager.render();
    }
    
    gameManager.shutdown();
    writeln("Demo completed successfully!");
}
