/*
 * Entity-Component-System Game Engine - D Language Implementation
 * Modern ECS architecture demonstrating D's powerful features
 */

import std.stdio;
import std.container;
import std.typecons;
import std.variant;
import std.algorithm;
import std.range;
import std.traits;
import std.uuid;

// Entity ID type
alias EntityId = ulong;

// Component base interface
interface IComponent {
    EntityId entityId;
    string componentType() const;
}

// Component storage system
class ComponentStore(T : IComponent) if (is(T : IComponent)) {
    private T[EntityId] components;
    
    void addComponent(T component) {
        components[component.entityId] = component;
    }
    
    void removeComponent(EntityId entityId) {
        components.remove(entityId);
    }
    
    T* getComponent(EntityId entityId) {
        return entityId in components ? &components[entityId] : null;
    }
    
    T[EntityId] getAllComponents() {
        return components;
    }
    
    EntityId[] getEntitiesWithComponent() {
        return components.keys;
    }
}

// System base class
abstract class System {
    abstract void update(float deltaTime);
    abstract void onEntityAdded(EntityId entityId);
    abstract void onEntityRemoved(EntityId entityId);
}

// Transform component
class TransformComponent : IComponent {
    EntityId entityId;
    float[2] position;
    float[2] velocity;
    float rotation;
    float[2] scale;
    
    this(EntityId id, float[2] pos = [0, 0]) {
        entityId = id;
        position = pos;
        velocity = [0, 0];
        rotation = 0;
        scale = [1, 1];
    }
    
    string componentType() const {
        return "Transform";
    }
    
    @property {
        float[2] Position() const { return position; }
        void Position(float[2] value) { position = value; }
        
        float[2] Velocity() const { return velocity; }
        void Velocity(float[2] value) { velocity = value; }
        
        float Rotation() const { return rotation; }
        void Rotation(float value) { rotation = value; }
        
        float[2] Scale() const { return scale; }
        void Scale(float[2] value) { scale = value; }
    }
}

// Render component
class RenderComponent : IComponent {
    EntityId entityId;
    string spriteName;
    Color color;
    int layer;
    bool visible;
    
    this(EntityId id, string sprite = "default", Color col = Color.white) {
        entityId = id;
        spriteName = sprite;
        color = col;
        layer = 0;
        visible = true;
    }
    
    string componentType() const {
        return "Render";
    }
}

// Color utility
struct Color {
    float r, g, b, a;
    
    this(float r, float g, float b, float a = 1.0f) {
        this.r = r; this.g = g; this.b = b; this.a = a;
    }
    
    static Color white() { return Color(1, 1, 1); }
    static Color red() { return Color(1, 0, 0); }
    static Color green() { return Color(0, 1, 0); }
    static Color blue() { return Color(0, 0, 1); }
}

// Health component
class HealthComponent : IComponent {
    EntityId entityId;
    float currentHealth;
    float maxHealth;
    bool isDead;
    
    this(EntityId id, float maxHp = 100.0f) {
        entityId = id;
        currentHealth = maxHp;
        maxHealth = maxHp;
        isDead = false;
    }
    
    string componentType() const {
        return "Health";
    }
    
    void takeDamage(float damage) {
        currentHealth -= damage;
        if (currentHealth <= 0) {
            currentHealth = 0;
            isDead = true;
        }
    }
    
    void heal(float amount) {
        currentHealth = Math.min(currentHealth + amount, maxHealth);
        if (currentHealth > 0) {
            isDead = false;
        }
    }
}

// Physics system
class PhysicsSystem : System {
    private ComponentStore!TransformComponent transformStore;
    
    this(ComponentStore!TransformComponent store) {
        transformStore = store;
    }
    
    void update(float deltaTime) {
        foreach (entityId, ref transform; transformStore.getAllComponents()) {
            // Update position based on velocity
            transform.position[0] += transform.velocity[0] * deltaTime;
            transform.position[1] += transform.velocity[1] * deltaTime;
            
            // Apply friction
            transform.velocity[0] *= 0.95f;
            transform.velocity[1] *= 0.95f;
        }
    }
    
    void onEntityAdded(EntityId entityId) {
        // Handle new entity
    }
    
    void onEntityRemoved(EntityId entityId) {
        // Handle entity removal
    }
}

// Health system
class HealthSystem : System {
    private ComponentStore!HealthComponent healthStore;
    private ComponentStore!TransformComponent transformStore;
    
    this(ComponentStore!HealthComponent healthStore, 
        ComponentStore!TransformComponent transformStore) {
        this.healthStore = healthStore;
        this.transformStore = transformStore;
    }
    
    void update(float deltaTime) {
        foreach (entityId, ref health; healthStore.getAllComponents()) {
            if (health.isDead) {
                // Handle death
                auto transform = transformStore.getComponent(entityId);
                if (transform) {
                    transform.velocity = [0, 0]; // Stop movement
                    writeln("Entity ", entityId, " has died!");
                }
            }
        }
    }
    
    void onEntityAdded(EntityId entityId) {
        writeln("New entity added to HealthSystem: ", entityId);
    }
    
    void onEntityRemoved(EntityId entityId) {
        writeln("Entity removed from HealthSystem: ", entityId);
    }
}

// Render system
class RenderSystem : System {
    private ComponentStore!TransformComponent transformStore;
    private ComponentStore!RenderComponent renderStore;
    
    this(ComponentStore!TransformComponent transformStore,
        ComponentStore!RenderComponent renderStore) {
        this.transformStore = transformStore;
        this.renderStore = renderStore;
    }
    
    void update(float deltaTime) {
        // Sort by layer
        auto renderables = renderStore.getAllComponents()
            .filter!(render => render.visible)
            .array
            .sort!((a, b) => a.layer < b.layer);
        
        writeln("Rendering ", renderables.length, " entities:");
        
        foreach (render; renderables) {
            auto transform = transformStore.getComponent(render.entityId);
            if (transform) {
                writeln("  Entity ", render.entityId, ": ", render.spriteName,
                       " at (", transform.position[0], ", ", transform.position[1], ")");
            }
        }
    }
    
    void onEntityAdded(EntityId entityId) {
        writeln("New renderable entity added: ", entityId);
    }
    
    void onEntityRemoved(EntityId entityId) {
        writeln("Renderable entity removed: ", entityId);
    }
}

// Entity Manager
class EntityManager {
    private {
        EntityId nextEntityId = 1;
        EntityId[] entities;
        bool[EntityId] activeEntities;
    }
    
    EntityId createEntity() {
        auto id = nextEntityId++;
        entities ~= id;
        activeEntities[id] = true;
        return id;
    }
    
    void destroyEntity(EntityId entityId) {
        activeEntities[entityId] = false;
    }
    
    bool isActive(EntityId entityId) {
        return activeEntities.get(entityId, false);
    }
    
    EntityId[] getActiveEntities() {
        return entities.filter!(id => isActive(id)).array;
    }
}

// Game Engine
class GameEngine {
    private {
        EntityManager entityManager;
        System[] systems;
        ComponentStore!TransformComponent transformStore;
        ComponentStore!RenderComponent renderStore;
        ComponentStore!HealthComponent healthStore;
        float deltaTime;
    }
    
    this() {
        entityManager = new EntityManager();
        transformStore = new ComponentStore!TransformComponent();
        renderStore = new ComponentStore!RenderComponent();
        healthStore = new ComponentStore!HealthComponent();
        
        // Initialize systems
        systems = [
            cast(System)new PhysicsSystem(transformStore),
            cast(System)new HealthSystem(healthStore, transformStore),
            cast(System)new RenderSystem(transformStore, renderStore)
        ];
    }
    
    EntityId createEntity(float[2] position, string spriteName = "default") {
        auto entityId = entityManager.createEntity();
        
        // Add transform component
        auto transform = new TransformComponent(entityId, position);
        transformStore.addComponent(transform);
        
        // Add render component
        auto render = new RenderComponent(entityId, spriteName);
        renderStore.addComponent(render);
        
        // Add health component
        auto health = new HealthComponent(entityId);
        healthStore.addComponent(health);
        
        return entityId;
    }
    
    void destroyEntity(EntityId entityId) {
        transformStore.removeComponent(entityId);
        renderStore.removeComponent(entityId);
        healthStore.removeComponent(entityId);
        entityManager.destroyEntity(entityId);
    }
    
    void addComponent(T)(EntityId entityId, T component) if (is(T : IComponent)) {
        static if (is(T == TransformComponent)) {
            transformStore.addComponent(component);
        } else static if (is(T == RenderComponent)) {
            renderStore.addComponent(component);
        } else static if (is(T == HealthComponent)) {
            healthStore.addComponent(component);
        }
    }
    
    void update(float dt) {
        deltaTime = dt;
        
        foreach (system; systems) {
            system.update(deltaTime);
        }
    }
    
    void damageEntity(EntityId entityId, float damage) {
        auto health = healthStore.getComponent(entityId);
        if (health) {
            health.takeDamage(damage);
        }
    }
    
    void setEntityPosition(EntityId entityId, float[2] position) {
        auto transform = transformStore.getComponent(entityId);
        if (transform) {
            transform.Position = position;
        }
    }
    
    float[2] getEntityPosition(EntityId entityId) {
        auto transform = transformStore.getComponent(entityId);
        return transform ? transform.Position : [0, 0];
    }
}

// Demo program
void main() {
    writeln("=== D Language ECS Game Engine Demo ===\n");
    
    auto engine = new GameEngine();
    
    // Create entities
    writeln("Creating game entities...");
    auto playerId = engine.createEntity([10, 10], "player_sprite");
    auto enemy1Id = engine.createEntity([20, 15], "enemy_sprite");
    auto enemy2Id = engine.createEntity([30, 5], "enemy_sprite");
    
    writeln("Player entity: ", playerId);
    writeln("Enemy 1 entity: ", enemy1Id);
    writeln("Enemy 2 entity: ", enemy2Id);
    
    // Set initial positions
    engine.setEntityPosition(playerId, [5, 5]);
    engine.setEntityPosition(enemy1Id, [25, 10]);
    engine.setEntityPosition(enemy2Id, [15, 20]);
    
    writeln("\n--- Game Loop Simulation ---");
    
    // Simulate game loop
    foreach (frame; 0..10) {
        writeln("\nFrame ", frame, ":");
        
        // Simulate some player movement
        if (frame % 3 == 0) {
            auto pos = engine.getEntityPosition(playerId);
            pos[0] += 1; // Move right
            engine.setEntityPosition(playerId, pos);
        }
        
        // Simulate enemy AI
        auto enemyPos = engine.getEntityPosition(enemy1Id);
        enemyPos[1] += 0.5f; // Move down
        engine.setEntityPosition(enemy1Id, enemyPos);
        
        // Damage enemy occasionally
        if (frame == 5) {
            writeln("  Damaging enemy 1...");
            engine.damageEntity(enemy1Id, 50.0f);
        }
        
        // Update systems
        engine.update(0.016f); // ~60 FPS
        
        // Show entity positions
        writeln("  Player position: ", engine.getEntityPosition(playerId));
        writeln("  Enemy 1 position: ", engine.getEntityPosition(enemy1Id));
        writeln("  Enemy 2 position: ", engine.getEntityPosition(enemy2Id));
    }
    
    writeln("\n--- D Language ECS Features Demonstrated ---");
    writeln("✓ Template metaprogramming for component systems");
    writeln("✓ Range-based algorithms for entity queries");
    writeln("✓ Type-safe component storage");
    writeln("✓ Compile-time polymorphism");
    writeln("✓ Memory safety with RAII");
    writeln("✓ High-performance data structures");
    writeln("✓ Modern D language features");
    
    writeln("\nECS Game Engine demo completed!");
}

