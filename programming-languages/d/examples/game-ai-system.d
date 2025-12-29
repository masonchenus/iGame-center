/*
 * Game AI System - D Language Implementation
 * Advanced AI behaviors for game entities
 */

import std.stdio;
import std.algorithm;
import std.range;
import std.array;
import std.typecons;
import std.conv;
import std.math;
import std.random;
import std.datetime;
import std.container;

/**
 * AI State base class
 */
abstract class AIState {
    string name;
    
    this(string stateName) {
        name = stateName;
    }
    
    abstract void enter(AIEntity entity);
    abstract void update(AIEntity entity, float deltaTime);
    abstract void exit(AIEntity entity);
}

/**
 * AI State Machine
 */
class StateMachine {
    private {
        AIState currentState;
        AIState[AIState] transitions;
    }
    
    this() {
        currentState = null;
    }
    
    void setState(AIState newState) {
        if (currentState) {
            currentState.exit(null); // Entity passed later
        }
        
        currentState = newState;
        if (currentState) {
            currentState.enter(null); // Entity passed later
        }
    }
    
    void update(AIEntity entity, float deltaTime) {
        if (currentState) {
            currentState.update(entity, deltaTime);
        }
    }
    
    @property {
        AIState CurrentState() const { return currentState; }
        string CurrentStateName() const { return currentState ? currentState.name : "None"; }
    }
}

/**
 * AI Entity base class
 */
class AIEntity {
    private {
        string id;
        float[2] position;
        float[2] velocity;
        float health;
        float maxHealth;
        StateMachine stateMachine;
        AIEntity[] knownEntities;
        float[2] targetPosition;
        bool hasTarget;
        float sightRange;
        float hearingRange;
    }
    
    this(string entityId) {
        id = entityId;
        position = [0, 0];
        velocity = [0, 0];
        health = 100.0f;
        maxHealth = 100.0f;
        sightRange = 50.0f;
        hearingRange = 25.0f;
        hasTarget = false;
        stateMachine = new StateMachine();
    }
    
    void update(float deltaTime) {
        // Update state machine
        stateMachine.update(this, deltaTime);
        
        // Update position based on velocity
        position[0] += velocity[0] * deltaTime;
        position[1] += velocity[1] * deltaTime;
    }
    
    void setTarget(float[2] target) {
        targetPosition = target;
        hasTarget = true;
    }
    
    void clearTarget() {
        hasTarget = false;
    }
    
    float distanceTo(AIEntity other) {
        auto dx = position[0] - other.position[0];
        auto dy = position[1] - other.position[1];
        return sqrt(dx * dx + dy * dy);
    }
    
    float distanceToPosition(float[2] pos) {
        auto dx = position[0] - pos[0];
        auto dy = position[1] - pos[1];
        return sqrt(dx * dx + dy * dy);
    }
    
    void moveTowards(float[2] target, float speed) {
        auto dx = target[0] - position[0];
        auto dy = target[1] - position[1];
        auto distance = sqrt(dx * dx + dy * dy);
        
        if (distance > 0.1f) {
            velocity[0] = (dx / distance) * speed;
            velocity[1] = (dy / distance) * speed;
        } else {
            velocity = [0, 0];
        }
    }
    
    @property {
        string Id() const { return id; }
        float[2] Position() const { return position; }
        void Position(float[2] value) { position = value; }
        float[2] Velocity() const { return velocity; }
        void Velocity(float[2] value) { velocity = value; }
        float Health() const { return health; }
        void Health(float value) { health = value; }
        float MaxHealth() const { return maxHealth; }
        void MaxHealth(float value) { maxHealth = value; }
        bool IsAlive() const { return health > 0; }
        float SightRange() const { return sightRange; }
        void SightRange(float value) { sightRange = value; }
        float HearingRange() const { return hearingRange; }
        void HearingRange(float value) { hearingRange = value; }
        StateMachine StateMachineRef() { return stateMachine; }
        AIEntity[] KnownEntities() { return knownEntities; }
        void KnownEntities(AIEntity[] value) { knownEntities = value; }
        bool HasTarget() const { return hasTarget; }
        float[2] TargetPosition() const { return targetPosition; }
    }
}

/**
 * Patrol state for AI
 */
class PatrolState : AIState {
    private {
        float[2][] patrolPoints;
        int currentPointIndex;
        float waitTime;
        float maxWaitTime;
    }
    
    this(float[2][] points) {
        super("Patrol");
        patrolPoints = points;
        currentPointIndex = 0;
        maxWaitTime = 2.0f;
        waitTime = 0;
    }
    
    void enter(AIEntity entity) {
        writeln(entity.Id, " started patrolling");
    }
    
    void update(AIEntity entity, float deltaTime) {
        if (patrolPoints.length == 0) return;
        
        auto currentPoint = patrolPoints[currentPointIndex];
        auto distance = entity.distanceToPosition(currentPoint);
        
        if (distance > 2.0f) {
            // Move towards patrol point
            entity.moveTowards(currentPoint, 10.0f);
        } else {
            // Reached patrol point, wait or move to next
            if (waitTime > 0) {
                waitTime -= deltaTime;
                entity.Velocity = [0, 0];
            } else {
                currentPointIndex = (currentPointIndex + 1) % patrolPoints.length;
                waitTime = maxWaitTime;
            }
        }
    }
    
    void exit(AIEntity entity) {
        writeln(entity.Id, " stopped patrolling");
    }
}

/**
 * Chase state for AI
 */
class ChaseState : AIState {
    private {
        AIEntity target;
        float lostTimer;
        float maxLostTime;
    }
    
    this() {
        super("Chase");
        target = null;
        maxLostTime = 5.0f;
        lostTimer = 0;
    }
    
    void setTarget(AIEntity newTarget) {
        target = newTarget;
        lostTimer = 0;
    }
    
    void enter(AIEntity entity) {
        writeln(entity.Id, " started chasing ", target.Id);
    }
    
    void update(AIEntity entity, float deltaTime) {
        if (!target || !target.IsAlive) {
            lostTimer += deltaTime;
            if (lostTimer > maxLostTime) {
                entity.clearTarget();
                return;
            }
        } else {
            lostTimer = 0;
        }
        
        if (target) {
            entity.moveTowards(target.Position, 15.0f);
            
            // Check if we can attack
            auto distance = entity.distanceTo(target);
            if (distance < 5.0f) {
                // Transition to attack state
                // entity.StateMachineRef().setState(new AttackState());
            }
        }
    }
    
    void exit(AIEntity entity) {
        writeln(entity.Id, " stopped chasing");
        target = null;
    }
    
    @property {
        AIEntity Target() const { return target; }
    }
}

/**
 * Attack state for AI
 */
class AttackState : AIState {
    private {
        AIEntity target;
        float attackCooldown;
        float currentCooldown;
        float attackRange;
        float attackDamage;
    }
    
    this() {
        super("Attack");
        target = null;
        attackCooldown = 1.0f;
        currentCooldown = 0;
        attackRange = 5.0f;
        attackDamage = 10.0f;
    }
    
    void setTarget(AIEntity newTarget) {
        target = newTarget;
    }
    
    void enter(AIEntity entity) {
        writeln(entity.Id, " started attacking ", target.Id);
        entity.Velocity = [0, 0]; // Stop moving when attacking
    }
    
    void update(AIEntity entity, float deltaTime) {
        if (!target || !target.IsAlive) {
            entity.clearTarget();
            return;
        }
        
        auto distance = entity.distanceTo(target);
        
        if (distance > attackRange) {
            // Target moved out of range, chase again
            entity.setTarget(target.Position);
            return;
        }
        
        // Face target (simplified)
        // In real implementation, would rotate towards target
        
        // Attack if cooldown expired
        if (currentCooldown <= 0) {
            performAttack(entity, target);
            currentCooldown = attackCooldown;
        } else {
            currentCooldown -= deltaTime;
        }
    }
    
    void exit(AIEntity entity) {
        writeln(entity.Id, " stopped attacking");
        target = null;
    }
    
    private {
        void performAttack(AIEntity attacker, AIEntity target) {
            writeln(attacker.Id, " attacks ", target.Id, " for ", attackDamage, " damage");
            target.Health -= attackDamage;
            
            if (!target.IsAlive) {
                writeln(target.Id, " has been defeated!");
            }
        }
    }
}

/**
 * Flee state for AI
 */
class FleeState : AIState {
    private {
        AIEntity threat;
        float fleeDistance;
        float[2] fleeDirection;
    }
    
    this() {
        super("Flee");
        threat = null;
        fleeDistance = 50.0f;
        fleeDirection = [0, 0];
    }
    
    void setThreat(AIEntity newThreat) {
        threat = newThreat;
        calculateFleeDirection();
    }
    
    void enter(AIEntity entity) {
        writeln(entity.Id, " started fleeing from ", threat.Id);
    }
    
    void update(AIEntity entity, float deltaTime) {
        if (!threat || !threat.IsAlive) {
            entity.clearTarget();
            return;
        }
        
        // Move away from threat
        entity.Velocity = fleeDirection * 20.0f;
        
        // Check if we've fled far enough
        auto distance = entity.distanceTo(threat);
        if (distance > fleeDistance) {
            writeln(entity.Id, " has safely fled from ", threat.Id);
            entity.clearTarget();
        }
    }
    
    void exit(AIEntity entity) {
        writeln(entity.Id, " stopped fleeing");
        threat = null;
    }
    
    private {
        void calculateFleeDirection() {
            if (threat) {
                auto dx = threat.Position[0] - threat.Position[0]; // Simplified
                auto dy = threat.Position[1] - threat.Position[1];
                auto distance = sqrt(dx * dx + dy * dy);
                
                if (distance > 0) {
                    fleeDirection = [dx / distance, dy / distance];
                }
            }
        }
    }
}

/**
 * Idle state for AI
 */
class IdleState : AIState {
    private {
        float idleTime;
        float maxIdleTime;
    }
    
    this(float maxIdle = 5.0f) {
        super("Idle");
        maxIdleTime = maxIdle;
        idleTime = 0;
    }
    
    void enter(AIEntity entity) {
        writeln(entity.Id, " is idle");
    }
    
    void update(AIEntity entity, float deltaTime) {
        idleTime += deltaTime;
        
        // Occasionally look around
        if (idleTime > maxIdleTime) {
            idleTime = 0;
            writeln(entity.Id, " looks around");
        }
    }
    
    void exit(AIEntity entity) {
        writeln(entity.Id, " stopped being idle");
    }
}

/**
 * AI Perception System
 */
class PerceptionSystem {
    private {
        AIEntity[] entities;
    }
    
    this() {
        entities = [];
    }
    
    void addEntity(AIEntity entity) {
        entities ~= entity;
    }
    
    void updatePerceptions() {
        foreach (entity; entities) {
            if (!entity.IsAlive) continue;
            
            entity.KnownEntities = [];
            
            foreach (other; entities) {
                if (entity is other || !other.IsAlive) continue;
                
                auto distance = entity.distanceTo(other);
                
                // Check sight range
                if (distance <= entity.SightRange) {
                    // Check line of sight (simplified)
                    if (hasLineOfSight(entity.Position, other.Position)) {
                        entity.KnownEntities ~= other;
                        continue;
                    }
                }
                
                // Check hearing range
                if (distance <= entity.HearingRange) {
                    entity.KnownEntities ~= other;
                }
            }
        }
    }
    
    private {
        bool hasLineOfSight(float[2] from, float[2] to) {
            // Simplified line of sight - in real implementation would use raycasting
            return true;
        }
    }
}

/**
 * AI Decision Making System
 */
class DecisionSystem {
    private {
        PerceptionSystem perception;
    }
    
    this(PerceptionSystem percSystem) {
        perception = percSystem;
    }
    
    void makeDecisions() {
        foreach (entity; perception.entities) {
            if (!entity.IsAlive) continue;
            
            processEntityDecisions(entity);
        }
    }
    
    private {
        void processEntityDecisions(AIEntity entity) {
            auto currentState = entity.StateMachineRef().CurrentState;
            auto knownEntities = entity.KnownEntities;
            
            // Priority-based decision making
            auto threats = knownEntities.filter!(e => isThreat(entity, e)).array;
            auto allies = knownEntities.filter!(e => isAlly(entity, e)).array;
            auto neutral = knownEntities.filter!(e => !isThreat(entity, e) && !isAlly(entity, e)).array;
            
            // Threat response
            if (threats.length > 0) {
                auto threat = threats[0]; // Nearest threat
                
                if (currentState && currentState.name == "Flee") {
                    // Already fleeing, continue
                    return;
                }
                
                // Decide between fight or flight
                if (shouldFlee(entity, threat)) {
                    entity.StateMachineRef().setState(new FleeState());
                    auto fleeState = cast(FleeState)entity.StateMachineRef().CurrentState;
                    fleeState.setThreat(threat);
                } else {
                    entity.StateMachineRef().setState(new ChaseState());
                    auto chaseState = cast(ChaseState)entity.StateMachineRef().CurrentState;
                    chaseState.setTarget(threat);
                }
                
                return;
            }
            
            // Combat AI
            if (currentState && (currentState.name == "Chase" || currentState.name == "Attack")) {
                auto chaseState = cast(ChaseState)currentState;
                if (chaseState && chaseState.Target) {
                    auto target = chaseState.Target;
                    
                    // Check if we should continue chasing
                    if (entity.distanceTo(target) < entity.SightRange) {
                        if (entity.distanceTo(target) < 5.0f) {
                            entity.StateMachineRef().setState(new AttackState());
                            auto attackState = cast(AttackState)entity.StateMachineRef().CurrentState;
                            attackState.setTarget(target);
                        }
                        return; // Continue current action
                    }
                }
            }
            
            // Patrol behavior
            if (!entity.HasTarget) {
                // Set random patrol target or return to patrol route
                if (currentState && currentState.name != "Patrol") {
                    auto patrolPoints = generatePatrolPoints(entity.Position);
                    entity.StateMachineRef().setState(new PatrolState(patrolPoints));
                }
            }
        }
        
        bool isThreat(AIEntity entity, AIEntity other) {
            // Simple threat detection - based on health ratio
            return other.Health > entity.Health * 1.2f;
        }
        
        bool isAlly(AIEntity entity, AIEntity other) {
            // Simple ally detection - could be faction-based
            return other.Health < entity.Health * 0.8f;
        }
        
        bool shouldFlee(AIEntity entity, AIEntity threat) {
            // Flee if significantly outmatched
            return threat.Health > entity.Health * 1.5f;
        }
        
        float[2][] generatePatrolPoints(float[2] center) {
            import std.random : Random, unpredictableSeed, uniform;
            auto rng = Random(unpredictableSeed);
            auto points = appender!(float[2])();
            
            // Generate 3-5 patrol points in a rough circle
            auto pointCount = uniform(3, 6, rng);
            auto radius = uniform(10.0f, 30.0f, rng);
            
            foreach (i; 0..pointCount) {
                auto angle = (i / cast(float)pointCount) * 2 * PI;
                auto x = center[0] + cos(angle) * radius;
                auto y = center[1] + sin(angle) * radius;
                points.put([x, y]);
            }
            
            return points.data;
        }
    }
}

/**
 * AI Animation System
 */
class AnimationSystem {
    private {
        string[AIEntity] currentAnimations;
    }
    
    void updateAnimations() {
        foreach (entity, animation; currentAnimations) {
            updateEntityAnimation(entity, animation);
        }
    }
    
    void setAnimation(AIEntity entity, string animation) {
        if (currentAnimations.get(entity, "") != animation) {
            currentAnimations[entity] = animation;
            writeln("Playing animation '", animation, "' for ", entity.Id);
        }
    }
    
    private {
        void updateEntityAnimation(AIEntity entity, string animation) {
            // In real implementation, would update animation timing
            // and handle animation events
        }
    }
}

/**
 * AI Performance Profiler
 */
class AIProfiler {
    private {
        int[AIEntity] updateCounts;
        float[AIEntity] totalUpdateTime;
        TickDuration[AIEntity] lastUpdateStart;
    }
    
    void beginUpdate(AIEntity entity) {
        lastUpdateStart[entity] = Clock.currTime().toTickDuration;
    }
    
    void endUpdate(AIEntity entity) {
        auto endTime = Clock.currTime().toTickDuration;
        auto duration = endTime - lastUpdateStart[entity];
        
        updateCounts[entity]++;
        totalUpdateTime[entity] += duration.to!"usecs";
    }
    
    void generateReport() {
        writeln("\n=== AI Performance Report ===");
        
        foreach (entity, count; updateCounts) {
            auto avgTime = totalUpdateTime[entity] / count;
            writeln(entity.Id, ": ", count, " updates, avg ", avgTime, " usecs/update");
        }
    }
}

/**
 * Demo program
 */
void main() {
    writeln("=== D Language Game AI System Demo ===\n");
    
    // Create AI entities
    auto player = new AIEntity("Player");
    player.Position = [10, 10];
    player.Health = 100;
    
    auto enemy1 = new AIEntity("Enemy1");
    enemy1.Position = [50, 30];
    enemy1.Health = 80;
    
    auto enemy2 = new AIEntity("Enemy2");
    enemy2.Position = [30, 60];
    enemy2.Health = 120;
    
    auto ally = new AIEntity("Ally");
    ally.Position = [20, 20];
    ally.Health = 60;
    
    // Create AI systems
    auto perception = new PerceptionSystem();
    perception.addEntity(player);
    perception.addEntity(enemy1);
    perception.addEntity(enemy2);
    perception.addEntity(ally);
    
    auto decision = new DecisionSystem(perception);
    auto animation = new AnimationSystem();
    auto profiler = new AIProfiler();
    
    writeln("Created AI entities:");
    writeln("  ", player.Id, " at (", player.Position[0], ",", player.Position[1], ")");
    writeln("  ", enemy1.Id, " at (", enemy1.Position[0], ",", enemy1.Position[1], ")");
    writeln("  ", enemy2.Id, " at (", enemy2.Position[0], ",", enemy2.Position[1], ")");
    writeln("  ", ally.Id, " at (", ally.Position[0], ",", ally.Position[1], ")");
    
    // Simulate AI behavior
    writeln("\n=== AI Behavior Simulation ===");
    
    foreach (frame; 0..20) {
        writeln("\nFrame ", frame, ":");
        
        // Update perceptions
        perception.updatePerceptions();
        
        // Make decisions
        decision.makeDecisions();
        
        // Update all entities
        auto entities = [player, enemy1, enemy2, ally];
        foreach (entity; entities) {
            profiler.beginUpdate(entity);
            
            // Update AI state
            entity.update(0.016f); // 60 FPS
            
            // Update animations based on state
            auto state = entity.StateMachineRef().CurrentState;
            if (state) {
                animation.setAnimation(entity, state.name);
            }
            
            profiler.endUpdate(entity);
            
            // Print status
            writeln("  ", entity.Id, " - State: ", entity.StateMachineRef().CurrentStateName,
                   " - Position: (", entity.Position[0].to!int, ",", entity.Position[1].to!int, ")",
                   " - Health: ", entity.Health.to!int);
        }
        
        // Simulate some combat
        if (frame == 10) {
            writeln("\n  Combat simulation: Enemy1 attacks Player");
            player.Health -= 25;
            writeln("  Player health: ", player.Health.to!int);
        }
        
        // Simulate health recovery
        if (frame == 15) {
            writeln("\n  Health recovery simulation");
            foreach (entity; entities) {
                if (entity.Health < entity.MaxHealth) {
                    entity.Health += 5;
                    writeln("  ", entity.Id, " recovered to ", entity.Health.to!int, " health");
                }
            }
        }
    }
    
    // Generate performance report
    profiler.generateReport();
    
    // Test state transitions
    writeln("\n=== State Transition Testing ===");
    auto testEntity = new AIEntity("TestEntity");
    testEntity.Position = [0, 0];
    
    // Test patrol state
    auto patrolPoints = [[10, 0], [10, 10], [0, 10], [0, 0]];
    testEntity.StateMachineRef().setState(new PatrolState(patrolPoints));
    writeln("Set patrol state: ", testEntity.StateMachineRef().CurrentStateName);
    
    // Test chase state
    auto chaseTarget = new AIEntity("Target");
    chaseTarget.Position = [20, 20];
    testEntity.StateMachineRef().setState(new ChaseState());
    auto chaseState = cast(ChaseState)testEntity.StateMachineRef().CurrentState;
    chaseState.setTarget(chaseTarget);
    writeln("Set chase state: ", testEntity.StateMachineRef().CurrentStateName);
    
    // Test attack state
    testEntity.StateMachineRef().setState(new AttackState());
    auto attackState = cast(AttackState)testEntity.StateMachineRef().CurrentState;
    attackState.setTarget(chaseTarget);
    writeln("Set attack state: ", testEntity.StateMachineRef().CurrentStateName);
    
    // Test flee state
    testEntity.StateMachineRef().setState(new FleeState());
    auto fleeState = cast(FleeState)testEntity.StateMachineRef().CurrentState;
    fleeState.setThreat(chaseTarget);
    writeln("Set flee state: ", testEntity.StateMachineRef().CurrentStateName);
    
    writeln("\n=== D Language AI System Features ===");
    writeln("✓ Finite State Machine architecture");
    writeln("✓ Modular AI states (Patrol, Chase, Attack, Flee, Idle)");
    writeln("✓ Perception system with sight and hearing");
    writeln("✓ Decision making with threat assessment");
    writeln("✓ Animation integration system");
    writeln("✓ Performance profiling and monitoring");
    writeln("✓ Template-based AI entity system");
    writeln("✓ Extensible behavior framework");
    
    writeln("\nGame AI system demo completed!");
}

