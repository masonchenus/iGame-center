package main

import (
	"fmt"
	"math"
	"sync"
	"time"
)

// PhysicsEngine handles all physics calculations and simulations
type PhysicsEngine struct {
	objects    map[string]*PhysicsObject
	constraints []PhysicsConstraint
	gravity    Vector3
	timeStep   float64
	maxSubSteps int
	mutex      sync.RWMutex
	isRunning  bool
	worldBounds WorldBounds
}

// PhysicsObject represents an object with physics properties
type PhysicsObject struct {
	ID          string
	Position    Vector3
	Velocity    Vector3
	Acceleration Vector3
	Mass        float64
	Radius      float64
	IsStatic    bool
	IsActive    bool
	CollisionGroup string
	Material    *PhysicsMaterial
	Force       Vector3
	Impulse     Vector3
	CreatedAt   time.Time
}

// Vector3 represents a 3D vector
type Vector3 struct {
	X, Y, Z float64
}

// PhysicsMaterial defines material properties
type PhysicsMaterial struct {
	Density       float64
	Restitution   float64 // Bounciness
	Friction      float64
	Damping       float64
	Resting       bool
}

// PhysicsConstraint represents a constraint between objects
type PhysicsConstraint struct {
	ID        string
	ObjectA   string
	ObjectB   string
	Type      ConstraintType
	Distance  float64
	Strength  float64
	Active    bool
}

// WorldBounds defines the physics world boundaries
type WorldBounds struct {
	Min Vector3
	Max Vector3
}

// PhysicsSettings contains physics engine configuration
type PhysicsSettings struct {
	Gravity       Vector3
	TimeStep      float64
	MaxSubSteps   int
	EnableSleep   bool
	MaxVelocity   float64
	MaxForce      float64
	WorldBounds   WorldBounds
}

// CollisionEvent represents a collision event
type CollisionEvent struct {
	ObjectA      string
	ObjectB      string
	ContactPoint Vector3
	Normal       Vector3
	Penetration  float64
	Impulse      float64
	Timestamp    time.Time
}

// NewPhysicsEngine creates a new physics engine
func NewPhysicsEngine(settings PhysicsSettings) *PhysicsEngine {
	return &PhysicsEngine{
		objects:    make(map[string]*PhysicsObject),
		constraints: make([]PhysicsConstraint, 0),
		gravity:    settings.Gravity,
		timeStep:   settings.TimeStep,
		maxSubSteps: settings.MaxSubSteps,
		worldBounds: settings.WorldBounds,
		isRunning:  false,
	}
}

// Start starts the physics simulation
func (pe *PhysicsEngine) Start() {
	pe.isRunning = true
	
	// Start simulation loop in a goroutine
	go pe.simulationLoop()
}

// Stop stops the physics simulation
func (pe *PhysicsEngine) Stop() {
	pe.isRunning = false
}

// simulationLoop runs the main physics simulation
func (pe *PhysicsEngine) simulationLoop() {
	ticker := time.NewTicker(time.Millisecond * 16) // ~60 FPS
	defer ticker.Stop()

	for pe.isRunning {
		select {
		case <-ticker.C:
			pe.Update()
		}
	}
}

// Update updates the physics simulation
func (pe *PhysicsEngine) Update() {
	pe.mutex.Lock()
	defer pe.mutex.Unlock()

	// Apply gravity and forces
	pe.applyForces()
	
	// Integrate motion
	pe.integrateMotion()
	
	// Check collisions
	pe.checkCollisions()
	
	// Apply constraints
	pe.applyConstraints()
	
	// Check world bounds
	pe.checkWorldBounds()
}

// AddObject adds a physics object to the simulation
func (pe *PhysicsEngine) AddObject(obj *PhysicsObject) error {
	pe.mutex.Lock()
	defer pe.mutex.Unlock()

	if _, exists := pe.objects[obj.ID]; exists {
		return fmt.Errorf("object already exists: %s", obj.ID)
	}

	// Set default material if not provided
	if obj.Material == nil {
		obj.Material = &PhysicsMaterial{
			Density:      1.0,
			Restitution:  0.3,
			Friction:     0.4,
			Damping:      0.1,
			Resting:      false,
		}
	}

	pe.objects[obj.ID] = obj
	return nil
}

// RemoveObject removes a physics object from the simulation
func (pe *PhysicsEngine) RemoveObject(objectID string) error {
	pe.mutex.Lock()
	defer pe.mutex.Unlock()

	if _, exists := pe.objects[objectID]; !exists {
		return fmt.Errorf("object not found: %s", objectID)
	}

	delete(pe.objects, objectID)
	
	// Remove associated constraints
	pe.constraints = pe.removeConstraintsForObject(pe.constraints, objectID)
	
	return nil
}

// GetObject retrieves a physics object
func (pe *PhysicsEngine) GetObject(objectID string) (*PhysicsObject, error) {
	pe.mutex.RLock()
	defer pe.mutex.RUnlock()

	obj, exists := pe.objects[objectID]
	if !exists {
		return nil, fmt.Errorf("object not found: %s", objectID)
	}

	return obj, nil
}

// ApplyForce applies a force to an object
func (pe *PhysicsEngine) ApplyForce(objectID string, force Vector3) error {
	pe.mutex.Lock()
	defer pe.mutex.Unlock()

	obj, exists := pe.objects[objectID]
	if !exists {
		return fmt.Errorf("object not found: %s", objectID)
	}

	if obj.IsStatic {
		return fmt.Errorf("cannot apply force to static object: %s", objectID)
	}

	obj.Force = addVectors(obj.Force, force)
	return nil
}

// ApplyImpulse applies an impulse to an object
func (pe *PhysicsEngine) ApplyImpulse(objectID string, impulse Vector3) error {
	pe.mutex.Lock()
	defer pe.mutex.Unlock()

	obj, exists := pe.objects[objectID]
	if !exists {
		return fmt.Errorf("object not found: %s", objectID)
	}

	if obj.IsStatic {
		return fmt.Errorf("cannot apply impulse to static object: %s", objectID)
	}

	obj.Impulse = addVectors(obj.Impulse, impulse)
	return nil
}

// SetVelocity sets the velocity of an object
func (pe *PhysicsEngine) SetVelocity(objectID string, velocity Vector3) error {
	pe.mutex.Lock()
	defer pe.mutex.Unlock()

	obj, exists := pe.objects[objectID]
	if !exists {
		return fmt.Errorf("object not found: %s", objectID)
	}

	obj.Velocity = velocity
	return nil
}

// GetVelocity gets the velocity of an object
func (pe *PhysicsEngine) GetVelocity(objectID string) (Vector3, error) {
	pe.mutex.RLock()
	defer pe.mutex.RUnlock()

	obj, exists := pe.objects[objectID]
	if !exists {
		return Vector3{}, fmt.Errorf("object not found: %s", objectID)
	}

	return obj.Velocity, nil
}

// AddConstraint adds a constraint between two objects
func (pe *PhysicsEngine) AddConstraint(constraint PhysicsConstraint) error {
	pe.mutex.Lock()
	defer pe.mutex.Unlock()

	// Verify objects exist
	if _, exists := pe.objects[constraint.ObjectA]; !exists {
		return fmt.Errorf("object A not found: %s", constraint.ObjectA)
	}
	if _, exists := pe.objects[constraint.ObjectB]; !exists {
		return fmt.Errorf("object B not found: %s", constraint.ObjectB)
	}

	constraint.ID = fmt.Sprintf("constraint_%d", time.Now().Unix())
	constraint.Active = true
	
	pe.constraints = append(pe.constraints, constraint)
	return nil
}

// Raycast performs a raycast to find objects along a ray
func (pe *PhysicsEngine) Raycast(origin, direction Vector3, maxDistance float64) (*RaycastResult, error) {
	pe.mutex.RLock()
	defer pe.mutex.RUnlock()

	var closestHit *RaycastResult
	closestDistance := maxDistance

	for _, obj := range pe.objects {
		if !obj.IsActive || obj.IsStatic {
			continue
		}

		// Ray-sphere intersection
		distance := pe.raySphereDistance(origin, direction, obj.Position, obj.Radius)
		
		if distance >= 0 && distance < closestDistance {
			hitPoint := addVectors(origin, scaleVector(direction, distance))
			normal := normalizeVector(subtractVectors(hitPoint, obj.Position))
			
			if closestHit == nil || distance < closestDistance {
				closestHit = &RaycastResult{
					ObjectID:   obj.ID,
					Distance:   distance,
					HitPoint:   hitPoint,
					Normal:     normal,
					Hit:        true,
				}
				closestDistance = distance
			}
		}
	}

	if closestHit == nil {
		return &RaycastResult{
			Hit: false,
		}, nil
	}

	return closestHit, nil
}

// RaycastResult represents the result of a raycast
type RaycastResult struct {
	ObjectID   string
	Distance   float64
	HitPoint   Vector3
	Normal     Vector3
	Hit        bool
}

// GetAllObjects returns all physics objects
func (pe *PhysicsEngine) GetAllObjects() []*PhysicsObject {
	pe.mutex.RLock()
	defer pe.mutex.RUnlock()

	objects := make([]*PhysicsObject, 0, len(pe.objects))
	for _, obj := range pe.objects {
		objects = append(objects, obj)
	}

	return objects
}

// GetObjectsInRadius returns objects within a radius of a point
func (pe *PhysicsEngine) GetObjectsInRadius(center Vector3, radius float64) []*PhysicsObject {
	pe.mutex.RLock()
	defer pe.mutex.RUnlock()

	objects := make([]*PhysicsObject, 0)
	radiusSquared := radius * radius

	for _, obj := range pe.objects {
		if !obj.IsActive {
			continue
		}

		distanceSquared := distanceSquared(center, obj.Position)
		if distanceSquared <= radiusSquared {
			objects = append(objects, obj)
		}
	}

	return objects
}

// Internal physics methods

// applyForces applies all forces to objects
func (pe *PhysicsEngine) applyForces() {
	for _, obj := range pe.objects {
		if !obj.IsActive || obj.IsStatic {
			continue
		}

		// Reset acceleration
		obj.Acceleration = Vector3{0, 0, 0}

		// Apply gravity
		if !obj.Material.Resting {
			gravityForce := scaleVector(pe.gravity, obj.Mass)
			obj.Acceleration = addVectors(obj.Acceleration, scaleVector(gravityForce, 1/obj.Mass))
		}

		// Apply custom forces
		if obj.Force.X != 0 || obj.Force.Y != 0 || obj.Force.Z != 0 {
			forceAcceleration := scaleVector(obj.Force, 1/obj.Mass)
			obj.Acceleration = addVectors(obj.Acceleration, forceAcceleration)
		}

		// Apply damping
		dampingForce := scaleVector(obj.Velocity, -obj.Material.Damping)
		obj.Acceleration = addVectors(obj.Acceleration, scaleVector(dampingForce, 1/obj.Mass))

		// Clear forces
		obj.Force = Vector3{0, 0, 0}
	}
}

// integrateMotion integrates object motion
func (pe *PhysicsEngine) integrateMotion() {
	timeStep := pe.timeStep

	for _, obj := range pe.objects {
		if !obj.IsActive || obj.IsStatic {
			continue
		}

		// Semi-implicit Euler integration
		obj.Velocity = addVectors(obj.Velocity, scaleVector(obj.Acceleration, timeStep))
		
		// Apply impulse
		if obj.Impulse.X != 0 || obj.Impulse.Y != 0 || obj.Impulse.Z != 0 {
			impulseVelocity := scaleVector(obj.Impulse, 1/obj.Mass)
			obj.Velocity = addVectors(obj.Velocity, impulseVelocity)
			obj.Impulse = Vector3{0, 0, 0}
		}

		// Update position
		obj.Position = addVectors(obj.Position, scaleVector(obj.Velocity, timeStep))
	}
}

// checkCollisions checks for collisions between objects
func (pe *PhysicsEngine) checkCollisions() {
	objects := pe.GetAllObjects()
	
	for i := 0; i < len(objects); i++ {
		for j := i + 1; j < len(objects); j++ {
			objA := objects[i]
			objB := objects[j]

			if !objA.IsActive || !objB.IsActive {
				continue
			}

			if objA.IsStatic && objB.IsStatic {
				continue
			}

			// Check collision
			if pe.checkCollision(objA, objB) {
				pe.resolveCollision(objA, objB)
			}
		}
	}
}

// checkCollision checks if two objects are colliding
func (pe *PhysicsEngine) checkCollision(objA, objB *PhysicsObject) bool {
	distance := distance(objA.Position, objB.Position)
	minDistance := objA.Radius + objB.Radius
	
	return distance < minDistance
}

// resolveCollision resolves collision between two objects
func (pe *PhysicsEngine) resolveCollision(objA, objB *PhysicsObject) {
	// Calculate collision normal
	normal := normalizeVector(subtractVectors(objB.Position, objA.Position))
	
	// Calculate penetration depth
	distance := distance(objA.Position, objB.Position)
	minDistance := objA.Radius + objB.Radius
	penetration := minDistance - distance

	// Separate objects
	totalMass := objA.Mass + objB.Mass
	if totalMass > 0 {
		separationA := penetration * (objB.Mass / totalMass)
		separationB := penetration * (objA.Mass / totalMass)
		
		if !objA.IsStatic {
			objA.Position = subtractVectors(objA.Position, scaleVector(normal, separationA))
		}
		if !objB.IsStatic {
			objB.Position = addVectors(objB.Position, scaleVector(normal, separationB))
		}
	}

	// Calculate relative velocity
	relativeVelocity := subtractVectors(objB.Velocity, objA.Velocity)
	velocityAlongNormal := dotProduct(relativeVelocity, normal)

	// Do not resolve if velocities are separating
	if velocityAlongNormal > 0 {
		return
	}

	// Calculate restitution
	restitution := math.Min(objA.Material.Restitution, objB.Material.Restitution)

	// Calculate impulse scalar
	j := -(1 + restitution) * velocityAlongNormal
	j /= (1/objA.Mass + 1/objB.Mass)

	// Apply impulse
	impulse := scaleVector(normal, j)
	
	if !objA.IsStatic {
		objA.Velocity = subtractVectors(objA.Velocity, scaleVector(impulse, 1/objA.Mass))
	}
	if !objB.IsStatic {
		objB.Velocity = addVectors(objB.Velocity, scaleVector(impulse, 1/objB.Mass))
	}

	// Apply friction
	tangent := subtractVectors(relativeVelocity, scaleVector(normal, velocityAlongNormal))
	tangentMagnitude := magnitude(tangent)
	
	if tangentMagnitude > 0 {
		tangent = scaleVector(tangent, 1/tangentMagnitude)
		
		// Calculate friction coefficient
		mu := math.Sqrt(objA.Material.Friction * objB.Material.Friction)
		
		// Apply friction impulse
		frictionImpulse := scaleVector(tangent, -j*mu)
		
		if !objA.IsStatic {
			objA.Velocity = subtractVectors(objA.Velocity, scaleVector(frictionImpulse, 1/objA.Mass))
		}
		if !objB.IsStatic {
			objB.Velocity = addVectors(objB.Velocity, scaleVector(frictionImpulse, 1/objB.Mass))
		}
	}
}

// applyConstraints applies physics constraints
func (pe *PhysicsEngine) applyConstraints() {
	for _, constraint := range pe.constraints {
		if !constraint.Active {
			continue
		}

		pe.applyConstraint(constraint)
	}
}

// applyConstraint applies a single constraint
func (pe *PhysicsEngine) applyConstraint(constraint PhysicsConstraint) {
	objA, existsA := pe.objects[constraint.ObjectA]
	objB, existsB := pe.objects[constraint.ObjectB]
	
	if !existsA || !existsB {
		return
	}

	switch constraint.Type {
	case DistanceConstraint:
		pe.applyDistanceConstraint(objA, objB, constraint)
	case FixedConstraint:
		pe.applyFixedConstraint(objA, objB, constraint)
	case HingeConstraint:
		pe.applyHingeConstraint(objA, objB, constraint)
	}
}

// applyDistanceConstraint maintains a fixed distance between objects
func (pe *PhysicsEngine) applyDistanceConstraint(objA, objB *PhysicsObject, constraint PhysicsConstraint) {
	distance := distance(objA.Position, objB.Position)
	difference := distance - constraint.Distance
	
	if abs(difference) < 0.01 { // Small threshold to avoid jitter
		return
	}

	normal := scaleVector(subtractVectors(objB.Position, objA.Position), 1/distance)
	
	// Apply correction
	correction := scaleVector(normal, difference*0.5*constraint.Strength)
	
	if !objA.IsStatic {
		objA.Position = addVectors(objA.Position, correction)
	}
	if !objB.IsStatic {
		objB.Position = subtractVectors(objB.Position, correction)
	}
}

// applyFixedConstraint fixes objects at the same position
func (pe *PhysicsEngine) applyFixedConstraint(objA, objB *PhysicsObject, constraint PhysicsConstraint) {
	center := scaleVector(addVectors(objA.Position, objB.Position), 0.5)
	
	if !objA.IsStatic {
		objA.Position = center
		objA.Velocity = Vector3{0, 0, 0}
	}
	if !objB.IsStatic {
		objB.Position = center
		objB.Velocity = Vector3{0, 0, 0}
	}
}

// applyHingeConstraint allows rotation around a hinge point
func (pe *PhysicsEngine) applyHingeConstraint(objA, objB *PhysicsObject, constraint PhysicsConstraint) {
	// Simplified hinge constraint implementation
	pe.applyDistanceConstraint(objA, objB, constraint)
}

// checkWorldBounds checks if objects are within world bounds
func (pe *PhysicsEngine) checkWorldBounds() {
	for _, obj := range pe.objects {
		if !obj.IsActive || obj.IsStatic {
			continue
		}

		// Check X bounds
		if obj.Position.X < pe.worldBounds.Min.X {
			obj.Position.X = pe.worldBounds.Min.X
			if obj.Velocity.X < 0 {
				obj.Velocity.X = -obj.Velocity.X * obj.Material.Restitution
			}
		} else if obj.Position.X > pe.worldBounds.Max.X {
			obj.Position.X = pe.worldBounds.Max.X
			if obj.Velocity.X > 0 {
				obj.Velocity.X = -obj.Velocity.X * obj.Material.Restitution
			}
		}

		// Check Y bounds
		if obj.Position.Y < pe.worldBounds.Min.Y {
			obj.Position.Y = pe.worldBounds.Min.Y
			if obj.Velocity.Y < 0 {
				obj.Velocity.Y = -obj.Velocity.Y * obj.Material.Restitution
			}
		} else if obj.Position.Y > pe.worldBounds.Max.Y {
			obj.Position.Y = pe.worldBounds.Max.Y
			if obj.Velocity.Y > 0 {
				obj.Velocity.Y = -obj.Velocity.Y * obj.Material.Restitution
			}
		}

		// Check Z bounds
		if obj.Position.Z < pe.worldBounds.Min.Z {
			obj.Position.Z = pe.worldBounds.Min.Z
			if obj.Velocity.Z < 0 {
				obj.Velocity.Z = -obj.Velocity.Z * obj.Material.Restitution
			}
		} else if obj.Position.Z > pe.worldBounds.Max.Z {
			obj.Position.Z = pe.worldBounds.Max.Z
			if obj.Velocity.Z > 0 {
				obj.Velocity.Z = -obj.Velocity.Z * obj.Material.Restitution
			}
		}
	}
}

// raySphereDistance calculates ray-sphere intersection distance
func (pe *PhysicsEngine) raySphereDistance(origin, direction Vector3, center Vector3, radius float64) float64 {
	oc := subtractVectors(origin, center)
	a := dotProduct(direction, direction)
	b := 2.0 * dotProduct(oc, direction)
	c := dotProduct(oc, oc) - radius*radius
	
	discriminant := b*b - 4*a*c
	if discriminant < 0 {
		return -1 // No intersection
	}
	
	t := (-b - math.Sqrt(discriminant)) / (2 * a)
	if t < 0 {
		t = (-b + math.Sqrt(discriminant)) / (2 * a)
	}
	
	return t
}

// removeConstraintsForObject removes constraints involving a specific object
func (pe *PhysicsEngine) removeConstraintsForObject(constraints []PhysicsConstraint, objectID string) []PhysicsConstraint {
	filtered := make([]PhysicsConstraint, 0)
	
	for _, constraint := range constraints {
		if constraint.ObjectA != objectID && constraint.ObjectB != objectID {
			filtered = append(filtered, constraint)
		}
	}
	
	return filtered
}

// Vector utility functions

func addVectors(a, b Vector3) Vector3 {
	return Vector3{a.X + b.X, a.Y + b.Y, a.Z + b.Z}
}

func subtractVectors(a, b Vector3) Vector3 {
	return Vector3{a.X - b.X, a.Y - b.Y, a.Z - b.Z}
}

func scaleVector(v Vector3, scalar float64) Vector3 {
	return Vector3{v.X * scalar, v.Y * scalar, v.Z * scalar}
}

func dotProduct(a, b Vector3) float64 {
	return a.X*b.X + a.Y*b.Y + a.Z*b.Z
}

func magnitude(v Vector3) float64 {
	return math.Sqrt(v.X*v.X + v.Y*v.Y + v.Z*v.Z)
}

func normalizeVector(v Vector3) Vector3 {
	mag := magnitude(v)
	if mag == 0 {
		return Vector3{0, 0, 0}
	}
	return scaleVector(v, 1/mag)
}

func distance(a, b Vector3) float64 {
	return magnitude(subtractVectors(a, b))
}

func distanceSquared(a, b Vector3) float64 {
	diff := subtractVectors(a, b)
	return diff.X*diff.X + diff.Y*diff.Y + diff.Z*diff.Z
}

func abs(x float64) float64 {
	if x < 0 {
		return -x
	}
	return x
}

// Constraint types
type ConstraintType string

const (
	DistanceConstraint ConstraintType = "distance"
	FixedConstraint   ConstraintType = "fixed"
	HingeConstraint   ConstraintType = "hinge"
)

// Example usage
func main() {
	settings := PhysicsSettings{
		Gravity:       Vector3{0, -9.81, 0},
		TimeStep:      0.016, // 60 FPS
		MaxSubSteps:   4,
		EnableSleep:   true,
		MaxVelocity:   100.0,
		MaxForce:      1000.0,
		WorldBounds:   WorldBounds{
			Min: Vector3{-50, -50, -50},
			Max: Vector3{50, 50, 50},
		},
	}

	engine := NewPhysicsEngine(settings)
	
	// Create some physics objects
	ball1 := &PhysicsObject{
		ID:       "ball1",
		Position: Vector3{0, 10, 0},
		Velocity: Vector3{5, 0, 0},
		Mass:     1.0,
		Radius:   1.0,
		IsStatic: false,
		IsActive: true,
	}

	ball2 := &PhysicsObject{
		ID:       "ball2",
		Position: Vector3{5, 15, 0},
		Velocity: Vector3{-3, 0, 0},
		Mass:     1.5,
		Radius:   1.2,
		IsStatic: false,
		IsActive: true,
	}

	// Add objects to physics engine
	engine.AddObject(ball1)
	engine.AddObject(ball2)

	// Start physics simulation
	engine.Start()

	// Run simulation for a few seconds
	time.Sleep(time.Second * 5)

	// Get final positions
	pos1, _ := engine.GetObject("ball1")
	pos2, _ := engine.GetObject("ball2")

	fmt.Printf("Final positions:\n")
	fmt.Printf("Ball 1: (%.2f, %.2f, %.2f)\n", pos1.Position.X, pos1.Position.Y, pos1.Position.Z)
	fmt.Printf("Ball 2: (%.2f, %.2f, %.2f)\n", pos2.Position.X, pos2.Position.Y, pos2.Position.Z)

	// Test raycast
	rayOrigin := Vector3{-10, 20, 0}
	rayDirection := Vector3{1, -1, 0}
	result, err := engine.Raycast(rayOrigin, rayDirection, 30)
	if err != nil {
		fmt.Printf("Raycast error: %v\n", err)
	} else if result.Hit {
		fmt.Printf("Raycast hit: %s at distance %.2f\n", result.ObjectID, result.Distance)
	} else {
		fmt.Println("Raycast: No hit")
	}

	engine.Stop()
	fmt.Println("Physics Engine initialized successfully!")
}
