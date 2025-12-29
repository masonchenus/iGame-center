package main

import (
	"fmt"
	"math"
	"sync"
	"time"
)

// CollisionSystem handles collision detection and response for game objects
type CollisionSystem struct {
	objects    map[string]*CollidableObject
	broadphase *SpatialHash
	narrowphase *NarrowPhaseDetector
	handlers   map[string]CollisionHandler
	settings   CollisionSettings
	mutex      sync.RWMutex
	isRunning  bool
}

// CollidableObject represents an object that can participate in collisions
type CollidableObject struct {
	ID          string
	Position    Vector3
	Velocity    Vector3
	Rotation    Vector3
	Scale       Vector3
	Shape       CollisionShape
	Material    *CollisionMaterial
	Active      bool
	Static      bool
	Trigger     bool
	Layer       int
	Mask        int
	Mass        float32
	Restitution float32
	Friction    float32
	Tag         string
	Owner       string
}

// CollisionShape defines the collision shape of an object
type CollisionShape struct {
	Type      ShapeType
	Radius    float32
	Extents   Vector3
	Vertices  []Vector3
	Height    float32
	Center    Vector3
	Transform *CollisionTransform
}

// CollisionTransform represents transformation for collision shapes
type CollisionTransform struct {
	Position Vector3
	Rotation Vector3
	Scale    Vector3
	Matrix   [16]float32
}

// CollisionMaterial defines physical properties for collisions
type CollisionMaterial struct {
	Density        float32
	Friction       float32
	Restitution    float32
	FrictionCombine CombineMethod
	RestitutionCombine CombineMethod
}

// CollisionEvent represents a collision event
type CollisionEvent struct {
	ObjectA      string
	ObjectB      string
	ContactPoint Vector3
	Normal       Vector3
	Penetration  float32
	RelativeVel  Vector3
	Impulse      float32
	Timestamp    time.Time
	EventType    CollisionEventType
}

// CollisionManifold contains collision information between two objects
type CollisionManifold struct {
	ObjectA      string
	ObjectB      string
	Normal       Vector3
	Penetration  float32
	Contacts     []Vector3
	Restitution  float32
	Friction     float32
	Valid        bool
}

// SpatialHash provides broad-phase collision detection
type SpatialHash struct {
	CellSize    float32
	Cells       map[string]*SpatialCell
	ObjectMap   map[string][]string
	mutex       sync.RWMutex
}

// SpatialCell represents a cell in the spatial hash grid
type SpatialCell struct {
	Min Vector3
	Max Vector3
	Objects []string
}

// NarrowPhaseDetector performs detailed collision detection
type NarrowPhaseDetector struct {
	handlers map[ShapePair]CollisionHandler
	mutex    sync.RWMutex
}

// CollisionHandler handles collision between specific shape pairs
type CollisionHandler interface {
	DetectCollision(objA, objB *CollidableObject) *CollisionManifold
	ResolveCollision(manifold *CollisionManifold)
}

// CircleCircleHandler handles circle-circle collisions
type CircleCircleHandler struct{}

type ShapePair struct {
	TypeA, TypeB ShapeType
}

type ShapeType int

const (
	CircleShape   ShapeType = iota
	BoxShape
	PolygonShape
	CapsuleShape
	PlaneShape
	ConvexHullShape
)

type CombineMethod int

const (
	AverageCombine   CombineMethod = iota
	MinCombine
	MaxCombine
	MultiplyCombine
)

type CollisionEventType int

const (
	CollisionStart CollisionEventType = iota
	CollisionEnd
	CollisionStay
	TriggerEnter
	TriggerExit
)

// CollisionSettings contains collision system configuration
type CollisionSettings struct {
	MaxObjects           int
	CellSize            float32
	MaxPenetration      float32
	Slop                float32
	Baumgarte           float32
	MaxIterations       int
	EnableTriggerEvents bool
	EnableDebugDraw     bool
	LayerCollisionMask  map[int][]int
}

// Vector3 represents 3D vector
type Vector3 struct {
	X, Y, Z float32
}

// NewCollisionSystem creates a new collision system
func NewCollisionSystem(settings CollisionSettings) *CollisionSystem {
	return &CollisionSystem{
		objects:   make(map[string]*CollidableObject),
		broadphase: NewSpatialHash(settings.CellSize),
		narrowphase: NewNarrowPhaseDetector(),
		handlers:   make(map[string]CollisionHandler),
		settings:   settings,
		isRunning:  false,
	}
}

// Start starts the collision system
func (cs *CollisionSystem) Start() error {
	cs.mutex.Lock()
	defer cs.mutex.Unlock()

	cs.isRunning = true

	// Register default handlers
	cs.registerDefaultHandlers()

	// Start collision processing loop
	go cs.collisionLoop()

	fmt.Println("Collision System started")
	return nil
}

// Stop stops the collision system
func (cs *CollisionSystem) Stop() {
	cs.mutex.Lock()
	defer cs.mutex.Unlock()

	cs.isRunning = false
	cs.objects = make(map[string]*CollidableObject)
	cs.broadphase.Clear()

	fmt.Println("Collision System stopped")
}

// AddObject adds a collidable object to the system
func (cs *CollisionSystem) AddObject(obj *CollidableObject) error {
	cs.mutex.Lock()
	defer cs.mutex.Unlock()

	if !cs.isRunning {
		return fmt.Errorf("collision system is not running")
	}

	if _, exists := cs.objects[obj.ID]; exists {
		return fmt.Errorf("object already exists: %s", obj.ID)
	}

	// Validate object
	if obj.Shape.Type == 0 {
		return fmt.Errorf("object must have a collision shape: %s", obj.ID)
	}

	if obj.Mass <= 0 && !obj.Static {
		return fmt.Errorf("non-static objects must have positive mass: %s", obj.ID)
	}

	cs.objects[obj.ID] = obj
	
	// Add to spatial hash
	cs.broadphase.AddObject(obj)

	fmt.Printf("Added collidable object: %s (%v)\n", obj.ID, obj.Shape.Type)
	return nil
}

// RemoveObject removes a collidable object from the system
func (cs *CollisionSystem) RemoveObject(objectID string) error {
	cs.mutex.Lock()
	defer cs.mutex.Unlock()

	obj, exists := cs.objects[objectID]
	if !exists {
		return fmt.Errorf("object not found: %s", objectID)
	}

	delete(cs.objects, objectID)
	cs.broadphase.RemoveObject(objectID)

	fmt.Printf("Removed collidable object: %s\n", objectID)
	return nil
}

// UpdateObject updates an object's properties
func (cs *CollisionSystem) UpdateObject(objectID string, updateFunc func(*CollidableObject)) error {
	cs.mutex.Lock()
	defer cs.mutex.Unlock()

	obj, exists := cs.objects[objectID]
	if !exists {
		return fmt.Errorf("object not found: %s", objectID)
	}

	// Store old position for spatial hash update
	oldPos := obj.Position
	
	// Apply update
	updateFunc(obj)
	
	// Update spatial hash if position changed
	if obj.Position != oldPos {
		cs.broadphase.UpdateObject(objectID, obj.Position)
	}

	return nil
}

// DetectCollisions performs collision detection
func (cs *CollisionSystem) DetectCollisions() []CollisionEvent {
	cs.mutex.RLock()
	defer cs.mutex.RUnlock()

	events := make([]CollisionEvent, 0)
	
	// Broad phase: get potential collision pairs
	potentialPairs := cs.broadphase.GetPotentialPairs()
	
	// Narrow phase: check actual collisions
	for _, pair := range potentialPairs {
		objA, existsA := cs.objects[pair[0]]
		objB, existsB := cs.objects[pair[1]]
		
		if !existsA || !existsB {
			continue
		}

		// Skip if objects shouldn't collide (layers, masks, etc.)
		if !cs.shouldObjectsCollide(objA, objB) {
			continue
		}

		// Get collision handler
		handler := cs.narrowphase.GetHandler(objA.Shape.Type, objB.Shape.Type)
		if handler == nil {
			continue
		}

		// Detect collision
		manifold := handler.DetectCollision(objA, objB)
		if manifold != nil && manifold.Valid {
			// Create collision event
			event := CollisionEvent{
				ObjectA:      objA.ID,
				ObjectB:      objB.ID,
				ContactPoint: cs.getContactPoint(manifold),
				Normal:       manifold.Normal,
				Penetration:  manifold.Penetration,
				RelativeVel:  cs.calculateRelativeVelocity(objA, objB),
				Impulse:      cs.calculateImpulse(manifold, objA, objB),
				Timestamp:    time.Now(),
				EventType:    CollisionStart,
			}
			events = append(events, event)
		}
	}

	return events
}

// ResolveCollisions resolves detected collisions
func (cs *CollisionSystem) ResolveCollisions(events []CollisionEvent) {
	for _, event := range events {
		cs.resolveCollision(event)
	}
}

// Update performs a collision system update
func (cs *CollisionSystem) Update(deltaTime time.Duration) error {
	// Detect collisions
	events := cs.DetectCollisions()
	
	// Resolve collisions
	cs.ResolveCollisions(events)
	
	return nil
}

// Raycast performs a raycast to find objects along a ray
func (cs *CollisionSystem) Raycast(origin, direction Vector3, maxDistance float32) ([]RaycastHit, error) {
	cs.mutex.RLock()
	defer cs.mutex.RUnlock()

	hits := make([]RaycastHit, 0)
	
	// Normalize direction
	dir := normalizeVector3(direction)
	
	for _, obj := range cs.objects {
		if !obj.Active {
			continue
		}

		hit := cs.raycastObject(origin, dir, maxDistance, obj)
		if hit.Hit {
			hits = append(hits, hit)
		}
	}

	// Sort hits by distance
	cs.sortRaycastHits(hits)

	return hits, nil
}

// RaycastHit represents a raycast hit result
type RaycastHit struct {
	Object      string
	Point       Vector3
	Normal      Vector3
	Distance    float32
	UV          Vector2
	TriangleIndex int
}

// GetObject retrieves a collidable object
func (cs *CollisionSystem) GetObject(objectID string) (*CollidableObject, error) {
	cs.mutex.RLock()
	defer cs.mutex.RUnlock()

	obj, exists := cs.objects[objectID]
	if !exists {
		return nil, fmt.Errorf("object not found: %s", objectID)
	}

	return obj, nil
}

// GetObjectsInRadius returns objects within a radius of a point
func (cs *CollisionSystem) GetObjectsInRadius(center Vector3, radius float32) []*CollidableObject {
	cs.mutex.RLock()
	defer cs.mutex.RUnlock()

	return cs.broadphase.GetObjectsInRadius(center, radius)
}

// GetObjectsInAABB returns objects within an axis-aligned bounding box
func (cs *CollisionSystem) GetObjectsInAABB(min, max Vector3) []*CollidableObject {
	cs.mutex.RLock()
	defer cs.mutex.RUnlock()

	return cs.broadphase.GetObjectsInAABB(min, max)
}

// GetCollisionStats returns collision system statistics
func (cs *CollisionSystem) GetCollisionStats() CollisionStats {
	cs.mutex.RLock()
	defer cs.mutex.RUnlock()

	return CollisionStats{
		TotalObjects:    len(cs.objects),
		ActiveObjects:   cs.countActiveObjects(),
		SpatialCells:    cs.broadphase.GetCellCount(),
		PotentialPairs:  cs.broadphase.GetPotentialPairCount(),
		IsRunning:       cs.isRunning,
	}
}

// CollisionStats represents collision system statistics
type CollisionStats struct {
	TotalObjects   int
	ActiveObjects  int
	SpatialCells   int
	PotentialPairs int
	IsRunning      bool
}

// Internal methods

func (cs *CollisionSystem) collisionLoop() {
	ticker := time.NewTicker(time.Millisecond * 16) // ~60 FPS
	defer ticker.Stop()

	for cs.isRunning {
		select {
		case <-ticker.C:
			cs.Update(time.Millisecond * 16)
		}
	}
}

func (cs *CollisionSystem) registerDefaultHandlers() {
	// Register circle-circle handler
	cs.narrowphase.RegisterHandler(CircleShape, CircleShape, &CircleCircleHandler{})
	
	// Add more handlers as needed
	// cs.narrowphase.RegisterHandler(BoxShape, BoxShape, &BoxBoxHandler{})
	// cs.narrowphase.RegisterHandler(CircleShape, BoxShape, &CircleBoxHandler{})
}

func (cs *CollisionSystem) shouldObjectsCollide(objA, objB *CollidableObject) bool {
	// Check if either object is inactive
	if !objA.Active || !objB.Active {
		return false
	}

	// Check if both are static (static-static collisions ignored)
	if objA.Static && objB.Static {
		return false
	}

	// Check layer collision
	if cs.settings.LayerCollisionMask != nil {
		maskA := cs.settings.LayerCollisionMask[objA.Layer]
		maskB := cs.settings.LayerCollisionMask[objB.Layer]
		
		// Check if layer A can collide with layer B
		canCollideA := false
		for _, layer := range maskA {
			if layer == objB.Layer {
				canCollideA = true
				break
			}
		}
		
		// Check if layer B can collide with layer A
		canCollideB := false
		for _, layer := range maskB {
			if layer == objA.Layer {
				canCollideB = true
				break
			}
		}
		
		return canCollideA && canCollideB
	}

	// Check object masks
	return (objA.Mask&objB.Layer) != 0 && (objB.Mask&objA.Layer) != 0
}

func (cs *CollisionSystem) resolveCollision(event CollisionEvent) {
	objA, _ := cs.GetObject(event.ObjectA)
	objB, _ := cs.GetObject(event.ObjectB)
	
	if objA == nil || objB == nil {
		return
	}

	// Skip trigger objects
	if objA.Trigger || objB.Trigger {
		// Handle trigger events
		return
	}

	// Resolve collision based on object types
	if objA.Static && !objB.Static {
		cs.resolveStaticDynamicCollision(objA, objB, event)
	} else if !objA.Static && objB.Static {
		cs.resolveStaticDynamicCollision(objB, objA, event)
	} else if !objA.Static && !objB.Static {
		cs.resolveDynamicDynamicCollision(objA, objB, event)
	}
}

func (cs *CollisionSystem) resolveStaticDynamicCollision(static, dynamic *CollidableObject, event CollisionEvent) {
	// Separate dynamic object from static
	separation := multiplyVector3(event.Normal, event.Penetration)
	
	// Move dynamic object out of collision
	dynamic.Position = addVector3(dynamic.Position, separation)
	
	// Adjust velocity
	velocityAlongNormal := dotProduct3(dynamic.Velocity, event.Normal)
	
	if velocityAlongNormal < 0 {
		// Reflect velocity
		restitution := (static.Restitution + dynamic.Restitution) * 0.5
		impulse := -(1 + restitution) * velocityAlongNormal
		
		dynamic.Velocity = addVector3(dynamic.Velocity, 
			multiplyVector3(event.Normal, impulse))
		
		// Apply friction
		cs.applyFriction(dynamic, event.Normal)
	}
}

func (cs *CollisionSystem) resolveDynamicDynamicCollision(objA, objB *CollidableObject, event CollisionEvent) {
	// Calculate masses
	massA := objA.Mass
	massB := objB.Mass
	totalMass := massA + massB
	
	if totalMass <= 0 {
		return
	}

	// Separate objects
	separationA := multiplyVector3(event.Normal, event.Penetration * (massB / totalMass))
	separationB := multiplyVector3(event.Normal, -event.Penetration * (massA / totalMass))
	
	objA.Position = addVector3(objA.Position, separationA)
	objB.Position = addVector3(objB.Position, separationB)

	// Calculate relative velocity
	relativeVel := subtractVector3(objA.Velocity, objB.Velocity)
	velocityAlongNormal := dotProduct3(relativeVel, event.Normal)

	// Don't resolve if velocities are separating
	if velocityAlongNormal > 0 {
		return
	}

	// Calculate restitution
	restitution := (objA.Restitution + objB.Restitution) * 0.5

	// Calculate impulse scalar
	j := -(1 + restitution) * velocityAlongNormal
	j /= (1/massA + 1/massB)

	// Apply impulse
	impulse := multiplyVector3(event.Normal, j)
	objA.Velocity = addVector3(objA.Velocity, multiplyVector3(impulse, 1/massA))
	objB.Velocity = subtractVector3(objB.Velocity, multiplyVector3(impulse, 1/massB))

	// Apply friction
	cs.applyFriction(objA, event.Normal)
	cs.applyFriction(objB, event.Normal)
}

func (cs *CollisionSystem) applyFriction(obj *CollidableObject, normal Vector3) {
	// Calculate tangent vector
	tangent := subtractVector3(obj.Velocity, 
		multiplyVector3(normal, dotProduct3(obj.Velocity, normal)))
	
	tangentMag := lengthVector3(tangent)
	if tangentMag > 0 {
		tangent = multiplyVector3(tangent, 1/tangentMag)
		
		// Calculate friction coefficient
		mu := obj.Friction
		
		// Apply friction impulse
		frictionImpulse := multiplyVector3(tangent, -mu * lengthVector3(obj.Velocity))
		obj.Velocity = addVector3(obj.Velocity, frictionImpulse)
	}
}

func (cs *CollisionSystem) getContactPoint(manifold *CollisionManifold) Vector3 {
	if len(manifold.Contacts) == 0 {
		return Vector3{0, 0, 0}
	}
	
	// Return average of contact points
	var sum Vector3
	for _, contact := range manifold.Contacts {
		sum = addVector3(sum, contact)
	}
	
	return multiplyVector3(sum, 1/float32(len(manifold.Contacts)))
}

func (cs *CollisionSystem) calculateRelativeVelocity(objA, objB *CollidableObject) Vector3 {
	return subtractVector3(objA.Velocity, objB.Velocity)
}

func (cs *CollisionSystem) calculateImpulse(manifold *CollisionManifold, objA, objB *CollidableObject) float32 {
	relativeVel := cs.calculateRelativeVelocity(objA, objB)
	velocityAlongNormal := dotProduct3(relativeVel, manifold.Normal)
	
	if velocityAlongNormal > 0 {
		return 0
	}

	restitution := (objA.Restitution + objB.Restitution) * 0.5
	j := -(1 + restitution) * velocityAlongNormal
	j /= (1/objA.Mass + 1/objB.Mass)
	
	return math.Abs(float32(j))
}

func (cs *CollisionSystem) raycastObject(origin, direction Vector3, maxDistance float32, obj *CollidableObject) RaycastHit {
	// Simplified raycast implementation
	// In a real implementation, this would use proper ray-shape intersection
	
	distance := lengthVector3(subtractVector3(obj.Position, origin))
	
	if distance <= maxDistance {
		return RaycastHit{
			Object:   obj.ID,
			Point:    obj.Position,
			Normal:   Vector3{0, 1, 0},
			Distance: distance,
		}
	}
	
	return RaycastHit{Hit: false}
}

func (cs *CollisionSystem) sortRaycastHits(hits []RaycastHit) {
	// Sort by distance
	for i := 0; i < len(hits); i++ {
		for j := i + 1; j < len(hits); j++ {
			if hits[i].Distance > hits[j].Distance {
				hits[i], hits[j] = hits[j], hits[i]
			}
		}
	}
}

func (cs *CollisionSystem) countActiveObjects() int {
	count := 0
	for _, obj := range cs.objects {
		if obj.Active {
			count++
		}
	}
	return count
}

// Vector math utilities
func addVector3(a, b Vector3) Vector3 {
	return Vector3{a.X + b.X, a.Y + b.Y, a.Z + b.Z}
}

func subtractVector3(a, b Vector3) Vector3 {
	return Vector3{a.X - b.X, a.Y - b.Y, a.Z - b.Z}
}

func multiplyVector3(v Vector3, scalar float32) Vector3 {
	return Vector3{v.X * scalar, v.Y * scalar, v.Z * scalar}
}

func dotProduct3(a, b Vector3) float32 {
	return a.X*b.X + a.Y*b.Y + a.Z*b.Z
}

func lengthVector3(v Vector3) float32 {
	return float32(math.Sqrt(float64(v.X*v.X + v.Y*v.Y + v.Z*v.Z)))
}

func normalizeVector3(v Vector3) Vector3 {
	length := lengthVector3(v)
	if length > 0 {
		return multiplyVector3(v, 1/length)
	}
	return Vector3{0, 0, 0}
}

// SpatialHash implementation
func NewSpatialHash(cellSize float32) *SpatialHash {
	return &SpatialHash{
		CellSize:  cellSize,
		Cells:     make(map[string]*SpatialCell),
		ObjectMap: make(map[string][]string),
	}
}

func (sh *SpatialHash) AddObject(obj *CollidableObject) {
	// Calculate which cells the object occupies
	cells := sh.calculateObjectCells(obj)
	
	for _, cellID := range cells {
		cell, exists := sh.Cells[cellID]
		if !exists {
			cell = &SpatialCell{
				Objects: make([]string, 0),
			}
			sh.Cells[cellID] = cell
		}
		
		// Add object to cell if not already present
		if !containsString(cell.Objects, obj.ID) {
			cell.Objects = append(cell.Objects, obj.ID)
		}
	}
	
	// Update object mapping
	sh.ObjectMap[obj.ID] = cells
}

func (sh *SpatialHash) RemoveObject(objectID string) {
	cells, exists := sh.ObjectMap[objectID]
	if !exists {
		return
	}
	
	for _, cellID := range cells {
		cell, exists := sh.Cells[cellID]
		if exists {
			// Remove object from cell
			for i, objID := range cell.Objects {
				if objID == objectID {
					cell.Objects = append(cell.Objects[:i], cell.Objects[i+1:]...)
					break
				}
			}
			
			// Remove empty cells
			if len(cell.Objects) == 0 {
				delete(sh.Cells, cellID)
			}
		}
	}
	
	delete(sh.ObjectMap, objectID)
}

func (sh *SpatialHash) UpdateObject(objectID string, newPosition Vector3) {
	// Remove from old cells
	sh.RemoveObject(objectID)
	
	// Find object to update its position
	// This would need to be implemented properly
}

func (sh *SpatialHash) GetPotentialPairs() [][]string {
	pairs := make([][]string, 0)
	processed := make(map[string]bool)
	
	for _, cell := range sh.Cells {
		for i, objA := range cell.Objects {
			for j := i + 1; j < len(cell.Objects); j++ {
				objB := cell.Objects[j]
				
				pairKey := objA + ":" + objB
				if !processed[pairKey] {
					pairs = append(pairs, []string{objA, objB})
					processed[pairKey] = true
				}
			}
		}
	}
	
	return pairs
}

func (sh *SpatialHash) GetObjectsInRadius(center Vector3, radius float32) []*CollidableObject {
	// Calculate which cells intersect with the radius
	// Return objects from those cells
	return make([]*CollidableObject, 0)
}

func (sh *SpatialHash) GetObjectsInAABB(min, max Vector3) []*CollidableObject {
	// Calculate which cells intersect with the AABB
	// Return objects from those cells
	return make([]*CollidableObject, 0)
}

func (sh *SpatialHash) GetCellCount() int {
	return len(sh.Cells)
}

func (sh *SpatialHash) GetPotentialPairCount() int {
	return len(sh.GetPotentialPairs())
}

func (sh *SpatialHash) Clear() {
	sh.Cells = make(map[string]*SpatialCell)
	sh.ObjectMap = make(map[string][]string)
}

func (sh *SpatialHash) calculateObjectCells(obj *CollidableObject) []string {
	// Calculate which cells an object occupies based on its shape and position
	return make([]string, 0)
}

func containsString(slice []string, item string) bool {
	for _, s := range slice {
		if s == item {
			return true
		}
	}
	return false
}

// NarrowPhaseDetector implementation
func NewNarrowPhaseDetector() *NarrowPhaseDetector {
	return &NarrowPhaseDetector{
		handlers: make(map[ShapePair]CollisionHandler),
	}
}

func (npd *NarrowPhaseDetector) RegisterHandler(shapeA, shapeB ShapeType, handler CollisionHandler) {
	pair := ShapePair{TypeA: shapeA, TypeB: shapeB}
	npd.handlers[pair] = handler
}

func (npd *NarrowPhaseDetector) GetHandler(shapeA, shapeB ShapeType) CollisionHandler {
	// Try both orderings of the pair
	pair1 := ShapePair{TypeA: shapeA, TypeB: shapeB}
	pair2 := ShapePair{TypeA: shapeB, TypeB: shapeA}
	
	if handler, exists := npd.handlers[pair1]; exists {
		return handler
	}
	
	if handler, exists := npd.handlers[pair2]; exists {
		return handler
	}
	
	return nil
}

// CircleCircleHandler implementation
func (cch *CircleCircleHandler) DetectCollision(objA, objB *CollidableObject) *CollisionManifold {
	// Calculate distance between centers
	distance := lengthVector3(subtractVector3(objB.Position, objA.Position))
	
	// Calculate radii
	radiusA := objA.Shape.Radius * objA.Scale.X
	radiusB := objB.Shape.Radius * objB.Scale.X
	
	// Check if circles overlap
	if distance < radiusA+radiusB {
		normal := normalizeVector3(subtractVector3(objB.Position, objA.Position))
		penetration := (radiusA + radiusB) - distance
		
		// Calculate contact point
		contactPoint := addVector3(objA.Position, 
			multiplyVector3(normal, radiusA))
		
		return &CollisionManifold{
			ObjectA:     objA.ID,
			ObjectB:     objB.ID,
			Normal:      normal,
			Penetration: penetration,
			Contacts:    []Vector3{contactPoint},
			Restitution: (objA.Restitution + objB.Restitution) * 0.5,
			Friction:    (objA.Friction + objB.Friction) * 0.5,
			Valid:       true,
		}
	}
	
	return &CollisionManifold{Valid: false}
}

func (cch *CircleCircleHandler) ResolveCollision(manifold *CollisionManifold) {
	// Circle-circle collision resolution
	// This would be handled by the main collision system
}

// Vector2 represents 2D coordinates
type Vector2 struct {
	X, Y float32
}

// Example usage
func main() {
	settings := CollisionSettings{
		MaxObjects:        1000,
		CellSize:          1.0,
		MaxPenetration:    0.1,
		Slop:             0.01,
		Baumgarte:        0.2,
		MaxIterations:    10,
		EnableTriggerEvents: true,
		EnableDebugDraw:   false,
		LayerCollisionMask: map[int][]int{
			1: {1, 2}, // Layer 1 collides with layers 1 and 2
			2: {1, 2}, // Layer 2 collides with layers 1 and 2
		},
	}

	cs := NewCollisionSystem(settings)
	
	// Start collision system
	if err := cs.Start(); err != nil {
		fmt.Printf("Error starting collision system: %v\n", err)
		return
	}

	// Create some test objects
	circle1 := &CollidableObject{
		ID:           "circle1",
		Position:     Vector3{0, 0, 0},
		Velocity:     Vector3{1, 0, 0},
		Shape:        CollisionShape{Type: CircleShape, Radius: 0.5},
		Active:       true,
		Static:       false,
		Mass:         1.0,
		Restitution:  0.8,
		Friction:     0.4,
		Layer:        1,
		Mask:         1 | 2,
	}

	circle2 := &CollidableObject{
		ID:           "circle2",
		Position:     Vector3{1, 0, 0},
		Velocity:     Vector3{-1, 0, 0},
		Shape:        CollisionShape{Type: CircleShape, Radius: 0.5},
		Active:       true,
		Static:       false,
		Mass:         1.0,
		Restitution:  0.8,
		Friction:     0.4,
		Layer:        1,
		Mask:         1 | 2,
	}

	// Add objects to collision system
	cs.AddObject(circle1)
	cs.AddObject(circle2)

	// Run collision detection
	events := cs.DetectCollisions()
	fmt.Printf("Detected %d collision events\n", len(events))
	
	for _, event := range events {
		fmt.Printf("Collision: %s <-> %s, penetration: %.2f\n", 
			event.ObjectA, event.ObjectB, event.Penetration)
	}

	// Test raycast
	hits, _ := cs.Raycast(Vector3{-2, 0, 0}, Vector3{1, 0, 0}, 5.0)
	fmt.Printf("Raycast hit %d objects\n", len(hits))
	
	for _, hit := range hits {
		fmt.Printf("Hit: %s at distance %.2f\n", hit.Object, hit.Distance)
	}

	// Get statistics
	stats := cs.GetCollisionStats()
	fmt.Printf("\nCollision System Statistics:\n")
	fmt.Printf("Total Objects: %d\n", stats.TotalObjects)
	fmt.Printf("Active Objects: %d\n", stats.ActiveObjects)
	fmt.Printf("Spatial Cells: %d\n", stats.SpatialCells)
	fmt.Printf("Potential Pairs: %d\n", stats.PotentialPairs)

	// Stop collision system
	cs.Stop()

	fmt.Println("\nCollision System initialized successfully!")
}
