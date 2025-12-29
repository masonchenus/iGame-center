// Physics Utilities - Physics calculations, collision detection, and simulations
use std::collections::HashMap;
use std::f32::consts::PI;
use std::ops::{Add, Sub, Mul, Div};
use serde::{Deserialize, Serialize};

/// 3D Vector for physics calculations
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct PhysicsVector3 {
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

impl PhysicsVector3 {
    pub fn new(x: f32, y: f32, z: f32) -> Self {
        Self { x, y, z }
    }
    
    pub fn zero() -> Self {
        Self { x: 0.0, y: 0.0, z: 0.0 }
    }
    
    pub fn one() -> Self {
        Self { x: 1.0, y: 1.0, z: 1.0 }
    }
    
    pub fn length(&self) -> f32 {
        (self.x * self.x + self.y * self.y + self.z * self.z).sqrt()
    }
    
    pub fn length_squared(&self) -> f32 {
        self.x * self.x + self.y * self.y + self.z * self.z
    }
    
    pub fn normalize(&self) -> Self {
        let len = self.length();
        if len > 0.0 {
            Self {
                x: self.x / len,
                y: self.y / len,
                z: self.z / len,
            }
        } else {
            *self
        }
    }
    
    pub fn dot(&self, other: &Self) -> f32 {
        self.x * other.x + self.y * other.y + self.z * other.z
    }
    
    pub fn cross(&self, other: &Self) -> Self {
        Self {
            x: self.y * other.z - self.z * other.y,
            y: self.z * other.x - self.x * other.z,
            z: self.x * other.y - self.y * other.x,
        }
    }
    
    pub fn distance(&self, other: &Self) -> f32 {
        (*self - *other).length()
    }
    
    pub fn lerp(&self, other: &Self, t: f32) -> Self {
        *self + (*other - *self) * t
    }
}

impl Add for PhysicsVector3 {
    type Output = Self;
    
    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        }
    }
}

impl Sub for PhysicsVector3 {
    type Output = Self;
    
    fn sub(self, other: Self) -> Self {
        Self {
            x: self.x - other.x,
            y: self.y - other.y,
            z: self.z - other.z,
        }
    }
}

impl Mul<f32> for PhysicsVector3 {
    type Output = Self;
    
    fn mul(self, scalar: f32) -> Self {
        Self {
            x: self.x * scalar,
            y: self.y * scalar,
            z: self.z * scalar,
        }
    }
}

impl Div<f32> for PhysicsVector3 {
    type Output = Self;
    
    fn div(self, scalar: f32) -> Self {
        Self {
            x: self.x / scalar,
            y: self.y / scalar,
            z: self.z / scalar,
        }
    }
}

/// Quaternion for rotations
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct Quaternion {
    pub x: f32,
    pub y: f32,
    pub z: f32,
    pub w: f32,
}

impl Quaternion {
    pub fn new(x: f32, y: f32, z: f32, w: f32) -> Self {
        Self { x, y, z, w }
    }
    
    pub fn identity() -> Self {
        Self { x: 0.0, y: 0.0, z: 0.0, w: 1.0 }
    }
    
    pub fn from_euler(pitch: f32, yaw: f32, roll: f32) -> Self {
        let cy = (yaw * 0.5).cos();
        let sy = (yaw * 0.5).sin();
        let cp = (pitch * 0.5).cos();
        let sp = (pitch * 0.5).sin();
        let cr = (roll * 0.5).cos();
        let sr = (roll * 0.5).sin();

        Self {
            w: cr * cp * cy + sr * sp * sy,
            x: sr * cp * cy - cr * sp * sy,
            y: cr * sp * cy + sr * cp * sy,
            z: cr * cp * sy - sr * sp * cy,
        }
    }
    
    pub fn normalize(&self) -> Self {
        let len = (self.x * self.x + self.y * self.y + self.z * self.z + self.w * self.w).sqrt();
        if len > 0.0 {
            Self {
                x: self.x / len,
                y: self.y / len,
                z: self.z / len,
                w: self.w / len,
            }
        } else {
            *self
        }
    }
    
    pub fn to_euler(&self) -> (f32, f32, f32) {
        let sqw = self.w * self.w;
        let sqx = self.x * self.x;
        let sqy = self.y * self.y;
        let sqz = self.z * self.z;

        let yaw = (2.0 * (self.w * self.z + self.x * self.y)) / (sqx + sqy + sqz + sqw).atan2(2.0 * (self.w * self.y - self.z * self.x));
        let pitch = (2.0 * (self.w * self.x + self.y * self.z)) / (sqx + sqy + sqz + sqw).asin();
        let roll = (2.0 * (self.w * self.y - self.z * self.x)) / (sqx + sqy + sqz + sqw).atan2(2.0 * (self.w * self.x + self.y * self.z));

        (pitch, yaw, roll)
    }
    
    pub fn multiply(&self, other: &Self) -> Self {
        Self {
            w: self.w * other.w - self.x * other.x - self.y * other.y - self.z * other.z,
            x: self.w * other.x + self.x * other.w + self.y * other.z - self.z * other.y,
            y: self.w * other.y - self.x * other.z + self.y * other.w + self.z * other.x,
            z: self.w * other.z + self.x * other.y - self.y * other.x + self.z * other.w,
        }
    }
}

/// Physics object types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PhysicsObjectType {
    Sphere,
    Box,
    Capsule,
    Cylinder,
    Mesh,
    Plane,
}

/// Physics material properties
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PhysicsMaterial {
    pub density: f32,
    pub restitution: f32,
    pub friction: f32,
    pub mass: f32,
    pub is_static: bool,
}

/// Physics collision shape
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PhysicsShape {
    pub object_type: PhysicsObjectType,
    pub center: PhysicsVector3,
    pub rotation: Quaternion,
    pub size: PhysicsVector3, // For boxes: (width, height, depth)
    pub radius: f32,          // For spheres and capsules
    pub height: f32,          // For capsules
}

/// Physics body with physical properties
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PhysicsBody {
    pub id: String,
    pub shape: PhysicsShape,
    pub material: PhysicsMaterial,
    pub position: PhysicsVector3,
    pub velocity: PhysicsVector3,
    pub acceleration: PhysicsVector3,
    pub rotation: Quaternion,
    pub angular_velocity: PhysicsVector3,
    pub is_active: bool,
    pub is_trigger: bool,
    pub collision_layer: u32,
    pub collision_mask: u32,
}

/// Collision detection result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CollisionResult {
    pub has_collision: bool,
    pub contact_point: PhysicsVector3,
    pub normal: PhysicsVector3,
    pub penetration: f32,
    pub relative_velocity: PhysicsVector3,
    pub impulse: f32,
}

/// Physics world settings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PhysicsWorldSettings {
    pub gravity: PhysicsVector3,
    pub time_step: f32,
    pub max_sub_steps: u32,
    pub enable_sleeping: bool,
    pub solver_iterations: u32,
    pub enable_debug_draw: bool,
    pub broad_phase_algorithm: BroadPhaseAlgorithm,
    pub narrow_phase_algorithm: NarrowPhaseAlgorithm,
}

/// Broad-phase collision detection algorithms
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BroadPhaseAlgorithm {
    SAP, // Sweep and Prune
    Grid,
    Quadtree,
    Octree,
    UniformGrid,
}

/// Narrow-phase collision detection algorithms
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum NarrowPhaseAlgorithm {
    GJK,    // Gilbert-Johnson-Keerthi
    EPA,    // Expanding Polytope Algorithm
    SAT,    // Separating Axis Theorem
    Raycast,
}

/// Physics simulation world
pub struct PhysicsWorld {
    bodies: HashMap<String, PhysicsBody>,
    settings: PhysicsWorldSettings,
    broad_phase: Box<dyn BroadPhaseDetector>,
    narrow_phase: Box<dyn NarrowPhaseDetector>,
}

/// Broad-phase collision detection trait
pub trait BroadPhaseDetector: Send + Sync {
    fn add_body(&mut self, body: &PhysicsBody);
    fn remove_body(&mut self, body_id: &str);
    fn update_body(&mut self, body: &PhysicsBody);
    fn get_potential_pairs(&self) -> Vec<(String, String)>;
}

/// Narrow-phase collision detection trait
pub trait NarrowPhaseDetector: Send + Sync {
    fn detect_collision(&self, body_a: &PhysicsBody, body_b: &PhysicsBody) -> Option<CollisionResult>;
    fn raycast(&self, origin: PhysicsVector3, direction: PhysicsVector3, max_distance: f32) -> Vec<RaycastHit>;
}

/// Raycast hit result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RaycastHit {
    pub body_id: String,
    pub hit_point: PhysicsVector3,
    pub normal: PhysicsVector3,
    pub distance: f32,
    pub fraction: f32,
}

/// Simple broad-phase detector using spatial hashing
pub struct SpatialHashBroadPhase {
    cell_size: f32,
    buckets: HashMap<String, Vec<String>>,
}

impl SpatialHashBroadPhase {
    pub fn new(cell_size: f32) -> Self {
        Self {
            cell_size,
            buckets: HashMap::new(),
        }
    }
    
    fn get_cell_key(&self, position: &PhysicsVector3) -> String {
        let x = (position.x / self.cell_size).floor() as i32;
        let y = (position.y / self.cell_size).floor() as i32;
        let z = (position.z / self.cell_size).floor() as i32;
        format!("{},{},{}", x, y, z)
    }
    
    fn get_occupied_cells(&self, shape: &PhysicsShape) -> Vec<String> {
        let center = shape.center;
        let size = shape.size;
        
        let mut cells = Vec::new();
        
        match shape.object_type {
            PhysicsObjectType::Sphere => {
                let radius = shape.radius;
                let min = PhysicsVector3::new(
                    center.x - radius,
                    center.y - radius,
                    center.z - radius,
                );
                let max = PhysicsVector3::new(
                    center.x + radius,
                    center.y + radius,
                    center.z + radius,
                );
                
                let min_cell = self.get_cell_key(&min);
                let max_cell = self.get_cell_key(&max);
                
                // For simplicity, just return the center cell
                cells.push(self.get_cell_key(&center));
            },
            PhysicsObjectType::Box => {
                let half_size = size * 0.5;
                let min = center - half_size;
                let max = center + half_size;
                
                cells.push(self.get_cell_key(&min));
                cells.push(self.get_cell_key(&max));
                cells.push(self.get_cell_key(&center));
            },
            _ => {
                cells.push(self.get_cell_key(&center));
            }
        }
        
        cells
    }
}

impl BroadPhaseDetector for SpatialHashBroadPhase {
    fn add_body(&mut self, body: &PhysicsBody) {
        let cells = self.get_occupied_cells(&body.shape);
        for cell in cells {
            self.buckets.entry(cell).or_insert_with(Vec::new).push(body.id.clone());
        }
    }
    
    fn remove_body(&mut self, body_id: &str) {
        for bucket in self.buckets.values_mut() {
            bucket.retain(|id| id != body_id);
        }
    }
    
    fn update_body(&mut self, body: &PhysicsBody) {
        self.remove_body(&body.id);
        self.add_body(body);
    }
    
    fn get_potential_pairs(&self) -> Vec<(String, String)> {
        let mut pairs = Vec::new();
        let mut processed = std::collections::HashSet::new();
        
        for bucket in self.buckets.values() {
            for i in 0..bucket.len() {
                for j in (i + 1)..bucket.len() {
                    let id1 = &bucket[i];
                    let id2 = &bucket[j];
                    
                    let pair_key = if id1 < id2 {
                        format!("{},{}", id1, id2)
                    } else {
                        format!("{},{}", id2, id1)
                    };
                    
                    if !processed.contains(&pair_key) {
                        processed.insert(pair_key.clone());
                        pairs.push((id1.clone(), id2.clone()));
                    }
                }
            }
        }
        
        pairs
    }
}

/// Simple narrow-phase detector using GJK algorithm
pub struct GJKNarrowPhase;

impl GJKNarrowPhase {
    pub fn new() -> Self {}
        Self
    }
    
    fn support_point(shape: &PhysicsShape, direction: &PhysicsVector3) -> PhysicsVector3 {
        match shape.object_type {
            PhysicsObjectType::Sphere => {
                let dir = direction.normalize();
                shape.center + dir * shape.radius
            },
            PhysicsObjectType::Box => {
                let half_size = shape.size * 0.5;
                PhysicsVector3::new(
                    if direction.x >= 0.0 { half_size.x } else { -half_size.x },
                    if direction.y >= 0.0 { half_size.y } else { -half_size.y },
                    if direction.z >= 0.0 { half_size.z } else { -half_size.z },
                )
            },
            _ => shape.center,
        }
    }
    
    fn gjk_distance(shape_a: &PhysicsShape, shape_b: &PhysicsShape) -> f32 {
        // Simplified GJK distance calculation
        let center_distance = shape_a.center.distance(&shape_b.center);
        
        match (shape_a.object_type.clone(), shape_b.object_type.clone()) {
            (PhysicsObjectType::Sphere, PhysicsObjectType::Sphere) => {
                let combined_radius = shape_a.radius + shape_b.radius;
                (center_distance - combined_radius).max(0.0)
            },
            (PhysicsObjectType::Box, PhysicsObjectType::Box) => {
                // Simplified box distance
                (center_distance - 1.0).max(0.0)
            },
            (PhysicsObjectType::Sphere, PhysicsObjectType::Box) => {
                let combined_radius = shape_a.radius + 0.5;
                (center_distance - combined_radius).max(0.0)
            },
            _ => center_distance,
        }
    }
}

impl NarrowPhaseDetector for GJKNarrowPhase {
    fn detect_collision(&self, body_a: &PhysicsBody, body_b: &PhysicsBody) -> Option<CollisionResult> {
        let distance = Self::gjk_distance(&body_a.shape, &body_b.shape);
        
        if distance <= 0.0 {
            let normal = (body_b.shape.center - body_a.shape.center).normalize();
            let contact_point = body_a.shape.center + normal * body_a.shape.radius;
            
            Some(CollisionResult {
                has_collision: true,
                contact_point,
                normal,
                penetration: -distance,
                relative_velocity: body_a.velocity - body_b.velocity,
                impulse: 0.0,
            })
        } else {
            None
        }
    }
    
    fn raycast(&self, origin: PhysicsVector3, direction: PhysicsVector3, max_distance: f32) -> Vec<RaycastHit> {
        let mut hits = Vec::new();
        
        // Simplified raycast - in real implementation, check against all shapes
        // For demo purposes, return a mock hit
        let hit_point = origin + direction.normalize() * max_distance * 0.5;
        
        hits.push(RaycastHit {
            body_id: "mock_body".to_string(),
            hit_point,
            normal: PhysicsVector3::new(0.0, 1.0, 0.0),
            distance: max_distance * 0.5,
            fraction: 0.5,
        });
        
        hits
    }
}

impl PhysicsWorld {
    /// Create a new physics world
    pub fn new(settings: PhysicsWorldSettings) -> Self {
        let broad_phase: Box<dyn BroadPhaseDetector> = match settings.broad_phase_algorithm {
            BroadPhaseAlgorithm::SAP | BroadPhaseAlgorithm::Grid | BroadPhaseAlgorithm::UniformGrid => {
                Box::new(SpatialHashBroadPhase::new(1.0))
            },
            _ => Box::new(SpatialHashBroadPhase::new(1.0)),
        };
        
        let narrow_phase: Box<dyn NarrowPhaseDetector> = match settings.narrow_phase_algorithm {
            NarrowPhaseAlgorithm::GJK | NarrowPhaseAlgorithm::EPA | NarrowPhaseAlgorithm::SAT => {
                Box::new(GJKNarrowPhase::new())
            },
            _ => Box::new(GJKNarrowPhase::new()),
        };

        Self {
            bodies: HashMap::new(),
            settings,
            broad_phase,
            narrow_phase,
        }
    }

    /// Add a physics body to the world
    pub fn add_body(&mut self, body: PhysicsBody) {
        self.bodies.insert(body.id.clone(), body.clone());
        self.broad_phase.add_body(&body);
    }

    /// Remove a physics body from the world
    pub fn remove_body(&mut self, body_id: &str) {
        self.bodies.remove(body_id);
        self.broad_phase.remove_body(body_id);
    }

    /// Update physics body
    pub fn update_body(&mut self, body_id: &str, updates: PhysicsBodyUpdates) -> Result<(), PhysicsError> {
        if let Some(body) = self.bodies.get_mut(body_id) {
            if let Some(position) = updates.position {
                body.position = position;
            }
            if let Some(velocity) = updates.velocity {
                body.velocity = velocity;
            }
            if let Some(rotation) = updates.rotation {
                body.rotation = rotation;
            }
            if let Some(is_active) = updates.is_active {
                body.is_active = is_active;
            }
            
            self.broad_phase.update_body(body);
            Ok(())
        } else {
            Err(PhysicsError::BodyNotFound)
        }
    }

    /// Step the physics simulation
    pub fn step(&mut self, delta_time: f32) {
        let sub_step = self.settings.time_step;
        let sub_steps = (delta_time / sub_step).ceil() as u32;
        
        for _ in 0..sub_steps.min(self.settings.max_sub_steps) {
            self.integrate_forces(sub_step);
            self.integrate_velocities(sub_step);
            self.detect_collisions();
            self.resolve_collisions();
            self.update_transforms();
        }
    }

    /// Check collision between two bodies
    pub fn check_collision(&self, body_id_a: &str, body_id_b: &str) -> Option<CollisionResult> {
        if let (Some(body_a), Some(body_b)) = (self.bodies.get(body_id_a), self.bodies.get(body_id_b)) {
            self.narrow_phase.detect_collision(body_a, body_b)
        } else {
            None
        }
    }

    /// Raycast against all bodies
    pub fn raycast(&self, origin: PhysicsVector3, direction: PhysicsVector3, max_distance: f32) -> Vec<RaycastHit> {
        self.narrow_phase.raycast(origin, direction, max_distance)
    }

    /// Get all bodies
    pub fn get_bodies(&self) -> Vec<&PhysicsBody> {
        self.bodies.values().collect()
    }

    /// Get body by ID
    pub fn get_body(&self, body_id: &str) -> Option<&PhysicsBody> {
        self.bodies.get(body_id)
    }

    /// Apply force to a body
    pub fn apply_force(&mut self, body_id: &str, force: PhysicsVector3) -> Result<(), PhysicsError> {
        if let Some(body) = self.bodies.get_mut(body_id) {
            if !body.is_active {
                return Err(PhysicsError::BodyInactive);
            }
            
            // F = ma, so a = F/m
            if body.material.mass > 0.0 {
                let acceleration = force / body.material.mass;
                body.acceleration = body.acceleration + acceleration;
            }
            
            Ok(())
        } else {
            Err(PhysicsError::BodyNotFound)
        }
    }

    /// Apply impulse to a body
    pub fn apply_impulse(&mut self, body_id: &str, impulse: PhysicsVector3) -> Result<(), PhysicsError> {
        if let Some(body) = self.bodies.get_mut(body_id) {
            if !body.is_active {
                return Err(PhysicsError::BodyInactive);
            }
            
            // Impulse changes velocity directly: v += J/m
            if body.material.mass > 0.0 {
                body.velocity = body.velocity + impulse / body.material.mass;
            }
            
            Ok(())
        } else {
            Err(PhysicsError::BodyNotFound)
        }
    }

    /// Set gravity for the world
    pub fn set_gravity(&mut self, gravity: PhysicsVector3) {
        self.settings.gravity = gravity;
    }

    /// Get gravity
    pub fn get_gravity(&self) -> PhysicsVector3 {
        self.settings.gravity
    }

    /// Internal simulation methods

    fn integrate_forces(&mut self, delta_time: f32) {
        for body in self.bodies.values_mut() {
            if !body.is_active || body.material.is_static {
                continue;
            }

            // Apply gravity
            let gravity_force = self.settings.gravity * body.material.mass;
            body.acceleration = body.acceleration + gravity_force / body.material.mass;
        }
    }

    fn integrate_velocities(&mut self, delta_time: f32) {
        for body in self.bodies.values_mut() {
            if !body.is_active || body.material.is_static {
                continue;
            }

            // Semi-implicit Euler integration
            body.velocity = body.velocity + body.acceleration * delta_time;
            body.position = body.position + body.velocity * delta_time;
            
            // Reset acceleration
            body.acceleration = PhysicsVector3::zero();
        }
    }

    fn detect_collisions(&mut self) {
        let potential_pairs = self.broad_phase.get_potential_pairs();
        
        for (body_id_a, body_id_b) in potential_pairs {
            if let (Some(body_a), Some(body_b)) = (self.bodies.get(&body_id_a), self.bodies.get(&body_id_b)) {
                if body_a.is_active && body_b.is_active && !body_a.is_trigger && !body_b.is_trigger {
                    self.narrow_phase.detect_collision(body_a, body_b);
                }
            }
        }
    }

    fn resolve_collisions(&mut self) {
        // Simplified collision resolution
        // In a real implementation, this would be more sophisticated
        for body in self.bodies.values_mut() {
            if !body.is_active || body.material.is_static {
                continue;
            }

            // Simple ground collision
            if body.position.y < 0.0 {
                body.position.y = 0.0;
                if body.velocity.y < 0.0 {
                    body.velocity.y = -body.velocity.y * body.material.restitution;
                }
                
                // Apply friction
                body.velocity.x *= 0.8;
                body.velocity.z *= 0.8;
            }
        }
    }

    fn update_transforms(&mut self) {
        // Update any transforms or other derived data
        // This would typically update graphics transforms, etc.
    }
}

/// Physics body update structure
#[derive(Debug, Clone)]
pub struct PhysicsBodyUpdates {
    pub position: Option<PhysicsVector3>,
    pub velocity: Option<PhysicsVector3>,
    pub rotation: Option<Quaternion>,
    pub is_active: Option<bool>,
}

impl PhysicsBodyUpdates {
    pub fn new() -> Self {
        Self {
            position: None,
            velocity: None,
            rotation: None,
            is_active: None,
        }
    }
    
    pub fn with_position(mut self, position: PhysicsVector3) -> Self {
        self.position = Some(position);
        self
    }
    
    pub fn with_velocity(mut self, velocity: PhysicsVector3) -> Self {
        self.velocity = Some(velocity);
        self
    }
    
    pub fn with_rotation(mut self, rotation: Quaternion) -> Self {
        self.rotation = Some(rotation);
        self
    }
    
    pub fn with_active(mut self, is_active: bool) -> Self {
        self.is_active = Some(is_active);
        self
    }
}

/// Physics error types
#[derive(Debug, thiserror::Error)]
pub enum PhysicsError {
    #[error("Physics body not found")]
    BodyNotFound,
    
    #[error("Physics body is inactive")]
    BodyInactive,
    
    #[error("Invalid physics parameters")]
    InvalidParameters,
}

/// Utility functions

/// Calculate bounding box for a shape
pub fn calculate_bounding_box(shape: &PhysicsShape) -> (PhysicsVector3, PhysicsVector3) {
    match shape.object_type {
        PhysicsObjectType::Sphere => {
            let center = shape.center;
            let radius = shape.radius;
            (center - PhysicsVector3::new(radius, radius, radius),
             center + PhysicsVector3::new(radius, radius, radius))
        },
        PhysicsObjectType::Box => {
            let center = shape.center;
            let half_size = shape.size * 0.5;
            (center - half_size, center + half_size)
        },
        _ => {
            (shape.center - PhysicsVector3::one(), shape.center + PhysicsVector3::one())
        }
    }
}

/// Check if two shapes intersect
pub fn shapes_intersect(shape_a: &PhysicsShape, shape_b: &PhysicsShape) -> bool {
    match (shape_a.object_type.clone(), shape_b.object_type.clone()) {
        (PhysicsObjectType::Sphere, PhysicsObjectType::Sphere) => {
            let distance = shape_a.center.distance(&shape_b.center);
            distance <= shape_a.radius + shape_b.radius
        },
        _ => false, // Simplified - would implement more intersection tests
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vector_operations() {
        let v1 = PhysicsVector3::new(1.0, 2.0, 3.0);
        let v2 = PhysicsVector3::new(4.0, 5.0, 6.0);
        
        let add = v1 + v2;
        assert_eq!(add.x, 5.0);
        assert_eq!(add.y, 7.0);
        assert_eq!(add.z, 9.0);
        
        let sub = v1 - v2;
        assert_eq!(sub.x, -3.0);
        assert_eq!(sub.y, -3.0);
        assert_eq!(sub.z, -3.0);
        
        let mul = v1 * 2.0;
        assert_eq!(mul.x, 2.0);
        assert_eq!(mul.y, 4.0);
        assert_eq!(mul.z, 6.0);
        
        let length = v1.length();
        assert!((length - 3.74165738677).abs() < 0.001);
    }

    #[test]
    fn test_quaternion_operations() {
        let q1 = Quaternion::from_euler(0.0, 0.0, 0.0);
        assert!((q1.w - 1.0).abs() < 0.001);
        
        let q2 = Quaternion::from_euler(PI/2.0, 0.0, 0.0);
        let q3 = q1.multiply(&q2);
        
        assert!(q3.w.abs() < 0.001);
        assert!(q3.x.abs() < 0.001);
        assert!(q3.y.abs() < 0.001);
    }

    #[test]
    fn test_physics_world() {
        let settings = PhysicsWorldSettings {
            gravity: PhysicsVector3::new(0.0, -9.81, 0.0),
            time_step: 0.016,
            max_sub_steps: 4,
            enable_sleeping: true,
            solver_iterations: 10,
            enable_debug_draw: false,
            broad_phase_algorithm: BroadPhaseAlgorithm::UniformGrid,
            narrow_phase_algorithm: NarrowPhaseAlgorithm::GJK,
        };
        
        let mut world = PhysicsWorld::new(settings);
        
        let body = PhysicsBody {
            id: "test_body".to_string(),
            shape: PhysicsShape {
                object_type: PhysicsObjectType::Sphere,
                center: PhysicsVector3::new(0.0, 10.0, 0.0),
                rotation: Quaternion::identity(),
                size: PhysicsVector3::one(),
                radius: 1.0,
                height: 2.0,
            },
            material: PhysicsMaterial {
                density: 1.0,
                restitution: 0.8,
                friction: 0.4,
                mass: 1.0,
                is_static: false,
            },
            position: PhysicsVector3::new(0.0, 10.0, 0.0),
            velocity: PhysicsVector3::zero(),
            acceleration: PhysicsVector3::zero(),
            rotation: Quaternion::identity(),
            angular_velocity: PhysicsVector3::zero(),
            is_active: true,
            is_trigger: false,
            collision_layer: 1,
            collision_mask: 1,
        };
        
        world.add_body(body);
        assert_eq!(world.get_bodies().len(), 1);
        
        // Step simulation
        world.step(0.016);
        
        // Check that position changed due to gravity
        let body = world.get_body("test_body").unwrap();
        assert!(body.position.y < 10.0);
    }
}
