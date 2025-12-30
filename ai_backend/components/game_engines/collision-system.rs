// Collision System - Advanced collision detection and response
use std::collections::{HashMap, HashSet};
use std::f32::consts::PI;
use serde::{Deserialize, Serialize};
use crate::physics_utils::{PhysicsVector3, PhysicsBody, PhysicsShape, PhysicsObjectType, CollisionResult};

/// Collision types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CollisionType {
    Static,
    Dynamic,
    Trigger,
    Sensor,
}

/// Collision response types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CollisionResponse {
    Bounce,
    Slide,
    Stop,
    PassThrough,
    Custom,
}

/// Collision layer configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CollisionLayer {
    pub name: String,
    pub id: u32,
    pub masks: HashSet<u32>,
    pub enabled: bool,
}

/// Collision shape for advanced detection
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CollisionShape {
    pub shape_type: CollisionShapeType,
    pub position: PhysicsVector3,
    pub rotation: f32,
    pub scale: PhysicsVector3,
    pub bounds: AABB,
}

/// Collision shape types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CollisionShapeType {
    Box { size: PhysicsVector3 },
    Sphere { radius: f32 },
    Capsule { radius: f32, height: f32 },
    Cylinder { radius: f32, height: f32 },
    ConvexHull { points: Vec<PhysicsVector3> },
    Mesh { vertices: Vec<PhysicsVector3>, indices: Vec<u32> },
}

/// Axis-Aligned Bounding Box
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct AABB {
    pub min: PhysicsVector3,
    pub max: PhysicsVector3,
}

impl AABB {
    pub fn new(min: PhysicsVector3, max: PhysicsVector3) -> Self {
        Self { min, max }
    }
    
    pub fn contains_point(&self, point: &PhysicsVector3) -> bool {
        point.x >= self.min.x && point.x <= self.max.x &&
        point.y >= self.min.y && point.y <= self.max.y &&
        point.z >= self.min.z && point.z <= self.max.z
    }
    
    pub fn intersects(&self, other: &AABB) -> bool {
        self.min.x <= other.max.x && self.max.x >= other.min.x &&
        self.min.y <= other.max.y && self.max.y >= other.min.y &&
        self.min.z <= other.max.z && self.max.z >= other.min.z
    }
    
    pub fn expanded(&self, amount: f32) -> AABB {
        AABB::new(
            PhysicsVector3::new(self.min.x - amount, self.min.y - amount, self.min.z - amount),
            PhysicsVector3::new(self.max.x + amount, self.max.y + amount, self.max.z + amount),
        )
    }
}

/// Collision event
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CollisionEvent {
    pub event_id: String,
    pub body_a_id: String,
    pub body_b_id: String,
    pub collision_type: CollisionType,
    pub contact_point: PhysicsVector3,
    pub normal: PhysicsVector3,
    pub penetration: f32,
    pub relative_velocity: PhysicsVector3,
    pub timestamp: std::time::Instant,
}

/// Collision contact
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Contact {
    pub position: PhysicsVector3,
    pub normal: PhysicsVector3,
    pub penetration: f32,
    pub impulse: f32,
    pub separation_velocity: f32,
}

/// Collision manifold for complex interactions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CollisionManifold {
    pub body_a_id: String,
    pub body_b_id: String,
    pub contacts: Vec<Contact>,
    pub normal: PhysicsVector3,
    pub penetration: f32,
    pub restitution: f32,
    pub friction: f32,
}

/// Collision system configuration
#[derive(Debug, Clone)]
pub struct CollisionSystemConfig {
    pub enable_spatial_partitioning: bool,
    pub broad_phase_algorithm: BroadPhaseAlgorithm,
    pub narrow_phase_algorithm: NarrowPhaseAlgorithm,
    pub max_contacts: usize,
    pub max_iterations: u32,
    pub enable_deactivation: bool,
    pub deactivation_time: f32,
    pub enable_continuous_collision: bool,
    pub time_of_impact_iterations: u32,
}

/// Broad-phase algorithms
#[derive(Debug, Clone, Copy)]
pub enum BroadPhaseAlgorithm {
    BruteForce,
    UniformGrid,
    SpatialHash,
    SweepAndPrune,
    Quadtree,
}

/// Narrow-phase algorithms
#[derive(Debug, Clone, Copy)]
pub enum NarrowPhaseAlgorithm {
    GJK,
    EPA,
    SAT,
    Hybrid,
}

/// Spatial partitioning structure
#[derive(Debug, Clone)]
pub struct SpatialGrid {
    pub cell_size: f32,
    pub cells: HashMap<String, HashSet<String>>,
    pub bounds: AABB,
}

impl SpatialGrid {
    pub fn new(cell_size: f32, bounds: AABB) -> Self {
        Self {
            cell_size,
            cells: HashMap::new(),
            bounds,
        }
    }
    
    pub fn get_cell_key(&self, position: &PhysicsVector3) -> String {
        let x = (position.x / self.cell_size).floor() as i32;
        let y = (position.y / self.cell_size).floor() as i32;
        let z = (position.z / self.cell_size).floor() as i32;
        format!("{},{},{}", x, y, z)
    }
    
    pub fn get_neighboring_cells(&self, position: &PhysicsVector3) -> Vec<String> {
        let base_key = self.get_cell_key(position);
        let mut neighbors = Vec::new();
        
        // Add current cell
        neighbors.push(base_key.clone());
        
        // Add adjacent cells
        for dx in -1..=1 {
            for dy in -1..=1 {
                for dz in -1..=1 {
                    if dx == 0 && dy == 0 && dz == 0 {
                        continue;
                    }
                    
                    let parts: Vec<&str> = base_key.split(',').collect();
                    if parts.len() == 3 {
                        let x: i32 = parts[0].parse().unwrap_or(0) + dx;
                        let y: i32 = parts[1].parse().unwrap_or(0) + dy;
                        let z: i32 = parts[2].parse().unwrap_or(0) + dz;
                        neighbors.push(format!("{},{},{}", x, y, z));
                    }
                }
            }
        }
        
        neighbors
    }
}

/// Advanced collision detection system
pub struct CollisionSystem {
    bodies: HashMap<String, PhysicsBody>,
    collision_shapes: HashMap<String, CollisionShape>,
    collision_layers: HashMap<u32, CollisionLayer>,
    spatial_grid: Option<SpatialGrid>,
    collision_events: Vec<CollisionEvent>,
    active_contacts: HashMap<String, CollisionManifold>,
    config: CollisionSystemConfig,
    stats: CollisionStats,
}

/// Collision system statistics
#[derive(Debug, Clone)]
pub struct CollisionStats {
    pub total_checks: u32,
    pub collision_count: u32,
    pub broad_phase_time: f32,
    pub narrow_phase_time: f32,
    pub average_contacts_per_collision: f32,
    pub spatial_grid_hits: u32,
}

impl CollisionSystem {
    /// Create a new collision system
    pub fn new(config: CollisionSystemConfig) -> Self {
        let spatial_grid = if config.enable_spatial_partitioning {
            Some(SpatialGrid::new(2.0, AABB::new(
                PhysicsVector3::new(-1000.0, -1000.0, -1000.0),
                PhysicsVector3::new(1000.0, 1000.0, 1000.0),
            )))
        } else {
            None
        };

        Self {
            bodies: HashMap::new(),
            collision_shapes: HashMap::new(),
            collision_layers: Self::default_layers(),
            spatial_grid,
            collision_events: Vec::new(),
            active_contacts: HashMap::new(),
            config,
            stats: CollisionStats {
                total_checks: 0,
                collision_count: 0,
                broad_phase_time: 0.0,
                narrow_phase_time: 0.0,
                average_contacts_per_collision: 0.0,
                spatial_grid_hits: 0,
            },
        }
    }

    /// Add a physics body to the collision system
    pub fn add_body(&mut self, body: PhysicsBody) {
        self.bodies.insert(body.id.clone(), body.clone());
        
        // Create collision shape from physics shape
        let collision_shape = self.create_collision_shape(&body);
        self.collision_shapes.insert(body.id, collision_shape);
        
        // Add to spatial grid if enabled
        if let Some(ref mut grid) = self.spatial_grid {
            let key = grid.get_cell_key(&body.position);
            grid.cells.entry(key).or_insert_with(HashSet::new).insert(body.id.clone());
        }
    }

    /// Remove a body from the collision system
    pub fn remove_body(&mut self, body_id: &str) {
        self.bodies.remove(body_id);
        self.collision_shapes.remove(body_id);
        self.active_contacts.remove(body_id);
        
        // Remove from spatial grid
        if let Some(ref mut grid) = self.spatial_grid {
            for cell in grid.cells.values_mut() {
                cell.remove(body_id);
            }
        }
    }

    /// Update collision system
    pub fn update(&mut self, delta_time: f32) {
        self.collision_events.clear();
        
        // Broad phase collision detection
        let start_broad = std::time::Instant::now();
        let potential_pairs = self.broad_phase_detection();
        self.stats.broad_phase_time = start_broad.elapsed().as_secs_f32();
        
        // Narrow phase collision detection
        let start_narrow = std::time::Instant::now();
        let collisions = self.narrow_phase_detection(&potential_pairs);
        self.stats.narrow_phase_time = start_narrow.elapsed().as_secs_f32();
        
        // Process collisions
        self.process_collisions(collisions);
        
        self.stats.total_checks = potential_pairs.len() as u32;
        self.stats.collision_count = collisions.len() as u32;
    }

    /// Check if two bodies are colliding
    pub fn check_collision(&self, body_a_id: &str, body_b_id: &str) -> Option<CollisionManifold> {
        let body_a = self.bodies.get(body_a_id)?;
        let body_b = self.bodies.get(body_b_id)?;
        
        let shape_a = self.collision_shapes.get(body_a_id)?;
        let shape_b = self.collision_shapes.get(body_b_id)?;
        
        // Layer check
        if !self.layers_collide(body_a.collision_layer, body_b.collision_layer) {
            return None;
        }
        
        // Shape-based collision detection
        self.detect_shape_collision(shape_a, shape_b, body_a_id, body_b_id)
    }

    /// Get collision events
    pub fn get_collision_events(&self) -> &[CollisionEvent] {
        &self.collision_events
    }

    /// Get active contacts
    pub fn get_active_contacts(&self) -> &HashMap<String, CollisionManifold> {
        &self.active_contacts
    }

    /// Get collision statistics
    pub fn get_stats(&self) -> &CollisionStats {
        &self.stats
    }

    /// Set collision layer mask
    pub fn set_collision_layer_mask(&mut self, layer_id: u32, masks: HashSet<u32>) {
        if let Some(layer) = self.collision_layers.get_mut(&layer_id) {
            layer.masks = masks;
        }
    }

    /// Create collision shape from physics shape
    fn create_collision_shape(&self, body: &PhysicsBody) -> CollisionShape {
        let bounds = match body.shape.object_type {
            PhysicsObjectType::Sphere => {
                let half_extents = PhysicsVector3::new(body.shape.radius, body.shape.radius, body.shape.radius);
                AABB::new(body.position - half_extents, body.position + half_extents)
            },
            PhysicsObjectType::Box => {
                let half_extents = body.shape.size * 0.5;
                AABB::new(body.position - half_extents, body.position + half_extents)
            },
            _ => {
                AABB::new(body.position - PhysicsVector3::one(), body.position + PhysicsVector3::one())
            }
        };

        let shape_type = match body.shape.object_type {
            PhysicsObjectType::Sphere => CollisionShapeType::Sphere { radius: body.shape.radius },
            PhysicsObjectType::Box => CollisionShapeType::Box { size: body.shape.size },
            PhysicsObjectType::Capsule => CollisionShapeType::Capsule {
                radius: body.shape.radius,
                height: body.shape.height,
            },
            _ => CollisionShapeType::Box { size: PhysicsVector3::one() },
        };

        CollisionShape {
            shape_type,
            position: body.position,
            rotation: 0.0, // Would be calculated from quaternion
            scale: PhysicsVector3::one(),
            bounds,
        }
    }

    /// Broad phase collision detection
    fn broad_phase_detection(&self) -> Vec<(String, String)> {
        let mut pairs = Vec::new();
        
        if let Some(ref grid) = self.spatial_grid {
            // Spatial grid approach
            let mut processed = HashSet::new();
            
            for (cell_key, bodies) in &grid.cells {
                for i in 0..bodies.len() {
                    for j in (i + 1)..bodies.len() {
                        let body_ids: Vec<String> = bodies.iter().cloned().collect();
                        let body_a_id = &body_ids[i];
                        let body_b_id = &body_ids[j];
                        
                        let pair_key = if body_a_id < body_b_id {
                            format!("{},{}", body_a_id, body_b_id)
                        } else {
                            format!("{},{}", body_b_id, body_a_id)
                        };
                        
                        if !processed.contains(&pair_key) {
                            processed.insert(pair_key);
                            
                            // Layer check
                            let body_a = self.bodies.get(body_a_id)?;
                            let body_b = self.bodies.get(body_b_id)?;
                            
                            if self.layers_collide(body_a.collision_layer, body_b.collision_layer) {
                                pairs.push((body_a_id.clone(), body_b_id.clone()));
                            }
                        }
                    }
                }
            }
            
            self.stats.spatial_grid_hits = pairs.len() as u32;
        } else {
            // Brute force approach
            let body_ids: Vec<String> = self.bodies.keys().cloned().collect();
            
            for i in 0..body_ids.len() {
                for j in (i + 1)..body_ids.len() {
                    let body_a_id = &body_ids[i];
                    let body_b_id = &body_ids[j];
                    
                    let body_a = self.bodies.get(body_a_id)?;
                    let body_b = self.bodies.get(body_b_id)?;
                    
                    if self.layers_collide(body_a.collision_layer, body_b.collision_layer) {
                        // Quick AABB check
                        let shape_a = self.collision_shapes.get(body_a_id)?;
                        let shape_b = self.collision_shapes.get(body_b_id)?;
                        
                        if shape_a.bounds.intersects(&shape_b.bounds) {
                            pairs.push((body_a_id.clone(), body_b_id.clone()));
                        }
                    }
                }
            }
        }
        
        pairs
    }

    /// Narrow phase collision detection
    fn narrow_phase_detection(&self, pairs: &[(String, String)]) -> Vec<CollisionManifold> {
        let mut collisions = Vec::new();
        
        for (body_a_id, body_b_id) in pairs {
            if let Some(manifold) = self.check_collision(body_a_id, body_b_id) {
                collisions.push(manifold);
            }
        }
        
        collisions
    }

    /// Shape-based collision detection
    fn detect_shape_collision(
        &self,
        shape_a: &CollisionShape,
        shape_b: &CollisionShape,
        body_a_id: &str,
        body_b_id: &str,
    ) -> Option<CollisionManifold> {
        match (&shape_a.shape_type, &shape_b.shape_type) {
            (CollisionShapeType::Sphere { radius: radius_a }, 
             CollisionShapeType::Sphere { radius: radius_b }) => {
                self.sphere_sphere_collision(shape_a, shape_b, body_a_id, body_b_id, *radius_a, *radius_b)
            },
            (CollisionShapeType::Box { size: size_a }, 
             CollisionShapeType::Box { size: size_b }) => {
                self.box_box_collision(shape_a, shape_b, body_a_id, body_b_id, *size_a, *size_b)
            },
            (CollisionShapeType::Sphere { radius: radius_a }, 
             CollisionShapeType::Box { size: size_b }) => {
                self.sphere_box_collision(shape_a, shape_b, body_a_id, body_b_id, *radius_a, *size_b)
            },
            (CollisionShapeType::Box { size: size_a }, 
             CollisionShapeType::Sphere { radius: radius_b }) => {
                self.sphere_box_collision(shape_b, shape_a, body_b_id, body_a_id, *radius_b, *size_a)
            },
            _ => None,
        }
    }

    /// Sphere-sphere collision detection
    fn sphere_sphere_collision(
        &self,
        shape_a: &CollisionShape,
        shape_b: &CollisionShape,
        body_a_id: &str,
        body_b_id: &str,
        radius_a: f32,
        radius_b: f32,
    ) -> Option<CollisionManifold> {
        let delta = shape_b.position - shape_a.position;
        let distance = delta.length();
        let combined_radius = radius_a + radius_b;
        
        if distance < combined_radius && distance > 0.0 {
            let normal = delta / distance;
            let penetration = combined_radius - distance;
            
            let contact = Contact {
                position: shape_a.position + normal * (radius_a - penetration * 0.5),
                normal,
                penetration,
                impulse: 0.0,
                separation_velocity: 0.0,
            };
            
            Some(CollisionManifold {
                body_a_id: body_a_id.to_string(),
                body_b_id: body_b_id.to_string(),
                contacts: vec![contact],
                normal,
                penetration,
                restitution: 0.5,
                friction: 0.4,
            })
        } else {
            None
        }
    }

    /// Box-box collision detection using SAT
    fn box_box_collision(
        &self,
        shape_a: &CollisionShape,
        shape_b: &CollisionShape,
        body_a_id: &str,
        body_b_id: &str,
        size_a: PhysicsVector3,
        size_b: PhysicsVector3,
    ) -> Option<CollisionManifold> {
        // Simplified box-box collision using AABB
        if shape_a.bounds.intersects(&shape_b.bounds) {
            let delta = shape_b.position - shape_a.position;
            let half_size_a = size_a * 0.5;
            let half_size_b = size_b * 0.5;
            
            let overlap_x = half_size_a.x + half_size_b.x - delta.x.abs();
            let overlap_y = half_size_a.y + half_size_b.y - delta.y.abs();
            let overlap_z = half_size_a.z + half_size_b.z - delta.z.abs();
            
            if overlap_x > 0.0 && overlap_y > 0.0 && overlap_z > 0.0 {
                // Find minimum penetration axis
                let (penetration, normal) = if overlap_x < overlap_y && overlap_x < overlap_z {
                    (overlap_x, PhysicsVector3::new(if delta.x >= 0.0 { 1.0 } else { -1.0 }, 0.0, 0.0))
                } else if overlap_y < overlap_z {
                    (overlap_y, PhysicsVector3::new(0.0, if delta.y >= 0.0 { 1.0 } else { -1.0 }, 0.0))
                } else {
                    (overlap_z, PhysicsVector3::new(0.0, 0.0, if delta.z >= 0.0 { 1.0 } else { -1.0 }))
                };
                
                let contact = Contact {
                    position: shape_a.position + normal * (penetration * 0.5),
                    normal,
                    penetration,
                    impulse: 0.0,
                    separation_velocity: 0.0,
                };
                
                Some(CollisionManifold {
                    body_a_id: body_a_id.to_string(),
                    body_b_id: body_b_id.to_string(),
                    contacts: vec![contact],
                    normal,
                    penetration,
                    restitution: 0.3,
                    friction: 0.6,
                })
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Sphere-box collision detection
    fn sphere_box_collision(
        &self,
        sphere_shape: &CollisionShape,
        box_shape: &CollisionShape,
        sphere_body_id: &str,
        box_body_id: &str,
        sphere_radius: f32,
        box_size: PhysicsVector3,
    ) -> Option<CollisionManifold> {
        // Find closest point on box to sphere center
        let half_size = box_size * 0.5;
        let box_min = box_shape.position - half_size;
        let box_max = box_shape.position + half_size;
        
        let closest_point = PhysicsVector3::new(
            sphere_shape.position.x.clamp(box_min.x, box_max.x),
            sphere_shape.position.y.clamp(box_min.y, box_max.y),
            sphere_shape.position.z.clamp(box_min.z, box_max.z),
        );
        
        let delta = sphere_shape.position - closest_point;
        let distance = delta.length();
        
        if distance < sphere_radius && distance > 0.0 {
            let normal = delta / distance;
            let penetration = sphere_radius - distance;
            
            let contact = Contact {
                position: closest_point,
                normal,
                penetration,
                impulse: 0.0,
                separation_velocity: 0.0,
            };
            
            Some(CollisionManifold {
                body_a_id: sphere_body_id.to_string(),
                body_b_id: box_body_id.to_string(),
                contacts: vec![contact],
                normal,
                penetration,
                restitution: 0.4,
                friction: 0.5,
            })
        } else if distance == 0.0 {
            // Sphere center is inside the box
            let delta_to_center = sphere_shape.position - box_shape.position;
            
            // Find the face with minimum distance
            let distances = [
                (half_size.x - delta_to_center.x, PhysicsVector3::new(-1.0, 0.0, 0.0)),
                (half_size.x + delta_to_center.x, PhysicsVector3::new(1.0, 0.0, 0.0)),
                (half_size.y - delta_to_center.y, PhysicsVector3::new(0.0, -1.0, 0.0)),
                (half_size.y + delta_to_center.y, PhysicsVector3::new(0.0, 1.0, 0.0)),
                (half_size.z - delta_to_center.z, PhysicsVector3::new(0.0, 0.0, -1.0)),
                (half_size.z + delta_to_center.z, PhysicsVector3::new(0.0, 0.0, 1.0)),
            ];
            
            let (penetration, normal) = distances.into_iter()
                .min_by(|a, b| a.0.partial_cmp(&b.0).unwrap())
                .unwrap();
            
            let contact = Contact {
                position: sphere_shape.position - normal * sphere_radius,
                normal,
                penetration,
                impulse: 0.0,
                separation_velocity: 0.0,
            };
            
            Some(CollisionManifold {
                body_a_id: sphere_body_id.to_string(),
                body_b_id: box_body_id.to_string(),
                contacts: vec![contact],
                normal,
                penetration,
                restitution: 0.4,
                friction: 0.5,
            })
        } else {
            None
        }
    }

    /// Process collision results
    fn process_collisions(&mut self, collisions: Vec<CollisionManifold>) {
        for manifold in collisions {
            let contact = &manifold.contacts[0];
            
            // Create collision event
            let event = CollisionEvent {
                event_id: format!("{}-{}", manifold.body_a_id, manifold.body_b_id),
                body_a_id: manifold.body_a_id.clone(),
                body_b_id: manifold.body_b_id.clone(),
                collision_type: CollisionType::Dynamic,
                contact_point: contact.position,
                normal: contact.normal,
                penetration: contact.penetration,
                relative_velocity: PhysicsVector3::zero(), // Would be calculated from body velocities
                timestamp: std::time::Instant::now(),
            };
            
            self.collision_events.push(event);
            
            // Store active contact
            self.active_contacts.insert(
                format!("{}-{}", manifold.body_a_id, manifold.body_b_id),
                manifold,
            );
        }
        
        // Remove contacts that are no longer colliding
        let active_keys: HashSet<String> = collisions.into_iter()
            .map(|m| format!("{}-{}", m.body_a_id, m.body_b_id))
            .collect();
        
        let keys_to_remove: Vec<String> = self.active_contacts.keys()
            .filter(|key| !active_keys.contains(key))
            .cloned()
            .collect();
        
        for key in keys_to_remove {
            self.active_contacts.remove(&key);
        }
    }

    /// Check if two collision layers collide
    fn layers_collide(&self, layer_a: u32, layer_b: u32) -> bool {
        if layer_a == layer_b {
            return true; // Same layer always collides
        }
        
        if let Some(layer) = self.collision_layers.get(&layer_a) {
            layer.masks.contains(&layer_b)
        } else {
            false
        }
    }

    /// Create default collision layers
    fn default_layers() -> HashMap<u32, CollisionLayer> {
        let mut layers = HashMap::new();
        
        // Default layer
        layers.insert(1, CollisionLayer {
            name: "Default".to_string(),
            id: 1,
            masks: vec![1, 2, 3].into_iter().collect(),
            enabled: true,
        });
        
        // Environment layer
        layers.insert(2, CollisionLayer {
            name: "Environment".to_string(),
            id: 2,
            masks: vec![1, 3].into_iter().collect(),
            enabled: true,
        });
        
        // Player layer
        layers.insert(3, CollisionLayer {
            name: "Player".to_string(),
            id: 3,
            masks: vec![1, 2, 4].into_iter().collect(),
            enabled: true,
        });
        
        // Trigger layer
        layers.insert(4, CollisionLayer {
            name: "Trigger".to_string(),
            id: 4,
            masks: HashSet::new(),
            enabled: true,
        });
        
        layers
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::physics_utils::{PhysicsVector3, PhysicsBody, PhysicsShape, PhysicsObjectType, PhysicsMaterial};

    #[test]
    fn test_aabb_intersection() {
        let aabb1 = AABB::new(
            PhysicsVector3::new(0.0, 0.0, 0.0),
            PhysicsVector3::new(1.0, 1.0, 1.0),
        );
        
        let aabb2 = AABB::new(
            PhysicsVector3::new(0.5, 0.5, 0.5),
            PhysicsVector3::new(1.5, 1.5, 1.5),
        );
        
        assert!(aabb1.intersects(&aabb2));
        
        let aabb3 = AABB::new(
            PhysicsVector3::new(2.0, 2.0, 2.0),
            PhysicsVector3::new(3.0, 3.0, 3.0),
        );
        
        assert!(!aabb1.intersects(&aabb3));
    }

    #[test]
    fn test_sphere_sphere_collision() {
        let config = CollisionSystemConfig {
            enable_spatial_partitioning: false,
            broad_phase_algorithm: BroadPhaseAlgorithm::BruteForce,
            narrow_phase_algorithm: NarrowPhaseAlgorithm::GJK,
            max_contacts: 4,
            max_iterations: 10,
            enable_deactivation: false,
            deactivation_time: 0.0,
            enable_continuous_collision: false,
            time_of_impact_iterations: 5,
        };
        
        let mut system = CollisionSystem::new(config);
        
        // Create two spheres that should collide
        let body1 = PhysicsBody {
            id: "sphere1".to_string(),
            shape: PhysicsShape {
                object_type: PhysicsObjectType::Sphere,
                center: PhysicsVector3::new(0.0, 0.0, 0.0),
                rotation: crate::physics_utils::Quaternion::identity(),
                size: PhysicsVector3::one(),
                radius: 1.0,
                height: 2.0,
            },
            material: PhysicsMaterial {
                density: 1.0,
                restitution: 0.5,
                friction: 0.4,
                mass: 1.0,
                is_static: false,
            },
            position: PhysicsVector3::new(0.0, 0.0, 0.0),
            velocity: PhysicsVector3::zero(),
            acceleration: PhysicsVector3::zero(),
            rotation: crate::physics_utils::Quaternion::identity(),
            angular_velocity: PhysicsVector3::zero(),
            is_active: true,
            is_trigger: false,
            collision_layer: 1,
            collision_mask: 1,
        };
        
        let body2 = PhysicsBody {
            id: "sphere2".to_string(),
            shape: PhysicsShape {
                object_type: PhysicsObjectType::Sphere,
                center: PhysicsVector3::new(1.5, 0.0, 0.0),
                rotation: crate::physics_utils::Quaternion::identity(),
                size: PhysicsVector3::one(),
                radius: 1.0,
                height: 2.0,
            },
            material: PhysicsMaterial {
                density: 1.0,
                restitution: 0.5,
                friction: 0.4,
                mass: 1.0,
                is_static: false,
            },
            position: PhysicsVector3::new(1.5, 0.0, 0.0),
            velocity: PhysicsVector3::zero(),
            acceleration: PhysicsVector3::zero(),
            rotation: crate::physics_utils::Quaternion::identity(),
            angular_velocity: PhysicsVector3::zero(),
            is_active: true,
            is_trigger: false,
            collision_layer: 1,
            collision_mask: 1,
        };
        
        system.add_body(body1);
        system.add_body(body2);
        
        let collision = system.check_collision("sphere1", "sphere2");
        assert!(collision.is_some());
        
        let manifold = collision.unwrap();
        assert!(manifold.penetration > 0.0);
        assert_eq!(manifold.contacts.len(), 1);
    }

    #[test]
    fn test_collision_layers() {
        let config = CollisionSystemConfig {
            enable_spatial_partitioning: false,
            broad_phase_algorithm: BroadPhaseAlgorithm::BruteForce,
            narrow_phase_algorithm: NarrowPhaseAlgorithm::GJK,
            max_contacts: 4,
            max_iterations: 10,
            enable_deactivation: false,
            deactivation_time: 0.0,
            enable_continuous_collision: false,
            time_of_impact_iterations: 5,
        };
        
        let system = CollisionSystem::new(config);
        
        // Default layers should exist
        assert_eq!(system.collision_layers.len(), 4);
        assert!(system.collision_layers.contains_key(&1));
        assert!(system.collision_layers.contains_key(&2));
        assert!(system.collision_layers.contains_key(&3));
        assert!(system.collision_layers.contains_key(&4));
    }
}

