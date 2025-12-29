#include "physics-engine.h"
#include <iostream>
#include <algorithm>

namespace GameEngine {
    namespace Physics {
        
        // CircleCollider implementation
        bool CircleCollider::intersects(const Collider& other) const {
            switch (other.getType()) {
                case CollisionType::Circle: {
                    const CircleCollider& circle = static_cast<const CircleCollider&>(other);
                    float distanceSquared = (center - circle.center).magnitudeSquared();
                    float radiusSum = radius + circle.radius;
                    return distanceSquared <= radiusSum * radiusSum;
                }
                case CollisionType::Box: {
                    const BoxCollider& box = static_cast<const BoxCollider&>(other);
                    Vector2D closest = Utils::closestPoint(center, box);
                    float distanceSquared = (center - closest).magnitudeSquared();
                    return distanceSquared <= radius * radius;
                }
                case CollisionType::Point: {
                    const PointCollider& point = static_cast<const PointCollider&>(other);
                    float distanceSquared = (center - point.getPosition()).magnitudeSquared();
                    return distanceSquared <= radius * radius;
                }
                default:
                    return false;
            }
        }
        
        // BoxCollider implementation
        bool BoxCollider::intersects(const Collider& other) const {
            switch (other.getType()) {
                case CollisionType::Circle: {
                    const CircleCollider& circle = static_cast<const CircleCollider&>(other);
                    Vector2D closest = Utils::closestPoint(circle.getCenter(), *this);
                    float distanceSquared = (circle.getCenter() - closest).magnitudeSquared();
                    return distanceSquared <= circle.getRadius() * circle.getRadius();
                }
                case CollisionType::Box: {
                    const BoxCollider& box = static_cast<const BoxCollider&>(other);
                    Vector2D min1 = getMin();
                    Vector2D max1 = getMax();
                    Vector2D min2 = box.getMin();
                    Vector2D max2 = box.getMax();
                    
                    return !(max1.x < min2.x || min1.x > max2.x ||
                             max1.y < min2.y || min1.y > max2.y);
                }
                case CollisionType::Point: {
                    const PointCollider& point = static_cast<const PointCollider&>(other);
                    Vector2D pos = point.getPosition();
                    Vector2D min = getMin();
                    Vector2D max = getMax();
                    return pos.x >= min.x && pos.x <= max.x &&
                           pos.y >= min.y && pos.y <= max.y;
                }
                default:
                    return false;
            }
        }
        
        // PointCollider implementation
        bool PointCollider::intersects(const Collider& other) const {
            switch (other.getType()) {
                case CollisionType::Circle: {
                    const CircleCollider& circle = static_cast<const CircleCollider&>(other);
                    float distanceSquared = (position - circle.getCenter()).magnitudeSquared();
                    return distanceSquared <= circle.getRadius() * circle.getRadius();
                }
                case CollisionType::Box: {
                    const BoxCollider& box = static_cast<const BoxCollider&>(other);
                    Vector2D min = box.getMin();
                    Vector2D max = box.getMax();
                    return position.x >= min.x && position.x <= max.x &&
                           position.y >= min.y && position.y <= max.y;
                }
                case CollisionType::Point: {
                    const PointCollider& point = static_cast<const PointCollider&>(other);
                    return position == point.getPosition();
                }
                default:
                    return false;
            }
        }
        
        // RigidBody implementation
        void RigidBody::update(float deltaTime) {
            if (isStatic) return;
            
            // Apply gravity
            if (useGravity) {
                acceleration += PhysicsWorld::Vector2D(0.0f, GRAVITY);
            }
            
            // Update velocity
            velocity += acceleration * deltaTime;
            
            // Apply air resistance
            if (AIR_RESISTANCE > 0.0f) {
                velocity = velocity * (1.0f - AIR_RESISTANCE * deltaTime);
            }
            
            // Update position
            position += velocity * deltaTime;
            
            // Update angle (simple rotation)
            angle += angularVelocity * deltaTime;
            
            // Reset acceleration
            acceleration = Vector2D::zero();
        }
        
        void RigidBody::applyForce(const Vector2D& force) {
            if (isStatic || invMass == 0.0f) return;
            acceleration += force * invMass;
        }
        
        void RigidBody::applyImpulse(const Vector2D& impulse) {
            if (isStatic || invMass == 0.0f) return;
            velocity += impulse * invMass;
        }
        
        void RigidBody::applyTorque(float torque) {
            if (isStatic) return;
            angularVelocity += torque / mass;
        }
        
        void RigidBody::onCollision(const RigidBody& other) {
            // Handle collision response
            if (collider && other.collider) {
                if (collider->intersects(*other.collider)) {
                    // Calculate collision response
                    Vector2D normal = other.position - position;
                    float distance = normal.magnitude();
                    
                    if (distance > 0.0f) {
                        normal = normal / distance;
                        resolveCollision(const_cast<RigidBody&>(other), normal, 0.0f);
                    }
                }
            }
        }
        
        void RigidBody::resolveCollision(RigidBody& other, const Vector2D& normal, float penetration) {
            // Calculate relative velocity
            Vector2D relativeVelocity = other.velocity - velocity;
            float velocityAlongNormal = relativeVelocity.dot(normal);
            
            // Do not resolve if velocities are separating
            if (velocityAlongNormal > 0.0f) return;
            
            // Calculate restitution (bounciness)
            float e = RESTITUTION;
            
            // Calculate impulse scalar
            float j = -(1.0f + e) * velocityAlongNormal;
            j /= invMass + other.invMass;
            
            // Apply impulse
            Vector2D impulse = normal * j;
            velocity -= impulse * invMass;
            other.velocity += impulse * other.invMass;
            
            // Positional correction to prevent sinking
            if (penetration > 0.0f) {
                Vector2D correction = normal * (penetration / (invMass + other.invMass)) * 0.8f;
                position -= correction * invMass;
                other.position += correction * other.invMass;
            }
        }
        
        // PhysicsWorld implementation
        void PhysicsWorld::update(float deltaTime) {
            // Update all rigid bodies
            for (auto& body : rigidBodies) {
                if (body) {
                    body->update(deltaTime);
                }
            }
            
            // Check collisions
            checkCollisions();
            
            // Apply world bounds
            if (boundsMax.x > boundsMin.x && boundsMax.y > boundsMin.y) {
                for (auto& body : rigidBodies) {
                    if (!body || body->isStaticBody()) continue;
                    
                    Vector2D pos = body->getPosition();
                    Vector2D vel = body->getVelocity();
                    
                    // Check X bounds
                    if (pos.x < boundsMin.x) {
                        pos.x = boundsMin.x;
                        vel.x = -vel.x * RESTITUTION;
                    } else if (pos.x > boundsMax.x) {
                        pos.x = boundsMax.x;
                        vel.x = -vel.x * RESTITUTION;
                    }
                    
                    // Check Y bounds
                    if (pos.y < boundsMin.y) {
                        pos.y = boundsMin.y;
                        vel.y = -vel.y * RESTITUTION;
                    } else if (pos.y > boundsMax.y) {
                        pos.y = boundsMax.y;
                        vel.y = -vel.y * RESTITUTION;
                    }
                    
                    body->setPosition(pos);
                    body->setVelocity(vel);
                }
            }
        }
        
        void PhysicsWorld::render() {
            // Console-based rendering for demonstration
            std::cout << "Physics World State:" << std::endl;
            std::cout << "Rigid Bodies: " << rigidBodies.size() << std::endl;
            std::cout << "Gravity: (" << gravity.x << ", " << gravity.y << ")" << std::endl;
            
            for (size_t i = 0; i < rigidBodies.size(); ++i) {
                if (rigidBodies[i]) {
                    auto& body = rigidBodies[i];
                    Vector2D pos = body->getPosition();
                    Vector2D vel = body->getVelocity();
                    
                    std::cout << "Body " << i << ": "
                             << "Pos(" << pos.x << ", " << pos.y << ") "
                             << "Vel(" << vel.x << ", " << vel.y << ") "
                             << "Mass(" << body->getMass() << ")" << std::endl;
                }
            }
        }
        
        RigidBody* PhysicsWorld::addRigidBody(std::unique_ptr<RigidBody> body) {
            rigidBodies.push_back(std::move(body));
            return rigidBodies.back().get();
        }
        
        void PhysicsWorld::removeRigidBody(RigidBody* body) {
            auto it = std::find_if(rigidBodies.begin(), rigidBodies.end(),
                [body](const std::unique_ptr<RigidBody>& b) {
                    return b.get() == body;
                });
            
            if (it != rigidBodies.end()) {
                rigidBodies.erase(it);
            }
        }
        
        void PhysicsWorld::clear() {
            rigidBodies.clear();
        }
        
        void PhysicsWorld::checkCollisions() {
            // Simple pairwise collision detection
            for (size_t i = 0; i < rigidBodies.size(); ++i) {
                if (!rigidBodies[i]) continue;
                
                for (size_t j = i + 1; j < rigidBodies.size(); ++j) {
                    if (!rigidBodies[j]) continue;
                    
                    auto& bodyA = rigidBodies[i];
                    auto& bodyB = rigidBodies[j];
                    
                    if (bodyA->getCollider() && bodyB->getCollider()) {
                        if (bodyA->getCollider()->intersects(*bodyB->getCollider())) {
                            // Calculate collision response
                            Vector2D normal = bodyB->getPosition() - bodyA->getPosition();
                            float distance = normal.magnitude();
                            
                            if (distance > 0.0f) {
                                normal = normal / distance;
                                float penetration = 0.0f;
                                
                                // Simple penetration calculation
                                if (bodyA->getCollider()->getType() == CollisionType::Circle &&
                                    bodyB->getCollider()->getType() == CollisionType::Circle) {
                                    auto circleA = static_cast<CircleCollider*>(bodyA->getCollider());
                                    auto circleB = static_cast<CircleCollider*>(bodyB->getCollider());
                                    penetration = circleA->getRadius() + circleB->getRadius() - distance;
                                }
                                
                                bodyA->resolveCollision(*bodyB, normal, penetration);
                            }
                        }
                    }
                }
            }
        }
        
        std::vector<RigidBody*> PhysicsWorld::queryPoint(const Vector2D& point) {
            std::vector<RigidBody*> results;
            PointCollider pointCollider(point);
            
            for (auto& body : rigidBodies) {
                if (body && body->getCollider() && 
                    body->getCollider()->intersects(pointCollider)) {
                    results.push_back(body.get());
                }
            }
            
            return results;
        }
        
        std::vector<RigidBody*> PhysicsWorld::queryAABB(const Vector2D& min, const Vector2D& max) {
            std::vector<RigidBody*> results;
            BoxCollider boxCollider((min + max) * 0.5f, (max - min) * 0.5f);
            
            for (auto& body : rigidBodies) {
                if (body && body->getCollider() && 
                    body->getCollider()->intersects(boxCollider)) {
                    results.push_back(body.get());
                }
            }
            
            return results;
        }
        
        std::vector<RigidBody*> PhysicsWorld::queryCircle(const Vector2D& center, float radius) {
            std::vector<RigidBody*> results;
            CircleCollider circleCollider(center, radius);
            
            for (auto& body : rigidBodies) {
                if (body && body->getCollider() && 
                    body->getCollider()->intersects(circleCollider)) {
                    results.push_back(body.get());
                }
            }
            
            return results;
        }
        
        // Utility implementations
        namespace Utils {
            Vector2D closestPoint(const Vector2D& point, const BoxCollider& box) {
                Vector2D min = box.getMin();
                Vector2D max = box.getMax();
                
                return Vector2D(
                    std::max(min.x, std::min(point.x, max.x)),
                    std::min(min.y, std::min(point.y, max.y))
                );
            }
            
            Vector2D closestPoint(const Vector2D& point, const CircleCollider& circle) {
                Vector2D toPoint = point - circle.getCenter();
                float distance = toPoint.magnitude();
                
                if (distance < circle.getRadius()) {
                    return point; // Point is inside the circle
                } else if (distance > 0.0f) {
                    return circle.getCenter() + toPoint.normalized() * circle.getRadius();
                } else {
                    return circle.getCenter(); // Point is exactly at center
                }
            }
            
            RaycastHit raycast(const Vector2D& origin, const Vector2D& direction, float maxDistance) {
                RaycastHit hit;
                
                // Simplified raycast implementation
                // In a real implementation, this would check against all colliders
                
                Vector2D normalizedDir = direction.normalized();
                Vector2D endPoint = origin + normalizedDir * maxDistance;
                
                // This is a simplified implementation that always returns a hit
                hit.hit = true;
                hit.point = endPoint;
                hit.normal = normalizedDir;
                hit.distance = maxDistance;
                
                return hit;
            }
        }
    }
}
