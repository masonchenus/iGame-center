#pragma once

#include <vector>
#include <memory>
#include <cmath>
#include <algorithm>

namespace GameEngine {
    namespace Physics {
        // Physics constants
        const float GRAVITY = 9.81f;
        const float AIR_RESISTANCE = 0.1f;
        const float FRICTION = 0.8f;
        const float RESTITUTION = 0.7f;
        
        // Vector2D structure for 2D physics
        struct Vector2D {
            float x, y;
            
            Vector2D() : x(0), y(0) {}
            Vector2D(float x, float y) : x(x), y(y) {}
            
            // Vector operations
            Vector2D operator+(const Vector2D& other) const {
                return Vector2D(x + other.x, y + other.y);
            }
            
            Vector2D operator-(const Vector2D& other) const {
                return Vector2D(x - other.x, y - other.y);
            }
            
            Vector2D operator*(float scalar) const {
                return Vector2D(x * scalar, y * scalar);
            }
            
            Vector2D operator/(float scalar) const {
                return Vector2D(x / scalar, y / scalar);
            }
            
            Vector2D& operator+=(const Vector2D& other) {
                x += other.x;
                y += other.y;
                return *this;
            }
            
            Vector2D& operator-=(const Vector2D& other) {
                x -= other.x;
                y -= other.y;
                return *this;
            }
            
            float magnitude() const {
                return std::sqrt(x * x + y * y);
            }
            
            float magnitudeSquared() const {
                return x * x + y * y;
            }
            
            Vector2D normalized() const {
                float mag = magnitude();
                return mag > 0 ? Vector2D(x / mag, y / mag) : Vector2D(0, 0);
            }
            
            float dot(const Vector2D& other) const {
                return x * other.x + y * other.y;
            }
            
            float cross(const Vector2D& other) const {
                return x * other.y - y * other.x;
            }
            
            static Vector2D zero() { return Vector2D(0, 0); }
            static Vector2D up() { return Vector2D(0, -1); }
            static Vector2D down() { return Vector2D(0, 1); }
            static Vector2D left() { return Vector2D(-1, 0); }
            static Vector2D right() { return Vector2D(1, 0); }
        };
        
        // Collision types
        enum class CollisionType {
            None,
            Circle,
            Box,
            Point
        };
        
        // Base collider class
        class Collider {
        public:
            virtual ~Collider() = default;
            virtual CollisionType getType() const = 0;
            virtual bool intersects(const Collider& other) const = 0;
            virtual Vector2D getCenter() const = 0;
        };
        
        // Circle collider
        class CircleCollider : public Collider {
        private:
            Vector2D center;
            float radius;
            
        public:
            CircleCollider(const Vector2D& center, float radius)
                : center(center), radius(radius) {}
            
            CollisionType getType() const override {
                return CollisionType::Circle;
            }
            
            bool intersects(const Collider& other) const override;
            Vector2D getCenter() const override {
                return center;
            }
            
            float getRadius() const { return radius; }
            void setCenter(const Vector2D& newCenter) { center = newCenter; }
            void setRadius(float newRadius) { radius = newRadius; }
        };
        
        // Box collider (AABB - Axis-Aligned Bounding Box)
        class BoxCollider : public Collider {
        private:
            Vector2D center;
            Vector2D size; // half-size (width/2, height/2)
            
        public:
            BoxCollider(const Vector2D& center, const Vector2D& size)
                : center(center), size(size) {}
            
            CollisionType getType() const override {
                return CollisionType::Box;
            }
            
            bool intersects(const Collider& other) const override;
            Vector2D getCenter() const override {
                return center;
            }
            
            Vector2D getSize() const { return size; }
            Vector2D getMin() const {
                return center - size;
            }
            Vector2D getMax() const {
                return center + size;
            }
            void setCenter(const Vector2D& newCenter) { center = newCenter; }
            void setSize(const Vector2D& newSize) { size = newSize; }
        };
        
        // Point collider
        class PointCollider : public Collider {
        private:
            Vector2D position;
            
        public:
            PointCollider(const Vector2D& position) : position(position) {}
            
            CollisionType getType() const override {
                return CollisionType::Point;
            }
            
            bool intersects(const Collider& other) const override;
            Vector2D getCenter() const override {
                return position;
            }
            
            Vector2D getPosition() const { return position; }
            void setPosition(const Vector2D& newPosition) { position = newPosition; }
        };
        
        // Rigid body for physics simulation
        class RigidBody {
        private:
            Vector2D position;
            Vector2D velocity;
            Vector2D acceleration;
            float mass;
            float invMass;
            float angularVelocity;
            float angle;
            std::unique_ptr<Collider> collider;
            bool useGravity;
            bool isStatic;
            
        public:
            RigidBody(float mass = 1.0f)
                : mass(mass), invMass(mass > 0 ? 1.0f / mass : 0.0f),
                  angularVelocity(0.0f), angle(0.0f),
                  useGravity(true), isStatic(false) {}
            
            ~RigidBody() = default;
            
            // Physics simulation
            void update(float deltaTime);
            void applyForce(const Vector2D& force);
            void applyImpulse(const Vector2D& impulse);
            void applyTorque(float torque);
            
            // Collision response
            void onCollision(const RigidBody& other);
            void resolveCollision(RigidBody& other, const Vector2D& normal, float penetration);
            
            // Getters and setters
            Vector2D getPosition() const { return position; }
            Vector2D getVelocity() const { return velocity; }
            Vector2D getAcceleration() const { return acceleration; }
            float getMass() const { return mass; }
            float getInvMass() const { return invMass; }
            float getAngle() const { return angle; }
            float getAngularVelocity() const { return angularVelocity; }
            
            void setPosition(const Vector2D& pos) { position = pos; }
            void setVelocity(const Vector2D& vel) { velocity = vel; }
            void setMass(float newMass) { 
                mass = newMass;
                invMass = newMass > 0 ? 1.0f / newMass : 0.0f;
            }
            void setCollider(std::unique_ptr<Collider> newCollider) { 
                collider = std::move(newCollider);
            }
            void setUseGravity(bool use) { useGravity = use; }
            void setStatic(bool stat) { 
                isStatic = stat;
                invMass = stat ? 0.0f : (mass > 0 ? 1.0f / mass : 0.0f);
            }
            
            Collider* getCollider() const { return collider.get(); }
            bool isStaticBody() const { return isStatic; }
        };
        
        // Physics world to manage all rigid bodies
        class PhysicsWorld {
        private:
            std::vector<std::unique_ptr<RigidBody>> rigidBodies;
            Vector2D gravity;
            Vector2D boundsMin, boundsMax;
            
        public:
            PhysicsWorld()
                : gravity(0.0f, GRAVITY) {}
            
            ~PhysicsWorld() = default;
            
            void update(float deltaTime);
            void render();
            
            // Body management
            RigidBody* addRigidBody(std::unique_ptr<RigidBody> body);
            void removeRigidBody(RigidBody* body);
            void clear();
            
            // Collision detection and response
            void checkCollisions();
            
            // Utility methods
            std::vector<RigidBody*> queryPoint(const Vector2D& point);
            std::vector<RigidBody*> queryAABB(const Vector2D& min, const Vector2D& max);
            std::vector<RigidBody*> queryCircle(const Vector2D& center, float radius);
            
            // Getters and setters
            Vector2D getGravity() const { return gravity; }
            void setGravity(const Vector2D& newGravity) { gravity = newGravity; }
            Vector2D getBoundsMin() const { return boundsMin; }
            Vector2D getBoundsMax() const { return boundsMax; }
            void setBounds(const Vector2D& min, const Vector2D& max) {
                boundsMin = min;
                boundsMax = max;
            }
            
            const std::vector<std::unique_ptr<RigidBody>>& getRigidBodies() const {
                return rigidBodies;
            }
        };
        
        // Utility functions
        namespace Utils {
            Vector2D closestPoint(const Vector2D& point, const BoxCollider& box);
            Vector2D closestPoint(const Vector2D& point, const CircleCollider& circle);
            
            struct RaycastHit {
                RigidBody* body = nullptr;
                Vector2D point;
                Vector2D normal;
                float distance = 0.0f;
                bool hit = false;
            };
            
            RaycastHit raycast(const Vector2D& origin, const Vector2D& direction, float maxDistance = 1000.0f);
        }
    }
}

