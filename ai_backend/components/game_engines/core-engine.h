#pragma once

#include <iostream>
#include <vector>
#include <memory>
#include <string>

namespace GameEngine {
    // Forward declarations
    class GameObject;
    class Component;
    
    /**
     * Core Game Engine class that manages the entire game lifecycle
     */
    class CoreEngine {
    private:
        bool isRunning;
        float deltaTime;
        std::vector<std::shared_ptr<GameObject>> gameObjects;
        
    public:
        CoreEngine();
        ~CoreEngine();
        
        // Core game loop methods
        void initialize();
        void update(float deltaTime);
        void render();
        void shutdown();
        void run();
        
        // Game object management
        void addGameObject(std::shared_ptr<GameObject> gameObject);
        void removeGameObject(std::shared_ptr<GameObject> gameObject);
        std::shared_ptr<GameObject> findGameObject(const std::string& name);
        
        // Getters and setters
        bool getIsRunning() const { return isRunning; }
        float getDeltaTime() const { return deltaTime; }
        void setIsRunning(bool running) { isRunning = running; }
    };
    
    /**
     * Base GameObject class that represents any entity in the game
     */
    class GameObject {
    private:
        std::string name;
        bool isActive;
        std::vector<std::shared_ptr<Component>> components;
        float position[2]; // x, y coordinates
        
    public:
        GameObject(const std::string& name);
        virtual ~GameObject();
        
        // Lifecycle methods
        virtual void initialize();
        virtual void update(float deltaTime);
        virtual void render();
        virtual void cleanup();
        
        // Component management
        void addComponent(std::shared_ptr<Component> component);
        void removeComponent(std::shared_ptr<Component> component);
        
        // Getters and setters
        std::string getName() const { return name; }
        bool getIsActive() const { return isActive; }
        void setIsActive(bool active) { isActive = active; }
        void setPosition(float x, float y) { position[0] = x; position[1] = y; }
        void getPosition(float& x, float& y) const { x = position[0]; y = position[1]; }
    };
    
    /**
     * Base Component class for game object functionality
     */
    class Component {
    public:
        virtual ~Component() = default;
        virtual void initialize() = 0;
        virtual void update(float deltaTime) = 0;
        virtual void render() = 0;
    };
    
    /**
     * Transform component for handling position, rotation, and scale
     */
    class TransformComponent : public Component {
    private:
        float position[2];
        float rotation;
        float scale[2];
        
    public:
        TransformComponent();
        void initialize() override;
        void update(float deltaTime) override;
        void render() override;
        
        void setPosition(float x, float y);
        void setRotation(float degrees);
        void setScale(float x, float y);
        
        void getPosition(float& x, float& y) const;
        float getRotation() const;
        void getScale(float& x, float& y) const;
    };
}
