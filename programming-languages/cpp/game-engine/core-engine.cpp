#include "core-engine.h"
#include <algorithm>
#include <chrono>

namespace GameEngine {
    // CoreEngine implementation
    CoreEngine::CoreEngine() : isRunning(false), deltaTime(0.0f) {
        std::cout << "Game Engine Initialized" << std::endl;
    }
    
    CoreEngine::~CoreEngine() {
        std::cout << "Game Engine Shutting Down" << std::endl;
    }
    
    void CoreEngine::initialize() {
        std::cout << "Initializing Game Engine..." << std::endl;
        isRunning = true;
        
        // Initialize all game objects
        for (auto& gameObject : gameObjects) {
            if (gameObject) {
                gameObject->initialize();
            }
        }
    }
    
    void CoreEngine::update(float deltaTime) {
        this->deltaTime = deltaTime;
        
        // Update all active game objects
        for (auto& gameObject : gameObjects) {
            if (gameObject && gameObject->getIsActive()) {
                gameObject->update(deltaTime);
            }
        }
    }
    
    void CoreEngine::render() {
        // Clear screen (placeholder for graphics API)
        std::cout << "\033[2J\033[H"; // ANSI clear screen
        
        // Render all active game objects
        for (auto& gameObject : gameObjects) {
            if (gameObject && gameObject->getIsActive()) {
                gameObject->render();
            }
        }
    }
    
    void CoreEngine::shutdown() {
        std::cout << "Shutting down Game Engine..." << std::endl;
        
        // Cleanup all game objects
        for (auto& gameObject : gameObjects) {
            if (gameObject) {
                gameObject->cleanup();
            }
        }
        
        gameObjects.clear();
        isRunning = false;
    }
    
    void CoreEngine::run() {
        auto lastTime = std::chrono::high_resolution_clock::now();
        
        while (isRunning) {
            auto currentTime = std::chrono::high_resolution_clock::now();
            float deltaTime = std::chrono::duration<float>(currentTime - lastTime).count();
            lastTime = currentTime;
            
            update(deltaTime);
            render();
            
            // Simple frame rate limiting (60 FPS)
            std::this_thread::sleep_for(std::chrono::milliseconds(16));
        }
    }
    
    void CoreEngine::addGameObject(std::shared_ptr<GameObject> gameObject) {
        gameObjects.push_back(gameObject);
    }
    
    void CoreEngine::removeGameObject(std::shared_ptr<GameObject> gameObject) {
        gameObjects.erase(
            std::remove(gameObjects.begin(), gameObjects.end(), gameObject),
            gameObjects.end()
        );
    }
    
    std::shared_ptr<GameObject> CoreEngine::findGameObject(const std::string& name) {
        for (auto& gameObject : gameObjects) {
            if (gameObject && gameObject->getName() == name) {
                return gameObject;
            }
        }
        return nullptr;
    }
    
    // GameObject implementation
    GameObject::GameObject(const std::string& name) 
        : name(name), isActive(true) {
        position[0] = 0.0f;
        position[1] = 0.0f;
    }
    
    GameObject::~GameObject() {
        components.clear();
    }
    
    void GameObject::initialize() {
        for (auto& component : components) {
            if (component) {
                component->initialize();
            }
        }
    }
    
    void GameObject::update(float deltaTime) {
        for (auto& component : components) {
            if (component) {
                component->update(deltaTime);
            }
        }
    }
    
    void GameObject::render() {
        std::cout << "Rendering GameObject: " << name 
                  << " at position (" << position[0] << ", " << position[1] << ")" << std::endl;
    }
    
    void GameObject::cleanup() {
        for (auto& component : components) {
            if (component) {
                // Components are automatically cleaned up when shared_ptr goes out of scope
            }
        }
        components.clear();
    }
    
    void GameObject::addComponent(std::shared_ptr<Component> component) {
        components.push_back(component);
    }
    
    void GameObject::removeComponent(std::shared_ptr<Component> component) {
        components.erase(
            std::remove(components.begin(), components.end(), component),
            components.end()
        );
    }
    
    // TransformComponent implementation
    TransformComponent::TransformComponent() {
        position[0] = 0.0f;
        position[1] = 0.0f;
        rotation = 0.0f;
        scale[0] = 1.0f;
        scale[1] = 1.0f;
    }
    
    void TransformComponent::initialize() {
        std::cout << "Transform Component initialized" << std::endl;
    }
    
    void TransformComponent::update(float deltaTime) {
        // Handle any transform updates here
    }
    
    void TransformComponent::render() {
        // Transform doesn't render directly
    }
    
    void TransformComponent::setPosition(float x, float y) {
        position[0] = x;
        position[1] = y;
    }
    
    void TransformComponent::setRotation(float degrees) {
        rotation = degrees;
    }
    
    void TransformComponent::setScale(float x, float y) {
        scale[0] = x;
        scale[1] = y;
    }
    
    void TransformComponent::getPosition(float& x, float& y) const {
        x = position[0];
        y = position[1];
    }
    
    float TransformComponent::getRotation() const {
        return rotation;
    }
    
    void TransformComponent::getScale(float& x, float& y) const {
        x = scale[0];
        y = scale[1];
    }
}
