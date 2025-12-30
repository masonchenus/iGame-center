#pragma once

#include <string>
#include <vector>
#include <memory>
#include <unordered_map>

namespace GameEngine {
    namespace Graphics {
        
        // Color structure for RGBA colors
        struct Color {
            float r, g, b, a;
            
            Color() : r(1.0f), g(1.0f), b(1.0f), a(1.0f) {}
            Color(float r, float g, float b, float a = 1.0f) 
                : r(r), g(g), b(b), a(a) {}
            
            // Preset colors
            static Color white() { return Color(1.0f, 1.0f, 1.0f); }
            static Color black() { return Color(0.0f, 0.0f, 0.0f); }
            static Color red() { return Color(1.0f, 0.0f, 0.0f); }
            static Color green() { return Color(0.0f, 1.0f, 0.0f); }
            static Color blue() { return Color(0.0f, 0.0f, 1.0f); }
            static Color yellow() { return Color(1.0f, 1.0f, 0.0f); }
            static Color magenta() { return Color(1.0f, 0.0f, 1.0f); }
            static Color cyan() { return Color(0.0f, 1.0f, 1.0f); }
            static Color transparent() { return Color(0.0f, 0.0f, 0.0f, 0.0f); }
        };
        
        // 2D Vector for graphics operations
        struct Vector2D {
            float x, y;
            
            Vector2D() : x(0), y(0) {}
            Vector2D(float x, float y) : x(x), y(y) {}
            
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
            
            float magnitude() const {
                return std::sqrt(x * x + y * y);
            }
            
            Vector2D normalized() const {
                float mag = magnitude();
                return mag > 0 ? Vector2D(x / mag, y / mag) : Vector2D(0, 0);
            }
            
            static Vector2D zero() { return Vector2D(0, 0); }
            static Vector2D one() { return Vector2D(1, 1); }
        };
        
        // 2D Rectangle for rendering
        struct Rectangle {
            float x, y, width, height;
            
            Rectangle() : x(0), y(0), width(0), height(0) {}
            Rectangle(float x, float y, float width, float height)
                : x(x), y(y), width(width), height(height) {}
            
            bool contains(float x, float y) const {
                return x >= this->x && x <= this->x + width &&
                       y >= this->y && y <= this->y + height;
            }
            
            bool intersects(const Rectangle& other) const {
                return !(x + width < other.x || other.x + other.width < x ||
                         y + height < other.y || other.y + other.height < y);
            }
        };
        
        // Texture for 2D images
        class Texture {
        private:
            unsigned int id;
            int width, height;
            std::string filePath;
            
        public:
            Texture();
            ~Texture();
            
            bool loadFromFile(const std::string& filePath);
            bool loadFromMemory(const void* data, int width, int height);
            void generateFromPixels(const unsigned char* pixels, int width, int height);
            
            void bind() const;
            void unbind() const;
            
            int getWidth() const { return width; }
            int getHeight() const { return height; }
            unsigned int getID() const { return id; }
            std::string getFilePath() const { return filePath; }
        };
        
        // Sprite for rendering textured rectangles
        class Sprite {
        private:
            std::unique_ptr<Texture> texture;
            Vector2D position;
            Vector2D scale;
            float rotation;
            Rectangle sourceRect;
            Color tint;
            bool flipX, flipY;
            
        public:
            Sprite();
            Sprite(const std::string& texturePath);
            Sprite(std::unique_ptr<Texture> texture);
            ~Sprite();
            
            void setTexture(std::unique_ptr<Texture> texture);
            void setPosition(const Vector2D& pos);
            void setScale(const Vector2D& scale);
            void setRotation(float rotation);
            void setSourceRect(const Rectangle& rect);
            void setTint(const Color& color);
            void setFlip(bool flipX, bool flipY);
            
            Vector2D getPosition() const { return position; }
            Vector2D getScale() const { return scale; }
            float getRotation() const { return rotation; }
            Rectangle getSourceRect() const { return sourceRect; }
            Color getTint() const { return tint; }
            Texture* getTexture() const { return texture.get(); }
            
            void render() const;
        };
        
        // Font for text rendering
        class Font {
        private:
            void* fontData; // Platform-specific font data
            int size;
            std::string fontPath;
            
        public:
            Font();
            ~Font();
            
            bool loadFromFile(const std::string& filePath, int size);
            void setSize(int size);
            
            int getSize() const { return size; }
            std::string getFontPath() const { return fontPath; }
        };
        
        // Text renderer
        class Text {
        private:
            std::string text;
            Font* font;
            Vector2D position;
            Color color;
            int size;
            bool bold, italic, underline;
            
        public:
            Text();
            Text(const std::string& text, Font* font = nullptr);
            ~Text();
            
            void setText(const std::string& text);
            void setFont(Font* font);
            void setPosition(const Vector2D& position);
            void setColor(const Color& color);
            void setSize(int size);
            void setBold(bool bold);
            void setItalic(bool italic);
            void setUnderline(bool underline);
            
            std::string getText() const { return text; }
            Font* getFont() const { return font; }
            Vector2D getPosition() const { return position; }
            Color getColor() const { return color; }
            int getSize() const { return size; }
            
            void render() const;
        };
        
        // Camera for 2D viewport
        class Camera2D {
        private:
            Vector2D position;
            float zoom;
            Vector2D viewportSize;
            
        public:
            Camera2D();
            ~Camera2D();
            
            void setPosition(const Vector2D& position);
            void setZoom(float zoom);
            void setViewportSize(const Vector2D& size);
            void move(const Vector2D& offset);
            void zoomIn(float factor);
            void zoomOut(float factor);
            
            Vector2D getPosition() const { return position; }
            float getZoom() const { return zoom; }
            Vector2D getViewportSize() const { return viewportSize; }
            
            // Convert between world and screen coordinates
            Vector2D worldToScreen(const Vector2D& worldPos) const;
            Vector2D screenToWorld(const Vector2D& screenPos) const;
        };
        
        // 2D Renderer for drawing primitives and sprites
        class Renderer2D {
        private:
            Camera2D* camera;
            
        public:
            Renderer2D();
            ~Renderer2D();
            
            void begin();
            void end();
            void setCamera(Camera2D* camera);
            
            // Primitive drawing
            void drawPoint(const Vector2D& position, const Color& color);
            void drawLine(const Vector2D& start, const Vector2D& end, const Color& color, float thickness = 1.0f);
            void drawRectangle(const Rectangle& rect, const Color& color, bool filled = false);
            void drawCircle(const Vector2D& center, float radius, const Color& color, bool filled = false);
            void drawPolygon(const std::vector<Vector2D>& points, const Color& color, bool filled = false);
            
            // Sprite rendering
            void drawSprite(const Sprite& sprite);
            void drawSpriteTiled(const Sprite& sprite, const Rectangle& destination, const Vector2D& tileSize);
            void drawSprite9Slice(const Sprite& sprite, const Rectangle& destination);
            
            // Text rendering
            void drawText(const Text& text);
            void drawTextBox(const Text& text, const Rectangle& bounds);
            
            // UI elements
            void drawButton(const Rectangle& rect, const std::string& text, bool pressed = false);
            void drawProgressBar(const Rectangle& rect, float progress);
            void drawSlider(const Rectangle& rect, float value);
            
            // Effects
            void drawGlow(const Vector2D& position, float radius, const Color& color);
            void drawShadow(const Rectangle& rect, float offset, float blur);
            
            Camera2D* getCamera() const { return camera; }
        };
        
        // Animation system
        class Animation {
        private:
            std::vector<std::unique_ptr<Texture>> frames;
            float frameDuration;
            float currentFrameTime;
            int currentFrame;
            bool loop;
            bool playing;
            
        public:
            Animation();
            ~Animation();
            
            void addFrame(std::unique_ptr<Texture> frame);
            void setFrameDuration(float duration);
            void setLoop(bool loop);
            
            void play();
            void pause();
            void stop();
            void reset();
            
            void update(float deltaTime);
            
            Texture* getCurrentFrame() const;
            int getCurrentFrameIndex() const { return currentFrame; }
            int getFrameCount() const { return static_cast<int>(frames.size()); }
            bool isPlaying() const { return playing; }
            bool isFinished() const { return !loop && currentFrame >= frames.size() - 1; }
        };
        
        // Particle system for effects
        class ParticleSystem {
        private:
            struct Particle {
                Vector2D position;
                Vector2D velocity;
                Vector2D acceleration;
                float lifetime;
                float maxLifetime;
                float size;
                float rotation;
                float rotationSpeed;
                Color color;
                Color startColor;
                Color endColor;
                bool active;
            };
            
            std::vector<Particle> particles;
            std::vector<Vector2D> emissionPoints;
            int maxParticles;
            int emissionRate;
            float particleLifetime;
            
        public:
            ParticleSystem(int maxParticles = 1000);
            ~ParticleSystem();
            
            void emit(const Vector2D& position, int count = 1);
            void emitBurst(const Vector2D& position, int count);
            void emitLine(const Vector2D& start, const Vector2D& end, int count);
            void emitCircle(const Vector2D& center, float radius, int count);
            
            void setEmissionRate(int rate);
            void setParticleLifetime(float lifetime);
            void setMaxParticles(int max);
            void setColorGradient(const Color& start, const Color& end);
            
            void update(float deltaTime);
            void render(Renderer2D& renderer);
            
            int getParticleCount() const;
            void clear();
        };
        
        // Graphics engine manager
        class GraphicsEngine {
        private:
            static GraphicsEngine* instance;
            std::unique_ptr<Renderer2D> renderer;
            std::unique_ptr<Camera2D> camera;
            std::unordered_map<std::string, std::unique_ptr<Texture>> textures;
            std::unordered_map<std::string, std::unique_ptr<Font>> fonts;
            bool initialized;
            
        public:
            GraphicsEngine();
            ~GraphicsEngine();
            
            static GraphicsEngine* getInstance();
            
            bool initialize(int width, int height, const std::string& title);
            void shutdown();
            void clear(const Color& color);
            void present();
            
            // Resource management
            Texture* loadTexture(const std::string& filePath);
            Font* loadFont(const std::string& filePath, int size);
            void unloadTexture(const std::string& filePath);
            void unloadFont(const std::string& filePath);
            
            // Drawing utilities
            void drawLine(const Vector2D& start, const Vector2D& end, const Color& color);
            void drawRectangle(const Rectangle& rect, const Color& color, bool filled = false);
            void drawCircle(const Vector2D& center, float radius, const Color& color, bool filled = false);
            void drawText(const std::string& text, const Vector2D& position, const Color& color, Font* font = nullptr);
            
            Renderer2D* getRenderer() const { return renderer.get(); }
            Camera2D* getCamera() const { return camera.get(); }
            
            void update(); // Call every frame
        };
    }
}
