#pragma once

#include <string>
#include <vector>
#include <memory>
#include <functional>

namespace GameEngine {
    namespace Audio {
        
        // Audio clip for sound effects and music
        class AudioClip {
        private:
            std::string filePath;
            float* audioData;
            int sampleRate;
            int channels;
            int length;
            
        public:
            AudioClip(const std::string& path);
            ~AudioClip();
            
            bool loadFromFile(const std::string& path);
            void play();
            void stop();
            void setVolume(float volume);
            void setLooping(bool looping);
            
            float* getData() const { return audioData; }
            int getSampleRate() const { return sampleRate; }
            int getChannels() const { return channels; }
            int getLength() const { return length; }
        };
        
        // Music track for background music
        class MusicTrack {
        private:
            std::unique_ptr<AudioClip> clip;
            float volume;
            bool isPlaying;
            bool isLooping;
            
        public:
            MusicTrack();
            ~MusicTrack();
            
            bool load(const std::string& filePath);
            void play();
            void stop();
            void pause();
            void setVolume(float volume);
            void setLooping(bool looping);
            
            bool isPlayingMusic() const { return isPlaying; }
            float getVolume() const { return volume; }
        };
        
        // Sound effect manager
        class SoundEffect {
        private:
            std::unique_ptr<AudioClip> clip;
            float volume;
            float pitch;
            
        public:
            SoundEffect();
            ~SoundEffect();
            
            bool load(const std::string& filePath);
            void play();
            void setVolume(float volume);
            void setPitch(float pitch);
        };
        
        // 3D spatial audio position
        struct AudioPosition {
            float x, y, z;
            
            AudioPosition() : x(0), y(0), z(0) {}
            AudioPosition(float x, float y, float z) : x(x), y(y), z(z) {}
        };
        
        // 3D Audio source for positional audio
        class AudioSource3D {
        private:
            std::unique_ptr<AudioClip> clip;
            AudioPosition position;
            float volume;
            float maxDistance;
            float rolloffFactor;
            
        public:
            AudioSource3D();
            ~AudioSource3D();
            
            bool load(const std::string& filePath);
            void play();
            void stop();
            void setPosition(const AudioPosition& pos);
            void setVolume(float volume);
            void setMaxDistance(float maxDist);
            void setRolloffFactor(float rolloff);
            
            AudioPosition getPosition() const { return position; }
        };
        
        // Audio listener for 3D audio
        class AudioListener {
        private:
            AudioPosition position;
            AudioPosition forward;
            AudioPosition up;
            
        public:
            AudioListener();
            ~AudioListener();
            
            void setPosition(const AudioPosition& pos);
            void setOrientation(const AudioPosition& forward, const AudioPosition& up);
            void setMasterVolume(float volume);
            
            AudioPosition getPosition() const { return position; }
            AudioPosition getForward() const { return forward; }
            AudioPosition getUp() const { return up; }
        };
        
        // Audio mixer for managing multiple audio sources
        class AudioMixer {
        private:
            float masterVolume;
            float musicVolume;
            float sfxVolume;
            float voiceVolume;
            
            std::vector<std::unique_ptr<MusicTrack>> musicTracks;
            std::vector<std::unique_ptr<SoundEffect>> soundEffects;
            std::vector<std::unique_ptr<AudioSource3D>> audioSources3D;
            
        public:
            AudioMixer();
            ~AudioMixer();
            
            // Music management
            void playMusic(const std::string& trackName, float volume = 1.0f);
            void stopMusic();
            void pauseMusic();
            
            // Sound effects
            void playSound(const std::string& soundName, float volume = 1.0f);
            void playSound3D(const std::string& soundName, const AudioPosition& pos, float volume = 1.0f);
            
            // Volume controls
            void setMasterVolume(float volume);
            void setMusicVolume(float volume);
            void setSFXVolume(float volume);
            void setVoiceVolume(float volume);
            
            float getMasterVolume() const { return masterVolume; }
            float getMusicVolume() const { return musicVolume; }
            float getSFXVolume() const { return sfxVolume; }
            float getVoiceVolume() const { return voiceVolume; }
        };
        
        // Audio engine manager
        class AudioEngine {
        private:
            static AudioEngine* instance;
            std::unique_ptr<AudioMixer> mixer;
            std::unique_ptr<AudioListener> listener;
            bool initialized;
            
        public:
            AudioEngine();
            ~AudioEngine();
            
            static AudioEngine* getInstance();
            
            bool initialize();
            void shutdown();
            
            AudioMixer* getMixer() const { return mixer.get(); }
            AudioListener* getListener() const { return listener.get(); }
            
            void update(); // Call every frame to update 3D audio
        };
        
        // Audio utilities
        namespace AudioUtils {
            float dbToLinear(float db);
            float linearToDb(float linear);
            void generateSineWave(float* buffer, int samples, float frequency, float amplitude);
            void generateNoise(float* buffer, int samples, float amplitude);
            void fadeInOut(float* buffer, int samples, float fadeInTime, float fadeOutTime);
        }
    }
}
