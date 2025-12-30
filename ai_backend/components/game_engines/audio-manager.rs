
// Audio Manager - Advanced audio system for games
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::time::Duration;
use serde::{Deserialize, Serialize};

/// Audio types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AudioType {
    Music,
    SoundEffect,
    Voice,
    Ambient,
    Ui,
}

/// Audio formats
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AudioFormat {
    WAV,
    MP3,
    OGG,
    FLAC,
    AAC,
}

/// Audio channel configuration
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AudioChannel {
    Mono,
    Stereo,
    Surround5_1,
    Surround7_1,
}

/// Audio source representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AudioSource {
    pub id: String,
    pub name: String,
    pub file_path: String,
    pub audio_type: AudioType,
    pub format: AudioFormat,
    pub channel: AudioChannel,
    pub sample_rate: u32,
    pub bit_depth: u16,
    pub duration: Duration,
    pub file_size: u64,
    pub loaded: bool,
    pub streaming: bool,
}

/// Audio buffer for loaded audio data
#[derive(Debug, Clone)]
pub struct AudioBuffer {
    pub source_id: String,
    pub data: Vec<f32>,
    pub sample_count: usize,
    pub channel_count: u16,
    pub sample_rate: u32,
    pub duration: Duration,
}

/// Audio playback state
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PlaybackState {
    Stopped,
    Playing,
    Paused,
    FadingIn,
    FadingOut,
}

/// Audio playback instance
#[derive(Debug, Clone)]
pub struct AudioInstance {
    pub instance_id: String,
    pub source_id: String,
    pub buffer: Option<Arc<AudioBuffer>>,
    pub state: PlaybackState,
    pub current_position: Duration,
    pub volume: f32,
    pub target_volume: f32,
    pub pitch: f32,
    pub loop_enabled: bool,
    pub fade_in_duration: Option<Duration>,
    pub fade_out_duration: Option<Duration>,
    pub start_time: std::time::Instant,
    pub paused_at: Option<Duration>,
}

/// Audio effect types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AudioEffect {
    Reverb {
        room_size: f32,
        damping: f32,
        wet_level: f32,
        dry_level: f32,
    },
    Echo {
        delay: Duration,
        feedback: f32,
        wet_level: f32,
    },
    LowPassFilter {
        cutoff_frequency: f32,
        resonance: f32,
    },
    HighPassFilter {
        cutoff_frequency: f32,
        resonance: f32,
    },
    Distortion {
        drive: f32,
        tone: f32,
        level: f32,
    },
    Chorus {
        rate: f32,
        depth: f32,
        mix: f32,
    },
}

/// Audio bus for grouping and mixing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AudioBus {
    pub name: String,
    pub volume: f32,
    pub muted: bool,
    pub effects: Vec<AudioEffect>,
    pub send_levels: HashMap<String, f32>, // Bus-to-bus sends
}

/// Audio manager configuration
#[derive(Debug, Clone)]
pub struct AudioManagerConfig {
    pub master_volume: f32,
    pub sample_rate: u32,
    pub buffer_size: usize,
    pub max_voices: usize,
    pub enable_reverb: bool,
    pub enable_compression: bool,
    pub streaming_threshold: Duration,
    pub spatial_audio_enabled: bool,
}

/// Audio statistics
#[derive(Debug, Clone)]
pub struct AudioStats {
    pub active_sources: usize,
    pub active_instances: usize,
    pub total_loaded_buffers: usize,
    pub streaming_sources: usize,
    pub total_memory_usage: u64,
    pub cpu_usage: f32,
    pub average_latency: Duration,
    pub dropped_voices: u32,
}

/// Spatial audio settings
#[derive(Debug, Clone)]
pub struct SpatialAudioConfig {
    pub enabled: bool,
    pub distance_model: DistanceModel,
    pub reference_distance: f32,
    pub rolloff_factor: f32,
    pub max_distance: f32,
    pub doppler_factor: f32,
    pub speed_of_sound: f32,
}

/// Distance attenuation models
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DistanceModel {
    Linear,
    Inverse,
    Exponential,
}

/// 3D position for spatial audio
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct AudioPosition {
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

/// Audio listener (camera/player position)
#[derive(Debug, Clone)]
pub struct AudioListener {
    pub position: AudioPosition,
    pub forward: AudioPosition,
    pub up: AudioPosition,
    pub velocity: AudioPosition,
}

/// Main audio manager
pub struct AudioManager {
    config: AudioManagerConfig,
    spatial_config: SpatialAudioConfig,
    
    // Audio sources and buffers
    sources: HashMap<String, AudioSource>,
    buffers: HashMap<String, Arc<AudioBuffer>>,
    active_instances: HashMap<String, AudioInstance>,
    
    // Audio buses for mixing
    buses: HashMap<String, AudioBus>,
    master_bus: AudioBus,
    
    // Audio state
    listener: AudioListener,
    stats: Arc<RwLock<AudioStats>>,
    
    // Internal state
    is_initialized: bool,
    last_update_time: std::time::Instant,
}

impl AudioManager {
    /// Create a new Audio Manager
    pub fn new(config: AudioManagerConfig, spatial_config: SpatialAudioConfig) -> Self {
        let mut manager = Self {
            config,
            spatial_config,
            sources: HashMap::new(),
            buffers: HashMap::new(),
            active_instances: HashMap::new(),
            buses: HashMap::new(),
            master_bus: AudioBus {
                name: "Master".to_string(),
                volume: 1.0,
                muted: false,
                effects: Vec::new(),
                send_levels: HashMap::new(),
            },
            listener: AudioListener {
                position: AudioPosition { x: 0.0, y: 0.0, z: 0.0 },
                forward: AudioPosition { x: 0.0, y: 0.0, z: -1.0 },
                up: AudioPosition { x: 0.0, y: 1.0, z: 0.0 },
                velocity: AudioPosition { x: 0.0, y: 0.0, z: 0.0 },
            },
            stats: Arc::new(RwLock::new(AudioStats {
                active_sources: 0,
                active_instances: 0,
                total_loaded_buffers: 0,
                streaming_sources: 0,
                total_memory_usage: 0,
                cpu_usage: 0.0,
                average_latency: Duration::from_millis(10),
                dropped_voices: 0,
            })),
            is_initialized: false,
            last_update_time: std::time::Instant::now(),
        };

        // Initialize default buses
        manager.initialize_default_buses();
        
        manager
    }

    /// Initialize the audio system
    pub fn initialize(&mut self) -> Result<(), AudioError> {
        if self.is_initialized {
            return Err(AudioError::AlreadyInitialized);
        }

        // Initialize audio device
        self.initialize_audio_device()?;
        
        // Create audio threads
        self.start_audio_threads()?;
        
        self.is_initialized = true;
        println!("Audio system initialized successfully");
        
        Ok(())
    }

    /// Load an audio source
    pub fn load_source(&mut self, source: AudioSource) -> Result<(), AudioError> {
        if self.sources.contains_key(&source.id) {
            return Err(AudioError::SourceAlreadyLoaded);
        }

        // Load audio data based on format
        let buffer = match source.format {
            AudioFormat::WAV => self.load_wav_file(&source)?,
            AudioFormat::MP3 => self.load_mp3_file(&source)?,
            AudioFormat::OGG => self.load_ogg_file(&source)?,
            _ => return Err(AudioError::UnsupportedFormat),
        };

        self.sources.insert(source.id.clone(), source.clone());
        self.buffers.insert(source.id, Arc::new(buffer));

        // Update stats
        let mut stats = self.stats.write().unwrap();
        stats.total_loaded_buffers += 1;

        println!("Loaded audio source: {}", source.name);
        Ok(())
    }

    /// Unload an audio source
    pub fn unload_source(&mut self, source_id: &str) -> Result<(), AudioError> {
        // Stop all instances using this source
        let instances_to_stop: Vec<String> = self.active_instances
            .values()
            .filter(|instance| instance.source_id == source_id)
            .map(|instance| instance.instance_id.clone())
            .collect();

        for instance_id in instances_to_stop {
            self.stop_instance(&instance_id)?;
        }

        // Remove source and buffer
        self.sources.remove(source_id);
        self.buffers.remove(source_id);

        println!("Unloaded audio source: {}", source_id);
        Ok(())
    }

    /// Play an audio source
    pub fn play(&mut self, source_id: &str, volume: f32, pitch: f32) -> Result<String, AudioError> {
        let buffer = self.buffers.get(source_id)
            .ok_or_else(|| AudioError::SourceNotFound)?;
        
        let instance_id = self.generate_instance_id();
        
        let instance = AudioInstance {
            instance_id: instance_id.clone(),
            source_id: source_id.to_string(),
            buffer: Some(buffer.clone()),
            state: PlaybackState::Playing,
            current_position: Duration::from_secs(0),
            volume,
            target_volume: volume,
            pitch,
            loop_enabled: false,
            fade_in_duration: None,
            fade_out_duration: None,
            start_time: std::time::Instant::now(),
            paused_at: None,
        };

        self.active_instances.insert(instance_id.clone(), instance);
        println!("Playing audio source: {} (instance: {})", source_id, instance_id);
        
        Ok(instance_id)
    }

    /// Play an audio source with spatial positioning
    pub fn play_at_position(&mut self, source_id: &str, position: AudioPosition, volume: f32, pitch: f32) -> Result<String, AudioError> {
        let instance_id = self.play(source_id, volume, pitch)?;
        
        // Apply spatial audio processing
        if let Some(instance) = self.active_instances.get_mut(&instance_id) {
            let spatial_volume = self.calculate_spatial_volume(position);
            instance.volume = volume * spatial_volume;
        }
        
        Ok(instance_id)
    }

    /// Stop an audio instance
    pub fn stop_instance(&mut self, instance_id: &str) -> Result<(), AudioError> {
        if let Some(instance) = self.active_instances.get_mut(instance_id) {
            instance.state = PlaybackState::Stopped;
            Ok(())
        } else {
            Err(AudioError::InstanceNotFound)
        }
    }

    /// Pause an audio instance
    pub fn pause_instance(&mut self, instance_id: &str) -> Result<(), AudioError> {
        if let Some(instance) = self.active_instances.get_mut(instance_id) {
            if instance.state == PlaybackState::Playing {
                instance.paused_at = Some(instance.current_position);
                instance.state = PlaybackState::Paused;
            }
            Ok(())
        } else {
            Err(AudioError::InstanceNotFound)
        }
    }

    /// Resume a paused audio instance
    pub fn resume_instance(&mut self, instance_id: &str) -> Result<(), AudioError> {
        if let Some(instance) = self.active_instances.get_mut(instance_id) {
            if instance.state == PlaybackState::Paused {
                instance.state = PlaybackState::Playing;
                instance.paused_at = None;
            }
            Ok(())
        } else {
            Err(AudioError::InstanceNotFound)
        }
    }

    /// Set volume for an audio instance
    pub fn set_instance_volume(&mut self, instance_id: &str, volume: f32) -> Result<(), AudioError> {
        if let Some(instance) = self.active_instances.get_mut(instance_id) {
            instance.target_volume = volume.clamp(0.0, 1.0);
            Ok(())
        } else {
            Err(AudioError::InstanceNotFound)
        }
    }

    /// Set pitch for an audio instance
    pub fn set_instance_pitch(&mut self, instance_id: &str, pitch: f32) -> Result<(), AudioError> {
        if let Some(instance) = self.active_instances.get_mut(instance_id) {
            instance.pitch = pitch.max(0.1);
            Ok(())
        } else {
            Err(AudioError::InstanceNotFound)
        }
    }

    /// Enable looping for an audio instance
    pub fn set_instance_looping(&mut self, instance_id: &str, loop_enabled: bool) -> Result<(), AudioError> {
        if let Some(instance) = self.active_instances.get_mut(instance_id) {
            instance.loop_enabled = loop_enabled;
            Ok(())
        } else {
            Err(AudioError::InstanceNotFound)
        }
    }

    /// Fade in an audio instance
    pub fn fade_in_instance(&mut self, instance_id: &str, duration: Duration) -> Result<(), AudioError> {
        if let Some(instance) = self.active_instances.get_mut(instance_id) {
            instance.fade_in_duration = Some(duration);
            instance.state = PlaybackState::FadingIn;
            Ok(())
        } else {
            Err(AudioError::InstanceNotFound)
        }
    }

    /// Fade out an audio instance
    pub fn fade_out_instance(&mut self, instance_id: &str, duration: Duration) -> Result<(), AudioError> {
        if let Some(instance) = self.active_instances.get_mut(instance_id) {
            instance.fade_out_duration = Some(duration);
            instance.state = PlaybackState::FadingOut;
            Ok(())
        } else {
            Err(AudioError::InstanceNotFound)
        }
    }

    /// Stop all audio instances
    pub fn stop_all(&mut self) {
        for instance in self.active_instances.values_mut() {
            instance.state = PlaybackState::Stopped;
        }
    }

    /// Set master volume
    pub fn set_master_volume(&mut self, volume: f32) {
        self.master_bus.volume = volume.clamp(0.0, 1.0);
    }

    /// Get master volume
    pub fn get_master_volume(&self) -> f32 {
        self.master_bus.volume
    }

    /// Create an audio bus
    pub fn create_bus(&mut self, name: &str) -> Result<(), AudioError> {
        if self.buses.contains_key(name) {
            return Err(AudioError::BusAlreadyExists);
        }

        let bus = AudioBus {
            name: name.to_string(),
            volume: 1.0,
            muted: false,
            effects: Vec::new(),
            send_levels: HashMap::new(),
        };

        self.buses.insert(name.to_string(), bus);
        Ok(())
    }

    /// Set bus volume
    pub fn set_bus_volume(&mut self, bus_name: &str, volume: f32) -> Result<(), AudioError> {
        if let Some(bus) = self.buses.get_mut(bus_name) {
            bus.volume = volume.clamp(0.0, 1.0);
            Ok(())
        } else {
            Err(AudioError::BusNotFound)
        }
    }

    /// Mute/unmute a bus
    pub fn set_bus_muted(&mut self, bus_name: &str, muted: bool) -> Result<(), AudioError> {
        if let Some(bus) = self.buses.get_mut(bus_name) {
            bus.muted = muted;
            Ok(())
        } else {
            Err(AudioError::BusNotFound)
        }
    }

    /// Add effect to a bus
    pub fn add_effect_to_bus(&mut self, bus_name: &str, effect: AudioEffect) -> Result<(), AudioError> {
        if let Some(bus) = self.buses.get_mut(bus_name) {
            bus.effects.push(effect);
            Ok(())
        } else {
            Err(AudioError::BusNotFound)
        }
    }

    /// Update audio listener position
    pub fn set_listener_position(&mut self, position: AudioPosition, forward: AudioPosition, up: AudioPosition) {
        self.listener.position = position;
        self.listener.forward = forward;
        self.listener.up = up;
    }

    /// Update audio listener velocity
    pub fn set_listener_velocity(&mut self, velocity: AudioPosition) {
        self.listener.velocity = velocity;
    }

    /// Get audio statistics
    pub fn get_stats(&self) -> AudioStats {
        self.stats.read().unwrap().clone()
    }

    /// Update the audio system
    pub fn update(&mut self) {
        let current_time = std::time::Instant::now();
        let delta_time = current_time.duration_since(self.last_update_time);
        self.last_update_time = current_time;

        // Update all active instances
        self.update_instances(delta_time);
        
        // Update statistics
        self.update_stats();
    }

    /// Internal methods

    fn initialize_default_buses(&mut self) {
        // Create default buses for different audio types
        let default_buses = ["Music", "SFX", "Voice", "Ambient", "UI"];
        
        for bus_name in &default_buses {
            let _ = self.create_bus(bus_name);
        }
    }

    fn initialize_audio_device(&self) -> Result<(), AudioError> {
        // In a real implementation, this would initialize the audio device
        println!("Initializing audio device at {} Hz", self.config.sample_rate);
        Ok(())
    }

    fn start_audio_threads(&self) -> Result<(), AudioError> {
        // In a real implementation, this would start audio processing threads
        println!("Starting audio processing threads");
        Ok(())
    }

    fn generate_instance_id(&self) -> String {
        format!("instance_{}", std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos())
    }

    fn load_wav_file(&self, source: &AudioSource) -> Result<AudioBuffer, AudioError> {
        // Simplified WAV loading (real implementation would parse WAV headers)
        let duration = source.duration;
        let sample_count = (source.sample_rate as usize) * duration.as_secs() as usize;
        
        let buffer = AudioBuffer {
            source_id: source.id.clone(),
            data: vec![0.0; sample_count],
            sample_count,
            channel_count: match source.channel {
                AudioChannel::Mono => 1,
                AudioChannel::Stereo => 2,
                _ => 2,
            },
            sample_rate: source.sample_rate,
            duration,
        };

        println!("Loaded WAV file: {} ({} samples)", source.name, sample_count);
        Ok(buffer)
    }

    fn load_mp3_file(&self, source: &AudioSource) -> Result<AudioBuffer, AudioError> {
        // Simplified MP3 loading (real implementation would use MP3 decoder)
        let duration = source.duration;
        let sample_count = (source.sample_rate as usize) * duration.as_secs() as usize;
        
        let buffer = AudioBuffer {
            source_id: source.id.clone(),
            data: vec![0.0; sample_count],
            sample_count,
            channel_count: 2, // MP3 is typically stereo
            sample_rate: source.sample_rate,
            duration,
        };

        println!("Loaded MP3 file: {} ({} samples)", source.name, sample_count);
        Ok(buffer)
    }

    fn load_ogg_file(&self, source: &AudioSource) -> Result<AudioBuffer, AudioError> {
        // Simplified OGG loading (real implementation would use OggVorbis decoder)
        let duration = source.duration;
        let sample_count = (source.sample_rate as usize) * duration.as_secs() as usize;
        
        let buffer = AudioBuffer {
            source_id: source.id.clone(),
            data: vec![0.0; sample_count],
            sample_count,
            channel_count: 2, // OGG is typically stereo
            sample_rate: source.sample_rate,
            duration,
        };

        println!("Loaded OGG file: {} ({} samples)", source.name, sample_count);
        Ok(buffer)
    }

    fn update_instances(&mut self, delta_time: Duration) {
        let mut instances_to_remove = Vec::new();

        for (instance_id, instance) in self.active_instances.iter_mut() {
            match instance.state {
                PlaybackState::Playing => {
                    instance.current_position += delta_time;
                    
                    // Check if looping is needed
                    if let Some(buffer) = &instance.buffer {
                        if !instance.loop_enabled && instance.current_position >= buffer.duration {
                            instances_to_remove.push(instance_id.clone());
                        }
                    }
                },
                PlaybackState::FadingIn => {
                    if let Some(fade_duration) = instance.fade_in_duration {
                        let elapsed = std::time::Instant::now().duration_since(instance.start_time);
                        let progress = (elapsed.as_secs_f32() / fade_duration.as_secs_f32()).min(1.0);
                        instance.volume = instance.target_volume * progress;
                        
                        if progress >= 1.0 {
                            instance.state = PlaybackState::Playing;
                        }
                    }
                },
                PlaybackState::FadingOut => {
                    if let Some(fade_duration) = instance.fade_out_duration {
                        let elapsed = std::time::Instant::now().duration_since(instance.start_time);
                        let progress = (elapsed.as_secs_f32() / fade_duration.as_secs_f32()).min(1.0);
                        instance.volume = instance.target_volume * (1.0 - progress);
                        
                        if progress >= 1.0 {
                            instance.state = PlaybackState::Stopped;
                        }
                    }
                },
                PlaybackState::Paused | PlaybackState::Stopped => {},
            }
        }

        // Remove finished instances
        for instance_id in instances_to_remove {
            self.active_instances.remove(&instance_id);
        }
    }

    fn update_stats(&mut self) {
        let mut stats = self.stats.write().unwrap();
        
        stats.active_sources = self.sources.len();
        stats.active_instances = self.active_instances.len();
        stats.total_loaded_buffers = self.buffers.len();
        
        // Calculate memory usage (simplified)
        let mut total_memory = 0;
        for buffer in self.buffers.values() {
            total_memory += buffer.data.len() * std::mem::size_of::<f32>();
        }
        stats.total_memory_usage = total_memory;
        
        // Update other stats
        stats.streaming_sources = self.sources.values()
            .filter(|s| s.streaming)
            .count();
    }

    fn calculate_spatial_volume(&self, source_position: AudioPosition) -> f32 {
        if !self.spatial_config.enabled {
            return 1.0;
        }

        // Calculate distance from listener
        let dx = source_position.x - self.listener.position.x;
        let dy = source_position.y - self.listener.position.y;
        let dz = source_position.z - self.listener.position.z;
        let distance = (dx * dx + dy * dy + dz * dz).sqrt();

        match self.spatial_config.distance_model {
            DistanceModel::Linear => {
                let max_distance = self.spatial_config.max_distance;
                if distance >= max_distance {
                    0.0
                } else {
                    1.0 - (distance / max_distance)
                }
            },
            DistanceModel::Inverse => {
                let ref_dist = self.spatial_config.reference_distance;
                1.0 / (1.0 + self.spatial_config.rolloff_factor * (distance - ref_dist) / ref_dist)
            },
            DistanceModel::Exponential => {
                let ref_dist = self.spatial_config.reference_distance;
                (distance / ref_dist).powf(-self.spatial_config.rolloff_factor)
            },
        }
    }
}

/// Audio error types
#[derive(Debug, thiserror::Error)]
pub enum AudioError {
    #[error("Audio system already initialized")]
    AlreadyInitialized,
    
    #[error("Audio device initialization failed")]
    DeviceInitializationFailed,
    
    #[error("Source already loaded")]
    SourceAlreadyLoaded,
    
    #[error("Source not found")]
    SourceNotFound,
    
    #[error("Instance not found")]
    InstanceNotFound,
    
    #[error("Unsupported audio format")]
    UnsupportedFormat,
    
    #[error("Bus already exists")]
    BusAlreadyExists,
    
    #[error("Bus not found")]
    BusNotFound,
    
    #[error("Audio buffer overflow")]
    BufferOverflow,
    
    #[error("Invalid audio parameters")]
    InvalidParameters,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_audio_manager_creation() {
        let config = AudioManagerConfig {
            master_volume: 1.0,
            sample_rate: 44100,
            buffer_size: 1024,
            max_voices: 32,
            enable_reverb: true,
            enable_compression: false,
            streaming_threshold: Duration::from_secs(30),
            spatial_audio_enabled: true,
        };

        let spatial_config = SpatialAudioConfig {
            enabled: true,
            distance_model: DistanceModel::Inverse,
            reference_distance: 1.0,
            rolloff_factor: 1.0,
            max_distance: 100.0,
            doppler_factor: 1.0,
            speed_of_sound: 343.0,
        };

        let manager = AudioManager::new(config, spatial_config);
        assert_eq!(manager.get_master_volume(), 1.0);
        assert_eq!(manager.get_stats().active_sources, 0);
    }

    #[test]
    fn test_audio_source_loading() {
        let config = AudioManagerConfig {
            master_volume: 1.0,
            sample_rate: 44100,
            buffer_size: 1024,
            max_voices: 32,
            enable_reverb: true,
            enable_compression: false,
            streaming_threshold: Duration::from_secs(30),
            spatial_audio_enabled: true,
        };

        let spatial_config = SpatialAudioConfig {
            enabled: true,
            distance_model: DistanceModel::Linear,
            reference_distance: 1.0,
            rolloff_factor: 1.0,
            max_distance: 100.0,
            doppler_factor: 1.0,
            speed_of_sound: 343.0,
        };

        let mut manager = AudioManager::new(config, spatial_config);

        let source = AudioSource {
            id: "test_sound".to_string(),
            name: "Test Sound".to_string(),
            file_path: "test.wav".to_string(),
            audio_type: AudioType::SoundEffect,
            format: AudioFormat::WAV,
            channel: AudioChannel::Stereo,
            sample_rate: 44100,
            bit_depth: 16,
            duration: Duration::from_secs(2),
            file_size: 1024,
            loaded: false,
            streaming: false,
        };

        let result = manager.load_source(source);
        assert!(result.is_ok());
        assert_eq!(manager.get_stats().total_loaded_buffers, 1);
    }

    #[test]
    fn test_audio_playback() {
        let config = AudioManagerConfig {
            master_volume: 1.0,
            sample_rate: 44100,
            buffer_size: 1024,
            max_voices: 32,
            enable_reverb: true,
            enable_compression: false,
            streaming_threshold: Duration::from_secs(30),
            spatial_audio_enabled: true,
        };

        let spatial_config = SpatialAudioConfig {
            enabled: true,
            distance_model: DistanceModel::Inverse,
            reference_distance: 1.0,
            rolloff_factor: 1.0,
            max_distance: 100.0,
            doppler_factor: 1.0,
            speed_of_sound: 343.0,
        };

        let mut manager = AudioManager::new(config, spatial_config);

        // Load a source first
        let source = AudioSource {
            id: "music_test".to_string(),
            name: "Test Music".to_string(),
            file_path: "music.mp3".to_string(),
            audio_type: AudioType::Music,
            format: AudioFormat::MP3,
            channel: AudioChannel::Stereo,
            sample_rate: 44100,
            bit_depth: 16,
            duration: Duration::from_secs(60),
            file_size: 2048,
            loaded: false,
            streaming: true,
        };

        let _ = manager.load_source(source);

        // Play the source
        let instance_id = manager.play("music_test", 0.8, 1.0);
        assert!(instance_id.is_ok());
        
        let instance_id = instance_id.unwrap();
        assert_eq!(manager.get_stats().active_instances, 1);

        // Update manager
        manager.update();

        // Stop the instance
        let result = manager.stop_instance(&instance_id);
        assert!(result.is_ok());
    }

    #[test]
    fn test_audio_buses() {
        let config = AudioManagerConfig {
            master_volume: 1.0,
            sample_rate: 44100,
            buffer_size: 1024,
            max_voices: 32,
            enable_reverb: true,
            enable_compression: false,
            streaming_threshold: Duration::from_secs(30),
            spatial_audio_enabled: true,
        };

        let spatial_config = SpatialAudioConfig {
            enabled: false,
            distance_model: DistanceModel::Linear,
            reference_distance: 1.0,
            rolloff_factor: 1.0,
            max_distance: 100.0,
            doppler_factor: 1.0,
            speed_of_sound: 343.0,
        };

        let mut manager = AudioManager::new(config, spatial_config);

        // Create a custom bus
        let result = manager.create_bus("CustomBus");
        assert!(result.is_ok());

        // Set bus volume
        let result = manager.set_bus_volume("CustomBus", 0.5);
        assert!(result.is_ok());

        // Add effect to bus
        let effect = AudioEffect::LowPassFilter {
            cutoff_frequency: 1000.0,
            resonance: 1.0,
        };

        let result = manager.add_effect_to_bus("CustomBus", effect);
        assert!(result.is_ok());
    }

    #[test]
    fn test_spatial_audio() {
        let config = AudioManagerConfig {
            master_volume: 1.0,
            sample_rate: 44100,
            buffer_size: 1024,
            max_voices: 32,
            enable_reverb: true,
            enable_compression: false,
            streaming_threshold: Duration::from_secs(30),
            spatial_audio_enabled: true,
        };

        let spatial_config = SpatialAudioConfig {
            enabled: true,
            distance_model: DistanceModel::Linear,
            reference_distance: 1.0,
            rolloff_factor: 1.0,
            max_distance: 100.0,
            doppler_factor: 1.0,
            speed_of_sound: 343.0,
        };

        let mut manager = AudioManager::new(config, spatial_config);

        // Set listener position
        manager.set_listener_position(
            AudioPosition { x: 0.0, y: 0.0, z: 0.0 },
            AudioPosition { x: 0.0, y: 0.0, z: -1.0 },
            AudioPosition { x: 0.0, y: 1.0, z: 0.0 },
        );

        // Test spatial volume calculation
        let close_position = AudioPosition { x: 1.0, y: 0.0, z: 0.0 };
        let far_position = AudioPosition { x: 50.0, y: 0.0, z: 0.0 };

        let close_volume = manager.calculate_spatial_volume(close_position);
        let far_volume = manager.calculate_spatial_volume(far_position);

        assert!(close_volume > far_volume);
        assert!(close_volume <= 1.0);
        assert!(far_volume >= 0.0);
    }

    #[test]
    fn test_audio_effects() {
        // Test different audio effects
        let reverb = AudioEffect::Reverb {
            room_size: 0.8,
            damping: 0.5,
            wet_level: 0.3,
            dry_level: 0.7,
        };

        let echo = AudioEffect::Echo {
            delay: Duration::from_millis(500),
            feedback: 0.3,
            wet_level: 0.2,
        };

        let lowpass = AudioEffect::LowPassFilter {
            cutoff_frequency: 2000.0,
            resonance: 1.5,
        };

        assert!(matches!(reverb, AudioEffect::Reverb { .. }));
        assert!(matches!(echo, AudioEffect::Echo { .. }));
        assert!(matches!(lowpass, AudioEffect::LowPassFilter { .. }));
    }
}


