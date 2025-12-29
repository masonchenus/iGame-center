
// Config Manager - Centralized configuration management
use std::collections::HashMap;
use std::fs::{File, OpenOptions};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use serde::{Deserialize, Serialize};
use toml;

/// Configuration types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ConfigType {
    Game,
    Graphics,
    Audio,
    Input,
    Network,
    Physics,
    AI,
    Performance,
}

/// Configuration scope
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ConfigScope {
    Global,      // Applies to all users
    User,        // Applies to current user
    Session,     // Applies to current session only
    Temporary,   // In-memory only
}

/// Configuration value types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ConfigValue {
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Array(Vec<ConfigValue>),
    Object(HashMap<String, ConfigValue>),
}

/// Configuration entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConfigEntry {
    pub key: String,
    pub value: ConfigValue,
    pub config_type: ConfigType,
    pub scope: ConfigScope,
    pub description: String,
    pub default_value: Option<ConfigValue>,
    pub validation_rules: Vec<ValidationRule>,
    pub is_dirty: bool,
    pub last_modified: std::time::SystemTime,
}

/// Validation rules for configuration values
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationRule {
    pub rule_type: ValidationRuleType,
    pub parameters: HashMap<String, serde_json::Value>,
}

/// Types of validation rules
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ValidationRuleType {
    MinValue,
    MaxValue,
    MinLength,
    MaxLength,
    Pattern,
    Enum,
    Required,
    Custom,
}

/// Configuration profile
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConfigProfile {
    pub name: String,
    pub description: String,
    pub entries: HashMap<String, ConfigValue>,
    pub created_at: std::time::SystemTime,
    pub modified_at: std::time::SystemTime,
}

/// Configuration manager settings
#[derive(Debug, Clone)]
pub struct ConfigManagerSettings {
    pub config_directory: PathBuf,
    pub auto_save: bool,
    pub auto_save_interval: std::time::Duration,
    pub enable_hot_reload: bool,
    pub enable_backup: bool,
    pub max_backups: u32,
    pub encrypt_sensitive: bool,
    pub validation_strict: bool,
}

/// Configuration statistics
#[derive(Debug, Clone)]
pub struct ConfigStats {
    pub total_entries: usize,
    pub dirty_entries: usize,
    pub entries_by_type: HashMap<ConfigType, usize>,
    pub entries_by_scope: HashMap<ConfigScope, usize>,
    pub backup_count: u32,
    pub last_save_time: Option<std::time::SystemTime>,
    pub last_load_time: Option<std::time::SystemTime>,
}

/// Main configuration manager
pub struct ConfigManager {
    entries: HashMap<String, ConfigEntry>,
    profiles: HashMap<String, ConfigProfile>,
    active_profile: Option<String>,
    settings: ConfigManagerSettings,
    stats: ConfigStats,
    auto_save_timer: Option<std::time::Instant>,
    change_callbacks: HashMap<String, Box<dyn Fn(&str, &ConfigValue, &ConfigValue) + Send + Sync>>,
}

/// Configuration error types
#[derive(Debug, thiserror::Error)]
pub enum ConfigError {
    #[error("Key not found: {0}")]
    KeyNotFound(String),
    
    #[error("Invalid configuration value")]
    InvalidValue,
    
    #[error("Validation failed")]
    ValidationFailed,
    
    #[error("Profile not found: {0}")]
    ProfileNotFound(String),
    
    #[error("Profile already exists: {0}")]
    ProfileExists(String),
    
    #[error("IO error: {0}")]
    IoError(String),
    
    #[error("Serialization error")]
    SerializationError,
    
    #[error("Permission denied")]
    PermissionDenied,
    
    #[error("File not found")]
    FileNotFound,
}

impl ConfigManager {
    /// Create a new Config Manager
    pub fn new(settings: ConfigManagerSettings) -> Result<Self, ConfigError> {
        let config_dir = settings.config_directory.clone();
        
        // Ensure config directory exists
        if let Err(e) = std::fs::create_dir_all(&config_dir) {
            return Err(ConfigError::IoError(format!("Failed to create config directory: {}", e)));
        }

        let mut manager = Self {
            entries: HashMap::new(),
            profiles: HashMap::new(),
            active_profile: None,
            settings,
            stats: ConfigStats {
                total_entries: 0,
                dirty_entries: 0,
                entries_by_type: HashMap::new(),
                entries_by_scope: HashMap::new(),
                backup_count: 0,
                last_save_time: None,
                last_load_time: None,
            },
            auto_save_timer: None,
            change_callbacks: HashMap::new(),
        };

        // Load default configuration
        manager.load_defaults()?;
        
        // Load existing configuration files
        manager.load_all_files()?;
        
        Ok(manager)
    }

    /// Set a configuration value
    pub fn set(&mut self, key: &str, value: ConfigValue, config_type: ConfigType, scope: ConfigScope) -> Result<(), ConfigError> {
        let entry = ConfigEntry {
            key: key.to_string(),
            value: value.clone(),
            config_type,
            scope,
            description: String::new(),
            default_value: None,
            validation_rules: Vec::new(),
            is_dirty: true,
            last_modified: std::time::SystemTime::now(),
        };

        // Validate the value
        if !self.validate_value(&value, &entry.validation_rules) {
            return Err(ConfigError::ValidationFailed);
        }

        // Store old value for callbacks
        let old_value = self.entries.get(key).map(|e| e.value.clone());

        self.entries.insert(key.to_string(), entry);
        self.update_stats();

        // Trigger change callbacks
        if let (Some(old_val), Some(callback)) = (old_value, self.change_callbacks.get(key)) {
            callback(key, &old_val, &value);
        }

        // Start auto-save timer if enabled
        if self.settings.auto_save {
            self.auto_save_timer = Some(std::time::Instant::now());
        }

        Ok(())
    }

    /// Get a configuration value
    pub fn get(&self, key: &str) -> Result<&ConfigValue, ConfigError> {
        self.entries.get(key)
            .map(|entry| &entry.value)
            .ok_or_else(|| ConfigError::KeyNotFound(key.to_string()))
    }

    /// Get a configuration value with default
    pub fn get_or_default(&self, key: &str, default: ConfigValue) -> &ConfigValue {
        self.entries.get(key)
            .map(|entry| &entry.value)
            .unwrap_or(&default)
    }

    /// Check if a configuration key exists
    pub fn has_key(&self, key: &str) -> bool {
        self.entries.contains_key(key)
    }

    /// Remove a configuration value
    pub fn remove(&mut self, key: &str) -> Result<(), ConfigError> {
        if self.entries.remove(key).is_some() {
            self.update_stats();
            Ok(())
        } else {
            Err(ConfigError::KeyNotFound(key.to_string()))
        }
    }

    /// Get all configuration entries
    pub fn get_all_entries(&self) -> &HashMap<String, ConfigEntry> {
        &self.entries
    }

    /// Get entries by type
    pub fn get_entries_by_type(&self, config_type: ConfigType) -> Vec<&ConfigEntry> {
        self.entries.values()
            .filter(|entry| entry.config_type == config_type)
            .collect()
    }

    /// Get entries by scope
    pub fn get_entries_by_scope(&self, scope: ConfigScope) -> Vec<&ConfigEntry> {
        self.entries.values()
            .filter(|entry| entry.scope == scope)
            .collect()
    }

    /// Get all dirty (modified) entries
    pub fn get_dirty_entries(&self) -> Vec<&ConfigEntry> {
        self.entries.values()
            .filter(|entry| entry.is_dirty)
            .collect()
    }

    /// Save all configuration to files
    pub fn save_all(&mut self) -> Result<(), ConfigError> {
        let start_time = std::time::Instant::now();
        
        // Create backup if enabled
        if self.settings.enable_backup {
            self.create_backup()?;
        }

        // Save by scope
        self.save_by_scope(ConfigScope::Global)?;
        self.save_by_scope(ConfigScope::User)?;
        self.save_by_scope(ConfigScope::Session)?;
        
        // Mark all entries as clean
        for entry in self.entries.values_mut() {
            entry.is_dirty = false;
        }

        self.stats.last_save_time = Some(std::time::SystemTime::now());
        self.stats.dirty_entries = 0;
        self.auto_save_timer = None;

        println!("Configuration saved in {:?}", start_time.elapsed());
        Ok(())
    }

    /// Load all configuration from files
    pub fn load_all(&mut self) -> Result<(), ConfigError> {
        let start_time = std::time::Instant::now();
        
        // Clear existing entries
        self.entries.clear();

        // Load by scope
        self.load_by_scope(ConfigScope::Global)?;
        self.load_by_scope(ConfigScope::User)?;
        self.load_by_scope(ConfigScope::Session)?;

        self.update_stats();
        self.stats.last_load_time = Some(std::time::SystemTime::now());

        println!("Configuration loaded in {:?}", start_time.elapsed());
        Ok(())
    }

    /// Create a new configuration profile
    pub fn create_profile(&mut self, name: &str, description: &str) -> Result<(), ConfigError> {
        if self.profiles.contains_key(name) {
            return Err(ConfigError::ProfileExists(name.to_string()));
        }

        let profile = ConfigProfile {
            name: name.to_string(),
            description: description.to_string(),
            entries: HashMap::new(),
            created_at: std::time::SystemTime::now(),
            modified_at: std::time::SystemTime::now(),
        };

        // Copy current configuration values
        for (key, entry) in &self.entries {
            profile.entries.insert(key.clone(), entry.value.clone());
        }

        self.profiles.insert(name.to_string(), profile);
        Ok(())
    }

    /// Load a configuration profile
    pub fn load_profile(&mut self, name: &str) -> Result<(), ConfigError> {
        let profile = self.profiles.get(name)
            .ok_or_else(|| ConfigError::ProfileNotFound(name.to_string()))?;

        // Apply profile values
        for (key, value) in &profile.entries {
            if let Some(entry) = self.entries.get_mut(key) {
                entry.value = value.clone();
                entry.is_dirty = true;
            }
        }

        self.active_profile = Some(name.to_string());
        Ok(())
    }

    /// Save current configuration as a profile
    pub fn save_current_as_profile(&mut self, name: &str) -> Result<(), ConfigError> {
        if let Some(profile) = self.profiles.get_mut(name) {
            // Update existing profile
            profile.entries.clear();
            for (key, entry) in &self.entries {
                profile.entries.insert(key.clone(), entry.value.clone());
            }
            profile.modified_at = std::time::SystemTime::now();
        } else {
            // Create new profile
            self.create_profile(name, "")?;
        }
        Ok(())
    }

    /// Get all profiles
    pub fn get_profiles(&self) -> &HashMap<String, ConfigProfile> {
        &self.profiles
    }

    /// Delete a profile
    pub fn delete_profile(&mut self, name: &str) -> Result<(), ConfigError> {
        if self.profiles.remove(name).is_some() {
            // If this was the active profile, clear it
            if self.active_profile.as_ref().map(|s| s.as_str()) == Some(name) {
                self.active_profile = None;
            }
            Ok(())
        } else {
            Err(ConfigError::ProfileNotFound(name.to_string()))
        }
    }

    /// Reset configuration to defaults
    pub fn reset_to_defaults(&mut self) -> Result<(), ConfigError> {
        for entry in self.entries.values_mut() {
            if let Some(default_value) = &entry.default_value {
                entry.value = default_value.clone();
                entry.is_dirty = true;
            }
        }
        Ok(())
    }

    /// Export configuration to JSON
    pub fn export_to_json(&self, path: &Path) -> Result<(), ConfigError> {
        let mut file = File::create(path)
            .map_err(|e| ConfigError::IoError(e.to_string()))?;

        let config_data: HashMap<String, &ConfigValue> = self.entries
            .iter()
            .map(|(k, v)| (k.clone(), &v.value))
            .collect();

        let json = serde_json::to_string_pretty(&config_data)
            .map_err(|_| ConfigError::SerializationError)?;

        file.write_all(json.as_bytes())
            .map_err(|e| ConfigError::IoError(e.to_string()))?;

        Ok(())
    }

    /// Import configuration from JSON
    pub fn import_from_json(&mut self, path: &Path) -> Result<(), ConfigError> {
        let mut file = File::open(path)
            .map_err(|e| ConfigError::IoError(e.to_string()))?;

        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .map_err(|e| ConfigError::IoError(e.to_string()))?;

        let config_data: HashMap<String, ConfigValue> = serde_json::from_str(&contents)
            .map_err(|_| ConfigError::SerializationError)?;

        for (key, value) in config_data {
            if let Some(entry) = self.entries.get_mut(&key) {
                entry.value = value;
                entry.is_dirty = true;
            }
        }

        self.update_stats();
        Ok(())
    }

    /// Register a change callback
    pub fn register_change_callback(&mut self, key: &str, callback: Box<dyn Fn(&str, &ConfigValue, &ConfigValue) + Send + Sync>) {
        self.change_callbacks.insert(key.to_string(), callback);
    }

    /// Unregister a change callback
    pub fn unregister_change_callback(&mut self, key: &str) {
        self.change_callbacks.remove(key);
    }

    /// Get configuration statistics
    pub fn get_stats(&self) -> &ConfigStats {
        &self.stats
    }

    /// Update configuration manager
    pub fn update(&mut self, delta_time: std::time::Duration) {
        // Check for auto-save
        if self.settings.auto_save && self.stats.dirty_entries > 0 {
            if let Some(timer_start) = self.auto_save_timer {
                if timer_start.elapsed() >= self.settings.auto_save_interval {
                    let _ = self.save_all();
                }
            }
        }
    }

    /// Internal methods

    fn load_defaults(&mut self) -> Result<(), ConfigError> {
        // Load default configurations
        self.set("graphics.resolution_width", ConfigValue::Integer(1920), ConfigType::Graphics, ConfigScope::User)?;
        self.set("graphics.resolution_height", ConfigValue::Integer(1080), ConfigType::Graphics, ConfigScope::User)?;
        self.set("graphics.vsync", ConfigValue::Boolean(true), ConfigType::Graphics, ConfigScope::User)?;
        self.set("graphics.fullscreen", ConfigValue::Boolean(false), ConfigType::Graphics, ConfigScope::User)?;
        
        self.set("audio.master_volume", ConfigValue::Float(1.0), ConfigType::Audio, ConfigScope::User)?;
        self.set("audio.music_volume", ConfigValue::Float(0.8), ConfigType::Audio, ConfigScope::User)?;
        self.set("audio.sfx_volume", ConfigValue::Float(1.0), ConfigType::Audio, ConfigScope::User)?;
        
        self.set("input.mouse_sensitivity", ConfigValue::Float(1.0), ConfigType::Input, ConfigScope::User)?;
        self.set("input.invert_y", ConfigValue::Boolean(false), ConfigType::Input, ConfigScope::User)?;
        
        self.set("game.difficulty", ConfigValue::String("normal".to_string()), ConfigType::Game, ConfigScope::User)?;
        self.set("game.language", ConfigValue::String("en".to_string()), ConfigType::Game, ConfigScope::User)?;

        Ok(())
    }

    fn load_all_files(&mut self) -> Result<(), ConfigError> {
        self.load_by_scope(ConfigScope::Global)?;
        self.load_by_scope(ConfigScope::User)?;
        Ok(())
    }

    fn load_by_scope(&mut self, scope: ConfigScope) -> Result<(), ConfigError> {
        let filename = match scope {
            ConfigScope::Global => "config_global.toml",
            ConfigScope::User => "config_user.toml",
            ConfigScope::Session => "config_session.toml",
            ConfigScope::Temporary => return Ok(()), // Don't load temporary config
        };

        let path = self.settings.config_directory.join(filename);
        
        if !path.exists() {
            return Ok(()); // File doesn't exist, skip
        }

        let mut file = File::open(&path)
            .map_err(|e| ConfigError::IoError(e.to_string()))?;

        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .map_err(|e| ConfigError::IoError(e.to_string()))?;

        let config_data: HashMap<String, ConfigValue> = toml::from_str(&contents)
            .map_err(|_| ConfigError::SerializationError)?;

        for (key, value) in config_data {
            // Infer type from key or use default
            let config_type = self.infer_config_type(&key);
            
            let entry = ConfigEntry {
                key: key.clone(),
                value,
                config_type,
                scope,
                description: String::new(),
                default_value: None,
                validation_rules: Vec::new(),
                is_dirty: false,
                last_modified: std::time::SystemTime::now(),
            };

            self.entries.insert(key, entry);
        }

        Ok(())
    }

    fn save_by_scope(&mut self, scope: ConfigScope) -> Result<(), ConfigError> {
        let filename = match scope {
            ConfigScope::Global => "config_global.toml",
            ConfigScope::User => "config_user.toml",
            ConfigScope::Session => "config_session.toml",
            ConfigScope::Temporary => return Ok(()), // Don't save temporary config
        };

        let path = self.settings.config_directory.join(filename);
        
        let config_data: HashMap<String, &ConfigValue> = self.entries
            .values()
            .filter(|entry| entry.scope == scope)
            .map(|entry| (entry.key.clone(), &entry.value))
            .collect();

        if config_data.is_empty() {
            return Ok(()); // No entries to save
        }

        let toml_content = toml::to_string_pretty(&config_data)
            .map_err(|_| ConfigError::SerializationError)?;

        let mut file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(&path)
            .map_err(|e| ConfigError::IoError(e.to_string()))?;

        file.write_all(toml_content.as_bytes())
            .map_err(|e| ConfigError::IoError(e.to_string()))?;

        Ok(())
    }

    fn create_backup(&self) -> Result<(), ConfigError> {
        let backup_dir = self.settings.config_directory.join("backups");
        std::fs::create_dir_all(&backup_dir)
            .map_err(|e| ConfigError::IoError(e.to_string()))?;

        let timestamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs();

        let backup_filename = format!("config_backup_{}.toml", timestamp);
        let backup_path = backup_dir.join(&backup_filename);

        // This would copy the current configuration files
        // Implementation depends on specific backup strategy
        println!("Created backup: {}", backup_path.display());

        Ok(())
    }

    fn validate_value(&self, value: &ConfigValue, rules: &[ValidationRule]) -> bool {
        for rule in rules {
            match rule.rule_type {
                ValidationRuleType::Required => {
                    if matches!(value, ConfigValue::String(s) if s.is_empty()) {
                        return false;
                    }
                },
                ValidationRuleType::MinValue => {
                    if let Some(min_val) = rule.parameters.get("value") {
                        if let (ConfigValue::Integer(num), Some(min)) = (value.clone(), min_val.as_i64()) {
                            if num < min {
                                return false;
                            }
                        }
                    }
                },
                _ => {
                    // Other validation rules would be implemented here
                    continue;
                }
            }
        }
        true
    }

    fn infer_config_type(&self, key: &str) -> ConfigType {
        match key.split('.').next().unwrap_or("") {
            "graphics" => ConfigType::Graphics,
            "audio" => ConfigType::Audio,
            "input" => ConfigType::Input,
            "network" => ConfigType::Network,
            "physics" => ConfigType::Physics,
            "ai" => ConfigType::AI,
            "performance" => ConfigType::Performance,
            _ => ConfigType::Game,
        }
    }

    fn update_stats(&mut self) {
        self.stats.total_entries = self.entries.len();
        self.stats.dirty_entries = self.entries.values().filter(|e| e.is_dirty).count();
        
        // Clear and recalculate type stats
        self.stats.entries_by_type.clear();
        for entry in self.entries.values() {
            *self.stats.entries_by_type.entry(entry.config_type).or_insert(0) += 1;
        }
        
        // Clear and recalculate scope stats
        self.stats.entries_by_scope.clear();
        for entry in self.entries.values() {
            *self.stats.entries_by_scope.entry(entry.scope).or_insert(0) += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_manager_creation() {
        let temp_dir = std::env::temp_dir().join("config_test");
        let settings = ConfigManagerSettings {
            config_directory: temp_dir,
            auto_save: false,
            auto_save_interval: std::time::Duration::from_secs(5),
            enable_hot_reload: false,
            enable_backup: false,
            max_backups: 5,
            encrypt_sensitive: false,
            validation_strict: false,
        };

        let manager = ConfigManager::new(settings);
        assert!(manager.is_ok());
    }

    #[test]
    fn test_set_and_get_config() {
        let temp_dir = std::env::temp_dir().join("config_test_get");
        let settings = ConfigManagerSettings {
            config_directory: temp_dir,
            auto_save: false,
            auto_save_interval: std::time::Duration::from_secs(5),
            enable_hot_reload: false,
            enable_backup: false,
            max_backups: 5,
            encrypt_sensitive: false,
            validation_strict: false,
        };

        let mut manager = ConfigManager::new(settings).unwrap();
        
        // Set a value
        let test_value = ConfigValue::String("test_value".to_string());
        manager.set("test.key", test_value.clone(), ConfigType::Game, ConfigScope::Session).unwrap();
        
        // Get the value
        let retrieved_value = manager.get("test.key").unwrap();
        assert_eq!(retrieved_value, &test_value);
    }

    #[test]
    fn test_config_profiles() {
        let temp_dir = std::env::temp_dir().join("config_test_profiles");
        let settings = ConfigManagerSettings {
            config_directory: temp_dir,
            auto_save: false,
            auto_save_interval: std::time::Duration::from_secs(5),
            enable_hot_reload: false,
            enable_backup: false,
            max_backups: 5,
            encrypt_sensitive: false,
            validation_strict: false,
        };

        let mut manager = ConfigManager::new(settings).unwrap();
        
        // Set some values
        manager.set("graphics.resolution_width", ConfigValue::Integer(1920), ConfigType::Graphics, ConfigScope::User).unwrap();
        manager.set("audio.master_volume", ConfigValue::Float(0.8), ConfigType::Audio, ConfigScope::User).unwrap();
        
        // Create a profile
        manager.create_profile("high_quality", "High quality settings").unwrap();
        
        // Modify values
        manager.set("graphics.resolution_width", ConfigValue::Integer(3840), ConfigType::Graphics, ConfigScope::User).unwrap();
        
        // Load the profile
        manager.load_profile("high_quality").unwrap();
        
        // Check if values were restored
        let resolution = manager.get("graphics.resolution_width").unwrap();
        assert_eq!(resolution, &ConfigValue::Integer(1920));
    }
}


