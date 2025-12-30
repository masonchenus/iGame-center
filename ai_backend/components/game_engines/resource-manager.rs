// Resource Manager - Asset loading, caching, and management
use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, RwLock};
use std::time::{Duration, Instant};
use serde::{Deserialize, Serialize};
use rayon::prelude::*;

/// Resource types supported by the system
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ResourceType {
    Texture,
    Audio,
    Model,
    Font,
    Shader,
    Data,
    Script,
}

/// Resource loading priority
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum LoadingPriority {
    Low,
    Normal,
    High,
    Critical,
}

/// Resource state
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ResourceState {
    Unloaded,
    Loading,
    Loaded,
    Error,
}

/// Resource representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Resource {
    pub id: String,
    pub name: String,
    pub resource_type: ResourceType,
    pub path: PathBuf,
    pub size: u64,
    pub checksum: String,
    pub state: ResourceState,
    pub loaded_at: Option<Instant>,
    pub access_count: u32,
    pub last_access: Option<Instant>,
    pub metadata: ResourceMetadata,
}

/// Resource metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceMetadata {
    pub width: Option<u32>,
    pub height: Option<u32>,
    pub format: Option<String>,
    pub duration: Option<Duration>,
    pub sample_rate: Option<u32>,
    pub channels: Option<u32>,
    pub vertex_count: Option<u32>,
    pub triangle_count: Option<u32>,
    pub custom_data: HashMap<String, serde_json::Value>,
}

/// Resource loading request
#[derive(Debug, Clone)]
pub struct ResourceRequest {
    pub id: String,
    pub path: PathBuf,
    pub resource_type: ResourceType,
    pub priority: LoadingPriority,
    pub callback: Option<Arc<dyn Fn(Result<Resource, ResourceError>) + Send + Sync>>,
}

/// Resource loading result
#[derive(Debug, Clone)]
pub struct ResourceResult {
    pub resource: Resource,
    pub data: Vec<u8>,
    pub load_time: Duration,
}

/// Resource cache configuration
#[derive(Debug, Clone)]
pub struct CacheConfig {
    pub max_size: u64,
    pub max_entries: u32,
    pub ttl: Duration,
    pub enable_compression: bool,
    pub enable_deduplication: bool,
}

/// Resource manager settings
#[derive(Debug, Clone)]
pub struct ResourceManagerSettings {
    pub base_path: PathBuf,
    pub cache_config: CacheConfig,
    pub max_concurrent_loads: usize,
    pub enable_async_loading: bool,
    pub enable_preloading: bool,
    pub enable_hot_reload: bool,
    pub worker_threads: usize,
    pub compression_enabled: bool,
    pub checksum_verification: bool,
}

/// Resource statistics
#[derive(Debug, Clone)]
pub struct ResourceStats {
    pub total_resources: u32,
    pub loaded_resources: u32,
    pub loading_resources: u32,
    pub error_resources: u32,
    pub cache_hit_rate: f32,
    pub total_cache_size: u64,
    pub total_memory_usage: u64,
    pub average_load_time: Duration,
    pub peak_concurrent_loads: usize,
}

/// Resource manager handles all resource operations
pub struct ResourceManager {
    resources: Arc<RwLock<HashMap<String, Resource>>>,
    cache: Arc<Mutex<ResourceCache>>,
    load_queue: Arc<Mutex<Vec<ResourceRequest>>>,
    loading_pool: Arc<Mutex<HashMap<String, ResourceLoadingJob>>>,
    settings: ResourceManagerSettings,
    stats: Arc<Mutex<ResourceStats>>,
}

/// Resource cache implementation
struct ResourceCache {
    entries: HashMap<String, CacheEntry>,
    config: CacheConfig,
    current_size: u64,
    access_order: Vec<String>, // LRU tracking
}

/// Cache(Debug, Clone)]
struct CacheEntry {
 entry
#[derive    resource_id: String,
    data: Vec<u8>,
    size: u64,
    created_at: Instant,
    last_access: Instant,
    access_count: u32,
}

/// Resource loading job
#[derive(Debug, Clone)]
struct ResourceLoadingJob {
    resource_id: String,
    path: PathBuf,
    resource_type: ResourceType,
    started_at: Instant,
    status: LoadingStatus,
}

/// Loading status
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LoadingStatus {
    Pending,
    Loading,
    Completed,
    Failed,
}

impl ResourceManager {
    /// Create a new Resource Manager
    pub fn new(settings: ResourceManagerSettings) -> Self {
        let cache = ResourceCache {
            entries: HashMap::new(),
            config: settings.cache_config.clone(),
            current_size: 0,
            access_order: Vec::new(),
        };

        let manager = Self {
            resources: Arc::new(RwLock::new(HashMap::new())),
            cache: Arc::new(Mutex::new(cache)),
            load_queue: Arc::new(Mutex::new(Vec::new())),
            loading_pool: Arc::new(Mutex::new(HashMap::new())),
            settings,
            stats: Arc::new(Mutex::new(ResourceStats {
                total_resources: 0,
                loaded_resources: 0,
                loading_resources: 0,
                error_resources: 0,
                cache_hit_rate: 0.0,
                total_cache_size: 0,
                total_memory_usage: 0,
                average_load_time: Duration::from_millis(0),
                peak_concurrent_loads: 0,
            })),
        };

        // Start background workers
        if manager.settings.enable_async_loading {
            manager.start_loading_workers();
        }

        manager
    }

    /// Load a resource synchronously
    pub fn load_resource(&self, id: String, path: PathBuf, resource_type: ResourceType) -> Result<Resource, ResourceError> {
        let start_time = Instant::now();

        // Check if resource is already loaded
        if let Some(resource) = self.get_resource(&id) {
            return Ok(resource);
        }

        // Check cache first
        if let Some((resource, data)) = self.load_from_cache(&id)? {
            self.insert_resource(resource.clone(), data)?;
            self.update_stats(start_time.elapsed(), true)?;
            return Ok(resource);
        }

        // Load from file system
        let (resource, data) = self.load_from_filesystem(&path, &id, resource_type.clone())?;

        // Store in cache
        self.insert_into_cache(&id, &data)?;

        // Insert resource
        self.insert_resource(resource.clone(), data)?;

        self.update_stats(start_time.elapsed(), false)?;
        Ok(resource)
    }

    /// Load a resource asynchronously
    pub fn load_resource_async(&self, request: ResourceRequest) -> Result<(), ResourceError> {
        let mut load_queue = self.load_queue.lock().map_err(|_| ResourceError::PoisonedLock)?;
        
        // Check if already loading
        let loading_pool = self.loading_pool.lock().map_err(|_| ResourceError::PoisonedLock)?;
        if loading_pool.contains_key(&request.id) {
            return Err(ResourceError::AlreadyLoading);
        }

        load_queue.push(request);
        Ok(())
    }

    /// Get a loaded resource
    pub fn get_resource(&self, id: &str) -> Option<Resource> {
        let resources = self.resources.read().ok()?;
        let resource = resources.get(id)?.clone();
        
        // Update access statistics
 drop(resources); // Release read lock
        self.update_resource_access(id)?;
        
        Some(resource)
    }

    /// Check if a resource is loaded
    pub fn is_resource_loaded(&self, id: &str) -> bool {
        let resources = self.resources.read().ok()?;
        resources.get(id).map(|r| r.state == ResourceState::Loaded).unwrap_or(false)
    }

    /// Unload a resource
    pub fn unload_resource(&self, id: &str) -> Result<(), ResourceError> {
        let mut resources = self.resources.write().map_err(|_| ResourceError::PoisonedLock)?;
        
        if let Some(resource) = resources.remove(id) {
            // Remove from cache
            let mut cache = self.cache.lock().map_err(|_| ResourceError::PoisonedLock)?;
            cache.entries.remove(id);
            cache.current_size = cache.current_size.saturating_sub(resource.size);

            // Update stats
            let mut stats = self.stats.lock().map_err(|_| ResourceError::PoisonedLock)?;
            stats.loaded_resources = stats.loaded_resources.saturating_sub(1);
            stats.total_resources = stats.total_resources.saturating_sub(1);

            println!("Unloaded resource: {}", id);
            Ok(())
        } else {
            Err(ResourceError::ResourceNotFound)
        }
    }

    /// Preload a set of resources
    pub fn preload_resources(&self, requests: Vec<ResourceRequest>) -> Result<(), ResourceError> {
        if !self.settings.enable_preloading {
            return Err(ResourceError::PreloadingDisabled);
        }

        // Process preloading in parallel
        requests.into_par_iter().for_each(|request| {
            let _ = self.load_resource_async(request);
        });

        Ok(())
    }

    /// Get all loaded resources
    pub fn get_all_resources(&self) -> Vec<Resource> {
        self.resources.read().ok()
            .map(|resources| resources.values().cloned().collect())
            .unwrap_or_default()
    }

    /// Get resources by type
    pub fn get_resources_by_type(&self, resource_type: ResourceType) -> Vec<Resource> {
        self.resources.read().ok()
            .map(|resources| {
                resources.values()
                    .filter(|r| r.resource_type == resource_type)
                    .cloned()
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Clear all resources
    pub fn clear_all_resources(&self) -> Result<(), ResourceError> {
        let mut resources = self.resources.write().map_err(|_| ResourceError::PoisonedLock)?;
        let mut cache = self.cache.lock().map_err(|_| ResourceError::PoisonedLock)?;
        
        resources.clear();
        cache.entries.clear();
        cache.current_size = 0;

        // Reset stats
        let mut stats = self.stats.lock().map_err(|_| ResourceError::PoisonedLock)?;
        *stats = ResourceStats {
            total_resources: 0,
            loaded_resources: 0,
            loading_resources: 0,
            error_resources: 0,
            cache_hit_rate: 0.0,
            total_cache_size: 0,
            total_memory_usage: 0,
            average_load_time: Duration::from_millis(0),
            peak_concurrent_loads: 0,
        };

        Ok(())
    }

    /// Get resource statistics
    pub fn get_stats(&self) -> Result<ResourceStats, ResourceError> {
        self.stats.lock().map_err(|_| ResourceError::PoisonedLock).cloned()
    }

    /// Enable hot reload for a resource
    pub fn enable_hot_reload(&self, id: &str) -> Result<(), ResourceError> {
        if !self.settings.enable_hot_reload {
            return Err(ResourceError::HotReloadDisabled);
        }

        // In a real implementation, this would set up file watching
        println!("Hot reload enabled for resource: {}", id);
        Ok(())
    }

    /// Compress a resource
    pub fn compress_resource(&self, id: &str) -> Result<(), ResourceError> {
        if !self.settings.compression_enabled {
            return Err(ResourceError::CompressionDisabled);
        }

        let mut resources = self.resources.write().map_err(|_| ResourceError::PoisonedLock)?;
        
        if let Some(resource) = resources.get_mut(id) {
            // In a real implementation, this would compress the resource data
            println!("Compressed resource: {}", id);
            Ok(())
        } else {
            Err(ResourceError::ResourceNotFound)
        }
    }

    /// Calculate resource checksum
    pub fn calculate_checksum(&self, path: &Path) -> Result<String, ResourceError> {
        let mut file = File::open(path).map_err(|e| ResourceError::FileSystemError(e.to_string()))?;
        let mut buffer = [0; 8192];
        let mut hasher = crc32fast::Hasher::new();
        
        loop {
            let bytes_read = file.read(&mut buffer).map_err(|e| ResourceError::FileSystemError(e.to_string()))?;
            if bytes_read == 0 {
                break;
            }
            hasher.update(&buffer[..bytes_read]);
        }
        
        Ok(format!("{:08x}", hasher.finalize()))
    }

    /// Internal methods

    fn load_from_cache(&self, id: &str) -> Result<Option<(Resource, Vec<u8>)>, ResourceError> {
        let mut cache = self.cache.lock().map_err(|_| ResourceError::PoisonedLock)?;
        
        if let Some(entry) = cache.entries.get_mut(id) {
            // Check TTL
            if Instant::now().duration_since(entry.created_at) > cache.config.ttl {
                cache.entries.remove(id);
                cache.current_size = cache.current_size.saturating_sub(entry.size);
                return Ok(None);
            }

            // Update access statistics
            entry.last_access = Instant::now();
            entry.access_count += 1;
            
            // Get resource
            let resources = self.resources.read().map_err(|_| ResourceError::PoisonedLock)?;
            let resource = resources.get(id).cloned();

            match resource {
                Some(resource) => Ok(Some((resource, entry.data.clone()))),
                None => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    fn insert_into_cache(&self, id: &str, data: &[u8]) -> Result<(), ResourceError> {
        let mut cache = self.cache.lock().map_err(|_| ResourceError::PoisonedLock)?;

        // Check cache size limits
        if cache.entries.len() >= cache.config.max_entries as usize || 
           cache.current_size + data.len() as u64 > cache.config.max_size {
            self.evict_cache_entries(&mut cache)?;
        }

        let entry = CacheEntry {
            resource_id: id.to_string(),
            data: data.to_vec(),
            size: data.len() as u64,
            created_at: Instant::now(),
            last_access: Instant::now(),
            access_count: 1,
        };

        cache.entries.insert(id.to_string(), entry);
        cache.current_size += data.len() as u64;

        Ok(())
    }

    fn load_from_filesystem(&self, path: &Path, id: &str, resource_type: ResourceType) -> Result<(Resource, Vec<u8>), ResourceError> {
        let full_path = self.settings.base_path.join(path);
        
        // Check if file exists
        if !full_path.exists() {
            return Err(ResourceError::FileNotFound(full_path.to_string_lossy().to_string()));
        }

        // Read file
        let mut file = File::open(&full_path).map_err(|e| ResourceError::FileSystemError(e.to_string()))?;
        let mut data = Vec::new();
        file.read_to_end(&mut data).map_err(|e| ResourceError::FileSystemError(e.to_string()))?;

        // Get file size
        let metadata = file.metadata().map_err(|e| ResourceError::FileSystemError(e.to_string()))?;
        let size = metadata.len();

        // Calculate checksum if enabled
        let checksum = if self.settings.checksum_verification {
            self.calculate_checksum(&full_path)?
        } else {
            String::new()
        };

        // Create resource
        let resource = Resource {
            id: id.to_string(),
            name: path.file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or(id)
                .to_string(),
            resource_type,
            path: path.to_path_buf(),
            size,
            checksum,
            state: ResourceState::Loaded,
            loaded_at: Some(Instant::now()),
            access_count: 1,
            last_access: Some(Instant::now()),
            metadata: self.extract_metadata(&full_path, &data, &resource_type)?,
        };

        Ok((resource, data))
    }

    fn insert_resource(&self, resource: Resource, data: Vec<u8>) -> Result<(), ResourceError> {
        let mut resources = self.resources.write().map_err(|_| ResourceError::PoisonedLock)?;
        
        resources.insert(resource.id.clone(), resource.clone());
        
        // Update stats
        let mut stats = self.stats.lock().map_err(|_| ResourceError::PoisonedLock)?;
        stats.total_resources += 1;
        stats.loaded_resources += 1;
        stats.total_memory_usage += resource.size;

        println!("Loaded resource: {} ({} bytes)", resource.name, resource.size);
        Ok(())
    }

    fn extract_metadata(&self, path: &Path, data: &[u8], resource_type: &ResourceType) -> Result<ResourceMetadata, ResourceError> {
        let mut metadata = ResourceMetadata {
            width: None,
            height: None,
            format: None,
            duration: None,
            sample_rate: None,
            channels: None,
            vertex_count: None,
            triangle_count: None,
            custom_data: HashMap::new(),
        };

        // Extract metadata based on resource type
        match resource_type {
            ResourceType::Texture => {
                // Simplified texture metadata extraction
                metadata.format = path.extension()
                    .and_then(|ext| ext.to_str())
                    .map(|s| s.to_string());
            },
            ResourceType::Audio => {
                // Simplified audio metadata extraction
                metadata.format = path.extension()
                    .and_then(|ext| ext.to_str())
                    .map(|s| s.to_string());
                metadata.duration = Some(Duration::from_secs(30)); // Mock duration
            },
            ResourceType::Model => {
                // Simplified model metadata extraction
                metadata.vertex_count = Some(1000); // Mock vertex count
                metadata.triangle_count = Some(500); // Mock triangle count
            },
            _ => {},
        }

        Ok(metadata)
    }

    fn update_resource_access(&self, id: &str) -> Result<(), ResourceError> {
        let mut resources = self.resources.write().map_err(|_| ResourceError::PoisonedLock)?;
        
        if let Some(resource) = resources.get_mut(id) {
            resource.access_count += 1;
            resource.last_access = Some(Instant::now());
        }

        Ok(())
    }

    fn update_stats(&self, load_time: Duration, from_cache: bool) -> Result<(), ResourceError> {
        let mut stats = self.stats.lock().map_err(|_| ResourceError::PoisonedLock)?;
        
        if from_cache {
            // Update cache hit rate (simplified)
            stats.cache_hit_rate = (stats.cache_hit_rate * 0.9) + 0.1;
        }

        // Update average load time
        let current_avg = stats.average_load_time;
        let new_avg = Duration::from_nanos(
            (current_avg.as_nanos() as f64 * 0.9 + load_time.as_nanos() as f64 * 0.1) as u64
        );
        stats.average_load_time = new_avg;

        Ok(())
    }

    fn evict_cache_entries(&self, cache: &mut ResourceCache) -> Result<(), ResourceError> {
        // Simple LRU eviction
        while cache.entries.len() > 0 && 
              (cache.entries.len() >= cache.config.max_entries as usize || 
               cache.current_size > cache.config.max_size) {
            
            // Find least recently used entry
            let mut lru_id = None;
            let mut lru_time = Instant::now();
            
            for (id, entry) in &cache.entries {
                if entry.last_access < lru_time {
                    lru_time = entry.last_access;
                    lru_id = Some(id.clone());
                }
            }
            
            if let Some(id) = lru_id {
                if let Some(entry) = cache.entries.remove(&id) {
                    cache.current_size = cache.current_size.saturating_sub(entry.size);
                }
            } else {
                break;
            }
        }

        Ok(())
    }

    fn start_loading_workers(&self) {
        let load_queue = self.load_queue.clone();
        let resources = self.resources.clone();
        let settings = self.settings.clone();
        let stats = self.stats.clone();

        // Start worker threads
        for _ in 0..settings.max_concurrent_loads {
            let load_queue = load_queue.clone();
            let resources = resources.clone();
            let settings = settings.clone();
            let stats = stats.clone();

            std::thread::spawn(move || {
                Self::loading_worker(load_queue, resources, settings, stats);
            });
        }
    }

    fn loading_worker(
        load_queue: Arc<Mutex<Vec<ResourceRequest>>>,
        resources: Arc<RwLock<HashMap<String, Resource>>>,
        settings: ResourceManagerSettings,
        stats: Arc<Mutex<ResourceStats>>,
    ) {
        loop {
            // Get next request
            let request = {
                let mut queue = load_queue.lock().unwrap();
                queue.pop()
            };

            if let Some(request) = request {
                // Process loading
                let start_time = Instant::now();
                
                // Update stats
                {
                    let mut stats_guard = stats.lock().unwrap();
                    stats_guard.loading_resources += 1;
                }

                // Load resource
                let result = Self::load_worker_resource(&request, &settings);
                
                match result {
                    Ok((resource, data)) => {
                        // Store result
                        {
                            let mut resources_guard = resources.write().unwrap();
                            resources_guard.insert(resource.id.clone(), resource);
                        }

                        // Update stats
                        {
                            let mut stats_guard = stats.lock().unwrap();
                            stats_guard.loading_resources = stats_guard.loading_resources.saturating_sub(1);
                            stats_guard.loaded_resources += 1;
                        }

                        // Execute callback
                        if let Some(callback) = &request.callback {
                            callback(Ok(resource));
                        }
                    },
                    Err(error) => {
                        // Handle error
                        {
                            let mut stats_guard = stats.lock().unwrap();
                            stats_guard.loading_resources = stats_guard.loading_resources.saturating_sub(1);
                            stats_guard.error_resources += 1;
                        }

                        if let Some(callback) = &request.callback {
                            callback(Err(error));
                        }
                    }
                }

                let load_time = start_time.elapsed();
                println!("Async load completed in {:?}", load_time);
            } else {
                // No work, sleep briefly
                std::thread::sleep(Duration::from_millis(10));
            }
        }
    }

    fn load_worker_resource(request: &ResourceRequest, settings: &ResourceManagerSettings) -> Result<(Resource, Vec<u8>), ResourceError> {
        let full_path = settings.base_path.join(&request.path);
        
        if !full_path.exists() {
            return Err(ResourceError::FileNotFound(full_path.to_string_lossy().to_string()));
        }

        let mut file = File::open(&full_path).map_err(|e| ResourceError::FileSystemError(e.to_string()))?;
        let mut data = Vec::new();
        file.read_to_end(&mut data).map_err(|e| ResourceError::FileSystemError(e.to_string()))?;

        let metadata = file.metadata().map_err(|e| ResourceError::FileSystemError(e.to_string()))?;
        let size = metadata.len();

        let resource = Resource {
            id: request.id.clone(),
            name: request.path.file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or(&request.id)
                .to_string(),
            resource_type: request.resource_type.clone(),
            path: request.path.clone(),
            size,
            checksum: String::new(),
            state: ResourceState::Loaded,
            loaded_at: Some(Instant::now()),
            access_count: 0,
            last_access: None,
            metadata: ResourceMetadata {
                width: None,
                height: None,
                format: None,
                duration: None,
                sample_rate: None,
                channels: None,
                vertex_count: None,
                triangle_count: None,
                custom_data: HashMap::new(),
            },
        };

        Ok((resource, data))
    }
}

/// Resource error types
#[derive(Debug, thiserror::Error)]
pub enum ResourceError {
    #[error("Resource not found")]
    ResourceNotFound,
    
    #[error("File not found: {0}")]
    FileNotFound(String),
    
    #[error("File system error: {0}")]
    FileSystemError(String),
    
    #[error("Resource already loading")]
    AlreadyLoading,
    
    #[error("Poisoned lock")]
    PoisonedLock,
    
    #[error("Preloading disabled")]
    PreloadingDisabled,
    
    #[error("Hot reload disabled")]
    HotReloadDisabled,
    
    #[error("Compression disabled")]
    CompressionDisabled,
    
    #[error("Invalid resource type")]
    InvalidResourceType,
    
    #[error("Resource loading failed: {0}")]
    LoadingFailed(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resource_creation() {
        let resource = Resource {
            id: "test".to_string(),
            name: "Test Resource".to_string(),
            resource_type: ResourceType::Texture,
            path: PathBuf::from("textures/test.png"),
            size: 1024,
            checksum: "abc123".to_string(),
            state: ResourceState::Loaded,
            loaded_at: Some(Instant::now()),
            access_count: 1,
            last_access: Some(Instant::now()),
            metadata: ResourceMetadata {
                width: Some(512),
                height: Some(512),
                format: Some("png".to_string()),
                duration: None,
                sample_rate: None,
                channels: None,
                vertex_count: None,
                triangle_count: None,
                custom_data: HashMap::new(),
            },
        };

        assert_eq!(resource.id, "test");
        assert_eq!(resource.resource_type, ResourceType::Texture);
        assert_eq!(resource.size, 1024);
    }

    #[test]
    fn test_cache_operations() {
        let config = CacheConfig {
            max_size: 1024 * 1024, // 1MB
            max_entries: 100,
            ttl: Duration::from_secs(3600),
            enable_compression: false,
            enable_deduplication: true,
        };

        let mut cache = ResourceCache {
            entries: HashMap::new(),
            config,
            current_size: 0,
            access_order: Vec::new(),
        };

        let entry = CacheEntry {
            resource_id: "test".to_string(),
            data: vec![1, 2, 3, 4, 5],
            size: 5,
            created_at: Instant::now(),
            last_access: Instant::now(),
            access_count: 1,
        };

        cache.entries.insert("test".to_string(), entry);
        cache.current_size = 5;

        assert_eq!(cache.entries.len(), 1);
        assert_eq!(cache.current_size, 5);
    }
}
