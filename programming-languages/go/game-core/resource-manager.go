 package main

import (
	"fmt"
	"sync"
	"time"
)

// ResourceManager handles loading, caching, and managing game resources
type ResourceManager struct {
	textures    map[string]*TextureResource
	sounds      map[string]*SoundResource
	models      map[string]*ModelResource
	fonts       map[string]*FontResource
	shaders     map[string]*ShaderResource
	data        map[string]*DataResource
	cache       *ResourceCache
	loader      *ResourceLoader
	settings    ResourceManagerSettings
	mutex       sync.RWMutex
	isRunning   bool
}

// TextureResource represents a texture resource
type TextureResource struct {
	ID          string
	Name        string
	Data        []byte
	Width       int
	Height      int
	Format      string
	Filter      TextureFilter
	Compression TextureCompression
	Loaded      bool
	LoadTime    time.Time
	AccessTime  time.Time
	ReferenceCount int
}

// SoundResource represents an audio resource
type SoundResource struct {
	ID          string
	Name        string
	Data        []byte
	Format      AudioFormat
	SampleRate  int
	Channels    int
	BitDepth    int
	Duration    time.Duration
	Loaded      bool
	LoadTime    time.Time
	AccessTime  time.Time
	ReferenceCount int
}

// ModelResource represents a 3D model resource
type ModelResource struct {
	ID          string
	Name        string
	Vertices    []Vector3
	Normals     []Vector3
	UVs         []Vector2
	Indices     []uint32
	Materials   []Material
	Animations  []Animation
	BoundingBox BoundingBox
	Loaded      bool
	LoadTime    time.Time
	AccessTime  time.Time
	ReferenceCount int
}

// FontResource represents a font resource
type FontResource struct {
	ID          string
	Name        string
	Glyphs      map[rune]Glyph
	LineHeight  float32
	BaseHeight  float32
	Loaded      bool
	LoadTime    time.Time
	AccessTime  time.Time
	ReferenceCount int
}

// ShaderResource represents a shader resource
type ShaderResource struct {
	ID          string
	Name        string
	Vertex      string
	Fragment    string
	Geometry    string
	Compute     string
	Uniforms    map[string]Uniform
	Loaded      bool
	LoadTime    time.Time
	AccessTime  time.Time
	ReferenceCount int
}

// DataResource represents generic data resource
type DataResource struct {
	ID          string
	Name        string
	Data        []byte
	Format      string
	Compressed  bool
	Loaded      bool
	LoadTime    time.Time
	AccessTime  time.Time
	ReferenceCount int
}

// ResourceCache manages resource caching
type ResourceCache struct {
	resources   map[string]CachedResource
	usage       map[string]ResourceUsage
	maxSize     int64
	currentSize int64
	mutex       sync.RWMutex
}

// CachedResource represents a cached resource
type CachedResource struct {
	ID          string
	Size        int64
	LoadTime    time.Time
	AccessTime  time.Time
	Compressed  bool
}

// ResourceUsage tracks resource usage
type ResourceUsage struct {
	ID           string
	LoadCount    int
	LastAccess   time.Time
	AvgLoadTime  time.Duration
	MemoryUsage  int64
}

// ResourceLoader handles async resource loading
type ResourceLoader struct {
	queue       chan LoadRequest
	results     chan LoadResult
	workers     int
	mutex       sync.RWMutex
	isRunning   bool
}

// LoadRequest represents a resource loading request
type LoadRequest struct {
	ID          string
	Type        ResourceType
	Path        string
	Parameters  map[string]interface{}
	Callback    func(result LoadResult)
	Priority    int
}

// LoadResult represents a loading result
type LoadResult struct {
	ID        string
	Success   bool
	Data      interface{}
	Error     error
	LoadTime  time.Duration
	MemorySize int64
}

// ResourceManagerSettings contains configuration
type ResourceManagerSettings struct {
	CacheSize       int64
	MaxResources    int
	WorkerCount     int
	EnableCompression bool
	EnableAsyncLoading bool
	PreloadOnStart   bool
	CompressionLevel int
	AutoCleanup      bool
	CleanupInterval  time.Duration
	EnableLogging    bool
}

// Supporting types
type Vector2 struct {
	X, Y float32
}

type Vector3 struct {
	X, Y, Z float32
}

type Material struct {
	Name       string
	Diffuse    Color4
	Specular   Color4
	Ambient    Color4
	Emissive   Color4
	Shininess  float32
}

type Color4 struct {
	R, G, B, A float32
}

type Animation struct {
	Name       string
	Duration   time.Duration
	Frames     []AnimationFrame
	Loop       bool
}

type AnimationFrame struct {
	Time      time.Duration
	Transform Transform
}

type Transform struct {
	Position Vector3
	Rotation Vector3
	Scale    Vector3
}

type Glyph struct {
	Char       rune
	Advance    float32
	Bearing    Vector2
	TextureRect Rectangle
}

type Rectangle struct {
	X, Y, Width, Height float32
}

type Uniform struct {
	Name     string
	Type     UniformType
	Location int
}

type UniformType string

const (
	UniformFloat   UniformType = "float"
	UniformVec2    UniformType = "vec2"
	UniformVec3    UniformType = "vec3"
	UniformVec4    UniformType = "vec4"
	UniformMat3    UniformType = "mat3"
	UniformMat4    UniformType = "mat4"
	UniformSampler UniformType = "sampler2D"
)

type TextureFilter int

const (
	NearestFilter     TextureFilter = iota
	LinearFilter
	TrilinearFilter
	AnisotropicFilter
)

type TextureCompression int

const (
	NoCompression   TextureCompression = iota
	BC1Compression
	BC3Compression
	BC5Compression
	ASTCCompression
)

type AudioFormat int

const (
	PCMFormat   AudioFormat = iota
	OGGFormat
	MP3Format
	FLACFormat
)

type ResourceType string

const (
	TextureResource  ResourceType = "texture"
	SoundResource    ResourceType = "sound"
	ModelResource    ResourceType = "model"
	FontResource     ResourceType = "font"
	ShaderResource   ResourceType = "shader"
	DataResource     ResourceType = "data"
)

type BoundingBox struct {
	Min Vector3
	Max Vector3
}

// NewResourceManager creates a new resource manager
func NewResourceManager(settings ResourceManagerSettings) *ResourceManager {
	rm := &ResourceManager{
		textures:  make(map[string]*TextureResource),
		sounds:    make(map[string]*SoundResource),
		models:    make(map[string]*ModelResource),
		fonts:     make(map[string]*FontResource),
		shaders:   make(map[string]*ShaderResource),
		data:      make(map[string]*DataResource),
		cache:     NewResourceCache(settings.CacheSize),
		loader:    NewResourceLoader(settings.WorkerCount),
		settings:  settings,
		isRunning: false,
	}

	return rm
}

// Start starts the resource manager
func (rm *ResourceManager) Start() error {
	rm.mutex.Lock()
	defer rm.mutex.Unlock()

	rm.isRunning = true

	// Start resource cache
	rm.cache.Start()

	// Start resource loader
	if rm.settings.EnableAsyncLoading {
		rm.loader.Start()
	}

	// Start cleanup timer if enabled
	if rm.settings.AutoCleanup {
		go rm.cleanupLoop()
	}

	fmt.Println("Resource Manager started")
	return nil
}

// Stop stops the resource manager
func (rm *ResourceManager) Stop() {
	rm.mutex.Lock()
	defer rm.mutex.Unlock()

	rm.isRunning = false

	// Stop resource cache
	rm.cache.Stop()

	// Stop resource loader
	if rm.settings.EnableAsyncLoading {
		rm.loader.Stop()
	}

	// Cleanup all resources
	rm.cleanupAll()

	fmt.Println("Resource Manager stopped")
}

// LoadTexture loads a texture resource
func (rm *ResourceManager) LoadTexture(id, path string, params map[string]interface{}) error {
	rm.mutex.Lock()
	defer rm.mutex.Unlock()

	if !rm.isRunning {
		return fmt.Errorf("resource manager is not running")
	}

	if _, exists := rm.textures[id]; exists {
		return fmt.Errorf("texture already exists: %s", id)
	}

	texture := &TextureResource{
		ID:          id,
		Name:        id,
		Width:       params["width"].(int),
		Height:      params["height"].(int),
		Format:      params["format"].(string),
		Filter:      params["filter"].(TextureFilter),
		Compression: params["compression"].(TextureCompression),
		Loaded:      false,
		LoadTime:    time.Now(),
		AccessTime:  time.Now(),
	}

	rm.textures[id] = texture

	// Load asynchronously if enabled
	if rm.settings.EnableAsyncLoading {
		return rm.loader.QueueLoad(LoadRequest{
			ID:         id,
			Type:       TextureResource,
			Path:       path,
			Parameters: params,
		})
	}

	// Load synchronously
	return rm.loadTextureSync(texture, path)

// LoadSound loads a sound resource
func (rm *ResourceManager) LoadSound(id, path string, params map[string]interface{}) error {
	rm.mutex.Lock()
	defer rm.mutex.Unlock()

	if !rm.isRunning {
		return fmt.Errorf("resource manager is not running")
	}

	if _, exists := rm.sounds[id]; exists {
		return fmt.Errorf("sound already exists: %s", id)
	}

	sound := &SoundResource{
		ID:          id,
		Name:        id,
		Format:      params["format"].(AudioFormat),
		SampleRate:  params["sample_rate"].(int),
		Channels:    params["channels"].(int),
		BitDepth:    params["bit_depth"].(int),
		Duration:    params["duration"].(time.Duration),
		Loaded:      false,
		LoadTime:    time.Now(),
		AccessTime:  time.Now(),
	}

	rm.sounds[id] = sound

	if rm.settings.EnableAsyncLoading {
		return rm.loader.QueueLoad(LoadRequest{
			ID:   id,
			Type: SoundResource,
			Path: path,
			Parameters: params,
		})
	}

	return rm.loadSoundSync(sound, path)
}

// LoadModel loads a 3D model resource
func (rm *ResourceManager) LoadModel(id, path string, params map[string]interface{}) error {
	rm.mutex.Lock()
	defer rm.mutex.Unlock()

	if !rm.isRunning {
		return fmt.Errorf("resource manager is not running")
	}

	if _, exists := rm.models[id]; exists {
		return fmt.Errorf("model already exists: %s", id)
	}

	model := &ModelResource{
		ID:          id,
		Name:        id,
		Loaded:      false,
		LoadTime:    time.Now(),
		AccessTime:  time.Now(),
	}

	rm.models[id] = model

	if rm.settings.EnableAsyncLoading {
		return rm.loader.QueueLoad(LoadRequest{
			ID:   id,
			Type: ModelResource,
			Path: path,
			Parameters: params,
		})
	}

	return rm.loadModelSync(model, path)
}

// GetTexture retrieves a texture resource
func (rm *ResourceManager) GetTexture(id string) (*TextureResource, error) {
	rm.mutex.RLock()
	defer rm.mutex.RUnlock()

	texture, exists := rm.textures[id]
	if !exists {
		return nil, fmt.Errorf("texture not found: %s", id)
	}

	texture.AccessTime = time.Now()
	return texture, nil
}

// GetSound retrieves a sound resource
func (rm *ResourceManager) GetSound(id string) (*SoundResource, error) {
	rm.mutex.RLock()
	defer rm.mutex.RUnlock()

	sound, exists := rm.sounds[id]
	if !exists {
		return nil, fmt.Errorf("sound not found: %s", id)
	}

	sound.AccessTime = time.Now()
	return sound, nil
}

// GetModel retrieves a model resource
func (rm *ResourceManager) GetModel(id string) (*ModelResource, error) {
	rm.mutex.RLock()
	defer rm.mutex.RUnlock()

	model, exists := rm.models[id]
	if !exists {
		return nil, fmt.Errorf("model not found: %s", id)
	}

	model.AccessTime = time.Now()
	return model, nil
}

// UnloadTexture unloads a texture resource
func (rm *ResourceManager) UnloadTexture(id string) error {
	rm.mutex.Lock()
	defer rm.mutex.Unlock()

	texture, exists := rm.textures[id]
	if !exists {
		return fmt.Errorf("texture not found: %s", id)
	}

	if texture.ReferenceCount > 0 {
		return fmt.Errorf("cannot unload texture with references: %s", id)
	}

	delete(rm.textures, id)
	rm.cache.Remove(id)

	return nil
}

// UnloadSound unloads a sound resource
func (rm *ResourceManager) UnloadSound(id string) error {
	rm.mutex.Lock()
	defer rm.mutex.Unlock()

	sound, exists := rm.sounds[id]
	if !exists {
		return fmt.Errorf("sound not found: %s", id)
	}

	if sound.ReferenceCount > 0 {
		return fmt.Errorf("cannot unload sound with references: %s", id)
	}

	delete(rm.sounds, id)
	rm.cache.Remove(id)

	return nil
}

// GetResourceStats returns resource manager statistics
func (rm *ResourceManager) GetResourceStats() ResourceStats {
	rm.mutex.RLock()
	defer rm.mutex.RUnlock()

	textureCount := len(rm.textures)
	soundCount := len(rm.sounds)
	modelCount := len(rm.models)
	fontCount := len(rm.fonts)
	shaderCount := len(rm.shaders)
	dataCount := len(rm.data)

	return ResourceStats{
		TotalTextures:    textureCount,
		TotalSounds:      soundCount,
		TotalModels:      modelCount,
		TotalFonts:       fontCount,
		TotalShaders:     shaderCount,
		TotalData:        dataCount,
		TotalResources:   textureCount + soundCount + modelCount + fontCount + shaderCount + dataCount,
		CacheSize:        rm.cache.GetCurrentSize(),
		CacheUsage:       rm.cache.GetUsage(),
		IsRunning:        rm.isRunning,
		AsyncLoading:     rm.settings.EnableAsyncLoading,
	}
}

// ResourceStats represents resource manager statistics
type ResourceStats struct {
	TotalTextures    int
	TotalSounds      int
	TotalModels      int
	TotalFonts       int
	TotalShaders     int
	TotalData        int
	TotalResources   int
	CacheSize        int64
	CacheUsage       float32
	IsRunning        bool
	AsyncLoading     bool
}

// ResourceCache implementation
func NewResourceCache(maxSize int64) *ResourceCache {
	return &ResourceCache{
		resources:  make(map[string]CachedResource),
		usage:      make(map[string]ResourceUsage),
		maxSize:    maxSize,
		currentSize: 0,
	}
}

func (rc *ResourceCache) Start() {
	// Initialize cache
}

func (rc *ResourceCache) Stop() {
	// Cleanup cache
}

func (rc *ResourceCache) Add(id string, size int64, compressed bool) {
	rc.mutex.Lock()
	defer rc.mutex.Unlock()

	rc.resources[id] = CachedResource{
		ID:          id,
		Size:        size,
		LoadTime:    time.Now(),
		AccessTime:  time.Now(),
		Compressed:  compressed,
	}

	rc.currentSize += size

	// Evict if over limit
	if rc.currentSize > rc.maxSize {
		rc.evictLRU()
	}
}

func (rc *ResourceCache) Remove(id string) {
	rc.mutex.Lock()
	defer rc.mutex.Unlock()

	if resource, exists := rc.resources[id]; exists {
		rc.currentSize -= resource.Size
		delete(rc.resources, id)
		delete(rc.usage, id)
	}
}

func (rc *ResourceCache) GetCurrentSize() int64 {
	return rc.currentSize
}

func (rc *ResourceCache) GetUsage() float32 {
	if rc.maxSize == 0 {
		return 0.0
	}
	return float32(rc.currentSize) / float32(rc.maxSize)
}

func (rc *ResourceCache) evictLRU() {
	// Simple LRU eviction
	var oldestID string
	var oldestTime time.Time

	for id, resource := range rc.resources {
		if oldestID == "" || resource.AccessTime.Before(oldestTime) {
			oldestID = id
			oldestTime = resource.AccessTime
		}
	}

	if oldestID != "" {
		rc.Remove(oldestID)
	}
}

// ResourceLoader implementation
func NewResourceLoader(workers int) *ResourceLoader {
	return &ResourceLoader{
		queue:   make(chan LoadRequest, 100),
		results: make(chan LoadResult, 100),
		workers: workers,
	}
}

func (rl *ResourceLoader) Start() {
	rl.mutex.Lock()
	defer rl.mutex.Unlock()

	rl.isRunning = true

	// Start worker goroutines
	for i := 0; i < rl.workers; i++ {
		go rl.workerLoop(i)
	}
}

func (rl *ResourceLoader) Stop() {
	rl.mutex.Lock()
	defer rl.mutex.Unlock()

	rl.isRunning = false
	close(rl.queue)
}

func (rl *ResourceLoader) QueueLoad(request LoadRequest) error {
	rl.mutex.RLock()
	defer rl.mutex.RUnlock()

	if !rl.isRunning {
		return fmt.Errorf("resource loader is not running")
	}

	select {
	case rl.queue <- request:
		return nil
	default:
		return fmt.Errorf("load queue is full")
	}
}

func (rl *ResourceLoader) workerLoop(workerID int) {
	for request := range rl.queue {
		startTime := time.Now()
		
		// Simulate resource loading
		result := LoadResult{
			ID:       request.ID,
			Success:  true,
			Data:     make([]byte, 1024), // Mock data
			LoadTime: time.Since(startTime),
			MemorySize: 1024,
		}

		if request.Callback != nil {
			go request.Callback(result)
		}

		select {
		case rl.results <- result:
		default:
		}
	}
}

// Internal loading methods
func (rm *ResourceManager) loadTextureSync(texture *TextureResource, path string) error {
	// Simulate texture loading
	texture.Loaded = true
	texture.ReferenceCount = 1
	
	size := int64(texture.Width * texture.Height * 4) // Assume RGBA
	rm.cache.Add(texture.ID, size, texture.Compression != NoCompression)
	
	return nil
}

func (rm *ResourceManager) loadSoundSync(sound *SoundResource, path string) error {
	// Simulate sound loading
	sound.Loaded = true
	sound.ReferenceCount = 1
	
	// Estimate size based on audio properties
	samples := int(sound.SampleRate) * int(sound.Duration.Seconds()) * sound.Channels
	size := int64(samples * sound.BitDepth / 8)
	rm.cache.Add(sound.ID, size, false)
	
	return nil
}

func (rm *ResourceManager) loadModelSync(model *ModelResource, path string) error {
	// Simulate model loading
	model.Loaded = true
	model.ReferenceCount = 1
	
	// Estimate size based on vertices/indices
	vertexSize := len(model.Vertices) * 3 * 4 // 3 floats per vertex, 4 bytes each
	normalSize := len(model.Normals) * 3 * 4
	uvSize := len(model.UVs) * 2 * 4
	indiceSize := len(model.Indices) * 4
	
	size := int64(vertexSize + normalSize + uvSize + indiceSize)
	rm.cache.Add(model.ID, size, false)
	
	return nil
}

func (rm *ResourceManager) cleanupLoop() {
	ticker := time.NewTicker(rm.settings.CleanupInterval)
	defer ticker.Stop()

	for range ticker.C {
		rm.cleanupUnused()
	}
}

func (rm *ResourceManager) cleanupUnused() {
	rm.mutex.Lock()
	defer rm.mutex.Unlock()

	cutoff := time.Now().Add(-time.Hour) // Clean up unused for 1 hour

	// Cleanup textures
	for id, texture := range rm.textures {
		if texture.ReferenceCount == 0 && texture.AccessTime.Before(cutoff) {
			delete(rm.textures, id)
			rm.cache.Remove(id)
		}
	}

	// Cleanup sounds
	for id, sound := range rm.sounds {
		if sound.ReferenceCount == 0 && sound.AccessTime.Before(cutoff) {
			delete(rm.sounds, id)
			rm.cache.Remove(id)
		}
	}

	// Cleanup models
	for id, model := range rm.models {
		if model.ReferenceCount == 0 && model.AccessTime.Before(cutoff) {
			delete(rm.models, id)
			rm.cache.Remove(id)
		}
	}
}

func (rm *ResourceManager) cleanupAll() {
	rm.textures = make(map[string]*TextureResource)
	rm.sounds = make(map[string]*SoundResource)
	rm.models = make(map[string]*ModelResource)
	rm.fonts = make(map[string]*FontResource)
	rm.shaders = make(map[string]*ShaderResource)
	rm.data = make(map[string]*DataResource)
	rm.cache.resources = make(map[string]CachedResource)
	rm.cache.currentSize = 0
}

// Example usage
func main() {
	settings := ResourceManagerSettings{
		CacheSize:          100 * 1024 * 1024, // 100MB
		MaxResources:       1000,
		WorkerCount:        4,
		EnableCompression:  true,
		EnableAsyncLoading: true,
		PreloadOnStart:     false,
		CompressionLevel:   6,
		AutoCleanup:        true,
		CleanupInterval:    time.Minute * 5,
		EnableLogging:      true,
	}

	rm := NewResourceManager(settings)
	
	// Start resource manager
	if err := rm.Start(); err != nil {
		fmt.Printf("Error starting resource manager: %v\n", err)
		return
	}

	// Load some textures
	textureParams := map[string]interface{}{
		"width":       1024,
		"height":      1024,
		"format":      "RGBA",
		"filter":      LinearFilter,
		"compression": BC1Compression,
	}

	err := rm.LoadTexture("player_sprite", "/assets/textures/player.png", textureParams)
	if err != nil {
		fmt.Printf("Error loading texture: %v\n", err)
	}

	// Load some sounds
	soundParams := map[string]interface{}{
		"format":     OGGFormat,
		"sample_rate": 44100,
		"channels":   2,
		"bit_depth":  16,
		"duration":   time.Second * 3,
	}

	err = rm.LoadSound("jump_sound", "/assets/sounds/jump.ogg", soundParams)
	if err != nil {
		fmt.Printf("Error loading sound: %v\n", err)
	}

	// Load a model
	modelParams := map[string]interface{}{
		"scale":       1.0,
		"lod_levels":  3,
		"compression": false,
	}

	err = rm.LoadModel("cube_model", "/assets/models/cube.obj", modelParams)
	if err != nil {
		fmt.Printf("Error loading model: %v\n", err)
	}

	// Retrieve resources
	texture, err := rm.GetTexture("player_sprite")
	if err != nil {
		fmt.Printf("Error getting texture: %v\n", err)
	} else {
		fmt.Printf("Loaded texture: %dx%d %s\n", texture.Width, texture.Height, texture.Format)
	}

	sound, err := rm.GetSound("jump_sound")
	if err != nil {
		fmt.Printf("Error getting sound: %v\n", err)
	} else {
		fmt.Printf("Loaded sound: %s, duration: %v\n", sound.Format, sound.Duration)
	}

	model, err := rm.GetModel("cube_model")
	if err != nil {
		fmt.Printf("Error getting model: %v\n", err)
	} else {
		fmt.Printf("Loaded model: %d vertices\n", len(model.Vertices))
	}

	// Get statistics
	stats := rm.GetResourceStats()
	fmt.Printf("\nResource Manager Statistics:\n")
	fmt.Printf("Total Resources: %d\n", stats.TotalResources)
	fmt.Printf("Cache Size: %.2f MB\n", float64(stats.CacheSize)/(1024*1024))
	fmt.Printf("Cache Usage: %.1f%%\n", stats.CacheUsage*100)
	fmt.Printf("Async Loading: %v\n", stats.AsyncLoading)

	// Cleanup
	rm.UnloadTexture("player_sprite")
	rm.UnloadSound("jump_sound")

	// Stop resource manager
	rm.Stop()

	fmt.Println("\nResource Manager initialized successfully!")
}
