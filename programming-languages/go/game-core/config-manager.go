package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"sync"
	"time"
)

// ConfigManager handles configuration loading, validation, and management
type ConfigManager struct {
	configs    map[string]*Config
	fileWatcher *FileWatcher
	validator  *ConfigValidator
	settings   ConfigManagerSettings
	mutex      sync.RWMutex
	isRunning  bool
}

// Config represents a configuration section
type Config struct {
	Name        string
	Data        map[string]interface{}
	Source      ConfigSource
	LoadedAt    time.Time
	ModifiedAt  time.Time
	Version     string
	Environment string
	Validated   bool
	Errors      []ConfigError
}

// ConfigSource represents the source of a configuration
type ConfigSource struct {
	Type     SourceType
	Path     string
	URL      string
	Priority int
	Watch    bool
}

// ConfigValidator validates configuration data
type ConfigValidator struct {
	rules     map[string]*ValidationRule
	schemas   map[string]*ConfigSchema
	mutex     sync.RWMutex
}

// ValidationRule defines validation rules for config values
type ValidationRule struct {
	Path       string
	Type       ValueType
	Required   bool
	Min        interface{}
	Max        interface{}
	Pattern    string
	CustomFunc func(interface{}) error
	Messages   map[string]string
}

// ConfigSchema defines the expected structure of a config
type ConfigSchema struct {
	Name        string
	Version     string
	Required    []string
	Optional    []string
	Types       map[string]ValueType
	Validation  []*ValidationRule
}

// ConfigError represents a configuration error
type ConfigError struct {
	Path    string
	Message string
	Type    ErrorType
	Line    int
	Column  int
}

// FileWatcher monitors configuration files for changes
type FileWatcher struct {
	watchers   map[string]*FileWatchEntry
	callback   func(string, ConfigSource)
	mutex      sync.RWMutex
	isRunning  bool
}

// FileWatchEntry represents a file being watched
type FileWatchEntry struct {
	Path        string
	LastModTime time.Time
	Source      ConfigSource
}

// ConfigManagerSettings contains configuration settings
type ConfigManagerSettings struct {
	AutoReload     bool
	ReloadDelay    time.Duration
	DefaultPath    string
	WatchInterval  time.Duration
	MaxConfigs     int
	ValidateOnLoad bool
	BackupConfigs  bool
	BackupPath     string
	EnableLogging  bool
}

// SourceType represents configuration source types
type SourceType string

const (
	FileSource   SourceType = "file"
	DatabaseSource SourceType = "database"
	APISource    SourceType = "api"
	MemorySource SourceType = "memory"
)

// ValueType represents configuration value types
type ValueType string

const (
	StringType  ValueType = "string"
	IntType     ValueType = "int"
	FloatType   ValueType = "float"
	BoolType    ValueType = "bool"
	ArrayType   ValueType = "array"
	ObjectType  ValueType = "object"
	AnyType     ValueType = "any"
)

// ErrorType represents configuration error types
type ErrorType string

const (
	ValidationError ErrorType = "validation"
	ParseError      ErrorType = "parse"
	IOError         ErrorType = "io"
	SchemaError     ErrorType = "schema"
	MissingError    ErrorType = "missing"
)

// NewConfigManager creates a new configuration manager
func NewConfigManager(settings ConfigManagerSettings) *ConfigManager {
	return &ConfigManager{
		configs:     make(map[string]*Config),
		fileWatcher: NewFileWatcher(settings.WatchInterval, settings.DefaultPath),
		validator:   NewConfigValidator(),
		settings:    settings,
		isRunning:   false,
	}
}

// Start starts the configuration manager
func (cm *ConfigManager) Start() error {
	cm.mutex.Lock()
	defer cm.mutex.Unlock()

	cm.isRunning = true

	// Start file watcher
	if cm.settings.AutoReload {
		cm.fileWatcher.Start(cm.handleFileChange)
	}

	// Load default configurations
	if cm.settings.DefaultPath != "" {
		err := cm.loadDefaultConfigs(cm.settings.DefaultPath)
		if err != nil && cm.settings.EnableLogging {
			fmt.Printf("Error loading default configs: %v\n", err)
		}
	}

	fmt.Println("Configuration Manager started")
	return nil
}

// Stop stops the configuration manager
func (cm *ConfigManager) Stop() {
	cm.mutex.Lock()
	defer cm.mutex.Unlock()

	cm.isRunning = false

	// Stop file watcher
	cm.fileWatcher.Stop()

	// Save any dirty configs
	cm.saveDirtyConfigs()

	cm.configs = make(map[string]*Config)

	fmt.Println("Configuration Manager stopped")
}

// LoadConfig loads a configuration from a source
func (cm *ConfigManager) LoadConfig(name string, source ConfigSource) error {
	cm.mutex.Lock()
	defer cm.mutex.Unlock()

	if !cm.isRunning {
		return fmt.Errorf("config manager is not running")
	}

	if len(cm.configs) >= cm.settings.MaxConfigs {
		return fmt.Errorf("maximum number of configs reached: %d", cm.settings.MaxConfigs)
	}

	// Load configuration based on source type
	var configData map[string]interface{}
	var err error

	switch source.Type {
	case FileSource:
		configData, err = cm.loadFromFile(source.Path)
	case DatabaseSource:
		configData, err = cm.loadFromDatabase(source.Path)
	case APISource:
		configData, err = cm.loadFromAPI(source.URL)
	case MemorySource:
		configData, err = cm.loadFromMemory(source.Path)
	default:
		return fmt.Errorf("unsupported config source type: %s", source.Type)
	}

	if err != nil {
		return fmt.Errorf("failed to load config %s: %v", name, err)
	}

	// Create config object
	config := &Config{
		Name:        name,
		Data:        configData,
		Source:      source,
		LoadedAt:    time.Now(),
		ModifiedAt:  time.Now(),
		Environment: "default",
		Validated:   false,
	}

	// Validate configuration if enabled
	if cm.settings.ValidateOnLoad {
		err := cm.ValidateConfig(config)
		if err != nil {
			return fmt.Errorf("config validation failed for %s: %v", name, err)
		}
	}

	cm.configs[name] = config

	// Start watching file if configured
	if source.Type == FileSource && source.Watch {
		cm.fileWatcher.AddWatcher(source.Path, source)
	}

	return nil
}

// GetConfig retrieves a configuration
func (cm *ConfigManager) GetConfig(name string) (*Config, error) {
	cm.mutex.RLock()
	defer cm.mutex.RUnlock()

	config, exists := cm.configs[name]
	if !exists {
		return nil, fmt.Errorf("config not found: %s", name)
	}

	return config, nil
}

// GetValue retrieves a value from a configuration
func (cm *ConfigManager) GetValue(configName, path string) (interface{}, error) {
	config, err := cm.GetConfig(configName)
	if err != nil {
		return nil, err
	}

	return cm.getValueByPath(config.Data, path)
}

// GetString retrieves a string value from configuration
func (cm *ConfigManager) GetString(configName, path string, defaultValue string) (string, error) {
	value, err := cm.GetValue(configName, path)
	if err != nil {
		return defaultValue, nil
	}

	if str, ok := value.(string); ok {
		return str, nil
	}

	return defaultValue, fmt.Errorf("value at path %s is not a string", path)
}

// GetInt retrieves an integer value from configuration
func (cm *ConfigManager) GetInt(configName, path string, defaultValue int) (int, error) {
	value, err := cm.GetValue(configName, path)
	if err != nil {
		return defaultValue, nil
	}

	switch v := value.(type) {
	case int:
		return v, nil
	case float64:
		return int(v), nil
	case string:
		var result int
		fmt.Sscanf(v, "%d", &result)
		return result, nil
	}

	return defaultValue, fmt.Errorf("value at path %s is not an integer", path)
}

// GetFloat retrieves a float value from configuration
func (cm *ConfigManager) GetFloat(configName, path string, defaultValue float64) (float64, error) {
	value, err := cm.GetValue(configName, path)
	if err != nil {
		return defaultValue, nil
	}

	switch v := value.(type) {
	case float64:
		return v, nil
	case int:
		return float64(v), nil
	case string:
		var result float64
		fmt.Sscanf(v, "%f", &result)
		return result, nil
	}

	return defaultValue, fmt.Errorf("value at path %s is not a float", path)
}

// GetBool retrieves a boolean value from configuration
func (cm *ConfigManager) GetBool(configName, path string, defaultValue bool) (bool, error) {
	value, err := cm.GetValue(configName, path)
	if err != nil {
		return defaultValue, nil
	}

	if b, ok := value.(bool); ok {
		return b, nil
	}

	if str, ok := value.(string); ok {
		var result bool
		fmt.Sscanf(str, "%t", &result)
		return result, nil
	}

	return defaultValue, fmt.Errorf("value at path %s is not a boolean", path)
}

// SetValue sets a value in a configuration
func (cm *ConfigManager) SetValue(configName, path string, value interface{}) error {
	cm.mutex.Lock()
	defer cm.mutex.Unlock()

	config, err := cm.GetConfig(configName)
	if err != nil {
		return err
	}

	err = cm.setValueByPath(config.Data, path, value)
	if err != nil {
		return err
	}

	config.ModifiedAt = time.Now()

	// Save if it's a file-based config
	if config.Source.Type == FileSource {
		err := cm.saveConfigToFile(config)
		if err != nil && cm.settings.EnableLogging {
			fmt.Printf("Error saving config %s: %v\n", configName, err)
		}
	}

	return nil
}

// SaveConfig saves a configuration to its source
func (cm *ConfigManager) SaveConfig(configName string) error {
	cm.mutex.Lock()
	defer cm.mutex.Unlock()

	config, err := cm.GetConfig(configName)
	if err != nil {
		return err
	}

	switch config.Source.Type {
	case FileSource:
		return cm.saveConfigToFile(config)
	case DatabaseSource:
		return cm.saveConfigToDatabase(config)
	case MemorySource:
		return nil // In-memory configs are already saved
	default:
		return fmt.Errorf("unsupported save operation for source type: %s", config.Source.Type)
	}
}

// ValidateConfig validates a configuration
func (cm *ConfigManager) ValidateConfig(config *Config) error {
	cm.mutex.Lock()
	defer cm.mutex.Unlock()

	config.Errors = make([]ConfigError, 0)

	// Run validation rules
	for _, rule := range cm.validator.rules {
		err := cm.validateValue(config.Data, rule)
		if err != nil {
			config.Errors = append(config.Errors, *err)
		}
	}

	config.Validated = len(config.Errors) == 0
	return nil
}

// ReloadConfig reloads a configuration from its source
func (cm *ConfigManager) ReloadConfig(configName string) error {
	cm.mutex.Lock()
	defer cm.mutex.Unlock()

	config, exists := cm.configs[configName]
	if !exists {
		return fmt.Errorf("config not found: %s", configName)
	}

	// Reload from source
	err := cm.LoadConfig(configName, config.Source)
	if err != nil {
		return err
	}

	return nil
}

// AddValidationRule adds a validation rule
func (cm *ConfigManager) AddValidationRule(rule *ValidationRule) {
	cm.validator.AddRule(rule)
}

// AddConfigSchema adds a configuration schema
func (cm *ConfigManager) AddConfigSchema(schema *ConfigSchema) {
	cm.validator.AddSchema(schema)
}

// GetAllConfigs returns all loaded configurations
func (cm *ConfigManager) GetAllConfigs() map[string]*Config {
	cm.mutex.RLock()
	defer cm.mutex.RUnlock()

	result := make(map[string]*Config)
	for name, config := range cm.configs {
		result[name] = config
	}

	return result
}

// GetConfigStats returns configuration manager statistics
func (cm *ConfigManager) GetConfigStats() ConfigStats {
	cm.mutex.RLock()
	defer cm.mutex.RUnlock()

	fileConfigs := 0
	dbConfigs := 0
	apiConfigs := 0
	memoryConfigs := 0
	validatedConfigs := 0
	totalErrors := 0

	for _, config := range cm.configs {
		switch config.Source.Type {
		case FileSource:
			fileConfigs++
		case DatabaseSource:
			dbConfigs++
		case APISource:
			apiConfigs++
		case MemorySource:
			memoryConfigs++
		}

		if config.Validated {
			validatedConfigs++
		}
		totalErrors += len(config.Errors)
	}

	return ConfigStats{
		TotalConfigs:    len(cm.configs),
		FileConfigs:     fileConfigs,
		DatabaseConfigs: dbConfigs,
		APIConfigs:      apiConfigs,
		MemoryConfigs:   memoryConfigs,
		ValidatedConfigs: validatedConfigs,
		TotalErrors:     totalErrors,
		IsRunning:       cm.isRunning,
		AutoReload:      cm.settings.AutoReload,
	}
}

// ConfigStats represents configuration manager statistics
type ConfigStats struct {
	TotalConfigs     int
	FileConfigs      int
	DatabaseConfigs  int
	APIConfigs       int
	MemoryConfigs    int
	ValidatedConfigs int
	TotalErrors      int
	IsRunning        bool
	AutoReload       bool
}

// Internal methods

func (cm *ConfigManager) loadDefaultConfigs(path string) error {
	// Load all config files from the default directory
	return filepath.Walk(path, func(filePath string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if !info.IsDir() && filepath.Ext(filePath) == ".json" {
			configName := filepath.Base(filePath[:len(filePath)-len(filepath.Ext(filePath))])
			source := ConfigSource{
				Type:   FileSource,
				Path:   filePath,
				Watch:  cm.settings.AutoReload,
				Priority: 0,
			}

			err := cm.LoadConfig(configName, source)
			if err != nil && cm.settings.EnableLogging {
				fmt.Printf("Error loading config %s: %v\n", configName, err)
			}
		}

		return nil
	})
}

func (cm *ConfigManager) loadFromFile(path string) (map[string]interface{}, error) {
	data, err := ioutil.ReadFile(path)
	if err != nil {
		return nil, err
	}

	var config map[string]interface{}
	err = json.Unmarshal(data, &config)
	if err != nil {
		return nil, err
	}

	return config, nil
}

func (cm *ConfigManager) saveConfigToFile(config *Config) error {
	data, err := json.MarshalIndent(config.Data, "", "  ")
	if err != nil {
		return err
	}

	// Create backup if enabled
	if cm.settings.BackupConfigs {
		backupPath := filepath.Join(cm.settings.BackupPath, config.Name+".backup.json")
		err = ioutil.WriteFile(backupPath, data, 0644)
		if err != nil && cm.settings.EnableLogging {
			fmt.Printf("Warning: Failed to create backup: %v\n", err)
		}
	}

	// Write config file
	err = ioutil.WriteFile(config.Source.Path, data, 0644)
	if err != nil {
		return err
	}

	return nil
}

func (cm *ConfigManager) loadFromDatabase(path string) (map[string]interface{}, error) {
	// Simulate database loading
	return map[string]interface{}{
		"database_config": "loaded_from_database",
		"path":           path,
	}, nil
}

func (cm *ConfigManager) saveConfigToDatabase(config *Config) error {
	// Simulate database saving
	return nil
}

func (cm *ConfigManager) loadFromAPI(url string) (map[string]interface{}, error) {
	// Simulate API loading
	return map[string]interface{}{
		"api_config": "loaded_from_api",
		"url":       url,
	}, nil
}

func (cm *ConfigManager) loadFromMemory(path string) (map[string]interface{}, error) {
	// Simulate memory loading
	return map[string]interface{}{
		"memory_config": "loaded_from_memory",
		"path":         path,
	}, nil
}

func (cm *ConfigManager) getValueByPath(data map[string]interface{}, path string) (interface{}, error) {
	keys := splitPath(path)
	current := data

	for i, key := range keys {
		if i == len(keys)-1 {
			return current[key], nil
		}

		next, exists := current[key]
		if !exists {
			return nil, fmt.Errorf("path not found: %s", path)
		}

		if nextMap, ok := next.(map[string]interface{}); ok {
			current = nextMap
		} else {
			return nil, fmt.Errorf("invalid path: %s", path)
		}
	}

	return nil, fmt.Errorf("invalid path: %s", path)
}

func (cm *ConfigManager) setValueByPath(data map[string]interface{}, path string, value interface{}) error {
	keys := splitPath(path)
	current := data

	for i, key := range keys {
		if i == len(keys)-1 {
			current[key] = value
			return nil
		}

		next, exists := current[key]
		if !exists {
			current[key] = make(map[string]interface{})
			next = current[key]
		}

		if nextMap, ok := next.(map[string]interface{}); ok {
			current = nextMap
		} else {
			return fmt.Errorf("invalid path: %s", path)
		}
	}

	return nil
}

func (cm *ConfigManager) saveDirtyConfigs() {
	for name, config := range cm.configs {
		if config.Source.Type == FileSource {
			err := cm.saveConfigToFile(config)
			if err != nil && cm.settings.EnableLogging {
				fmt.Printf("Error saving config %s: %v\n", name, err)
			}
		}
	}
}

func (cm *ConfigManager) handleFileChange(filePath string, source ConfigSource) {
	// Find config that uses this file
	for name, config := range cm.configs {
		if config.Source.Path == filePath {
			if cm.settings.EnableLogging {
				fmt.Printf("Detected change in config file: %s\n", filePath)
			}
			
			err := cm.ReloadConfig(name)
			if err != nil && cm.settings.EnableLogging {
				fmt.Printf("Error reloading config %s: %v\n", name, err)
			}
			break
		}
	}
}

func (cm *ConfigManager) validateValue(data map[string]interface{}, rule *ValidationRule) *ConfigError {
	value, exists := cm.getValueByPath(data, rule.Path)
	
	if !exists {
		if rule.Required {
			return &ConfigError{
				Path:    rule.Path,
				Message: fmt.Sprintf("required field missing: %s", rule.Path),
				Type:    MissingError,
			}
		}
		return nil
	}

	// Type validation
	if rule.Type != AnyType {
		if !cm.validator.checkType(value, rule.Type) {
			return &ConfigError{
				Path:    rule.Path,
				Message: fmt.Sprintf("invalid type for %s, expected %s", rule.Path, rule.Type),
				Type:    ValidationError,
			}
		}
	}

	// Range validation
	if rule.Min != nil {
		if !cm.validator.checkMin(value, rule.Min) {
			return &ConfigError{
				Path:    rule.Path,
				Message: fmt.Sprintf("value below minimum for %s", rule.Path),
				Type:    ValidationError,
			}
		}
	}

	if rule.Max != nil {
		if !cm.validator.checkMax(value, rule.Max) {
			return &ConfigError{
				Path:    rule.Path,
				Message: fmt.Sprintf("value above maximum for %s", rule.Path),
				Type:    ValidationError,
			}
		}
	}

	// Custom validation
	if rule.CustomFunc != nil {
		err := rule.CustomFunc(value)
		if err != nil {
			return &ConfigError{
				Path:    rule.Path,
				Message: err.Error(),
				Type:    ValidationError,
			}
		}
	}

	return nil
}

// ConfigValidator implementation

func NewConfigValidator() *ConfigValidator {
	return &ConfigValidator{
		rules:   make(map[string]*ValidationRule),
		schemas: make(map[string]*ConfigSchema),
	}
}

func (cv *ConfigValidator) AddRule(rule *ValidationRule) {
	cv.mutex.Lock()
	defer cv.mutex.Unlock()
	cv.rules[rule.Path] = rule
}

func (cv *ConfigValidator) AddSchema(schema *ConfigSchema) {
	cv.mutex.Lock()
	defer cv.mutex.Unlock()
	cv.schemas[schema.Name] = schema
}

func (cv *ConfigValidator) checkType(value interface{}, expectedType ValueType) bool {
	switch expectedType {
	case StringType:
		_, ok := value.(string)
		return ok
	case IntType:
		switch v := value.(type) {
		case int, float64:
			return true
		case string:
			// Try to parse as integer
			var result int
			return fmt.Sscanf(v, "%d", &result) == 1
		}
		return false
	case FloatType:
		switch v := value.(type) {
		case float64, int:
			return true
		case string:
			var result float64
			return fmt.Sscanf(v, "%f", &result) == 1
		}
		return false
	case BoolType:
		_, ok := value.(bool)
		return ok
	case ArrayType:
		_, ok := value.([]interface{})
		return ok
	case ObjectType:
		_, ok := value.(map[string]interface{})
		return ok
	case AnyType:
		return true
	default:
		return false
	}
}

func (cv *ConfigValidator) checkMin(value, min interface{}) bool {
	switch v := value.(type) {
	case int:
		if minInt, ok := min.(int); ok {
			return v >= minInt
		}
	case float64:
		if minFloat, ok := min.(float64); ok {
			return v >= minFloat
		}
	case string:
		if minStr, ok := min.(string); ok {
			return v >= minStr
		}
	}
	return true
}

func (cv *ConfigValidator) checkMax(value, max interface{}) bool {
	switch v := value.(type) {
	case int:
		if maxInt, ok := max.(int); ok {
			return v <= maxInt
		}
	case float64:
		if maxFloat, ok := max.(float64); ok {
			return v <= maxFloat
		}
	case string:
		if maxStr, ok := max.(string); ok {
			return v <= maxStr
		}
	}
	return true
}

// FileWatcher implementation

func NewFileWatcher(interval time.Duration, defaultPath string) *FileWatcher {
	return &FileWatcher{
		watchers: make(map[string]*FileWatchEntry),
		isRunning: false,
	}
}

func (fw *FileWatcher) Start(callback func(string, ConfigSource)) {
	fw.mutex.Lock()
	defer fw.mutex.Unlock()

	fw.isRunning = true
	fw.callback = callback

	// Start watching goroutine
	go fw.watchLoop()
}

func (fw *FileWatcher) Stop() {
	fw.mutex.Lock()
	defer fw.mutex.Unlock()

	fw.isRunning = false
	fw.watchers = make(map[string]*FileWatchEntry)
}

func (fw *FileWatcher) AddWatcher(filePath string, source ConfigSource) {
	fw.mutex.Lock()
	defer fw.mutex.Unlock()

	if !fw.isRunning {
		return
	}

	// Get file modification time
	info, err := os.Stat(filePath)
	if err != nil {
		return
	}

	fw.watchers[filePath] = &FileWatchEntry{
		Path:        filePath,
		LastModTime: info.ModTime(),
		Source:      source,
	}
}

func (fw *FileWatcher) RemoveWatcher(filePath string) {
	fw.mutex.Lock()
	defer fw.mutex.Unlock()

	delete(fw.watchers, filePath)
}

func (fw *FileWatcher) watchLoop() {
	ticker := time.NewTicker(time.Second)
	defer ticker.Stop()

	for fw.isRunning {
		select {
		case <-ticker.C:
			fw.checkForChanges()
		}
	}
}

func (fw *FileWatcher) checkForChanges() {
	fw.mutex.RLock()
	defer fw.mutex.RUnlock()

	for filePath, entry := range fw.watchers {
		info, err := os.Stat(filePath)
		if err != nil {
			continue
		}

		if info.ModTime().After(entry.LastModTime) {
			// File changed
			if fw.callback != nil {
				fw.callback(filePath, entry.Source)
			}
			
			// Update modification time
			entry.LastModTime = info.ModTime()
		}
	}
}

// Utility functions

func splitPath(path string) []string {
	// Simple path splitter for dot notation
	if path == "" {
		return []string{}
	}
	
	// Handle both dot notation and bracket notation
	parts := make([]string, 0)
	current := ""
	inBracket := false
	
	for _, char := range path {
		switch char {
		case '.':
			if !inBracket && current != "" {
				parts = append(parts, current)
				current = ""
			}
		case '[':
			if current != "" {
				parts = append(parts, current)
				current = ""
			}
			inBracket = true
		case ']':
			if current != "" {
				parts = append(parts, current)
				current = ""
			}
			inBracket = false
		default:
			current += string(char)
		}
	}
	
	if current != "" {
		parts = append(parts, current)
	}
	
	return parts
}

// Example usage
func main() {
	settings := ConfigManagerSettings{
		AutoReload:     true,
		ReloadDelay:    time.Second * 1,
		DefaultPath:    "./configs",
		WatchInterval:  time.Second * 5,
		MaxConfigs:     100,
		ValidateOnLoad: true,
		BackupConfigs:  true,
		BackupPath:     "./backups",
		EnableLogging:  true,
	}

	cm := NewConfigManager(settings)
	
	// Start configuration manager
	if err := cm.Start(); err != nil {
		fmt.Printf("Error starting config manager: %v\n", err)
		return
	}

	// Create a sample config file
	sampleConfig := map[string]interface{}{
		"database": map[string]interface{}{
			"host":     "localhost",
			"port":     5432,
			"name":     "gamecenter",
			"user":     "admin",
			"password": "secret",
		},
		"game": map[string]interface{}{
			"max_players":  100,
			"difficulty":   "normal",
			"sound_enabled": true,
			"graphics": map[string]interface{}{
				"resolution": "1920x1080",
				"fullscreen": false,
				"vsync":      true,
			},
		},
	}

	// Save sample config
	configData, _ := json.MarshalIndent(sampleConfig, "", "  ")
	ioutil.WriteFile("./sample_config.json", configData, 0644)

	// Load configuration
	source := ConfigSource{
		Type:   FileSource,
		Path:   "./sample_config.json",
		Watch:  true,
		Priority: 0,
	}

	err := cm.LoadConfig("game_settings", source)
	if err != nil {
		fmt.Printf("Error loading config: %v\n", err)
	} else {
		fmt.Println("Configuration loaded successfully")
	}

	// Add validation rules
	cm.AddValidationRule(&ValidationRule{
		Path:     "database.port",
		Type:     IntType,
		Required: true,
		Min:      1024,
		Max:      65535,
	})

	cm.AddValidationRule(&ValidationRule{
		Path:     "game.max_players",
		Type:     IntType,
		Required: true,
		Min:      1,
		Max:      1000,
	})

	// Test value retrieval
	dbHost, _ := cm.GetString("game_settings", "database.host", "localhost")
	dbPort, _ := cm.GetInt("game_settings", "database.port", 5432)
	soundEnabled, _ := cm.GetBool("game_settings", "game.sound_enabled", false)
	
	fmt.Printf("Database host: %s\n", dbHost)
	fmt.Printf("Database port: %d\n", dbPort)
	fmt.Printf("Sound enabled: %v\n", soundEnabled)

	// Test value modification
	err = cm.SetValue("game_settings", "game.max_players", 50)
	if err != nil {
		fmt.Printf("Error setting value: %v\n", err)
	} else {
		fmt.Println("Value updated successfully")
	}

	// Get statistics
	stats := cm.GetConfigStats()
	fmt.Printf("\nConfiguration Manager Statistics:\n")
	fmt.Printf("Total Configs: %d\n", stats.TotalConfigs)
	fmt.Printf("File Configs: %d\n", stats.FileConfigs)
	fmt.Printf("Validated Configs: %d\n", stats.ValidatedConfigs)
	fmt.Printf("Total Errors: %d\n", stats.TotalErrors)
	fmt.Printf("Auto Reload: %v\n", stats.AutoReload)

	// Save config
	err = cm.SaveConfig("game_settings")
	if err != nil {
		fmt.Printf("Error saving config: %v\n", err)
	}

	// Stop configuration manager
	cm.Stop()

	fmt.Println("\nConfiguration Manager initialized successfully!")
}
