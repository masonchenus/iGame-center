package main

import (
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"sync"
	"time"
)

// Logger represents the main logging system
type Logger struct {
	loggers    map[string]*GameLogger
	appenders  []LogAppender
	formatters []LogFormatter
	settings   LoggerSettings
	mutex      sync.RWMutex
	isRunning  bool
	startedAt  time.Time
}

// GameLogger represents a named logger instance
type GameLogger struct {
	Name        string
	Level       LogLevel
	Handlers    []LogHandler
	Parent      *GameLogger
	Children    []*GameLogger
	Additivity  bool
	CreatedAt   time.Time
}

// LogHandler represents a logging handler
type LogHandler interface {
	Handle(record *LogRecord)
	Close() error
	Level() LogLevel
	SetLevel(level LogLevel)
}

// LogAppender represents a log output destination
type LogAppender interface {
	Append(record *LogRecord) error
	Flush() error
	Close() error
	Name() string
}

// LogFormatter represents a log formatting function
type LogFormatter interface {
	Format(record *LogRecord) string
	Name() string
}

// LogRecord represents a single log entry
type LogRecord struct {
	LoggerName  string
	Level       LogLevel
	Message     string
	Timestamp   time.Time
	File        string
	Function    string
	Line        int
	GoroutineID int
	Data        map[string]interface{}
}

// ConsoleHandler represents a console output handler
type ConsoleHandler struct {
	writer      io.Writer
	level       LogLevel
	formatter   LogFormatter
	colored     bool
}

// FileHandler represents a file output handler
type FileHandler struct {
	filename    string
	writer      *os.File
	level       LogLevel
	formatter   LogFormatter
	maxSize     int64
	maxFiles    int
	currentSize int64
}

// NetworkHandler represents a network logging handler
type NetworkHandler struct {
	address     string
	level       LogLevel
	formatter   LogFormatter
	connected   bool
	reconnect   bool
}

// ConsoleAppender represents console output
type ConsoleAppender struct {
	writer io.Writer
	colored bool
	ANSI   bool
}

// FileAppender represents file output
type FileAppender struct {
	filename   string
	writer     *os.File
	maxSize    int64
	maxFiles   int
	rotate     bool
	compressed bool
}

// NetworkAppender represents network output
type NetworkAppender struct {
	address   string
	protocol  string
	connected bool
	timeout   time.Duration
}

// LoggerSettings contains logging configuration
type LoggerSettings struct {
	Level              LogLevel
	ConsoleEnabled     bool
	FileEnabled        bool
	NetworkEnabled     bool
	FilePath           string
	FileMaxSize        int64
	FileMaxFiles       int
	NetworkAddress     string
	ConsoleColored     bool
	ConsoleTimestamp   bool
	ConsoleFormat      string
	FileFormat         string
	NetworkFormat      string
	EnableRotation     bool
	EnableCompression  bool
	AsyncLogging       bool
	BufferSize         int
	FlushInterval      time.Duration
	EnableCallerInfo   bool
	EnableGoroutineID  bool
	EnableJSON         bool
	MaxMessageLength   int
	EnableStackTrace   bool
}

// LogLevel represents logging levels
type LogLevel int

const (
	TRACE LogLevel = iota
	DEBUG
	INFO
	WARN
	ERROR
	FATAL
)

// Level names
var LevelNames = map[LogLevel]string{
	TRACE: "TRACE",
	DEBUG: "DEBUG",
	INFO:  "INFO",
	WARN:  "WARN",
	ERROR: "ERROR",
	FATAL: "FATAL",
}

// Level priorities
var LevelPriorities = map[LogLevel]int{
	TRACE: 500,
	DEBUG: 400,
	INFO:  300,
	WARN:  200,
	ERROR: 100,
	FATAL: 0,
}

// SimpleFormatter represents a simple text formatter
type SimpleFormatter struct {
	template string
}

// JSONFormatter represents a JSON formatter
type JSONFormatter struct {
	pretty    bool
	includeFields bool
}

// PatternFormatter represents a pattern-based formatter
type PatternFormatter struct {
	pattern string
	dateFormat string
	timeFormat string
}

// NewLogger creates a new logger system
func NewLogger(settings LoggerSettings) *Logger {
	return &Logger{
		loggers:   make(map[string]*GameLogger),
		appenders: make([]LogAppender, 0),
		formatters: make([]LogFormatter, 0),
		settings:  settings,
		isRunning: false,
		startedAt: time.Now(),
	}
}

// Start initializes and starts the logger system
func (l *Logger) Start() error {
	l.mutex.Lock()
	defer l.mutex.Unlock()

	l.isRunning = true

	// Create and register default formatters
	l.registerDefaultFormatters()

	// Create and register default appenders
	err := l.registerDefaultAppenders()
	if err != nil {
		return err
	}

	// Create root logger
	rootLogger := &GameLogger{
		Name:       "root",
		Level:      l.settings.Level,
		Additivity: true,
		CreatedAt:  time.Now(),
	}
	l.loggers["root"] = rootLogger

	fmt.Println("Logger system started")
	return nil
}

// Stop stops the logger system
func (l *Logger) Stop() {
	l.mutex.Lock()
	defer l.mutex.Unlock()

	l.isRunning = false

	// Flush and close all appenders
	for _, appender := range l.appenders {
		appender.Flush()
		appender.Close()
	}

	// Clear loggers
	l.loggers = make(map[string]*GameLogger)

	fmt.Println("Logger system stopped")
}

// GetLogger retrieves or creates a logger by name
func (l *Logger) GetLogger(name string) *GameLogger {
	l.mutex.RLock()
	logger, exists := l.loggers[name]
	l.mutex.RUnlock()

	if !exists {
		l.mutex.Lock()
		defer l.mutex.Unlock()

		// Create parent hierarchy
		parts := strings.Split(name, ".")
		parentName := ""
		var parent *GameLogger

		for i, part := range parts {
			if i > 0 {
				parentName += "."
			}
			parentName += part

			parentLogger, exists := l.loggers[parentName]
			if !exists {
				parentLogger = &GameLogger{
					Name:       parentName,
					Level:      l.settings.Level,
					Additivity: true,
					CreatedAt:  time.Now(),
				}
				l.loggers[parentName] = parentLogger

				if parent != nil {
					parent.Children = append(parent.Children, parentLogger)
					parentLogger.Parent = parent
				}
			}
			parent = parentLogger
		}

		logger = parent
	}

	return logger
}

// Log logs a message at the specified level
func (l *Logger) Log(loggerName string, level LogLevel, message string, args ...interface{}) {
	if !l.isRunning {
		return
	}

	logger := l.GetLogger(loggerName)
	if !l.shouldLog(logger, level) {
		return
	}

	record := l.createLogRecord(loggerName, level, message, args...)
	l.processRecord(record, logger)
}

// Trace logs a trace message
func (l *Logger) Trace(loggerName, message string, args ...interface{}) {
	l.Log(loggerName, TRACE, message, args...)
}

// Debug logs a debug message
func (l *Logger) Debug(loggerName, message string, args ...interface{}) {
	l.Log(loggerName, DEBUG, message, args...)
}

// Info logs an info message
func (l *Logger) Info(loggerName, message string, args ...interface{}) {
	l.Log(loggerName, INFO, message, args...)
}

// Warn logs a warning message
func (l *Logger) Warn(loggerName, message string, args ...interface{}) {
	l.Log(loggerName, WARN, message, args...)
}

// Error logs an error message
func (l *Logger) Error(loggerName, message string, args ...interface{}) {
	l.Log(loggerName, ERROR, message, args...)
}

// Fatal logs a fatal message
func (l *Logger) Fatal(loggerName, message string, args ...interface{}) {
	l.Log(loggerName, FATAL, message, args...)
}

// Logf logs a formatted message
func (l *Logger) Logf(loggerName string, level LogLevel, format string, args ...interface{}) {
	l.Log(loggerName, level, fmt.Sprintf(format, args...))
}

// AddAppender adds a log appender
func (l *Logger) AddAppender(appender LogAppender) {
	l.mutex.Lock()
	defer l.mutex.Unlock()

	l.appenders = append(l.appenders, appender)
}

// RemoveAppender removes a log appender
func (l *Logger) RemoveAppender(name string) {
	l.mutex.Lock()
	defer l.mutex.Unlock()

	for i, appender := range l.appenders {
		if appender.Name() == name {
			appender.Close()
			l.appenders = append(l.appenders[:i], l.appenders[i+1:]...)
			break
		}
	}
}

// AddFormatter adds a log formatter
func (l *Logger) AddFormatter(formatter LogFormatter) {
	l.mutex.Lock()
	defer l.mutex.Unlock()

	l.formatters = append(l.formatters, formatter)
}

// SetLevel sets the logging level for a logger
func (l *Logger) SetLevel(loggerName string, level LogLevel) {
	logger := l.GetLogger(loggerName)
	logger.Level = level
}

// GetLevel gets the logging level for a logger
func (l *Logger) GetLevel(loggerName string) LogLevel {
	logger := l.GetLogger(loggerName)
	return logger.Level
}

// Flush flushes all pending log messages
func (l *Logger) Flush() {
	l.mutex.RLock()
	defer l.mutex.RUnlock()

	for _, appender := range l.appenders {
		appender.Flush()
	}
}

// GetStats returns logging system statistics
func (l *Logger) GetStats() LoggerStats {
	l.mutex.RLock()
	defer l.mutex.RUnlock()

	return LoggerStats{
		Uptime:         time.Since(l.startedAt),
		LoggerCount:    len(l.loggers),
		AppenderCount:  len(l.appenders),
		FormatterCount: len(l.formatters),
		IsRunning:      l.isRunning,
		Settings:       l.settings,
	}
}

// LoggerStats represents logging system statistics
type LoggerStats struct {
	Uptime         time.Duration
	LoggerCount    int
	AppenderCount  int
	FormatterCount int
	IsRunning      bool
	Settings       LoggerSettings
}

// Internal methods

func (l *Logger) shouldLog(logger *GameLogger, level LogLevel) bool {
	// Check if level is enabled
	if LevelPriorities[level] > LevelPriorities[logger.Level] {
		return false
	}

	// Check parent hierarchy if additivity is disabled
	if !logger.Additivity {
		return true
	}

	// Walk up the hierarchy to find a logger that doesn't inherit
	current := logger
	for current != nil {
		if !current.Additivity {
			return LevelPriorities[level] <= LevelPriorities[current.Level]
		}
		current = current.Parent
	}

	return true
}

func (l *Logger) createLogRecord(loggerName string, level LogLevel, message string, args ...interface{}) *LogRecord {
	record := &LogRecord{
		LoggerName: loggerName,
		Level:      level,
		Message:    message,
		Timestamp:  time.Now(),
		Data:       make(map[string]interface{}),
	}

	// Format message if args provided
	if len(args) > 0 {
		if formatStr, ok := args[0].(string); ok && len(args) > 1 {
			record.Message = fmt.Sprintf(formatStr, args[1:]...)
		}
	}

	// Add caller information
	if l.settings.EnableCallerInfo {
		pc, file, line, ok := runtime.Caller(2)
		if ok {
			record.File = file
			record.Line = line
			record.Function = runtime.FuncForPC(pc).Name()
		}
	}

	// Add goroutine ID
	if l.settings.EnableGoroutineID {
		record.GoroutineID = getGoroutineID()
	}

	// Truncate message if too long
	if len(record.Message) > l.settings.MaxMessageLength {
		record.Message = record.Message[:l.settings.MaxMessageLength] + "..."
	}

	return record
}

func (l *Logger) processRecord(record *LogRecord, logger *GameLogger) {
	// Send to all appenders
	for _, appender := range l.appenders {
		err := appender.Append(record)
		if err != nil && l.settings.EnableStackTrace {
			fmt.Printf("Error appending log: %v\n", err)
		}
	}

	// Handle fatal messages
	if record.Level == FATAL {
		l.Flush()
		if l.settings.EnableStackTrace {
			stack := make([]byte, 4096)
			runtime.Stack(stack, true)
			fmt.Printf("FATAL: %s\n%s\n", record.Message, string(stack))
		}
		os.Exit(1)
	}
}

func (l *Logger) registerDefaultFormatters() {
	// Simple formatter
	simpleFormatter := &SimpleFormatter{
		template: "[%s] %s %s:%d %s",
	}
	l.formatters = append(l.formatters, simpleFormatter)

	// JSON formatter
	if l.settings.EnableJSON {
		jsonFormatter := &JSONFormatter{
			pretty:       false,
			includeFields: true,
		}
		l.formatters = append(l.formatters, jsonFormatter)
	}

	// Pattern formatter
	patternFormatter := &PatternFormatter{
		pattern:     "%d{yyyy-MM-dd HH:mm:ss} %-5level %logger{36} - %msg%n",
		dateFormat:  "2006-01-02",
		timeFormat:  "15:04:05.000",
	}
	l.formatters = append(l.formatters, patternFormatter)
}

func (l *Logger) registerDefaultAppenders() error {
	// Console appender
	if l.settings.ConsoleEnabled {
		consoleAppender := &ConsoleAppender{
			writer: os.Stdout,
			colored: l.settings.ConsoleColored,
			ANSI:   supportsColor(),
		}
		l.AddAppender(consoleAppender)
	}

	// File appender
	if l.settings.FileEnabled {
		fileAppender, err := NewFileAppender(l.settings.FilePath, l.settings.FileMaxSize, l.settings.FileMaxFiles)
		if err != nil {
			return fmt.Errorf("failed to create file appender: %v", err)
		}
		l.AddAppender(fileAppender)
	}

	// Network appender
	if l.settings.NetworkEnabled {
		networkAppender := &NetworkAppender{
			address: l.settings.NetworkAddress,
			protocol: "udp",
			timeout: time.Second * 5,
		}
		l.AddAppender(networkAppender)
	}

	return nil
}

// GameLogger methods

func (gl *GameLogger) Trace(message string, args ...interface{}) {
	GetGlobalLogger().Log(gl.Name, TRACE, message, args...)
}

func (gl *GameLogger) Debug(message string, args ...interface{}) {
	GetGlobalLogger().Log(gl.Name, DEBUG, message, args...)
}

func (gl *GameLogger) Info(message string, args ...interface{}) {
	GetGlobalLogger().Log(gl.Name, INFO, message, args...)
}

func (gl *GameLogger) Warn(message string, args ...interface{}) {
	GetGlobalLogger().Log(gl.Name, WARN, message, args...)
}

func (gl *GameLogger) Error(message string, args ...interface{}) {
	GetGlobalLogger().Log(gl.Name, ERROR, message, args...)
}

func (gl *GameLogger) Fatal(message string, args ...interface{}) {
	GetGlobalLogger().Log(gl.Name, FATAL, message, args...)
}

func (gl *GameLogger) Log(level LogLevel, message string, args ...interface{}) {
	GetGlobalLogger().Log(gl.Name, level, message, args...)
}

// ConsoleAppender implementation

func (ca *ConsoleAppender) Append(record *LogRecord) error {
	var output string
	
	// Use formatter if available
	if l := GetGlobalLogger(); l != nil {
		l.mutex.RLock()
		defer l.mutex.RUnlock()
		
		for _, formatter := range l.formatters {
			output = formatter.Format(record)
			break // Use first formatter
		}
	}
	
	if output == "" {
		output = ca.formatSimple(record)
	}
	
	// Add color if enabled
	if ca.colored && ca.ANSI {
		output = ca.addColor(record.Level, output)
	}
	
	// Add timestamp if configured
	if GetGlobalLogger().settings.ConsoleTimestamp {
		timestamp := record.Timestamp.Format("15:04:05.000")
		output = fmt.Sprintf("%s %s", timestamp, output)
	}
	
	_, err := ca.writer.Write([]byte(output + "\n"))
	return err
}

func (ca *ConsoleAppender) Flush() error {
	return nil
}

func (ca *ConsoleAppender) Close() error {
	return nil
}

func (ca *ConsoleAppender) Name() string {
	return "console"
}

func (ca *ConsoleAppender) formatSimple(record *LogRecord) string {
	return fmt.Sprintf("[%s] %s: %s", 
		LevelNames[record.Level], record.LoggerName, record.Message)
}

func (ca *ConsoleAppender) addColor(level LogLevel, message string) string {
	colorCode := ""
	
	switch level {
	case TRACE:
		colorCode = "\033[36m" // Cyan
	case DEBUG:
		colorCode = "\033[34m" // Blue
	case INFO:
		colorCode = "\033[32m" // Green
	case WARN:
		colorCode = "\033[33m" // Yellow
	case ERROR:
		colorCode = "\033[31m" // Red
	case FATAL:
		colorCode = "\033[35m" // Magenta
	}
	
	return fmt.Sprintf("%s%s\033[0m", colorCode, message)
}

// FileAppender implementation

func NewFileAppender(filename string, maxSize int64, maxFiles int) (*FileAppender, error) {
	// Create directory if it doesn't exist
	dir := filepath.Dir(filename)
	if err := os.MkdirAll(dir, 0755); err != nil {
		return nil, err
	}
	
	file, err := os.OpenFile(filename, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
	if err != nil {
		return nil, err
	}
	
	// Get current file size
	info, err := file.Stat()
	if err != nil {
		file.Close()
		return nil, err
	}
	
	return &FileAppender{
		filename:   filename,
		writer:     file,
		maxSize:    maxSize,
		maxFiles:   maxFiles,
		rotate:     true,
		compressed: false,
		currentSize: info.Size(),
	}, nil
}

func (fa *FileAppender) Append(record *LogRecord) error {
	// Check if rotation is needed
	if fa.rotate && fa.currentSize > fa.maxSize {
		fa.rotateFile()
	}
	
	// Format record
	l := GetGlobalLogger()
	var output string
	
	if l != nil {
		l.mutex.RLock()
		for _, formatter := range l.formatters {
			output = formatter.Format(record)
			break
		}
		l.mutex.RUnlock()
	}
	
	if output == "" {
		output = fmt.Sprintf("[%s] %s %s:%d %s\n",
			record.Timestamp.Format("2006-01-02 15:04:05"),
			LevelNames[record.Level],
			record.File,
			record.Line,
			record.Message)
	}
	
	// Write to file
	data := []byte(output)
	_, err := fa.writer.Write(data)
	if err == nil {
		fa.currentSize += int64(len(data))
	}
	
	return err
}

func (fa *FileAppender) Flush() error {
	if fa.writer != nil {
		return fa.writer.Sync()
	}
	return nil
}

func (fa *FileAppender) Close() error {
	if fa.writer != nil {
		return fa.writer.Close()
	}
	return nil
}

func (fa *FileAppender) Name() string {
	return "file"
}

func (fa *FileAppender) rotateFile() error {
	// Close current file
	fa.writer.Close()
	
	// Rotate files
	for i := fa.maxFiles - 1; i > 0; i-- {
		oldName := fmt.Sprintf("%s.%d", fa.filename, i-1)
		newName := fmt.Sprintf("%s.%d", fa.filename, i)
		
		if fileExists(oldName) {
			os.Rename(oldName, newName)
		}
	}
	
	// Move current file to .1
	if fileExists(fa.filename) {
		os.Rename(fa.filename, fmt.Sprintf("%s.1", fa.filename))
	}
	
	// Open new file
	file, err := os.OpenFile(fa.filename, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
	if err != nil {
		return err
	}
	
	fa.writer = file
	fa.currentSize = 0
	
	return nil
}

// NetworkAppender implementation

func (na *NetworkAppender) Append(record *LogRecord) error {
	// Simulate network logging
	if !na.connected {
		// In a real implementation, this would establish a connection
		na.connected = true
	}
	
	// Format record for network transmission
	l := GetGlobalLogger()
	var output string
	
	if l != nil {
		l.mutex.RLock()
		for _, formatter := range l.formatters {
			output = formatter.Format(record)
			break
		}
		l.mutex.RUnlock()
	}
	
	if output == "" {
		output = fmt.Sprintf("[%s] %s: %s", 
			LevelNames[record.Level], record.LoggerName, record.Message)
	}
	
	// In a real implementation, this would send data over the network
	fmt.Printf("Network log: %s -> %s\n", output, na.address)
	
	return nil
}

func (na *NetworkAppender) Flush() error {
	return nil
}

func (na *NetworkAppender) Close() error {
	na.connected = false
	return nil
}

func (na *NetworkAppender) Name() string {
	return "network"
}

// Formatter implementations

func (sf *SimpleFormatter) Format(record *LogRecord) string {
	if record.File != "" && record.Line > 0 {
		return fmt.Sprintf(sf.template, 
			LevelNames[record.Level], 
			record.LoggerName,
			filepath.Base(record.File),
			record.Line,
			record.Message)
	}
	
	return fmt.Sprintf("[%s] %s %s", 
		LevelNames[record.Level], 
		record.LoggerName,
		record.Message)
}

func (sf *SimpleFormatter) Name() string {
	return "simple"
}

func (jf *JSONFormatter) Format(record *LogRecord) string {
	// Simple JSON formatting
	data := map[string]interface{}{
		"timestamp": record.Timestamp.Format(time.RFC3339),
		"level":     LevelNames[record.Level],
		"logger":    record.LoggerName,
		"message":   record.Message,
		"file":      record.File,
		"function":  record.Function,
		"line":      record.Line,
	}
	
	// Add extra data
	for k, v := range record.Data {
		data[k] = v
	}
	
	// Convert to JSON
	jsonData, err := json.Marshal(data)
	if err != nil {
		return fmt.Sprintf("{\"error\": \"json_format_error\", \"message\": %q}", record.Message)
	}
	
	return string(jsonData)
}

func (jf *JSONFormatter) Name() string {
	return "json"
}

func (pf *PatternFormatter) Format(record *LogRecord) string {
	// Simple pattern replacement
	output := pf.pattern
	
	// Replace common patterns
	output = strings.ReplaceAll(output, "%d", record.Timestamp.Format(pf.dateFormat+" "+pf.timeFormat))
	output = strings.ReplaceAll(output, "%d{yyyy-MM-dd HH:mm:ss}", record.Timestamp.Format("2006-01-02 15:04:05"))
	output = strings.ReplaceAll(output, "%-5level", fmt.Sprintf("%-5s", LevelNames[record.Level]))
	output = strings.ReplaceAll(output, "%logger", record.LoggerName)
	output = strings.ReplaceAll(output, "%logger{36}", record.LoggerName)
	output = strings.ReplaceAll(output, "%msg", record.Message)
	output = strings.ReplaceAll(output, "%n", "\n")
	
	if record.File != "" && record.Line > 0 {
		output = strings.ReplaceAll(output, "%file", filepath.Base(record.File))
		output = strings.ReplaceAll(output, "%line", fmt.Sprintf("%d", record.Line))
		output = strings.ReplaceAll(output, "%file:%line", 
			fmt.Sprintf("%s:%d", filepath.Base(record.File), record.Line))
	}
	
	return output
}

func (pf *PatternFormatter) Name() string {
	return "pattern"
}

// Utility functions

var globalLogger *Logger
var globalLoggerOnce sync.Once

func GetGlobalLogger() *Logger {
	globalLoggerOnce.Do(func() {
		settings := LoggerSettings{
			Level:             INFO,
			ConsoleEnabled:    true,
			FileEnabled:       false,
			ConsoleColored:    true,
			EnableCallerInfo:  true,
			EnableJSON:        false,
			MaxMessageLength:  8192,
		}
		globalLogger = NewLogger(settings)
		globalLogger.Start()
	})
	return globalLogger
}

func getGoroutineID() int {
	// Get current goroutine ID
	// This is a simplified implementation
	b := make([]byte, 64)
	runtime.Stack(b, false)
	// Extract goroutine ID from stack trace
	// This is platform-specific and not always accurate
	return 1
}

func fileExists(filename string) bool {
	_, err := os.Stat(filename)
	return !os.IsNotExist(err)
}

func supportsColor() bool {
	// Check if terminal supports color
	// This is a simplified check
	term := os.Getenv("TERM")
	return term != "" && term != "dumb"
}

// Example usage

func main() {
	// Create logger with custom settings
	settings := LoggerSettings{
		Level:              DEBUG,
		ConsoleEnabled:     true,
		FileEnabled:        true,
		NetworkEnabled:     false,
		FilePath:           "./logs/game.log",
		FileMaxSize:        10 * 1024 * 1024, // 10MB
		FileMaxFiles:       5,
		ConsoleColored:     true,
		ConsoleTimestamp:   true,
		EnableCallerInfo:   true,
		EnableJSON:         false,
		MaxMessageLength:   4096,
		EnableStackTrace:   true,
	}

	logger := NewLogger(settings)
	
	// Start logger
	if err := logger.Start(); err != nil {
		fmt.Printf("Error starting logger: %v\n", err)
		return
	}

	// Test logging with different loggers
	gameLogger := logger.GetLogger("game")
	networkLogger := logger.GetLogger("network")
	physicsLogger := logger.GetLogger("physics.engine")

	// Log messages at different levels
	gameLogger.Info("Game initialization started")
	gameLogger.Debug("Loading game assets from %s", "/assets")
	networkLogger.Info("Network connection established")
	physicsLogger.Debug("Physics engine initialized with %d objects", 100)
	
	// Log some warnings and errors
	gameLogger.Warn("Low memory detected, performance may be degraded")
	networkLogger.Error("Failed to connect to server: %s", "timeout")
	
	// Log with structured data
	record := &LogRecord{
		LoggerName: "game.player",
		Level:      INFO,
		Message:    "Player joined game",
		Timestamp:  time.Now(),
		Data: map[string]interface{}{
			"player_id":   "player_123",
			"player_name": "Alice",
			"session_id":  "session_456",
		},
	}
	
	// Process the record manually (in real usage, you'd use the logger methods)
	logger.processRecord(record, gameLogger)

	// Test different logging methods
	logger.Log("test.logger", INFO, "Direct logging test")
	logger.Logf("test.logger", DEBUG, "Formatted log: %s %d", "test", 42)
	
	// Get statistics
	stats := logger.GetStats()
	fmt.Printf("\nLogger Statistics:\n")
	fmt.Printf("Uptime: %v\n", stats.Uptime)
	fmt.Printf("Loggers: %d\n", stats.LoggerCount)
	fmt.Printf("Appenders: %d\n", stats.AppenderCount)
	fmt.Printf("Formatters: %d\n", stats.FormatterCount)

	// Test global logger
	GetGlobalLogger().Info("global", "Testing global logger")
	
	// Flush and stop
	logger.Flush()
	logger.Stop()

	fmt.Println("\nLogger system initialized successfully!")
}
