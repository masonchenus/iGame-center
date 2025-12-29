package main

import (
	"fmt"
	"sync"
	"time"
)

// GameStateManager handles game state transitions and management
type GameStateManager struct {
	currentState  GameState
	previousState GameState
	states        map[string]GameState
	transitions   map[TransitionKey]Transition
	stack         []*GameState
	eventHandler  *EventManager
	settings      StateManagerSettings
	mutex         sync.RWMutex
	isRunning     bool
}

// GameState represents a game state
type GameState interface {
	Name() string
	Enter() error
	Exit() error
	Update(deltaTime time.Duration) error
	Render() error
	HandleEvent(event Event) error
	IsActive() bool
}

// BaseGameState provides a base implementation for game states
type BaseGameState struct {
	name        string
	active      bool
	enterTime   time.Time
	manager     *GameStateManager
	eventHandler *EventManager
}

// Transition represents a state transition
type Transition struct {
	From        string
	To          string
	Condition   func() bool
	Duration    time.Duration
	Effect      TransitionEffect
	CanInterrupt bool
}

// Event represents a game event
type Event struct {
	Type      EventType
	Source    string
	Data      interface{}
	Timestamp time.Time
}

// EventManager manages event handling
type EventManager struct {
	listeners   map[EventType][]EventListener
	events      []Event
	eventQueue  chan Event
	mutex       sync.RWMutex
	maxEvents   int
}

// EventListener represents an event listener
type EventListener struct {
	ID         string
	EventType  EventType
	Callback   func(Event) error
	Priority   int
	Active     bool
}

// StateManagerSettings contains configuration
type StateManagerSettings struct {
	MaxStates     int
	MaxTransitions int
	EventBufferSize int
	EnableLogging  bool
	DebugMode     bool
}

// TransitionKey represents a transition key
type TransitionKey struct {
	From, To string
}

// TransitionEffect represents transition effects
type TransitionEffect string

const (
	FadeEffect     TransitionEffect = "fade"
	SlideEffect    TransitionEffect = "slide"
	ZoomEffect     TransitionEffect = "zoom"
	InstantEffect  TransitionEffect = "instant"
)

// EventType represents event types
type EventType string

const (
	EventStateChanged EventType = "state_changed"
	EventTransition  EventType = "transition"
	EventInput       EventType = "input"
	EventSystem      EventType = "system"
)

// MainMenuState represents the main menu state
type MainMenuState struct {
	*BaseGameState
	menuItems []MenuItem
	selected  int
}

// MenuItem represents a menu item
type MenuItem struct {
	Text     string
	Action   func() error
	Enabled  bool
	Visible  bool
}

// PlayState represents the gameplay state
type PlayState struct {
	*BaseGameState
	level      int
	score      int
	lives      int
	timeElapsed time.Duration
	paused     bool
}

// PauseState represents the pause state
type PauseState struct {
	*BaseGameState
	previousState string
	menuItems     []MenuItem
	selected      int
}

// SettingsState represents the settings state
type SettingsState struct {
	*BaseGameState
	settings    map[string]interface{}
	menuItems   []MenuItem
	selected    int
	changed     bool
}

// NewGameStateManager creates a new game state manager
func NewGameStateManager(settings StateManagerSettings) *GameStateManager {
	gsm := &GameStateManager{
		states:      make(map[string]GameState),
		transitions: make(map[TransitionKey]Transition),
		stack:       make([]*GameState, 0),
		eventHandler: NewEventManager(settings.EventBufferSize),
		settings:    settings,
		isRunning:   false,
	}

	// Register default states
	gsm.registerDefaultStates()

	return gsm
}

// Start starts the state manager
func (gsm *GameStateManager) Start() error {
	gsm.mutex.Lock()
	defer gsm.mutex.Unlock()

	gsm.isRunning = true

	// Start event processing
	gsm.eventHandler.Start()

	// Enter initial state if not set
	if gsm.currentState == nil {
		initialState := gsm.states["main_menu"]
		if initialState != nil {
			gsm.changeState(initialState)
		}
	}

	fmt.Println("Game State Manager started")
	return nil
}

// Stop stops the state manager
func (gsm *GameStateManager) Stop() {
	gsm.mutex.Lock()
	defer gsm.mutex.Unlock()

	gsm.isRunning = false

	// Exit current state
	if gsm.currentState != nil {
		gsm.currentState.Exit()
	}

	// Stop event processing
	gsm.eventHandler.Stop()

	fmt.Println("Game State Manager stopped")
}

// Update updates the current state
func (gsm *GameStateManager) Update(deltaTime time.Duration) error {
	gsm.mutex.Lock()
	defer gsm.mutex.Unlock()

	if !gsm.isRunning {
		return nil
	}

	// Process events
	gsm.eventHandler.ProcessEvents()

	// Update current state
	if gsm.currentState != nil && gsm.currentState.IsActive() {
		return gsm.currentState.Update(deltaTime)
	}

	return nil
}

// Render renders the current state
func (gsm *GameStateManager) Render() error {
	gsm.mutex.Lock()
	defer gsm.mutex.Unlock()

	if !gsm.isRunning {
		return nil
	}

	// Render current state
	if gsm.currentState != nil && gsm.currentState.IsActive() {
		return gsm.currentState.Render()
	}

	return nil
}

// ChangeState changes to a new state
func (gsm *GameStateManager) ChangeState(stateName string) error {
	gsm.mutex.Lock()
	defer gsm.mutex.Unlock()

	if !gsm.isRunning {
		return fmt.Errorf("state manager is not running")
	}

	targetState := gsm.states[stateName]
	if targetState == nil {
		return fmt.Errorf("state not found: %s", stateName)
	}

	return gsm.changeState(targetState)
}

// PushState pushes a state onto the stack
func (gsm *GameStateManager) PushState(stateName string) error {
	gsm.mutex.Lock()
	defer gsm.mutex.Unlock()

	if !gsm.isRunning {
		return fmt.Errorf("state manager is not running")
	}

	targetState := gsm.states[stateName]
	if targetState == nil {
		return fmt.Errorf("state not found: %s", stateName)
	}

	// Save current state to stack
	if gsm.currentState != nil {
		gsm.stack = append(gsm.stack, gsm.currentState)
	}

	return gsm.changeState(targetState)
}

// PopState pops a state from the stack
func (gsm *GameStateManager) PopState() error {
	gsm.mutex.Lock()
	defer gsm.mutex.Unlock()

	if !gsm.isRunning {
		return fmt.Errorf("state manager is not running")
	}

	if len(gsm.stack) == 0 {
		return fmt.Errorf("no states in stack to pop")
	}

	// Exit current state
	if gsm.currentState != nil {
		gsm.currentState.Exit()
	}

	// Pop previous state from stack
	gsm.previousState = gsm.currentState
	gsm.currentState = gsm.stack[len(gsm.stack)-1]
	gsm.stack = gsm.stack[:len(gsm.stack)-1]

	// Enter new state
	if gsm.currentState != nil {
		err := gsm.currentState.Enter()
		if err != nil {
			return err
		}
	}

	// Emit state change event
	gsm.emitStateChangeEvent()

	return nil
}

// RegisterState registers a new state
func (gsm *GameStateManager) RegisterState(state GameState) error {
	gsm.mutex.Lock()
	defer gsm.mutex.Unlock()

	stateName := state.Name()
	if _, exists := gsm.states[stateName]; exists {
		return fmt.Errorf("state already registered: %s", stateName)
	}

	gsm.states[stateName] = state

	// Set state manager reference
	if baseState, ok := state.(*BaseGameState); ok {
		baseState.manager = gsm
		baseState.eventHandler = gsm.eventHandler
	}

	return nil
}

// AddTransition adds a state transition
func (gsm *GameStateManager) AddTransition(transition Transition) error {
	gsm.mutex.Lock()
	defer gsm.mutex.Unlock()

	key := TransitionKey{From: transition.From, To: transition.To}
	gsm.transitions[key] = transition

	return nil
}

// GetCurrentState returns the current state
func (gsm *GameStateManager) GetCurrentState() GameState {
	gsm.mutex.RLock()
	defer gsm.mutex.RUnlock()
	return gsm.currentState
}

// GetCurrentStateName returns the current state name
func (gsm *GameStateManager) GetCurrentStateName() string {
	gsm.mutex.RLock()
	defer gsm.mutex.RUnlock()
	
	if gsm.currentState == nil {
		return ""
	}
	return gsm.currentState.Name()
}

// GetStateStatistics returns state manager statistics
func (gsm *GameStateManager) GetStateStatistics() StateStats {
	gsm.mutex.RLock()
	defer gsm.mutex.RUnlock()

	return StateStats{
		CurrentState:   gsm.getCurrentStateName(),
		PreviousState:  gsm.previousState.Name(),
		StatesInStack:  len(gsm.stack),
		RegisteredStates: len(gsm.states),
		TotalTransitions: len(gsm.transitions),
		IsRunning:      gsm.isRunning,
		EventQueueSize: gsm.eventHandler.GetQueueSize(),
	}
}

// StateStats represents state manager statistics
type StateStats struct {
	CurrentState      string
	PreviousState     string
	StatesInStack     int
	RegisteredStates  int
	TotalTransitions  int
	IsRunning         bool
	EventQueueSize    int
}

// Internal methods

func (gsm *GameStateManager) changeState(newState GameState) error {
	// Exit current state
	if gsm.currentState != nil {
		gsm.currentState.Exit()
	}

	// Set new state
	gsm.previousState = gsm.currentState
	gsm.currentState = newState

	// Enter new state
	err := gsm.currentState.Enter()
	if err != nil {
		return err
	}

	// Emit state change event
	gsm.emitStateChangeEvent()

	return nil
}

func (gsm *GameStateManager) emitStateChangeEvent() {
	event := Event{
		Type:      EventStateChanged,
		Source:    "state_manager",
		Data: map[string]interface{}{
			"current":  gsm.currentState.Name(),
			"previous": gsm.previousState.Name(),
		},
		Timestamp: time.Now(),
	}

	gsm.eventHandler.EmitEvent(event)
}

func (gsm *GameStateManager) getCurrentStateName() string {
	if gsm.currentState == nil {
		return ""
	}
	return gsm.currentState.Name()
}

func (gsm *GameStateManager) registerDefaultStates() {
	// Register default states
	gsm.RegisterState(NewMainMenuState())
	gsm.RegisterState(NewPlayState())
	gsm.RegisterState(NewPauseState())
	gsm.RegisterState(NewSettingsState())
}

// EventManager implementation

func NewEventManager(bufferSize int) *EventManager {
	return &EventManager{
		listeners:  make(map[EventType][]EventListener),
		events:     make([]Event, 0, bufferSize),
		eventQueue: make(chan Event, bufferSize),
		maxEvents:  bufferSize,
	}
}

func (em *EventManager) Start() {
	// Start event processing goroutine
	go em.eventProcessingLoop()
}

func (em *EventManager) Stop() {
	close(em.eventQueue)
}

func (em *EventManager) EmitEvent(event Event) {
	select {
	case em.eventQueue <- event:
	default:
		// Drop event if queue is full
	}
}

func (em *EventManager) AddListener(listener EventListener) {
	em.mutex.Lock()
	defer em.mutex.Unlock()

	em.listeners[listener.EventType] = append(em.listeners[listener.EventType], listener)
}

func (em *EventManager) RemoveListener(listenerID string) {
	em.mutex.Lock()
	defer em.mutex.Unlock()

	for eventType, listeners := range em.listeners {
		for i, listener := range listeners {
			if listener.ID == listenerID {
				em.listeners[eventType] = append(listeners[:i], listeners[i+1:]...)
				break
			}
		}
	}
}

func (em *EventManager) ProcessEvents() {
	em.mutex.Lock()
	defer em.mutex.Unlock()

	for eventType, listeners := range em.listeners {
		for _, listener := range listeners {
			if !listener.Active {
				continue
			}

			// Process events for this listener type
			for i := len(em.events) - 1; i >= 0; i-- {
				event := em.events[i]
				if event.Type == eventType {
					go listener.Callback(event)
					em.events = append(em.events[:i], em.events[i+1:]...)
				}
			}
		}
	}
}

func (em *EventManager) GetQueueSize() int {
	return len(em.eventQueue)
}

func (em *EventManager) eventProcessingLoop() {
	for event := range em.eventQueue {
		em.mutex.Lock()
		if len(em.events) < em.maxEvents {
			em.events = append(em.events, event)
		}
		em.mutex.Unlock()
	}
}

// BaseGameState methods

func NewBaseGameState(name string) *BaseGameState {
	return &BaseGameState{
		name:      name,
		active:    false,
		enterTime: time.Now(),
	}
}

func (bs *BaseGameState) Name() string {
	return bs.name
}

func (bs *BaseGameState) Enter() error {
	bs.active = true
	bs.enterTime = time.Now()
	return nil
}

func (bs *BaseGameState) Exit() error {
	bs.active = false
	return nil
}

func (bs *BaseGameState) Update(deltaTime time.Duration) error {
	return nil
}

func (bs *BaseGameState) Render() error {
	return nil
}

func (bs *BaseGameState) HandleEvent(event Event) error {
	return nil
}

func (bs *BaseGameState) IsActive() bool {
	return bs.active
}

// Specific state implementations

func NewMainMenuState() *MainMenuState {
	menuState := &MainMenuState{
		BaseGameState: NewBaseGameState("main_menu"),
		selected:      0,
	}

	menuState.menuItems = []MenuItem{
		{Text: "New Game", Action: menuState.startNewGame, Enabled: true, Visible: true},
		{Text: "Load Game", Action: menuState.loadGame, Enabled: true, Visible: true},
		{Text: "Settings", Action: menuState.openSettings, Enabled: true, Visible: true},
		{Text: "Exit", Action: menuState.exitGame, Enabled: true, Visible: true},
	}

	return menuState
}

func (mms *MainMenuState) Enter() error {
	mms.BaseGameState.Enter()
	fmt.Println("Entering Main Menu")
	return nil
}

func (mms *MainMenuState) Update(deltaTime time.Duration) error {
	// Handle menu navigation
	return nil
}

func (mms *MainMenuState) Render() error {
	fmt.Println("Rendering Main Menu")
	return nil
}

func (mms *MainMenuState) HandleEvent(event Event) error {
	if event.Type == EventInput {
		// Handle menu input
	}
	return nil
}

func (mms *MainMenuState) startNewGame() error {
	fmt.Println("Starting new game")
	return mms.manager.ChangeState("play")
}

func (mms *MainMenuState) loadGame() error {
	fmt.Println("Loading game")
	return nil
}

func (mms *MainMenuState) openSettings() error {
	fmt.Println("Opening settings")
	return mms.manager.ChangeState("settings")
}

func (mms *MainMenuState) exitGame() error {
	fmt.Println("Exiting game")
	return mms.manager.Stop()
}

func NewPlayState() *PlayState {
	return &PlayState{
		BaseGameState: NewBaseGameState("play"),
		level:         1,
		score:         0,
		lives:         3,
		paused:        false,
	}
}

func (ps *PlayState) Enter() error {
	ps.BaseGameState.Enter()
	fmt.Printf("Starting Play State - Level %d\n", ps.level)
	return nil
}

func (ps *PlayState) Update(deltaTime time.Duration) error {
	if ps.paused {
		return nil
	}
	
	ps.timeElapsed += deltaTime
	return nil
}

func (ps *PlayState) Render() error {
	fmt.Printf("Rendering Play State - Score: %d, Lives: %d\n", ps.score, ps.lives)
	return nil
}

func (ps *PlayState) HandleEvent(event Event) error {
	if event.Type == EventInput {
		// Handle gameplay input
	} else if event.Type == EventStateChanged {
		if event.Data == "pause" {
			ps.paused = true
		}
	}
	return nil
}

func (ps *PlayState) Pause() error {
	ps.paused = true
	return ps.manager.PushState("pause")
}

func (ps *PlayState) Resume() error {
	ps.paused = false
	return nil
}

func NewPauseState() *PauseState {
	pauseState := &PauseState{
		BaseGameState: NewBaseGameState("pause"),
		selected:      0,
	}

	pauseState.menuItems = []MenuItem{
		{Text: "Resume", Action: pauseState.resumeGame, Enabled: true, Visible: true},
		{Text: "Settings", Action: pauseState.openSettings, Enabled: true, Visible: true},
		{Text: "Main Menu", Action: pauseState.goToMainMenu, Enabled: true, Visible: true},
	}

	return pauseState
}

func (ps *PauseState) Enter() error {
	ps.BaseGameState.Enter()
	fmt.Println("Entering Pause State")
	return nil
}

func (ps *PauseState) Update(deltaTime time.Duration) error {
	return nil
}

func (ps *PauseState) Render() error {
	fmt.Println("Rendering Pause State")
	return nil
}

func (ps *PauseState) HandleEvent(event Event) error {
	if event.Type == EventInput {
		// Handle pause menu input
	}
	return nil
}

func (ps *PauseState) resumeGame() error {
	fmt.Println("Resuming game")
	return ps.manager.PopState()
}

func (ps *PauseState) openSettings() error {
	fmt.Println("Opening settings from pause")
	return ps.manager.PushState("settings")
}

func (ps *PauseState) goToMainMenu() error {
	fmt.Println("Going to main menu")
	return ps.manager.ChangeState("main_menu")
}

func NewSettingsState() *SettingsState {
	return &SettingsState{
		BaseGameState: NewBaseGameState("settings"),
		settings:      make(map[string]interface{}),
		selected:      0,
		changed:       false,
	}
}

func (ss *SettingsState) Enter() error {
	ss.BaseGameState.Enter()
	fmt.Println("Entering Settings State")
	return nil
}

func (ss *SettingsState) Update(deltaTime time.Duration) error {
	return nil
}

func (ss *SettingsState) Render() error {
	fmt.Println("Rendering Settings State")
	return nil
}

func (ss *SettingsState) HandleEvent(event Event) error {
	if event.Type == EventInput {
		// Handle settings input
	}
	return nil
}



// Example usage
func main() {
	settings := StateManagerSettings{
		MaxStates:        10,
		MaxTransitions:   20,
		EventBufferSize:  100,
		EnableLogging:    true,
		DebugMode:        true,
	}

	gsm := NewGameStateManager(settings)
	
	// Start state manager
	if err := gsm.Start(); err != nil {
		fmt.Printf("Error starting state manager: %v\n", err)
		return
	}

	// Main game loop
	for i := 0; i < 10; i++ {
		deltaTime := time.Millisecond * 16 // ~60 FPS
		
		// Update
		gsm.Update(deltaTime)
		
		// Render
		gsm.Render()
		
		// Change state after a few frames for demo
		if i == 3 {
			gsm.ChangeState("play")
		} else if i == 6 {
			gsm.ChangeState("pause")
		} else if i == 8 {
			gsm.ChangeState("main_menu")
		}
	}

	// Get statistics
	stats := gsm.GetStateStatistics()
	fmt.Printf("\nState Manager Statistics:\n")
	fmt.Printf("Current State: %s\n", stats.CurrentState)
	fmt.Printf("Registered States: %d\n", stats.RegisteredStates)
	fmt.Printf("States in Stack: %d\n", stats.StatesInStack)
	fmt.Printf("Total Transitions: %d\n", stats.TotalTransitions)
	fmt.Printf("Event Queue Size: %d\n", stats.EventQueueSize)

	// Stop state manager
	gsm.Stop()

	fmt.Println("\nGame State Manager initialized successfully!")
}
