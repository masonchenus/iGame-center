package main

import (
	"fmt"
	"sync"
	"time"
)

// InputManager handles all user input (keyboard, mouse, gamepad)
type InputManager struct {
	keys         map[KeyCode]KeyState
	mouse        MouseState
	gamepads     map[int]*GamepadState
	activeKeys   []KeyCode
	touchEvents  []TouchEvent
	gestures     []Gesture
	bindings     map[ActionName]KeyBinding
	pressed      map[ActionName]bool
	released     map[ActionName]bool
	settings     InputSettings
	mutex        sync.RWMutex
	isEnabled    bool
}

// KeyCode represents keyboard key codes
type KeyCode int

const (
	// Letters
	KeyA KeyCode = iota + 1
	KeyB
	KeyC
	KeyD
	KeyE
	KeyF
	KeyG
	KeyH
	KeyI
	KeyJ
	KeyK
	KeyL
	KeyM
	KeyN
	KeyO
	KeyP
	KeyQ
	KeyR
	KeyS
	KeyT
	KeyU
	KeyV
	KeyW
	KeyX
	KeyY
	KeyZ

	// Numbers
	Key0
	Key1
	Key2
	Key3
	Key4
	Key5
	Key6
	Key7
	Key8
	Key9

	// Function keys
	KeyF1
	KeyF2
	KeyF3
	KeyF4
	KeyF5
	KeyF6
	KeyF7
	KeyF8
	KeyF9
	KeyF10
	KeyF11
	KeyF12

	// Special keys
	Space
	Enter
	Escape
	Tab
	Backspace
	Delete
	Insert
	Home
	End
	PageUp
	PageDown

	// Arrow keys
	Up
	Down
	Left
	Right

	// Modifiers
	ShiftLeft
	ShiftRight
	ControlLeft
	ControlRight
	AltLeft
	AltRight

	// Mouse buttons
	MouseLeft
	MouseRight
	MouseMiddle
	MouseX1
	MouseX2
)

// KeyState represents the state of a key
type KeyState struct {
	Code      KeyCode
	Pressed   bool
	JustPressed  bool
	JustReleased bool
	Timestamp time.Time
	Repeat    bool
}

// MouseState represents mouse input state
type MouseState struct {
	X, Y          int
	DeltaX, DeltaY int
	WheelDelta    int
	Buttons       map[KeyCode]bool
	Visible       bool
}

// GamepadState represents gamepad input state
type GamepadState struct {
	ID            int
	Connected     bool
	Buttons       map[GamepadButton]bool
	Axes          map[GamepadAxis]float32
	DeadZone      float32
	Vibration     float32
}

// TouchEvent represents touch input
type TouchEvent struct {
	ID      int
	X, Y    float32
	DeltaX, DeltaY float32
	Pressure float32
	Phase   TouchPhase
	Time    time.Time
}

// Gesture represents gesture input
type Gesture struct {
	Type      GestureType
	StartPos  Vector2
	EndPos    Vector2
	Distance  float32
	Duration  time.Duration
	Velocity  float32
	Time      time.Time
}

// InputSettings contains input manager configuration
type InputSettings struct {
	MouseSensitivity float32
	KeyboardRepeatDelay time.Duration
	KeyboardRepeatRate time.Duration
	GamepadDeadZone    float32
	EnableTouch        bool
	EnableGestures     bool
	EnableMouse        bool
	EnableKeyboard     bool
	EnableGamepad      bool
}

// ActionName represents input action names
type ActionName string

const (
	ActionMove    ActionName = "move"
	ActionJump    ActionName = "jump"
	ActionAttack  ActionName = "attack"
	ActionInteract ActionName = "interact"
	ActionPause   ActionName = "pause"
	ActionMenu    ActionName = "menu"
	ActionConfirm ActionName = "confirm"
	ActionCancel  ActionName = "cancel"
)

// KeyBinding represents a key binding for an action
type KeyBinding struct {
	Action    ActionName
	Keys      []KeyCode
	Gamepad   *GamepadBinding
	Mouse     *MouseBinding
	Touch     *TouchBinding
}

// GamepadBinding represents gamepad button binding
type GamepadBinding struct {
	Buttons []GamepadButton
	Axes    []GamepadAxis
	Threshold float32
}

// MouseBinding represents mouse button binding
type MouseBinding struct {
	Button KeyCode
	ClickType ClickType
}

// TouchBinding represents touch gesture binding
type TouchBinding struct {
	Gesture GestureType
	Threshold float32
}

// GamepadButton represents gamepad buttons
type GamepadButton int

const (
	ButtonA GamepadButton = iota
	ButtonB
	ButtonX
	ButtonY
	LeftShoulder
	RightShoulder
	LeftTrigger
	RightTrigger
	LeftStick
	RightStick
	Start
	Back
	Guide
)

// GamepadAxis represents gamepad axes
type GamepadAxis int

const (
	LeftX GamepadAxis = iota
	LeftY
	RightX
	RightY
)

// TouchPhase represents touch event phases
type TouchPhase int

const (
	TouchBegan TouchPhase = iota
	TouchMoved
	TouchEnded
	TouchCanceled
)

// GestureType represents gesture types
type GestureType int

const (
	Tap GestureType = iota
	LongPress
	Swipe
	Pinch
	Rotate
)

// Vector2 represents 2D coordinates
type Vector2 struct {
	X, Y float32
}

// ClickType represents mouse click types
type ClickType int

const (
	SingleClick ClickType = iota
	DoubleClick
	TripleClick
	Hold
)

// NewInputManager creates a new input manager
func NewInputManager(settings InputSettings) *InputManager {
	im := &InputManager{
		keys:       make(map[KeyCode]KeyState),
		gamepads:   make(map[int]*GamepadState),
		bindings:   make(map[ActionName]KeyBinding),
		pressed:    make(map[ActionName]bool),
		released:   make(map[ActionName]bool),
		activeKeys: make([]KeyCode, 0),
		settings:   settings,
		isEnabled:  true,
	}

	// Initialize mouse state
	im.mouse.Buttons = make(map[KeyCode]bool)

	// Initialize default bindings
	im.setupDefaultBindings()

	return im
}

// Start starts the input manager
func (im *InputManager) Start() error {
	im.mutex.Lock()
	defer im.mutex.Unlock()

	im.isEnabled = true

	// Start input polling in goroutine
	go im.inputPollingLoop()

	fmt.Println("Input Manager started")
	return nil
}

// Stop stops the input manager
func (im *InputManager) Stop() {
	im.mutex.Lock()
	defer im.mutex.Unlock()

	im.isEnabled = false
	im.clearInput()

	fmt.Println("Input Manager stopped")
}

// Update updates the input state
func (im *InputManager) Update() {
	im.mutex.Lock()
	defer im.mutex.Unlock()

	if !im.isEnabled {
		return
	}

	// Reset just pressed/released states
	im.resetKeyStates()
	im.updateActionStates()
	im.processTouchEvents()
	im.processGestures()
}

// IsKeyPressed checks if a key is currently pressed
func (im *InputManager) IsKeyPressed(key KeyCode) bool {
	im.mutex.RLock()
	defer im.mutex.RUnlock()

	state, exists := im.keys[key]
	return exists && state.Pressed
}

// IsKeyJustPressed checks if a key was just pressed this frame
func (im *InputManager) IsKeyJustPressed(key KeyCode) bool {
	im.mutex.RLock()
	defer im.mutex.RUnlock()

	state, exists := im.keys[key]
	return exists && state.JustPressed
}

// IsKeyJustReleased checks if a key was just released this frame
func (im *InputManager) IsKeyJustReleased(key KeyCode) bool {
	im.mutex.RLock()
	defer im.mutex.RUnlock()

	state, exists := im.keys[key]
	return exists && state.JustReleased
}

// IsActionPressed checks if an action is currently pressed
func (im *InputManager) IsActionPressed(action ActionName) bool {
	im.mutex.RLock()
	defer im.mutex.RUnlock()

	return im.pressed[action]
}

// IsActionJustPressed checks if an action was just pressed
func (im *InputManager) IsActionJustPressed(action ActionName) bool {
	im.mutex.RLock()
	defer im.mutex.RUnlock()

	binding, exists := im.bindings[action]
	if !exists {
		return false
	}

	// Check keyboard
	for _, key := range binding.Keys {
		if im.IsKeyJustPressed(key) {
			return true
		}
	}

	// Check gamepad
	if binding.Gamepad != nil {
		// Check gamepad buttons and axes
		for _, button := range binding.Gamepad.Buttons {
			if im.IsGamepadButtonJustPressed(0, button) {
				return true
			}
		}
	}

	// Check mouse
	if binding.Mouse != nil {
		if im.IsMouseButtonJustPressed(binding.Mouse.Button) {
			return true
		}
	}

	// Check touch
	if binding.Touch != nil {
		if im.IsGestureJustPressed(binding.Touch.Gesture) {
			return true
		}
	}

	return false
}

// GetMousePosition returns the current mouse position
func (im *InputManager) GetMousePosition() (int, int) {
	im.mutex.RLock()
	defer im.mutex.RUnlock()

	return im.mouse.X, im.mouse.Y
}

// GetMouseDelta returns the mouse movement delta
func (im *InputManager) GetMouseDelta() (int, int) {
	im.mutex.RLock()
	defer im.mutex.RUnlock()

	return im.mouse.DeltaX, im.mouse.DeltaY
}

// IsMouseButtonPressed checks if a mouse button is pressed
func (im *InputManager) IsMouseButtonPressed(button KeyCode) bool {
	im.mutex.RLock()
	defer im.mutex.RUnlock()

	return im.mouse.Buttons[button]
}

// IsGamepadConnected checks if a gamepad is connected
func (im *InputManager) IsGamepadConnected(id int) bool {
	im.mutex.RLock()
	defer im.mutex.RUnlock()

	gamepad, exists := im.gamepads[id]
	return exists && gamepad.Connected
}

// GetGamepadAxis gets the value of a gamepad axis
func (im *InputManager) GetGamepadAxis(id int, axis GamepadAxis) float32 {
	im.mutex.RLock()
	defer im.mutex.RUnlock()

	gamepad, exists := im.gamepads[id]
	if !exists || !gamepad.Connected {
		return 0.0
	}

	value := gamepad.Axes[axis]
	
	// Apply dead zone
	if gamepad.DeadZone > 0 {
		if value > -gamepad.DeadZone && value < gamepad.DeadZone {
			return 0.0
		}
	}

	return value
}

// IsGamepadButtonPressed checks if a gamepad button is pressed
func (im *InputManager) IsGamepadButtonPressed(id int, button GamepadButton) bool {
	im.mutex.RLock()
	defer im.mutex.RUnlock()

	gamepad, exists := im.gamepads[id]
	if !exists || !gamepad.Connected {
		return false
	}

	return gamepad.Buttons[button]
}

// IsGamepadButtonJustPressed checks if a gamepad button was just pressed
func (im *InputManager) IsGamepadButtonJustPressed(id int, button GamepadButton) bool {
	im.mutex.RLock()
	defer im.mutex.RUnlock()

	gamepad, exists := im.gamepads[id]
	if !exists || !gamepad.Connected {
		return false
	}

	// This would need more state tracking for just pressed
	return gamepad.Buttons[button]
}

// GetTouchCount returns the number of active touches
func (im *InputManager) GetTouchCount() int {
	im.mutex.RLock()
	defer im.mutex.RUnlock()

	return len(im.touchEvents)
}

// GetTouchPosition returns the position of a touch
func (im *InputManager) GetTouchPosition(index int) (float32, float32, bool) {
	im.mutex.RLock()
	defer im.mutex.RUnlock()

	if index >= 0 && index < len(im.touchEvents) {
		touch := im.touchEvents[index]
		return touch.X, touch.Y, true
	}

	return 0, 0, false
}

// IsGestureJustPressed checks if a gesture was just performed
func (im *InputManager) IsGestureJustPressed(gesture GestureType) bool {
	im.mutex.RLock()
	defer im.mutex.RUnlock()

	for _, g := range im.gestures {
		if g.Type == gesture {
			return true
		}
	}

	return false
}

// AddBinding adds a key binding for an action
func (im *InputManager) AddBinding(binding KeyBinding) error {
	im.mutex.Lock()
	defer im.mutex.Unlock()

	if _, exists := im.bindings[binding.Action]; exists {
		return fmt.Errorf("binding already exists for action: %s", binding.Action)
	}

	im.bindings[binding.Action] = binding
	return nil
}

// RemoveBinding removes a key binding
func (im *InputManager) RemoveBinding(action ActionName) error {
	im.mutex.Lock()
	defer im.mutex.Unlock()

	if _, exists := im.bindings[action]; !exists {
		return fmt.Errorf("binding not found for action: %s", action)
	}

	delete(im.bindings, action)
	return nil
}

// SetMouseVisible sets mouse visibility
func (im *InputManager) SetMouseVisible(visible bool) {
	im.mutex.Lock()
	defer im.mutex.Unlock()

	im.mouse.Visible = visible
}

// GetInputStats returns input system statistics
func (im *InputManager) GetInputStats() InputStats {
	im.mutex.RLock()
	defer im.mutex.RUnlock()

	pressedKeys := 0
	for _, state := range im.keys {
		if state.Pressed {
			pressedKeys++
		}
	}

	connectedGamepads := 0
	for _, gamepad := range im.gamepads {
		if gamepad.Connected {
			connectedGamepads++
		}
	}

	return InputStats{
		PressedKeys:      pressedKeys,
		ActiveGamepads:   connectedGamepads,
		ActiveTouches:    len(im.touchEvents),
		DetectedGestures: len(im.gestures),
		InputBindings:    len(im.bindings),
		IsEnabled:        im.isEnabled,
	}
}

// InputStats represents input system statistics
type InputStats struct {
	PressedKeys      int
	ActiveGamepads   int
	ActiveTouches    int
	DetectedGestures int
	InputBindings    int
	IsEnabled        bool
}

// Internal methods

func (im *InputManager) inputPollingLoop() {
	ticker := time.NewTicker(time.Millisecond * 16) // 60 FPS polling
	defer ticker.Stop()

	for im.isEnabled {
		select {
		case <-ticker.C:
			im.pollInputDevices()
		}
	}
}

func (im *InputManager) pollInputDevices() {
	im.mutex.Lock()
	defer im.mutex.Unlock()

	// In a real implementation, this would poll actual input devices
	// For demo purposes, we'll simulate some input

	// Simulate some key presses for testing
	if len(im.activeKeys) == 0 {
		im.activeKeys = []KeyCode{KeyW, KeyA, KeyS, KeyD}
	}

	// Update mouse position (simulate movement)
	im.mouse.DeltaX = 1
	im.mouse.DeltaY = 0
	im.mouse.X += im.mouse.DeltaX
	im.mouse.Y += im.mouse.DeltaY

	// Simulate gamepad connection
	if len(im.gamepads) == 0 {
		im.gamepads[0] = &GamepadState{
			ID:          0,
			Connected:   true,
			Buttons:     make(map[GamepadButton]bool),
			Axes:        make(map[GamepadAxis]float32),
			DeadZone:    im.settings.GamepadDeadZone,
			Vibration:   0.0,
		}
	}
}

func (im *InputManager) resetKeyStates() {
	for key, state := range im.keys {
		state.JustPressed = false
		state.JustReleased = false
		im.keys[key] = state
	}
}

func (im *InputManager) updateActionStates() {
	// Reset pressed/released states
	for action := range im.pressed {
		delete(im.pressed, action)
	}
	for action := range im.released {
		delete(im.released, action)
	}

	// Check all bindings
	for action, binding := range im.bindings {
		pressed := false

		// Check keyboard
		for _, key := range binding.Keys {
			if im.IsKeyPressed(key) {
				pressed = true
				break
			}
		}

		// Check gamepad
		if !pressed && binding.Gamepad != nil {
			for _, button := range binding.Gamepad.Buttons {
				if im.IsGamepadButtonPressed(0, button) {
					pressed = true
					break
				}
			}
		}

		// Check mouse
		if !pressed && binding.Mouse != nil {
			if im.IsMouseButtonPressed(binding.Mouse.Button) {
				pressed = true
			}
		}

		// Update action state
		if pressed {
			im.pressed[action] = true
		}
	}
}

func (im *InputManager) processTouchEvents() {
	// Process touch events for gestures
	// In real implementation, this would handle actual touch input
}

func (im *InputManager) processGestures() {
	// Process touch events to detect gestures
	// In real implementation, this would analyze touch patterns
}

func (im *InputManager) clearInput() {
	// Clear all input states
	im.keys = make(map[KeyCode]KeyState)
	im.mouse.Buttons = make(map[KeyCode]bool)
	im.gamepads = make(map[int]*GamepadState)
	im.activeKeys = make([]KeyCode, 0)
	im.touchEvents = make([]TouchEvent, 0)
	im.gestures = make([]Gesture, 0)
	im.pressed = make(map[ActionName]bool)
	im.released = make(map[ActionName]bool)
}

func (im *InputManager) setupDefaultBindings() {
	// Set up default key bindings
	defaultBindings := []KeyBinding{
		{
			Action: ActionMove,
			Keys:   []KeyCode{KeyW, KeyA, KeyS, KeyD},
		},
		{
			Action: ActionJump,
			Keys:   []KeyCode{Space},
		},
		{
			Action: ActionAttack,
			Keys:   []KeyCode{KeyJ},
		},
		{
			Action: ActionInteract,
			Keys:   []KeyCode{KeyE},
		},
		{
			Action: ActionPause,
			Keys:   []KeyCode{Escape},
		},
		{
			Action: ActionMenu,
			Keys:   []KeyCode{KeyM},
		},
		{
			Action: ActionConfirm,
			Keys:   []KeyCode{Enter},
		},
		{
			Action: ActionCancel,
			Keys:   []KeyCode{Escape},
		},
	}

	for _, binding := range defaultBindings {
		im.bindings[binding.Action] = binding
	}
}

// SimulateInput simulates input for testing
func (im *InputManager) SimulateInput(key KeyCode, pressed bool) {
	im.mutex.Lock()
	defer im.mutex.Unlock()

	state := im.keys[key]
	wasPressed := state.Pressed
	
	state.Code = key
	state.Pressed = pressed
	state.Timestamp = time.Now()
	
	if pressed && !wasPressed {
		state.JustPressed = true
	} else if !pressed && wasPressed {
		state.JustReleased = true
	}
	
	im.keys[key] = state
}

// Example usage
func main() {
	settings := InputSettings{
		MouseSensitivity:     1.0,
		KeyboardRepeatDelay:  time.Millisecond * 250,
		KeyboardRepeatRate:   time.Millisecond * 50,
		GamepadDeadZone:      0.15,
		EnableTouch:          true,
		EnableGestures:       true,
		EnableMouse:          true,
		EnableKeyboard:       true,
		EnableGamepad:        true,
	}

	im := NewInputManager(settings)
	
	// Start input manager
	if err := im.Start(); err != nil {
		fmt.Printf("Error starting input manager: %v\n", err)
		return
	}

	// Test input simulation
	im.SimulateInput(KeyW, true)
	im.SimulateInput(KeyA, true)
	
	// Update input state
	im.Update()

	// Check states
	fmt.Printf("W key pressed: %v\n", im.IsKeyPressed(KeyW))
	fmt.Printf("A key just pressed: %v\n", im.IsKeyJustPressed(KeyA))
	fmt.Printf("Move action pressed: %v\n", im.IsActionPressed(ActionMove))
	fmt.Printf("Move action just pressed: %v\n", im.IsActionJustPressed(ActionMove))

	// Test mouse
	mouseX, mouseY := im.GetMousePosition()
	fmt.Printf("Mouse position: (%d, %d)\n", mouseX, mouseY)

	// Test gamepad
	fmt.Printf("Gamepad 0 connected: %v\n", im.IsGamepadConnected(0))
	
	if im.IsGamepadConnected(0) {
		leftX := im.GetGamepadAxis(0, LeftX)
		leftY := im.GetGamepadAxis(0, LeftY)
		fmt.Printf("Left stick: (%.2f, %.2f)\n", leftX, leftY)
	}

	// Get statistics
	stats := im.GetInputStats()
	fmt.Printf("\nInput Statistics:\n")
	fmt.Printf("Pressed keys: %d\n", stats.PressedKeys)
	fmt.Printf("Active gamepads: %d\n", stats.ActiveGamepads)
	fmt.Printf("Input bindings: %d\n", stats.InputBindings)
	fmt.Printf("Is enabled: %v\n", stats.IsEnabled)

	// Test input release
	im.SimulateInput(KeyW, false)
	im.Update()
	
	fmt.Printf("W key pressed after release: %v\n", im.IsKeyPressed(KeyW))

	// Stop input manager
	im.Stop()

	fmt.Println("\nInput Manager initialized successfully!")
}
