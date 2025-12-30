// Input Manager - Handle user input, keyboard, mouse, and controller input
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock};
use serde::{Deserialize, Serialize};

/// Input types supported by the system
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum InputType {
    Keyboard,
    Mouse,
    Gamepad,
    Touch,
    Gesture,
}

/// Key codes for keyboard input
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum KeyCode {
    // Function keys
    F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12,
    
    // Letter keys
    A, B, C, D, E, F, G, H, I, J, K, L, M,
    N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
    
    // Number keys
    Digit0, Digit1, Digit2, Digit3, Digit4,
    Digit5, Digit6, Digit7, Digit8, Digit9,
    
    // Special keys
    Space, Enter, Tab, Backspace, Delete,
    Insert, Home, End, PageUp, PageDown,
    
    // Arrow keys
    ArrowUp, ArrowDown, ArrowLeft, ArrowRight,
    
    // Modifier keys
    ShiftLeft, ShiftRight, ControlLeft, ControlRight,
    AltLeft, AltRight, MetaLeft, MetaRight,
    
    // Punctuation
    Minus, Equal, BracketLeft, BracketRight,
    Backslash, Semicolon, Quote, Comma, Period, Slash,
}

/// Mouse button codes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MouseButton {
    Left,
    Right,
    Middle,
    X1,
    X2,
}

/// Gamepad button codes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum GamepadButton {
    A, B, X, Y,
    LeftBumper, RightBumper,
    LeftTrigger, RightTrigger,
    Select, Start,
    LeftStick, RightStick,
    DPadUp, DPadDown, DPadLeft, DPadRight,
    Home,
}

/// Input action binding
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InputBinding {
    pub action_name: String,
    pub key_codes: Vec<KeyCode>,
    pub mouse_buttons: Vec<MouseButton>,
    pub gamepad_buttons: Vec<GamepadButton>,
    pub requires_all: bool, // If true, all inputs must be pressed
}

/// Input axis binding for analog input
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AxisBinding {
    pub axis_name: String,
    pub positive_key: Option<KeyCode>,
    pub negative_key: Option<KeyCode>,
    pub gamepad_axis: Option<GamepadAxis>,
    pub sensitivity: f32,
    pub dead_zone: f32,
}

/// Gamepad axis codes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum GamepadAxis {
    LeftX, LeftY,
    RightX, RightY,
    LeftTrigger,
    RightTrigger,
}

/// Input event types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum InputEvent {
    KeyPressed { key: KeyCode, timestamp: std::time::Instant },
    KeyReleased { key: KeyCode, timestamp: std::time::Instant },
    MouseButtonPressed { button: MouseButton, position: (i32, i32), timestamp: std::time::Instant },
    MouseButtonReleased { button: MouseButton, position: (i32, i32), timestamp: std::time::Instant },
    MouseMoved { position: (i32, i32), delta: (i32, i32), timestamp: std::time::Instant },
    MouseWheelScrolled { delta: i32, position: (i32, i32), timestamp: std::time::Instant },
    GamepadButtonPressed { button: GamepadButton, gamepad_id: u32, timestamp: std::time::Instant },
    GamepadButtonReleased { button: GamepadButton, gamepad_id: u32, timestamp: std::time::Instant },
    GamepadAxisChanged { axis: GamepadAxis, value: f32, gamepad_id: u32, timestamp: std::time::Instant },
    TouchBegan { touch_id: u32, position: (f32, f32), timestamp: std::time::Instant },
    TouchMoved { touch_id: u32, position: (f32, f32), delta: (f32, f32), timestamp: std::time::Instant },
    TouchEnded { touch_id: u32, position: (f32, f32), timestamp: std::time::Instant },
}

/// Input state for tracking key/button presses
#[derive(Debug, Clone)]
pub struct InputState {
    pub keys_pressed: HashSet<KeyCode>,
    pub keys_released: HashSet<KeyCode>,
    pub mouse_buttons_pressed: HashSet<MouseButton>,
    pub mouse_buttons_released: HashSet<MouseButton>,
    pub mouse_position: (i32, i32),
    pub mouse_delta: (i32, i32),
    pub mouse_wheel_delta: i32,
    pub gamepad_states: HashMap<u32, GamepadState>,
    pub touch_states: HashMap<u32, TouchState>,
}

/// Gamepad input state
#[derive(Debug, Clone)]
pub struct GamepadState {
    pub buttons_pressed: HashSet<GamepadButton>,
    pub buttons_released: HashSet<GamepadButton>,
    pub axes: HashMap<GamepadAxis, f32>,
    pub connected: bool,
    pub name: String,
}

/// Touch input state
#[derive(Debug, Clone)]
pub struct TouchState {
    pub position: (f32, f32),
    pub delta: (f32, f32),
    pub pressure: f32,
    pub active: bool,
}

/// Input configuration
#[derive(Debug, Clone)]
pub struct InputConfig {
    pub enable_keyboard: bool,
    pub enable_mouse: bool,
    pub enable_gamepad: bool,
    pub enable_touch: bool,
    pub mouse_sensitivity: f32,
    pub gamepad_dead_zone: f32,
    pub double_click_time: std::time::Duration,
    pub hold_threshold: std::time::Duration,
    pub max_touches: usize,
}

/// Input manager statistics
#[derive(Debug, Clone)]
pub struct InputStats {
    pub total_key_presses: u64,
    pub total_mouse_clicks: u64,
    pub total_gamepad_inputs: u64,
    pub total_touch_events: u64,
    pub average_frame_time: std::time::Duration,
    pub input_lag: std::time::Duration,
}

/// Main input manager
pub struct InputManager {
    state: Arc<RwLock<InputState>>,
    bindings: HashMap<String, InputBinding>,
    axis_bindings: HashMap<String, AxisBinding>,
    config: InputConfig,
    event_queue: Arc<RwLock<Vec<InputEvent>>>,
    stats: Arc<RwLock<InputStats>>,
    pressed_keys_frame: HashSet<KeyCode>,
    pressed_mouse_buttons_frame: HashSet<MouseButton>,
    pressed_gamepad_buttons_frame: HashMap<u32, HashSet<GamepadButton>>,
    axis_values: HashMap<String, f32>,
}

impl InputManager {
    /// Create a new Input Manager
    pub fn new(config: InputConfig) -> Self {
        Self {
            state: Arc::new(RwLock::new(InputState {
                keys_pressed: HashSet::new(),
                keys_released: HashSet::new(),
                mouse_buttons_pressed: HashSet::new(),
                mouse_buttons_released: HashSet::new(),
                mouse_position: (0, 0),
                mouse_delta: (0, 0),
                mouse_wheel_delta: 0,
                gamepad_states: HashMap::new(),
                touch_states: HashMap::new(),
            })),
            bindings: HashMap::new(),
            axis_bindings: HashMap::new(),
            config,
            event_queue: Arc::new(RwLock::new(Vec::new())),
            stats: Arc::new(RwLock::new(InputStats {
                total_key_presses: 0,
                total_mouse_clicks: 0,
                total_gamepad_inputs: 0,
                total_touch_events: 0,
                average_frame_time: std::time::Duration::from_millis(16),
                input_lag: std::time::Duration::from_millis(0),
            })),
            pressed_keys_frame: HashSet::new(),
            pressed_mouse_buttons_frame: HashSet::new(),
            pressed_gamepad_buttons_frame: HashMap::new(),
            axis_values: HashMap::new(),
        }
    }

    /// Update input state each frame
    pub fn update(&mut self, delta_time: std::time::Duration) {
        // Clear frame-specific state
        self.pressed_keys_frame.clear();
        self.pressed_mouse_buttons_frame.clear();
        self.pressed_gamepad_buttons_frame.clear();
        
        // Update axis values
        self.update_axis_values();
        
        // Process event queue
        self.process_events();
        
        // Update statistics
        self.update_stats(delta_time);
    }

    /// Check if a key is currently pressed
    pub fn is_key_pressed(&self, key: KeyCode) -> bool {
        let state = self.state.read().unwrap();
        state.keys_pressed.contains(&key)
    }

    /// Check if a key was just pressed this frame
    pub fn is_key_just_pressed(&self, key: KeyCode) -> bool {
        self.pressed_keys_frame.contains(&key)
    }

    /// Check if a key was just released this frame
    pub fn is_key_just_released(&self, key: KeyCode) -> bool {
        let state = self.state.read().unwrap();
        state.keys_released.contains(&key)
    }

    /// Check if a mouse button is currently pressed
    pub fn is_mouse_button_pressed(&self, button: MouseButton) -> bool {
        let state = self.state.read().unwrap();
        state.mouse_buttons_pressed.contains(&button)
    }

    /// Check if a mouse button was just pressed this frame
    pub fn is_mouse_button_just_pressed(&self, button: MouseButton) -> bool {
        self.pressed_mouse_buttons_frame.contains(&button)
    }

    /// Get mouse position
    pub fn get_mouse_position(&self) -> (i32, i32) {
        let state = self.state.read().unwrap();
        state.mouse_position
    }

    /// Get mouse delta (movement since last frame)
    pub fn get_mouse_delta(&self) -> (i32, i32) {
        let state = self.state.read().unwrap();
        state.mouse_delta
    }

    /// Get mouse wheel delta
    pub fn get_mouse_wheel_delta(&self) -> i32 {
        let state = self.state.read().unwrap();
        state.mouse_wheel_delta
    }

    /// Check if a gamepad button is pressed
    pub fn is_gamepad_button_pressed(&self, button: GamepadButton, gamepad_id: u32) -> bool {
        let state = self.state.read().unwrap();
        state.gamepad_states.get(&gamepad_id)
            .map(|gp_state| gp_state.buttons_pressed.contains(&button))
            .unwrap_or(false)
    }

    /// Get gamepad axis value
    pub fn get_gamepad_axis_value(&self, axis: GamepadAxis, gamepad_id: u32) -> f32 {
        let state = self.state.read().unwrap();
        state.gamepad_states.get(&gamepad_id)
            .and_then(|gp_state| gp_state.axes.get(&axis).copied())
            .unwrap_or(0.0)
    }

    /// Check if an input action is currently active
    pub fn is_action_pressed(&self, action_name: &str) -> bool {
        if let Some(binding) = self.bindings.get(action_name) {
            if binding.requires_all {
                // All inputs must be pressed
                let mut all_pressed = true;
                
                for key in &binding.key_codes {
                    if !self.is_key_pressed(*key) {
                        all_pressed = false;
                        break;
                    }
                }
                
                if all_pressed {
                    for button in &binding.mouse_buttons {
                        if !self.is_mouse_button_pressed(*button) {
                            all_pressed = false;
                            break;
                        }
                    }
                }
                
                if all_pressed {
                    for button in &binding.gamepad_buttons {
                        let mut gamepad_pressed = false;
                        let state = self.state.read().unwrap();
                        for gp_state in state.gamepad_states.values() {
                            if gp_state.buttons_pressed.contains(button) {
                                gamepad_pressed = true;
                                break;
                            }
                        }
                        if !gamepad_pressed {
                            all_pressed = false;
                            break;
                        }
                    }
                }
                
                all_pressed
            } else {
                // Any input can activate
                let mut any_pressed = false;
                
                for key in &binding.key_codes {
                    if self.is_key_pressed(*key) {
                        any_pressed = true;
                        break;
                    }
                }
                
                if !any_pressed {
                    for button in &binding.mouse_buttons {
                        if self.is_mouse_button_pressed(*button) {
                            any_pressed = true;
                            break;
                        }
                    }
                }
                
                if !any_pressed {
                    let state = self.state.read().unwrap();
                    for button in &binding.gamepad_buttons {
                        for gp_state in state.gamepad_states.values() {
                            if gp_state.buttons_pressed.contains(button) {
                                any_pressed = true;
                                break;
                            }
                        }
                        if any_pressed {
                            break;
                        }
                    }
                }
                
                any_pressed
            }
        } else {
            false
        }
    }

    /// Check if an input action was just activated this frame
    pub fn is_action_just_pressed(&self, action_name: &str) -> bool {
        if let Some(binding) = self.bindings.get(action_name) {
            if binding.requires_all {
                // Check if all inputs were just pressed
                let mut all_just_pressed = true;
                
                for key in &binding.key_codes {
                    if !self.is_key_just_pressed(*key) {
                        all_just_pressed = false;
                        break;
                    }
                }
                
                if all_just_pressed {
                    for button in &binding.mouse_buttons {
                        if !self.is_mouse_button_just_pressed(*button) {
                            all_just_pressed = false;
                            break;
                        }
                    }
                }
                
                all_just_pressed
            } else {
                // Check if any input was just pressed
                for key in &binding.key_codes {
                    if self.is_key_just_pressed(*key) {
                        return true;
                    }
                }
                
                for button in &binding.mouse_buttons {
                    if self.is_mouse_button_just_pressed(*button) {
                        return true;
                    }
                }
                
                // Check gamepad buttons
                let state = self.state.read().unwrap();
                for button in &binding.gamepad_buttons {
                    for (gp_id, gp_state) in &state.gamepad_states {
                        if gp_state.buttons_released.contains(button) {
                            // Gamepad buttons don't have "just pressed" in the same way
                            // This would need more sophisticated tracking
                            return true;
                        }
                    }
                }
                
                false
            }
        } else {
            false
        }
    }

    /// Get axis value for analog input
    pub fn get_axis_value(&self, axis_name: &str) -> f32 {
        self.axis_values.get(axis_name).copied().unwrap_or(0.0)
    }

    /// Add input binding
    pub fn add_binding(&mut self, binding: InputBinding) {
        self.bindings.insert(binding.action_name.clone(), binding);
    }

    /// Remove input binding
    pub fn remove_binding(&mut self, action_name: &str) {
        self.bindings.remove(action_name);
    }

    /// Add axis binding
    pub fn add_axis_binding(&mut self, binding: AxisBinding) {
        self.axis_bindings.insert(binding.axis_name.clone(), binding);
    }

    /// Remove axis binding
    pub fn remove_axis_binding(&mut self, axis_name: &str) {
        self.axis_bindings.remove(axis_name);
    }

    /// Get all available gamepads
    pub fn get_connected_gamepads(&self) -> Vec<u32> {
        let state = self.state.read().unwrap();
        state.gamepad_states.keys().cloned().collect()
    }

    /// Check if a gamepad is connected
    pub fn is_gamepad_connected(&self, gamepad_id: u32) -> bool {
        let state = self.state.read().unwrap();
        state.gamepad_states.get(&gamepad_id)
            .map(|gp| gp.connected)
            .unwrap_or(false)
    }

    /// Get input statistics
    pub fn get_stats(&self) -> InputStats {
        self.stats.read().unwrap().clone()
    }

    /// Vibrate a gamepad (if supported)
    pub fn vibrate_gamepad(&self, gamepad_id: u32, intensity: f32, duration: std::time::Duration) -> Result<(), InputError> {
        // In a real implementation, this would send vibration commands to the gamepad
        println!("Vibrating gamepad {}: intensity={}, duration={:?}", gamepad_id, intensity, duration);
        Ok(())
    }

    /// Internal methods

    fn update_axis_values(&mut self) {
        for (axis_name, binding) in &self.axis_bindings {
            let mut value = 0.0;
            
            // Check keyboard input
            if let Some(positive_key) = binding.positive_key {
                if self.is_key_pressed(positive_key) {
                    value += binding.sensitivity;
                }
            }
            
            if let Some(negative_key) = binding.negative_key {
                if self.is_key_pressed(negative_key) {
                    value -= binding.sensitivity;
                }
            }
            
            // Check gamepad input
            if let Some(gamepad_axis) = binding.gamepad_axis {
                let mut gp_value = 0.0;
                let state = self.state.read().unwrap();
                for gp_state in state.gamepad_states.values() {
                    if let Some(&axis_value) = gp_state.axes.get(&gamepad_axis) {
                        gp_value = axis_value;
                        break;
                    }
                }
                
                // Apply dead zone
                if gp_value.abs() > binding.dead_zone {
                    let normalized_value = (gp_value - gp_value.signum() * binding.dead_zone) / (1.0 - binding.dead_zone);
                    value += normalized_value * binding.sensitivity;
                }
            }
            
            // Clamp value to [-1, 1]
            value = value.clamp(-1.0, 1.0);
            self.axis_values.insert(axis_name.clone(), value);
        }
    }

    fn process_events(&mut self) {
        let mut event_queue = self.event_queue.write().unwrap();
        
        for event in event_queue.drain(..) {
            match event {
                InputEvent::KeyPressed { key, .. } => {
                    self.pressed_keys_frame.insert(key);
                    let mut state = self.state.write().unwrap();
                    state.keys_pressed.insert(key);
                    state.keys_released.remove(&key);
                    
                    let mut stats = self.stats.write().unwrap();
                    stats.total_key_presses += 1;
                },
                
                InputEvent::KeyReleased { key, .. } => {
                    let mut state = self.state.write().unwrap();
                    state.keys_pressed.remove(&key);
                    state.keys_released.insert(key);
                },
                
                InputEvent::MouseButtonPressed { button, position, .. } => {
                    self.pressed_mouse_buttons_frame.insert(button);
                    let mut state = self.state.write().unwrap();
                    state.mouse_buttons_pressed.insert(button);
                    state.mouse_buttons_released.remove(&button);
                    state.mouse_position = position;
                    
                    let mut stats = self.stats.write().unwrap();
                    stats.total_mouse_clicks += 1;
                },
                
                InputEvent::MouseButtonReleased { button, position, .. } => {
                    let mut state = self.state.write().unwrap();
                    state.mouse_buttons_pressed.remove(&button);
                    state.mouse_buttons_released.insert(button);
                    state.mouse_position = position;
                },
                
                InputEvent::MouseMoved { position, delta, .. } => {
                    let mut state = self.state.write().unwrap();
                    state.mouse_position = position;
                    state.mouse_delta = delta;
                },
                
                InputEvent::MouseWheelScrolled { delta, position, .. } => {
                    let mut state = self.state.write().unwrap();
                    state.mouse_wheel_delta = delta;
                    state.mouse_position = position;
                },
                
                InputEvent::GamepadButtonPressed { button, gamepad_id, .. } => {
                    if let Some(gp_buttons) = self.pressed_gamepad_buttons_frame.get_mut(&gamepad_id) {
                        gp_buttons.insert(button);
                    } else {
                        let mut buttons = HashSet::new();
                        buttons.insert(button);
                        self.pressed_gamepad_buttons_frame.insert(gamepad_id, buttons);
                    }
                    
                    let mut state = self.state.write().unwrap();
                    if let Some(gp_state) = state.gamepad_states.get_mut(&gamepad_id) {
                        gp_state.buttons_pressed.insert(button);
                        gp_state.buttons_released.remove(&button);
                    }
                    
                    let mut stats = self.stats.write().unwrap();
                    stats.total_gamepad_inputs += 1;
                },
                
                InputEvent::GamepadButtonReleased { button, gamepad_id, .. } => {
                    let mut state = self.state.write().unwrap();
                    if let Some(gp_state) = state.gamepad_states.get_mut(&gamepad_id) {
                        gp_state.buttons_pressed.remove(&button);
                        gp_state.buttons_released.insert(button);
                    }
                },
                
                InputEvent::GamepadAxisChanged { axis, value, gamepad_id, .. } => {
                    let mut state = self.state.write().unwrap();
                    if let Some(gp_state) = state.gamepad_states.get_mut(&gamepad_id) {
                        gp_state.axes.insert(axis, value);
                    }
                },
                
                InputEvent::TouchBegan { touch_id, position, .. } => {
                    let mut state = self.state.write().unwrap();
                    state.touch_states.insert(touch_id, TouchState {
                        position,
                        delta: (0.0, 0.0),
                        pressure: 1.0,
                        active: true,
                    });
                    
                    let mut stats = self.stats.write().unwrap();
                    stats.total_touch_events += 1;
                },
                
                InputEvent::TouchMoved { touch_id, position, delta, .. } => {
                    let mut state = self.state.write().unwrap();
                    if let Some(touch_state) = state.touch_states.get_mut(&touch_id) {
                        touch_state.position = position;
                        touch_state.delta = delta;
                        touch_state.active = true;
                    }
                },
                
                InputEvent::TouchEnded { touch_id, position, .. } => {
                    let mut state = self.state.write().unwrap();
                    if let Some(touch_state) = state.touch_states.get_mut(&touch_id) {
                        touch_state.position = position;
                        touch_state.active = false;
                    }
                },
            }
        }
    }

    fn update_stats(&mut self, delta_time: std::time::Duration) {
        let mut stats = self.stats.write().unwrap();
        
        // Update average frame time (exponential moving average)
        let current_avg = stats.average_frame_time;
        let new_avg = std::time::Duration::from_nanos(
            (current_avg.as_nanos() as f64 * 0.9 + delta_time.as_nanos() as f64 * 0.1) as u64
        );
        stats.average_frame_time = new_avg;
        
        // Calculate input lag (simplified)
        stats.input_lag = std::time::Duration::from_nanos(
            (stats.input_lag.as_nanos() as f64 * 0.95 + delta_time.as_nanos() as f64 * 0.05) as u64
        );
    }

    /// Add input event to queue (for external systems)
    pub fn queue_event(&self, event: InputEvent) {
        let mut event_queue = self.event_queue.write().unwrap();
        event_queue.push(event);
    }

    /// Clear all input state
    pub fn clear_all_input(&mut self) {
        let mut state = self.state.write().unwrap();
        state.keys_pressed.clear();
        state.keys_released.clear();
        state.mouse_buttons_pressed.clear();
        state.mouse_buttons_released.clear();
        state.mouse_delta = (0, 0);
        state.mouse_wheel_delta = 0;
        
        for gp_state in state.gamepad_states.values_mut() {
            gp_state.buttons_pressed.clear();
            gp_state.buttons_released.clear();
            for axis_value in gp_state.axes.values_mut() {
                *axis_value = 0.0;
            }
        }
        
        self.pressed_keys_frame.clear();
        self.pressed_mouse_buttons_frame.clear();
        self.pressed_gamepad_buttons_frame.clear();
        self.axis_values.clear();
        
        let mut event_queue = self.event_queue.write().unwrap();
        event_queue.clear();
    }

    /// Set mouse position (useful for cursor locking)
    pub fn set_mouse_position(&self, position: (i32, i32)) {
        let mut state = self.state.write().unwrap();
        state.mouse_position = position;
    }

    /// Show/hide cursor
    pub fn set_cursor_visible(&self, visible: bool) {
        // In a real implementation, this would control the system cursor
        println!("Cursor visible: {}", visible);
    }
}

/// Input error types
#[derive(Debug, thiserror::Error)]
pub enum InputError {
    #[error("Gamepad not connected")]
    GamepadNotConnected,
    
    #[error("Invalid gamepad ID")]
    InvalidGamepadId,
    
    #[error("Input binding not found")]
    BindingNotFound,
    
    #[error("Too many touches")]
    TooManyTouches,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_input_manager_creation() {
        let config = InputConfig {
            enable_keyboard: true,
            enable_mouse: true,
            enable_gamepad: true,
            enable_touch: false,
            mouse_sensitivity: 1.0,
            gamepad_dead_zone: 0.1,
            double_click_time: std::time::Duration::from_millis(300),
            hold_threshold: std::time::Duration::from_millis(500),
            max_touches: 5,
        };

        let manager = InputManager::new(config);
        assert_eq!(manager.get_connected_gamepads().len(), 0);
        assert_eq!(manager.get_stats().total_key_presses, 0);
    }

    #[test]
    fn test_input_binding() {
        let mut manager = InputManager::new(InputConfig {
            enable_keyboard: true,
            enable_mouse: true,
            enable_gamepad: false,
            enable_touch: false,
            mouse_sensitivity: 1.0,
            gamepad_dead_zone: 0.1,
            double_click_time: std::time::Duration::from_millis(300),
            hold_threshold: std::time::Duration::from_millis(500),
            max_touches: 5,
        });

        let binding = InputBinding {
            action_name: "jump".to_string(),
            key_codes: vec![KeyCode::Space],
            mouse_buttons: vec![],
            gamepad_buttons: vec![],
            requires_all: false,
        };

        manager.add_binding(binding);
        assert!(manager.bindings.contains_key("jump"));
        assert_eq!(manager.bindings["jump"].key_codes.len(), 1);
    }

    #[test]
    fn test_axis_binding() {
        let mut manager = InputManager::new(InputConfig {
            enable_keyboard: true,
            enable_mouse: false,
            enable_gamepad: true,
            enable_touch: false,
            mouse_sensitivity: 1.0,
            gamepad_dead_zone: 0.1,
            double_click_time: std::time::Duration::from_millis(300),
            hold_threshold: std::time::Duration::from_millis(500),
            max_touches: 5,
        });

        let axis_binding = AxisBinding {
            axis_name: "horizontal".to_string(),
            positive_key: Some(KeyCode::ArrowRight),
            negative_key: Some(KeyCode::ArrowLeft),
            gamepad_axis: Some(GamepadAxis::LeftX),
            sensitivity: 1.0,
            dead_zone: 0.1,
        };

        manager.add_axis_binding(axis_binding);
        assert!(manager.axis_bindings.contains_key("horizontal"));
        
        // Initially axis value should be 0
        assert_eq!(manager.get_axis_value("horizontal"), 0.0);
    }

    #[test]
    fn test_event_processing() {
        let config = InputConfig {
            enable_keyboard: true,
            enable_mouse: true,
            enable_gamepad: false,
            enable_touch: false,
            mouse_sensitivity: 1.0,
            gamepad_dead_zone: 0.1,
            double_click_time: std::time::Duration::from_millis(300),
            hold_threshold: std::time::Duration::from_millis(500),
            max_touches: 5,
        };

        let manager = InputManager::new(config);
        
        // Queue a key press event
        let event = InputEvent::KeyPressed {
            key: KeyCode::A,
            timestamp: std::time::Instant::now(),
        };
        
        manager.queue_event(event);
        manager.update(std::time::Duration::from_millis(16));
        
        // Should now detect the key press
        assert!(manager.is_key_pressed(KeyCode::A));
    }
}

