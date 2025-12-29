// Network Utilities - Network communication, multiplayer, and real-time sync
use std::collections::HashMap;
use std::net::{TcpListener, TcpStream, UdpSocket};
use std::sync::{Arc, Mutex, RwLock};
use std::thread;
use std::time::{Duration, Instant};
use tokio::net::{TcpListener as AsyncTcpListener, TcpStream as AsyncTcpStream};
use tokio::sync::{mpsc, oneshot};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// Network protocol types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum NetworkProtocol {
    TCP,
    UDP,
    WebSocket,
    HTTP,
    HTTPS,
}

/// Message types for network communication
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum NetworkMessage {
    PlayerJoin { player_id: String, username: String },
    PlayerLeave { player_id: String },
    GameState { game_id: String, state: GameStateData },
    PlayerMove { player_id: String, position: Vector3, rotation: Vector3 },
    ChatMessage { player_id: String, message: String, timestamp: Instant },
    GameAction { player_id: String, action: GameActionType, data: Vec<u8> },
    Ping { timestamp: Instant },
    Pong { timestamp: Instant, latency: Duration },
    Error { error_code: u16, message: String },
}

/// Game state data for network synchronization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GameStateData {
    pub game_id: String,
    pub timestamp: Instant,
    pub players: HashMap<String, PlayerNetworkData>,
    pub game_objects: Vec<GameObjectNetworkData>,
    pub score: HashMap<String, u32>,
    pub game_status: GameStatus,
}

/// Player network data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlayerNetworkData {
    pub player_id: String,
    pub username: String,
    pub position: Vector3,
    pub rotation: Vector3,
    pub velocity: Vector3,
    pub health: f32,
    pub score: u32,
    pub is_ready: bool,
}

/// Game object network data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GameObjectNetworkData {
    pub object_id: String,
    pub object_type: String,
    pub position: Vector3,
    pub rotation: Vector3,
    pub scale: Vector3,
    pub state: String,
    pub properties: HashMap<String, serde_json::Value>,
}

/// Game action types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GameActionType {
    Jump,
    Attack,
    UseItem,
    Interact,
    Custom(String),
}

/// Game status enumeration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GameStatus {
    Waiting,
    Starting,
    Playing,
    Paused,
    Finished,
}

/// 3D Vector for network synchronization
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct Vector3 {
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

/// Network client connection
#[derive(Debug, Clone)]
pub struct NetworkClient {
    pub id: String,
    pub username: String,
    pub stream: Arc<Mutex<TcpStream>>,
    pub address: String,
    pub connected_at: Instant,
    pub last_ping: Instant,
    pub latency: Duration,
    pub is_ai: bool,
    pub player_data: Option<PlayerNetworkData>,
}

/// Network server configuration
#[derive(Debug, Clone)]
pub struct NetworkServerConfig {
    pub host: String,
    pub port: u16,
    pub protocol: NetworkProtocol,
    pub max_connections: u32,
    pub connection_timeout: Duration,
    pub ping_interval: Duration,
    pub enable_encryption: bool,
    pub compression_enabled: bool,
    pub buffer_size: usize,
}

/// Network statistics
#[derive(Debug, Clone)]
pub struct NetworkStats {
    pub total_connections: u32,
    pub active_connections: u32,
    pub bytes_sent: u64,
    pub bytes_received: u64,
    pub messages_sent: u64,
    pub messages_received: u64,
    pub average_latency: Duration,
    pub uptime: Duration,
}

/// Network manager handles all networking operations
pub struct NetworkManager {
    server: Option<NetworkServer>,
    clients: Arc<RwLock<HashMap<String, NetworkClient>>>,
    message_queue: Arc<Mutex<Vec<NetworkMessage>>>,
    stats: Arc<Mutex<NetworkStats>>,
    config: NetworkManagerConfig,
}

/// Network manager configuration
#[derive(Debug, Clone)]
pub struct NetworkManagerConfig {
    pub enable_server: bool,
    pub enable_client: bool,
    pub server_config: NetworkServerConfig,
    pub client_config: NetworkClientConfig,
    pub message_buffer_size: usize,
    pub broadcast_interval: Duration,
    pub enable_compression: bool,
    pub enable_encryption: bool,
}

/// Client configuration
#[derive(Debug, Clone)]
pub struct NetworkClientConfig {
    pub server_host: String,
    pub server_port: u16,
    pub protocol: NetworkProtocol,
    pub reconnect_attempts: u32,
    pub reconnect_delay: Duration,
    pub connection_timeout: Duration,
    pub buffer_size: usize,
}

/// Internal network server
struct NetworkServer {
    listener: Option<AsyncTcpListener>,
    config: NetworkServerConfig,
    is_running: bool,
    handle: Option<tokio::task::JoinHandle<()>>,
}

impl NetworkManager {
    /// Create a new Network Manager
    pub fn new(config: NetworkManagerConfig) -> Self {
        Self {
            server: None,
            clients: Arc::new(RwLock::new(HashMap::new())),
            message_queue: Arc::new(Mutex::new(Vec::new())),
            stats: Arc::new(Mutex::new(NetworkStats {
                total_connections: 0,
                active_connections: 0,
                bytes_sent: 0,
                bytes_received: 0,
                messages_sent: 0,
                messages_received: 0,
                average_latency: Duration::from_millis(0),
                uptime: Duration::from_secs(0),
            })),
            config,
        }
    }

    /// Start network server
    pub async fn start_server(&mut self) -> Result<(), NetworkManagerError> {
        if !self.config.enable_server {
            return Err(NetworkManagerError::ServerDisabled);
        }

        let listener = AsyncTcpListener::bind(format!(
            "{}:{}",
            self.config.server_config.host,
            self.config.server_config.port
        )).await.map_err(|e| NetworkManagerError::BindFailed(e.to_string()))?;

        let server = NetworkServer {
            listener: Some(listener),
            config: self.config.server_config.clone(),
            is_running: true,
            handle: None,
        };

        self.server = Some(server);
        self.start_server_accept_loop().await?;
        
        println!("Network server started on {}:{}", 
            self.config.server_config.host, 
            self.config.server_config.port);
        Ok(())
    }

    /// Stop network server
    pub async fn stop_server(&mut self) -> Result<(), NetworkManagerError> {
        if let Some(mut server) = self.server.take() {
            server.is_running = false;
            
            if let Some(handle) = server.handle.take() {
                handle.abort();
            }
            
            println!("Network server stopped");
            Ok(())
        } else {
            Err(NetworkManagerError::ServerNotRunning)
        }
    }

    /// Connect to network server (as client)
    pub async fn connect_to_server(&self, username: String) -> Result<String, NetworkManagerError> {
        if !self.config.enable_client {
            return Err(NetworkManagerError::ClientDisabled);
        }

        let address = format!(
            "{}:{}",
            self.config.client_config.server_host,
            self.config.client_config.server_port
        );

        let stream = AsyncTcpStream::connect(&address).await
            .map_err(|e| NetworkManagerError::ConnectionFailed(e.to_string()))?;

        let client_id = Uuid::new_v4().to_string();
        
        // Create client
        let client = NetworkClient {
            id: client_id.clone(),
            username,
            stream: Arc::new(Mutex::new(TcpStream::connect(&address).map_err(|e| 
                NetworkManagerError::ConnectionFailed(e.to_string()))?)),
            address,
            connected_at: Instant::now(),
            last_ping: Instant::now(),
            latency: Duration::from_millis(0),
            is_ai: false,
            player_data: None,
        };

        // Send join message
        let join_message = NetworkMessage::PlayerJoin {
            player_id: client_id.clone(),
            username: client.username.clone(),
        };
        
        self.send_message_to_client(&client_id, join_message)?;

        // Store client
        let mut clients = self.clients.write().map_err(|_| NetworkManagerError::PoisonedLock)?;
        clients.insert(client_id.clone(), client);

        // Start client message handling
        self.start_client_message_loop(client_id.clone());

        println!("Connected to server as {} (ID: {})", username, client_id);
        Ok(client_id)
    }

    /// Disconnect from server
    pub async fn disconnect_from_server(&self, client_id: &str) -> Result<(), NetworkManagerError> {
        let mut clients = self.clients.write().map_err(|_| NetworkManagerError::PoisonedLock)?;
        
        if let Some(client) = clients.remove(client_id) {
            // Send leave message
            let leave_message = NetworkMessage::PlayerLeave {
                player_id: client_id.to_string(),
            };
            self.broadcast_message(leave_message)?;
            
            println!("Disconnected from server: {}", client.username);
            Ok(())
        } else {
            Err(NetworkManagerError::ClientNotFound)
        }
    }

    /// Send message to specific client
    pub fn send_message_to_client(&self, client_id: &str, message: NetworkMessage) -> Result<(), NetworkManagerError> {
        let clients = self.clients.read().map_err(|_| NetworkManagerError::PoisonedLock)?;
        
        if let Some(client) = clients.get(client_id) {
            let serialized = bincode::serialize(&message)
                .map_err(|e| NetworkManagerError::SerializationError(e.to_string()))?;
            
            let mut stream = client.stream.lock().map_err(|_| NetworkManagerError::PoisonedLock)?;
            stream.write_all(&serialized)
                .map_err(|e| NetworkManagerError::SendFailed(e.to_string()))?;
            
            // Update stats
            let mut stats = self.stats.lock().map_err(|_| NetworkManagerError::PoisonedLock)?;
            stats.messages_sent += 1;
            stats.bytes_sent += serialized.len() as u64;
            
            Ok(())
        } else {
            Err(NetworkManagerError::ClientNotFound)
        }
    }

    /// Broadcast message to all connected clients
    pub fn broadcast_message(&self, message: NetworkMessage) -> Result<(), NetworkManagerError> {
        let clients = self.clients.read().map_err(|_| NetworkManagerError::PoisonedLock)?;
        
        for client in clients.values() {
            let serialized = bincode::serialize(&message)
                .map_err(|e| NetworkManagerError::SerializationError(e.to_string()))?;
            
            let mut stream = client.stream.lock().map_err(|_| NetworkManagerError::PoisonedLock)?;
            if let Err(e) = stream.write_all(&serialized) {
                println!("Failed to send message to client {}: {}", client.username, e);
                continue;
            }
        }

        // Update stats
        let mut stats = self.stats.lock().map_err(|_| NetworkManagerError::PoisonedLock)?;
        stats.messages_sent += clients.len() as u64;
        
        Ok(())
    }

    /// Send game state to all clients
    pub fn broadcast_game_state(&self, game_state: GameStateData) -> Result<(), NetworkManagerError> {
        let message = NetworkMessage::GameState {
            game_id: game_state.game_id.clone(),
            state: game_state,
        };
        
        self.broadcast_message(message)
    }

    /// Get connected clients
    pub fn get_connected_clients(&self) -> Vec<NetworkClient> {
        self.clients.read().ok()
            .map(|clients| clients.values().cloned().collect())
            .unwrap_or_default()
    }

    /// Get client count
    pub fn get_client_count(&self) -> usize {
        self.clients.read().ok().map(|clients| clients.len()).unwrap_or(0)
    }

    /// Get network statistics
    pub fn get_stats(&self) -> Result<NetworkStats, NetworkManagerError> {
        self.stats.lock().map_err(|_| NetworkManagerError::PoisonedLock).cloned()
    }

    /// Ping all clients to measure latency
    pub fn ping_clients(&self) -> Result<HashMap<String, Duration>, NetworkManagerError> {
        let mut latencies = HashMap::new();
        let clients = self.clients.read().map_err(|_| NetworkManagerError::PoisonedLock)?;

        for client in clients.values() {
            let ping_time = Instant::now();
            let ping_message = NetworkMessage::Ping { timestamp: ping_time };
            
            let serialized = bincode::serialize(&ping_message)
                .map_err(|e| NetworkManagerError::SerializationError(e.to_string()))?;
            
            let mut stream = client.stream.lock().map_err(|_| NetworkManagerError::PoisonedLock)?;
            if let Err(e) = stream.write_all(&serialized) {
                println!("Failed to ping client {}: {}", client.username, e);
                continue;
            }
            
            latencies.insert(client.id.clone(), Duration::from_millis(100)); // Simplified
        }

        Ok(latencies)
    }

    /// Update player data
    pub fn update_player_data(&self, client_id: String, player_data: PlayerNetworkData) -> Result<(), NetworkManagerError> {
        let mut clients = self.clients.write().map_err(|_| NetworkManagerError::PoisonedLock)?;
        
        if let Some(client) = clients.get_mut(&client_id) {
            client.player_data = Some(player_data);
            Ok(())
        } else {
            Err(NetworkManagerError::ClientNotFound)
        }
    }

    /// Handle incoming messages (server side)
    pub async fn handle_messages(&self) -> Result<(), NetworkManagerError> {
        let clients = self.clients.read().map_err(|_| NetworkManagerError::PoisonedLock)?;
        
        for client in clients.values() {
            let mut stream = client.stream.lock().map_err(|_| NetworkManagerError::PoisonedLock)?;
            
            // Try to read message (non-blocking)
            let mut buffer = vec![0u8; 4096];
            match stream.read(&mut buffer) {
                Ok(0) => {
                    // Client disconnected
                    println!("Client {} disconnected", client.username);
                },
                Ok(n) => {
                    // Process message
                    let message_data = &buffer[..n];
                    match bincode::deserialize::<NetworkMessage>(message_data) {
                        Ok(message) => {
                            self.process_message(client.id.clone(), message)?;
                        },
                        Err(e) => {
                            println!("Failed to deserialize message from {}: {}", client.username, e);
                        }
                    }
                },
                Err(_) => {
                    // No data available or error
                }
            }
        }

        Ok(())
    }

    /// Internal methods

    async fn start_server_accept_loop(&mut self) -> Result<(), NetworkManagerError> {
        if let Some(server) = self.server.as_mut() {
            let clients = self.clients.clone();
            let stats = self.stats.clone();
            
            let handle = tokio::spawn(async move {
                if let Some(listener) = &server.listener {
                    loop {
                        if let Ok((stream, address)) = listener.accept().await {
                            let client_id = Uuid::new_v4().to_string();
                            
                            let client = NetworkClient {
                                id: client_id.clone(),
                                username: format!("Player_{}", client_id.chars().take(8).collect::<String>()),
                                stream: Arc::new(Mutex::new(TcpStream::connect(address.to_string()).unwrap())),
                                address: address.to_string(),
                                connected_at: Instant::now(),
                                last_ping: Instant::now(),
                                latency: Duration::from_millis(0),
                                is_ai: false,
                                player_data: None,
                            };

                            // Store client
                            {
                                let mut clients_guard = clients.write().unwrap();
                                clients_guard.insert(client_id.clone(), client);
                                
                                let mut stats_guard = stats.lock().unwrap();
                                stats_guard.total_connections += 1;
                                stats_guard.active_connections += 1;
                            }
                            
                            println!("New client connected: {} from {}", client_id, address);
                        }
                    }
                }
            });

            server.handle = Some(handle);
        }
        
        Ok(())
    }

    fn start_client_message_loop(&self, client_id: String) {
        let clients = self.clients.clone();
        let stats = self.stats.clone();
        
        tokio::spawn(async move {
            // Client message handling logic would go here
            loop {
                tokio::time::sleep(Duration::from_millis(100)).await;
                
                // Process incoming messages
                // Update latency measurements
                // Handle reconnections if needed
            }
        });
    }

    fn process_message(&self, client_id: String, message: NetworkMessage) -> Result<(), NetworkManagerError> {
        match message {
            NetworkMessage::PlayerJoin { player_id, username } => {
                println!("Player {} joined the game", username);
                self.broadcast_message(NetworkMessage::PlayerJoin { player_id, username })?;
            },
            NetworkMessage::PlayerLeave { player_id } => {
                println!("Player {} left the game", player_id);
                self.broadcast_message(NetworkMessage::PlayerLeave { player_id })?;
            },
            NetworkMessage::PlayerMove { player_id, position, rotation } => {
                // Update player position and broadcast
                if let Some(client) = self.clients.read().ok()?.get(&client_id) {
                    if let Some(ref mut player_data) = client.player_data {
                        player_data.position = position;
                        player_data.rotation = rotation;
                    }
                }
                self.broadcast_message(NetworkMessage::PlayerMove { player_id, position, rotation })?;
            },
            NetworkMessage::ChatMessage { player_id, message, timestamp } => {
                println!("Chat from {}: {}", player_id, message);
                self.broadcast_message(NetworkMessage::ChatMessage { player_id, message, timestamp })?;
            },
            NetworkMessage::Ping { timestamp } => {
                let latency = Instant::now().duration_since(timestamp);
                let pong = NetworkMessage::Pong { timestamp, latency };
                self.send_message_to_client(&client_id, pong)?;
            },
            NetworkMessage::GameAction { player_id, action, data } => {
                self.broadcast_message(NetworkMessage::GameAction { player_id, action, data })?;
            },
            _ => {},
        }

        // Update stats
        let mut stats = self.stats.lock().map_err(|_| NetworkManagerError::PoisonedLock)?;
        stats.messages_received += 1;

        Ok(())
    }
}

/// Network Manager error types
#[derive(Debug, thiserror::Error)]
pub enum NetworkManagerError {
    #[error("Server is disabled")]
    ServerDisabled,
    
    #[error("Client is disabled")]
    ClientDisabled,
    
    #[error("Server is not running")]
    ServerNotRunning,
    
    #[error("Failed to bind to address: {0}")]
    BindFailed(String),
    
    #[error("Connection failed: {0}")]
    ConnectionFailed(String),
    
    #[error("Client not found")]
    ClientNotFound,
    
    #[error("Failed to send message: {0}")]
    SendFailed(String),
    
    #[error("Serialization error: {0}")]
    SerializationError(String),
    
    #[error("Poisoned lock")]
    PoisonedLock,
}

impl Vector3 {
    pub fn new(x: f32, y: f32, z: f32) -> Self {
        Self { x, y, z }
    }
    
    pub fn distance(&self, other: &Vector3) -> f32 {
        let dx = self.x - other.x;
        let dy = self.y - other.y;
        let dz = self.z - other.z;
        (dx * dx + dy * dy + dz * dz).sqrt()
    }
    
    pub fn lerp(&self, other: &Vector3, t: f32) -> Vector3 {
        Vector3::new(
            self.x + (other.x - self.x) * t,
            self.y + (other.y - self.y) * t,
            self.z + (other.z - self.z) * t,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vector3_operations() {
        let v1 = Vector3::new(1.0, 2.0, 3.0);
        let v2 = Vector3::new(4.0, 5.0, 6.0);
        
        assert_eq!(v1.distance(&v2), (9.0 + 9.0 + 9.0).sqrt());
        
        let interpolated = v1.lerp(&v2, 0.5);
        assert!((interpolated.x - 2.5).abs() < 0.001);
        assert!((interpolated.y - 3.5).abs() < 0.001);
        assert!((interpolated.z - 4.5).abs() < 0.001);
    }

    #[test]
    fn test_network_message_serialization() {
        let message = NetworkMessage::ChatMessage {
            player_id: "player123".to_string(),
            message: "Hello, World!".to_string(),
            timestamp: Instant::now(),
        };

        let serialized = bincode::serialize(&message).unwrap();
        let deserialized: NetworkMessage = bincode::deserialize(&serialized).unwrap();
        
        match deserialized {
            NetworkMessage::ChatMessage { player_id, message: msg, .. } => {
                assert_eq!(player_id, "player123");
                assert_eq!(msg, "Hello, World!");
            },
            _ => panic!("Wrong message type"),
        }
    }
}
