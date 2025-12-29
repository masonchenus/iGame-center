// Game Manager - Core game management and coordination
use std::collections::HashMap;
use std::sync::{Arc, Mutex, RwLock};
use std::time::{Duration, Instant};
use serde::{Deserialize, Serialize};

/// Game represents a game instance
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Game {
    pub id: String,
    pub name: String,
    pub game_type: GameType,
    pub max_players: u32,
    pub current_players: u32,
    pub status: GameStatus,
    pub created_at: Instant,
    pub config: GameConfig,
}

/// Game type enumeration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GameType {
    Puzzle,
    Action,
    Strategy,
    Simulation,
    Sports,
    Arcade,
}

/// Game status enumeration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GameStatus {
    Waiting,
    Starting,
    InProgress,
    Paused,
    Finished,
    Cancelled,
}

/// Game configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GameConfig {
    pub difficulty: Difficulty,
    pub time_limit: Option<Duration>,
    pub score_target: Option<u32>,
    pub ai_enabled: bool,
    pub multiplayer: bool,
}

/// Difficulty levels
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Difficulty {
    Easy,
    Normal,
    Hard,
    Expert,
}

/// Game statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GameStats {
    pub games_played: u32,
    pub total_players: u32,
    pub average_duration: Duration,
    pub most_popular_type: GameType,
}

/// Game Manager handles game lifecycle management
pub struct GameManager {
    games: Arc<RwLock<HashMap<String, Game>>>,
    player_games: Arc<RwLock<HashMap<String, String>>>, // player_id -> game_id
    stats: Arc<Mutex<GameStats>>,
    settings: GameManagerSettings,
}

/// Game Manager settings
#[derive(Debug, Clone)]
pub struct GameManagerSettings {
    pub max_concurrent_games: u32,
    pub cleanup_interval: Duration,
    pub game_timeout: Duration,
    pub auto_cleanup: bool,
}

impl Default for GameManagerSettings {
    fn default() -> Self {
        Self {
            max_concurrent_games: 1000,
            cleanup_interval: Duration::from_secs(300), // 5 minutes
            game_timeout: Duration::from_secs(3600), // 1 hour
            auto_cleanup: true,
        }
    }
}

impl GameManager {
    /// Create a new Game Manager
    pub fn new(settings: GameManagerSettings) -> Self {
        let manager = Self {
            games: Arc::new(RwLock::new(HashMap::new())),
            player_games: Arc::new(RwLock::new(HashMap::new())),
            stats: Arc::new(Mutex::new(GameStats {
                games_played: 0,
                total_players: 0,
                average_duration: Duration::from_secs(0),
                most_popular_type: GameType::Puzzle,
            })),
            settings,
        };

        // Start cleanup task if enabled
        if settings.auto_cleanup {
            manager.start_cleanup_task();
        }

        manager
    }

    /// Create a new game
    pub fn create_game(&self, name: String, game_type: GameType, max_players: u32, config: GameConfig) -> Result<String, GameManagerError> {
        let mut games = self.games.write().map_err(|_| GameManagerError::PoisonedLock)?;
        let mut stats = self.stats.lock().map_err(|_| GameManagerError::PoisonedLock)?;

        // Check concurrent game limit
        if games.len() >= self.settings.max_concurrent_games as usize {
            return Err(GameManagerError::TooManyGames);
        }

        let game_id = format!("game_{}", Instant::now().elapsed().as_nanos());
        
        let game = Game {
            id: game_id.clone(),
            name,
            game_type,
            max_players,
            current_players: 0,
            status: GameStatus::Waiting,
            created_at: Instant::now(),
            config,
        };

        games.insert(game_id.clone(), game);
        stats.games_played += 1;

        println!("Created game: {} (Type: {:?})", game_id, game_type);
        Ok(game_id)
    }

    /// Join a game
    pub fn join_game(&self, game_id: String, player_id: String) -> Result<(), GameManagerError> {
        let mut games = self.games.write().map_err(|_| GameManagerError::PoisonedLock)?;
        let mut player_games = self.player_games.write().map_err(|_| GameManagerError::PoisonedLock)?;

        // Check if player is already in a game
        if player_games.contains_key(&player_id) {
            return Err(GameManagerError::PlayerAlreadyInGame);
        }

        let game = games.get_mut(&game_id).ok_or(GameManagerError::GameNotFound)?;

        // Check if game is joinable
        match game.status {
            GameStatus::Waiting | GameStatus::Starting => {},
 => return            _ Err(GameManagerError::GameNotJoinable),
        }

        // Check if game is full
        if game.current_players >= game.max_players {
            return Err(GameManagerError::GameFull);
        }

        // Join the game
        game.current_players += 1;
        player_games.insert(player_id, game_id.clone());

        // Auto-start if enough players
        if game.current_players > 0 && game.status == GameStatus::Waiting {
            game.status = GameStatus::Starting;
            
            // Start game after a short delay
            let game_id_clone = game_id.clone();
            let games_clone = self.games.clone();
            std::thread::spawn(move || {
                std::thread::sleep(Duration::from_secs(3));
                if let Ok(mut games) = games_clone.write() {
                    if let Some(game) = games.get_mut(&game_id_clone) {
                        game.status = GameStatus::InProgress;
                        println!("Game {} started!", game_id_clone);
                    }
                }
            });
        }

        println!("Player {} joined game {}", player_id, game_id);
        Ok(())
    }

    /// Leave a game
    pub fn leave_game(&self, player_id: String) -> Result<(), GameManagerError> {
        let mut games = self.games.write().map_err(|_| GameManagerError::PoisonedLock)?;
        let mut player_games = self.player_games.write().map_err(|_| GameManagerError::PoisonedLock)?;

        let game_id = player_games.remove(&player_id).ok_or(GameManagerError::PlayerNotInGame)?;

        let game = games.get_mut(&game_id).ok_or(GameManagerError::GameNotFound)?;
        if game.current_players > 0 {
            game.current_players -= 1;
        }

        // Check if game should be cancelled or finished
        if game.current_players == 0 {
            match game.status {
                GameStatus::Waiting | GameStatus::Starting => {
                    game.status = GameStatus::Cancelled;
                    println!("Game {} cancelled (no players)", game_id);
                },
                GameStatus::InProgress | GameStatus::Paused => {
                    game.status = GameStatus::Finished;
                    println!("Game {} finished (all players left)", game_id);
                },
                _ => {},
            }
        }

        println!("Player {} left game {}", player_id, game_id);
        Ok(())
    }

    /// Get game information
    pub fn get_game(&self, game_id: &str) -> Option<Game> {
        self.games.read().ok()?.get(game_id).cloned()
    }

    /// Get all games
    pub fn get_all_games(&self) -> Vec<Game> {
        self.games.read().ok().map(|games| games.values().cloned().collect()).unwrap_or_default()
    }

    /// Get games by status
    pub fn get_games_by_status(&self, status: GameStatus) -> Vec<Game> {
        self.games.read().ok()
            .map(|games| games.values().filter(|game| game.status == status).cloned().collect())
            .unwrap_or_default()
    }

    /// Get games by type
    pub fn get_games_by_type(&self, game_type: GameType) -> Vec<Game> {
        self.games.read().ok()
            .map(|games| games.values().filter(|game| game.game_type == game_type).cloned().collect())
            .unwrap_or_default()
    }

    /// Get available games (waiting for players)
    pub fn get_available_games(&self) -> Vec<Game> {
        self.get_games_by_status(GameStatus::Waiting)
    }

    /// Update game status
    pub fn update_game_status(&self, game_id: String, status: GameStatus) -> Result<(), GameManagerError> {
        let mut games = self.games.write().map_err(|_| GameManagerError::PoisonedLock)?;
        let game = games.get_mut(&game_id).ok_or(GameManagerError::GameNotFound)?;
        
        game.status = status;
        println!("Game {} status updated to {:?}", game_id, status);
        Ok(())
    }

    /// Get player statistics
    pub fn get_stats(&self) -> Result<GameStats, GameManagerError> {
        self.stats.lock().map_err(|_| GameManagerError::PoisonedLock).cloned()
    }

    /// Get player count
    pub fn get_player_count(&self) -> u32 {
        self.player_games.read().ok().map(|pg| pg.len() as u32).unwrap_or(0)
    }

    /// Get game count
    pub fn get_game_count(&self) -> usize {
        self.games.read().ok().map(|games| games.len()).unwrap_or(0)
    }

    /// Clean up old games
    pub fn cleanup_games(&self) -> Result<u32, GameManagerError> {
        let mut games = self.games.write().map_err(|_| GameManagerError::PoisonedLock)?;
        let mut cleaned_count = 0;
        let now = Instant::now();

        let games_to_remove: Vec<String> = games.iter()
            .filter(|(_, game)| {
                // Remove games that have timed out
                now.duration_since(game.created_at) > self.settings.game_timeout ||
                // Remove cancelled games after some time
                (matches!(game.status, GameStatus::Cancelled | GameStatus::Finished) &&
                 now.duration_since(game.created_at) > Duration::from_secs(600)) // 10 minutes
            })
            .map(|(id, _)| id.clone())
            .collect();

        for game_id in games_to_remove {
            games.remove(&game_id);
            cleaned_count += 1;
        }

        if cleaned_count > 0 {
            println!("Cleaned up {} old games", cleaned_count);
        }

        Ok(cleaned_count)
    }

    /// Start cleanup task
    fn start_cleanup_task(&self) {
        let games = self.games.clone();
        let interval = self.settings.cleanup_interval;

        std::thread::spawn(move || {
            let mut interval_timer = tokio::time::interval(interval);
            
            loop {
                interval_timer.tick().await;
                
                if let Ok(mut games_guard) = games.write() {
                    let now = Instant::now();
                    let games_to_remove: Vec<String> = games_guard.iter()
                        .filter(|(_, game)| {
                            now.duration_since(game.created_at) > Duration::from_secs(3600)
                        })
                        .map(|(id, _)| id.clone())
                        .collect();

                    for game_id in games_to_remove {
                        games_guard.remove(&game_id);
                    }
                }
            }
        });
    }
}

/// Game Manager Error types
#[derive(Debug, thiserror::Error)]
pub enum GameManagerError {
    #[error("Game not found")]
    GameNotFound,
    
    #[error("Player not in game")]
    PlayerNotInGame,
    
    #[error("Player already in game")]
    PlayerAlreadyInGame,
    
    #[error("Game is full")]
    GameFull,
    
    #[error("Game is not joinable")]
    GameNotJoinable,
    
    #[error("Too many concurrent games")]
    TooManyGames,
    
    #[error("Poisoned lock")]
    PoisonedLock,
}

/// Game event types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GameEvent {
    GameCreated { game_id: String, game_type: GameType },
    GameStarted { game_id: String },
    PlayerJoined { game_id: String, player_id: String },
    PlayerLeft { game_id: String, player_id: String },
    GameEnded { game_id: String, winner: Option<String> },
    GameError { game_id: String, error: String },
}

/// Game event handler trait
pub trait GameEventHandler: Send + Sync {
    fn handle_event(&self, event: GameEvent);
}

/// Simple game event handler implementation
pub struct ConsoleGameEventHandler;

impl GameEventHandler for ConsoleGameEventHandler {
    fn handle_event(&self, event: GameEvent) {
        match event {
            GameEvent::GameCreated { game_id, game_type } => {
                println!("🎮 Game created: {} (Type: {:?})", game_id, game_type);
            },
            GameEvent::GameStarted { game_id } => {
                println!("🚀 Game started: {}", game_id);
            },
            GameEvent::PlayerJoined { game_id, player_id } => {
                println!("👤 Player {} joined game {}", player_id, game_id);
            },
            GameEvent::PlayerLeft { game_id, player_id } => {
                println!("👋 Player {} left game {}", player_id, game_id);
            },
            GameEvent::GameEnded { game_id, winner } => {
                if let Some(winner_id) = winner {
                    println!("🏆 Game {} ended! Winner: {}", game_id, winner_id);
                } else {
                    println!("🎯 Game {} ended!", game_id);
                }
            },
            GameEvent::GameError { game_id, error } => {
                println!("❌ Game {} error: {}", game_id, error);
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_game() {
        let manager = GameManager::new(GameManagerSettings::default());
        
        let config = GameConfig {
            difficulty: Difficulty::Normal,
            time_limit: Some(Duration::from_secs(300)),
            score_target: Some(1000),
            ai_enabled: false,
            multiplayer: true,
        };

        let game_id = manager.create_game(
            "Test Game".to_string(),
            GameType::Puzzle,
            4,
            config
        ).unwrap();

        assert!(!game_id.is_empty());
        assert_eq!(manager.get_game_count(), 1);
    }

    #[test]
    fn test_join_leave_game() {
        let manager = GameManager::new(GameManagerSettings::default());
        
        let config = GameConfig {
            difficulty: Difficulty::Easy,
            time_limit: None,
            score_target: None,
            ai_enabled: true,
            multiplayer: false,
        };

        let game_id = manager.create_game(
            "Test Game".to_string(),
            GameType::Action,
            2,
            config
        ).unwrap();

        // Join game
        let result = manager.join_game(game_id.clone(), "player1".to_string());
        assert!(result.is_ok());

        // Try to join same player again (should fail)
        let result = manager.join_game(game_id.clone(), "player1".to_string());
        assert!(result.is_err());

        // Leave game
        let result = manager.leave_game("player1".to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_game_status_updates() {
        let manager = GameManager::new(GameManagerSettings::default());
        
        let config = GameConfig {
            difficulty: Difficulty::Hard,
            time_limit: Some(Duration::from_secs(600)),
            score_target: Some(5000),
            ai_enabled: true,
            multiplayer: true,
        };

        let game_id = manager.create_game(
            "Status Test".to_string(),
            GameType::Strategy,
            1,
            config
        ).unwrap();

        // Update status
        assert!(manager.update_game_status(game_id.clone(), GameStatus::InProgress).is_ok());
        
        let game = manager.get_game(&game_id).unwrap();
        assert_eq!(game.status, GameStatus::InProgress);
    }
}
