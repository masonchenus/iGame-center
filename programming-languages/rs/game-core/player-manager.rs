// Player Manager - Player registration, authentication, and profile management
use std::collections::HashMap;
use std::sync::{Arc, Mutex, RwLock};
use std::time::{Duration, Instant};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// Player represents a game player
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Player {
    pub id: String,
    pub username: String,
    pub email: String,
    pub display_name: String,
    pub avatar_url: Option<String>,
    pub level: u32,
    pub experience: u64,
    pub total_wins: u32,
    pub total_losses: u32,
    pub total_draws: u32,
    pub created_at: Instant,
    pub last_login: Option<Instant>,
    pub is_active: bool,
    pub preferences: PlayerPreferences,
    pub statistics: PlayerStatistics,
    pub achievements: Vec<Achievement>,
}

/// Player preferences and settings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlayerPreferences {
    pub sound_enabled: bool,
    pub music_enabled: bool,
    pub graphics_quality: GraphicsQuality,
    pub language: String,
    pub notifications: NotificationSettings,
}

/// Notification settings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NotificationSettings {
    pub game_invites: bool,
    pub achievements: bool,
    pub friend_requests: bool,
    pub game_results: bool,
}

/// Graphics quality levels
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GraphicsQuality {
    Low,
    Medium,
    High,
    Ultra,
}

/// Player statistics and metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlayerStatistics {
    pub games_played: u32,
    pub total_playtime: Duration,
    pub average_score: f64,
    pub best_score: u32,
    pub current_streak: u32,
    pub best_streak: u32,
    pub perfect_games: u32,
    pub comeback_wins: u32,
    pub speed_runs: u32,
}

/// Achievement representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Achievement {
    pub id: String,
    pub name: String,
    pub description: String,
    pub icon_url: Option<String>,
    pub points: u32,
    pub rarity: AchievementRarity,
    pub category: String,
    pub unlocked_at: Option<Instant>,
    pub progress: f32, // 0.0 to 1.0
}

/// Achievement rarity levels
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AchievementRarity {
    Common,
    Uncommon,
    Rare,
    Epic,
    Legendary,
}

/// Player session for authentication
#[derive(Debug, Clone)]
pub struct PlayerSession {
    pub player_id: String,
    pub token: String,
    pub created_at: Instant,
    pub last_activity: Instant,
    pub ip_address: String,
    pub expires_at: Instant,
}

/// Player Manager handles player lifecycle
pub struct PlayerManager {
    players: Arc<RwLock<HashMap<String, Player>>>,
    sessions: Arc<RwLock<HashMap<String, PlayerSession>>>,
    username_to_id: Arc<RwLock<HashMap<String, String>>>,
    stats: Arc<Mutex<PlayerManagerStats>>,
    settings: PlayerManagerSettings,
}

/// Player Manager statistics
#[derive(Debug, Clone)]
pub struct PlayerManagerStats {
    pub total_players: u32,
    pub active_players: u32,
    pub online_players: u32,
    pub total_sessions: u32,
    pub average_level: f32,
    pub most_popular_game_type: String,
}

/// Player Manager settings
#[derive(Debug, Clone)]
pub struct PlayerManagerSettings {
    pub max_players: u32,
    pub session_timeout: Duration,
    pub max_sessions_per_player: u32,
    pub enable_auto_cleanup: bool,
    pub cleanup_interval: Duration,
    pub min_username_length: usize,
    pub max_username_length: usize,
    pub require_email_verification: bool,
    pub allow_duplicate_usernames: bool,
}

impl Default for PlayerManagerSettings {
    fn default() -> Self {
        Self {
            max_players: 10000,
            session_timeout: Duration::from_secs(86400), // 24 hours
            max_sessions_per_player: 3,
            enable_auto_cleanup: true,
            cleanup_interval: Duration::from_secs(3600), // 1 hour
            min_username_length: 3,
            max_username_length: 20,
            require_email_verification: false,
            allow_duplicate_usernames: false,
        }
    }
}

impl PlayerManager {
    /// Create a new Player Manager
    pub fn new(settings: PlayerManagerSettings) -> Self {
        let manager = Self {
            players: Arc::new(RwLock::new(HashMap::new())),
            sessions: Arc::new(RwLock::new(HashMap::new())),
            username_to_id: Arc::new(RwLock::new(HashMap::new())),
            stats: Arc::new(Mutex::new(PlayerManagerStats {
                total_players: 0,
                active_players: 0,
                online_players: 0,
                total_sessions: 0,
                average_level: 0.0,
                most_popular_game_type: "Puzzle".to_string(),
            })),
            settings,
        };

        // Start cleanup task if enabled
        if settings.enable_auto_cleanup {
            manager.start_cleanup_task();
        }

        manager
    }

    /// Register a new player
    pub fn register_player(&self, username: String, email: String, display_name: Option<String>) -> Result<String, PlayerManagerError> {
        let mut players = self.players.write().map_err(|_| PlayerManagerError::PoisonedLock)?;
        let mut username_to_id = self.username_to_id.write().map_err(|_| PlayerManagerError::PoisonedLock)?;
        let mut stats = self.stats.lock().map_err(|_| PlayerManagerError::PoisonedLock)?;

        // Validate input
        self.validate_registration(&username, &email)?;

        // Check player limit
        if players.len() >= self.settings.max_players as usize {
            return Err(PlayerManagerError::PlayerLimitReached);
        }

        // Check username availability
        if !self.settings.allow_duplicate_usernames && username_to_id.contains_key(&username) {
            return Err(PlayerManagerError::UsernameTaken);
        }

        let player_id = Uuid::new_v4().to_string();
        let display_name = display_name.unwrap_or_else(|| username.clone());
        let now = Instant::now();

        let player = Player {
            id: player_id.clone(),
            username: username.clone(),
            email,
            display_name,
            avatar_url: None,
            level: 1,
            experience: 0,
            total_wins: 0,
            total_losses: 0,
            total_draws: 0,
            created_at: now,
            last_login: None,
            is_active: true,
            preferences: PlayerPreferences {
                sound_enabled: true,
                music_enabled: true,
                graphics_quality: GraphicsQuality::Medium,
                language: "en".to_string(),
                notifications: NotificationSettings {
                    game_invites: true,
                    achievements: true,
                    friend_requests: true,
                    game_results: true,
                },
            },
            statistics: PlayerStatistics {
                games_played: 0,
                total_playtime: Duration::from_secs(0),
                average_score: 0.0,
                best_score: 0,
                current_streak: 0,
                best_streak: 0,
                perfect_games: 0,
                comeback_wins: 0,
                speed_runs: 0,
            },
            achievements: Vec::new(),
        };

        // Store player
        players.insert(player_id.clone(), player);
        username_to_id.insert(username, player_id.clone());

        // Update stats
        stats.total_players += 1;
        stats.active_players += 1;

        println!("Player registered: {} (ID: {})", username, player_id);
        Ok(player_id)
    }

    /// Authenticate player and create session
    pub fn authenticate_player(&self, username: String, password: String) -> Result<String, PlayerManagerError> {
        let players = self.players.read().map_err(|_| PlayerManagerError::PoisonedLock)?;
        let username_to_id = self.username_to_id.read().map_err(|_| PlayerManagerError::PoisonedLock)?;
        let mut sessions = self.sessions.write().map_err(|_| PlayerManagerError::PoisonedLock)?;
        let mut stats = self.stats.lock().map_err(|_| PlayerManagerError::PoisonedLock)?;

        // Find player
        let player_id = username_to_id.get(&username).ok_or(PlayerManagerError::InvalidCredentials)?;
        let player = players.get(player_id).ok_or(PlayerManagerError::PlayerNotFound)?;

        // Check if player is active
        if !player.is_active {
            return Err(PlayerManagerError::AccountDeactivated);
        }

        // Verify password (simplified - in real implementation, use proper hashing)
        // For demo purposes, we'll accept any non-empty password
        if password.is_empty() {
            return Err(PlayerManagerError::InvalidCredentials);
        }

        // Check session limit
        let player_sessions: Vec<_> = sessions.values()
            .filter(|session| session.player_id == *player_id)
            .collect();
        
        if player_sessions.len() >= self.settings.max_sessions_per_player as usize {
            return Err(PlayerManagerError::TooManySessions);
        }

        // Create session
        let session_token = self.generate_session_token();
        let now = Instant::now();
        
        let session = PlayerSession {
            player_id: player_id.clone(),
            token: session_token.clone(),
            created_at: now,
            last_activity: now,
            ip_address: "127.0.0.1".to_string(), // In real implementation, get from request
            expires_at: now + self.settings.session_timeout,
        };

        sessions.insert(session_token.clone(), session);

        // Update player last login
        // Note: This would require mutable access in real implementation
        drop(players); // Release read lock

        // Update stats
        stats.total_sessions += 1;
        stats.online_players += 1;

        println!("Player authenticated: {} (Sessions: {})", username, player_sessions.len() + 1);
        Ok(session_token)
    }

    /// Validate session token
    pub fn validate_session(&self, token: &str) -> Result<String, PlayerManagerError> {
        let sessions = self.sessions.read().map_err(|_| PlayerManagerError::PoisonedLock)?;
        
        let session = sessions.get(token).ok_or(PlayerManagerError::InvalidSession)?;
        
        // Check expiration
        if Instant::now() > session.expires_at {
            return Err(PlayerManagerError::SessionExpired);
        }

        Ok(session.player_id.clone())
    }

    /// Logout and invalidate session
    pub fn logout_player(&self, token: &str) -> Result<(), PlayerManagerError> {
        let mut sessions = self.sessions.write().map_err(|_| PlayerManagerError::PoisonedLock)?;
        let mut stats = self.stats.lock().map_err(|_| PlayerManagerError::PoisonedLock)?;

        if sessions.remove(token).is_some() {
            stats.online_players = stats.online_players.saturating_sub(1);
            println!("Player logged out: {}", token);
            Ok(())
        } else {
            Err(PlayerManagerError::InvalidSession)
        }
    }

    /// Get player by ID
    pub fn get_player(&self, player_id: &str) -> Result<Player, PlayerManagerError> {
        let players = self.players.read().map_err(|_| PlayerManagerError::PoisonedLock)?;
        players.get(player_id).cloned().ok_or(PlayerManagerError::PlayerNotFound)
    }

    /// Get player by username
    pub fn get_player_by_username(&self, username: &str) -> Result<Player, PlayerManagerError> {
        let username_to_id = self.username_to_id.read().map_err(|_| PlayerManagerError::PoisonedLock)?;
        let player_id = username_to_id.get(username).ok_or(PlayerManagerError::PlayerNotFound)?;
        self.get_player(player_id)
    }

    /// Update player experience and handle level ups
    pub fn update_player_experience(&self, player_id: &str, exp_gained: u64) -> Result<(), PlayerManagerError> {
        let mut players = self.players.write().map_err(|_| PlayerManagerError::PoisonedLock)?;
        
        let player = players.get_mut(player_id).ok_or(PlayerManagerError::PlayerNotFound)?;
        
        let old_level = player.level;
        player.experience += exp_gained;

        // Calculate new level (every 1000 exp = 1 level)
        let new_level = (player.experience / 1000) + 1;
        
        if new_level > old_level {
            player.level = new_level;
            println!("Player {} leveled up! {} -> {}", player.username, old_level, new_level);
        }

        Ok(())
    }

    /// Update player statistics after a game
    pub fn update_player_statistics(&self, player_id: &str, game_result: GameResult, score: u32, duration: Duration) -> Result<(), PlayerManagerError> {
        let mut players = self.players.write().map_err(|_| PlayerManagerError::PoisonedLock)?;
        
        let player = players.get_mut(player_id).ok_or(PlayerManagerError::PlayerNotFound)?;
        
        // Update basic stats
        player.statistics.games_played += 1;
        player.statistics.total_playtime += duration;

        match game_result {
            GameResult::Win => {
                player.total_wins += 1;
                player.statistics.current_streak += 1;
                if player.statistics.current_streak > player.statistics.best_streak {
                    player.statistics.best_streak = player.statistics.current_streak;
                }
            },
            GameResult::Loss => {
                player.total_losses += 1;
                player.statistics.current_streak = 0;
            },
            GameResult::Draw => {
                player.total_draws += 1;
            },
        }

        // Update score statistics
        if score > player.statistics.best_score {
            player.statistics.best_score = score;
        }

        // Update average score
        let total_score = player.statistics.average_score * (player.statistics.games_played - 1) as f64;
        player.statistics.average_score = (total_score + score as f64) / player.statistics.games_played as f64;

        // Add experience based on performance
        let exp_gained = match game_result {
            GameResult::Win => 100 + (score / 10),
            GameResult::Draw => 50,
            GameResult::Loss => 25,
        };

        self.update_player_experience(player_id, exp_gained)?;

        Ok(())
    }

    /// Unlock achievement for player
    pub fn unlock_achievement(&self, player_id: &str, achievement: Achievement) -> Result<(), PlayerManagerError> {
        let mut players = self.players.write().map_err(|_| PlayerManagerError::PoisonedLock)?;
        
        let player = players.get_mut(player_id).ok_or(PlayerManagerError::PlayerNotFound)?;

        // Check if already unlocked
        if player.achievements.iter().any(|a| a.id == achievement.id) {
            return Err(PlayerManagerError::AchievementAlreadyUnlocked);
        }

        let mut unlocked_achievement = achievement;
        unlocked_achievement.unlocked_at = Some(Instant::now());
        unlocked_achievement.progress = 1.0;

        player.achievements.push(unlocked_achievement);
        
        println!("Achievement unlocked for player {}: {}", player.username, achievement.name);
        Ok(())
    }

    /// Get player achievements
    pub fn get_player_achievements(&self, player_id: &str) -> Result<Vec<Achievement>, PlayerManagerError> {
        let players = self.players.read().map_err(|_| PlayerManagerError::PoisonedLock)?;
        let player = players.get(player_id).ok_or(PlayerManagerError::PlayerNotFound)?;
        
        Ok(player.achievements.clone())
    }

    /// Get online players
    pub fn get_online_players(&self) -> Vec<Player> {
        let sessions = self.sessions.read().ok().unwrap_or_default();
        let players = self.players.read().ok().unwrap_or_default();
        
        let online_player_ids: Vec<String> = sessions.values()
            .filter(|session| Instant::now() < session.expires_at)
            .map(|session| session.player_id.clone())
            .collect();

        players.values()
            .filter(|player| online_player_ids.contains(&player.id))
            .cloned()
            .collect()
    }

    /// Get leaderboard (top players by experience)
    pub fn get_leaderboard(&self, limit: usize) -> Vec<Player> {
        let players = self.players.read().ok().unwrap_or_default();
        
        let mut players_vec: Vec<Player> = players.values()
            .filter(|player| player.is_active)
            .cloned()
            .collect();

        // Sort by experience (descending)
        players_vec.sort_by(|a, b| b.experience.cmp(&a.experience));

        players_vec.into_iter().take(limit).collect()
    }

    /// Get player rank
    pub fn get_player_rank(&self, player_id: &str) -> Result<u32, PlayerManagerError> {
        let players = self.players.read().map_err(|_| PlayerManagerError::PoisonedLock)?;
        let player = players.get(player_id).ok_or(PlayerManagerError::PlayerNotFound)?;

        let rank = players.values()
            .filter(|p| p.is_active && p.experience > player.experience)
            .count() as u32 + 1;

        Ok(rank)
    }

    /// Update player preferences
    pub fn update_player_preferences(&self, player_id: &str, preferences: PlayerPreferences) -> Result<(), PlayerManagerError> {
        let mut players = self.players.write().map_err(|_| PlayerManagerError::PoisonedLock)?;
        
        let player = players.get_mut(player_id).ok_or(PlayerManagerError::PlayerNotFound)?;
        player.preferences = preferences;

        Ok(())
    }

    /// Deactivate player account
    pub fn deactivate_player(&self, player_id: &str) -> Result<(), PlayerManagerError> {
        let mut players = self.players.write().map_err(|_| PlayerManagerError::PoisonedLock)?;
        
        let player = players.get_mut(player_id).ok_or(PlayerManagerError::PlayerNotFound)?;
        player.is_active = false;

        // Invalidate all sessions for this player
        let mut sessions = self.sessions.write().map_err(|_| PlayerManagerError::PoisonedLock)?;
        let session_tokens: Vec<String> = sessions.values()
            .filter(|session| session.player_id == player_id)
            .map(|session| session.token.clone())
            .collect();

        for token in session_tokens {
            sessions.remove(&token);
        }

        let mut stats = self.stats.lock().map_err(|_| PlayerManagerError::PoisonedLock)?;
        stats.active_players = stats.active_players.saturating_sub(1);

        Ok(())
    }

    /// Get statistics
    pub fn get_stats(&self) -> Result<PlayerManagerStats, PlayerManagerError> {
        self.stats.lock().map_err(|_| PlayerManagerError::PoisonedLock).cloned()
    }

    /// Clean up expired sessions
    pub fn cleanup_expired_sessions(&self) -> Result<u32, PlayerManagerError> {
        let mut sessions = self.sessions.write().map_err(|_| PlayerManagerError::PoisonedLock)?;
        let mut stats = self.stats.lock().map_err(|_| PlayerManagerError::PoisonedLock)?;

        let now = Instant::now();
        let expired_tokens: Vec<String> = sessions.values()
            .filter(|session| now > session.expires_at)
            .map(|session| session.token.clone())
            .collect();

        for token in expired_tokens {
            sessions.remove(&token);
        }

        let cleaned_count = expired_tokens.len() as u32;
        if cleaned_count > 0 {
            stats.online_players = stats.online_players.saturating_sub(cleaned_count);
            println!("Cleaned up {} expired sessions", cleaned_count);
        }

        Ok(cleaned_count)
    }

    /// Internal methods

    fn validate_registration(&self, username: &str, email: &str) -> Result<(), PlayerManagerError> {
        // Validate username
        if username.len() < self.settings.min_username_length {
            return Err(PlayerManagerError::UsernameTooShort);
        }
        if username.len() > self.settings.max_username_length {
            return Err(PlayerManagerError::UsernameTooLong);
        }

        // Validate username characters (alphanumeric and underscore only)
        if !username.chars().all(|c| c.is_alphanumeric() || c == '_' || c == '-') {
            return Err(PlayerManagerError::InvalidUsername);
        }

        // Validate email (basic validation)
        if !email.contains('@') || !email.contains('.') {
            return Err(PlayerManagerError::InvalidEmail);
        }

        Ok(())
    }

    fn generate_session_token(&self) -> String {
        Uuid::new_v4().to_string()
    }

    fn start_cleanup_task(&self) {
        let sessions = self.sessions.clone();
        let interval = self.settings.cleanup_interval;

        std::thread::spawn(move || {
            let mut interval_timer = tokio::time::interval(interval);
            
            loop {
                interval_timer.tick().await;
                
                let now = Instant::now();
                let mut sessions_guard = sessions.write().unwrap();
                let expired_tokens: Vec<String> = sessions_guard.values()
                    .filter(|session| now > session.expires_at)
                    .map(|session| session.token.clone())
                    .collect();

                for token in expired_tokens {
                    sessions_guard.remove(&token);
                }
            }
        });
    }
}

/// Game result enumeration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GameResult {
    Win,
    Loss,
    Draw,
}

/// Player Manager Error types
#[derive(Debug, thiserror::Error)]
pub enum PlayerManagerError {
    #[error("Player not found")]
    PlayerNotFound,
    
    #[error("Invalid credentials")]
    InvalidCredentials,
    
    #[error("Username already taken")]
    UsernameTaken,
    
    #[error("Player limit reached")]
    PlayerLimitReached,
    
    #[error("Account deactivated")]
    AccountDeactivated,
    
    #[error("Too many sessions")]
    TooManySessions,
    
    #[error("Invalid session")]
    InvalidSession,
    
    #[error("Session expired")]
    SessionExpired,
    
    #[error("Achievement already unlocked")]
    AchievementAlreadyUnlocked,
    
    #[error("Username too short")]
    UsernameTooShort,
    
    #[error("Username too long")]
    UsernameTooLong,
    
    #[error("Invalid username")]
    InvalidUsername,
    
    #[error("Invalid email")]
    InvalidEmail,
    
    #[error("Poisoned lock")]
    PoisonedLock,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_register_player() {
        let manager = PlayerManager::new(PlayerManagerSettings::default());
        
        let player_id = manager.register_player(
            "testuser".to_string(),
            "test@example.com".to_string(),
            None
        ).unwrap();

        assert!(!player_id.is_empty());
        assert_eq!(manager.get_stats().unwrap().total_players, 1);
    }

    #[test]
    fn test_authenticate_player() {
        let manager = PlayerManager::new(PlayerManagerSettings::default());
        
        let player_id = manager.register_player(
            "testuser".to_string(),
            "test@example.com".to_string(),
            None
        ).unwrap();

        let session_token = manager.authenticate_player(
            "testuser".to_string(),
            "password123".to_string()
        ).unwrap();

        assert!(!session_token.is_empty());
        assert_eq!(manager.validate_session(&session_token).unwrap(), player_id);
    }

    #[test]
    fn test_player_statistics() {
        let manager = PlayerManager::new(PlayerManagerSettings::default());
        
        let player_id = manager.register_player(
            "testuser".to_string(),
            "test@example.com".to_string(),
            None
        ).unwrap();

        let result = manager.update_player_statistics(
            &player_id,
            GameResult::Win,
            1500,
            Duration::from_secs(300)
        );

        assert!(result.is_ok());

        let player = manager.get_player(&player_id).unwrap();
        assert_eq!(player.total_wins, 1);
        assert_eq!(player.statistics.games_played, 1);
        assert_eq!(player.statistics.best_score, 1500);
    }
}
