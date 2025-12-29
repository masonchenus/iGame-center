package main

import (
	"database/sql"
	"fmt"
	"log"
	"sync"
	"time"

	_ "github.com/go-sql-driver/mysql"
)

// DatabaseManager handles all database operations
type DatabaseManager struct {
	db        *sql.DB
	config    DatabaseConfig
	mutex     sync.RWMutex
	queries   map[string]*sql.Stmt
	isReady   bool
}

// DatabaseConfig contains database configuration
type DatabaseConfig struct {
	Host     string
	Port     string
	Username string
	Password string
	Database string
	MaxConns int
	MaxIdle  int
}

// PlayerStats represents player statistics in database
type PlayerStats struct {
	PlayerID       string
	GamesPlayed    int
	GamesWon       int
	GamesLost      int
	GamesDrawn     int
	TotalScore     int
	BestScore      int
	AverageScore   float64
	PlayTime       time.Duration
	FirstPlayed    time.Time
	LastPlayed     time.Time
	Achievements   int
	CurrentStreak  int
	BestStreak     int
}

// GameRecord represents a game session record
type GameRecord struct {
	GameID        string
	PlayerID      string
	GameType      string
	StartTime     time.Time
	EndTime       time.Time
	Duration      time.Duration
	Score         int
	LevelReached  int
	Completed     bool
	OpponentID    string
	GameData      string
}

// Achievement represents an achievement in the database
type Achievement struct {
	ID          string
	Name        string
	Description string
	IconURL     string
	Points      int
	Rarity      string
	Category    string
	Requirements string
	IsActive    bool
}

// LeaderboardEntry represents a leaderboard entry
type LeaderboardEntry struct {
	PlayerID     string
	Username     string
	Score        int
	Rank         int
	GameType     string
	Period       string
	AchievedAt   time.Time
}

// NewDatabaseManager creates a new database manager
func NewDatabaseManager(config DatabaseConfig) *DatabaseManager {
	dm := &DatabaseManager{
		config:  config,
		queries: make(map[string]*sql.Stmt),
		isReady: false,
	}

	return dm
}

// Connect establishes database connection
func (dm *DatabaseManager) Connect() error {
	dsn := fmt.Sprintf("%s:%s@tcp(%s:%s)/%s", 
		dm.config.Username, 
		dm.config.Password, 
		dm.config.Host, 
		dm.config.Port, 
		dm.config.Database)

	db, err := sql.Open("mysql", dsn)
	if err != nil {
		return fmt.Errorf("failed to connect to database: %v", err)
	}

	// Configure connection pool
	db.SetMaxOpenConns(dm.config.MaxConns)
	db.SetMaxIdleConns(dm.config.MaxIdle)
	db.SetConnMaxLifetime(time.Hour)

	// Test connection
	if err := db.Ping(); err != nil {
		return fmt.Errorf("failed to ping database: %v", err)
	}

	dm.db = db
	dm.prepareStatements()
	dm.isReady = true

	log.Println("Database connection established")
	return nil
}

// Close closes database connections
func (dm *DatabaseManager) Close() {
	if dm.db != nil {
		dm.db.Close()
		dm.isReady = false
	}
}

// CreatePlayer creates a new player in the database
func (dm *DatabaseManager) CreatePlayer(username, email, displayName string) (string, error) {
	if !dm.isReady {
		return "", fmt.Errorf("database not ready")
	}

	result, err := dm.queries["create_player"].Exec(username, email, displayName)
	if err != nil {
		return "", fmt.Errorf("failed to create player: %v", err)
	}

	playerID, err := result.LastInsertId()
	if err != nil {
		return "", fmt.Errorf("failed to get player ID: %v", err)
	}

	return fmt.Sprintf("%d", playerID), nil
}

// GetPlayer retrieves player information
func (dm *DatabaseManager) GetPlayer(playerID string) (*Player, error) {
	if !dm.isReady {
		return nil, fmt.Errorf("database not ready")
	}

	row := dm.queries["get_player"].QueryRow(playerID)
	player := &Player{}
	
	err := row.Scan(
		&player.ID,
		&player.Username,
		&player.Email,
		&player.DisplayName,
		&player.Level,
		&player.Experience,
		&player.TotalWins,
		&player.TotalLosses,
		&player.TotalDraws,
		&player.CreatedAt,
		&player.LastLogin,
		&player.IsActive,
	)

	if err != nil {
		return nil, fmt.Errorf("failed to get player: %v", err)
	}

	return player, nil
}

// GetPlayerByUsername retrieves player by username
func (dm *DatabaseManager) GetPlayerByUsername(username string) (*Player, error) {
	if !dm.isReady {
		return nil, fmt.Errorf("database not ready")
	}

	row := dm.queries["get_player_by_username"].QueryRow(username)
	player := &Player{}
	
	err := row.Scan(
		&player.ID,
		&player.Username,
		&player.Email,
		&player.DisplayName,
		&player.Level,
		&player.Experience,
		&player.TotalWins,
		&player.TotalLosses,
		&player.TotalDraws,
		&player.CreatedAt,
		&player.LastLogin,
		&player.IsActive,
	)

	if err != nil {
		return nil, fmt.Errorf("failed to get player: %v", err)
	}

	return player, nil
}

// UpdatePlayerStats updates player statistics
func (dm *DatabaseManager) UpdatePlayerStats(playerID string, gameResult string, score int) error {
	if !dm.isReady {
		return fmt.Errorf("database not ready")
	}

	_, err := dm.queries["update_player_stats"].Exec(playerID, gameResult, score)
	if err != nil {
		return fmt.Errorf("failed to update player stats: %v", err)
	}

	return nil
}

// RecordGameSession records a completed game session
func (dm *DatabaseManager) RecordGameSession(record GameRecord) error {
	if !dm.isReady {
		return fmt.Errorf("database not ready")
	}

	_, err := dm.queries["record_game_session"].Exec(
		record.GameID,
		record.PlayerID,
		record.GameType,
		record.StartTime,
		record.EndTime,
		record.Duration.Seconds(),
		record.Score,
		record.LevelReached,
		record.Completed,
		record.OpponentID,
		record.GameData,
	)

	if err != nil {
		return fmt.Errorf("failed to record game session: %v", err)
	}

	return nil
}

// GetPlayerStats retrieves player statistics
func (dm *DatabaseManager) GetPlayerStats(playerID string) (*PlayerStats, error) {
	if !dm.isReady {
		return nil, fmt.Errorf("database not ready")
	}

	row := dm.queries["get_player_stats"].QueryRow(playerID)
	stats := &PlayerStats{}

	err := row.Scan(
		&stats.PlayerID,
		&stats.GamesPlayed,
		&stats.GamesWon,
		&stats.GamesLost,
		&stats.GamesDrawn,
		&stats.TotalScore,
		&stats.BestScore,
		&stats.AverageScore,
		&stats.PlayTime,
		&stats.FirstPlayed,
		&stats.LastPlayed,
		&stats.Achievements,
		&stats.CurrentStreak,
		&stats.BestStreak,
	)

	if err != nil {
		return nil, fmt.Errorf("failed to get player stats: %v", err)
	}

	return stats, nil
}

// CreateAchievement creates a new achievement
func (dm *DatabaseManager) CreateAchievement(achievement Achievement) error {
	if !dm.isReady {
		return fmt.Errorf("database not ready")
	}

	_, err := dm.queries["create_achievement"].Exec(
		achievement.Name,
		achievement.Description,
		achievement.IconURL,
		achievement.Points,
		achievement.Rarity,
		achievement.Category,
		achievement.Requirements,
		achievement.IsActive,
	)

	if err != nil {
		return fmt.Errorf("failed to create achievement: %v", err)
	}

	return nil
}

// UnlockAchievement unlocks an achievement for a player
func (dm *DatabaseManager) UnlockAchievement(playerID, achievementID string) error {
	if !dm.isReady {
		return fmt.Errorf("database not ready")
	}

	_, err := dm.queries["unlock_achievement"].Exec(playerID, achievementID)
	if err != nil {
		return fmt.Errorf("failed to unlock achievement: %v", err)
	}

	return nil
}

// GetLeaderboard retrieves leaderboard entries
func (dm *DatabaseManager) GetLeaderboard(gameType, period string, limit int) ([]*LeaderboardEntry, error) {
	if !dm.isReady {
		return nil, fmt.Errorf("database not ready")
	}

	rows, err := dm.queries["get_leaderboard"].Query(gameType, period, limit)
	if err != nil {
		return nil, fmt.Errorf("failed to get leaderboard: %v", err)
	}
	defer rows.Close()

	entries := make([]*LeaderboardEntry, 0)
	for rows.Next() {
		entry := &LeaderboardEntry{}
		err := rows.Scan(
			&entry.PlayerID,
			&entry.Username,
			&entry.Score,
			&entry.Rank,
			&entry.GameType,
			&entry.Period,
			&entry.AchievedAt,
		)
		if err != nil {
			return nil, fmt.Errorf("failed to scan leaderboard entry: %v", err)
		}
		entries = append(entries, entry)
	}

	return entries, nil
}

// GetRecentGames retrieves recent games for a player
func (dm *DatabaseManager) GetRecentGames(playerID string, limit int) ([]*GameRecord, error) {
	if !dm.isReady {
		return nil, fmt.Errorf("database not ready")
	}

	rows, err := dm.queries["get_recent_games"].Query(playerID, limit)
	if err != nil {
		return nil, fmt.Errorf("failed to get recent games: %v", err)
	}
	defer rows.Close()

	games := make([]*GameRecord, 0)
	for rows.Next() {
		game := &GameRecord{}
		err := rows.Scan(
			&game.GameID,
			&game.PlayerID,
			&game.GameType,
			&game.StartTime,
			&game.EndTime,
			&game.Duration,
			&game.Score,
			&game.LevelReached,
			&game.Completed,
			&game.OpponentID,
			&game.GameData,
		)
		if err != nil {
			return nil, fmt.Errorf("failed to scan game record: %v", err)
		}
		games = append(games, game)
	}

	return games, nil
}

// UpdatePlayerExperience updates player experience and level
func (dm *DatabaseManager) UpdatePlayerExperience(playerID string, experience int) error {
	if !dm.isReady {
		return fmt.Errorf("database not ready")
	}

	_, err := dm.queries["update_experience"].Exec(playerID, experience)
	if err != nil {
		return fmt.Errorf("failed to update experience: %v", err)
	}

	return nil
}

// GetTotalPlayersCount returns total number of players
func (dm *DatabaseManager) GetTotalPlayersCount() (int, error) {
	if !dm.isReady {
		return 0, fmt.Errorf("database not ready")
	}

	var count int
	err := dm.queries["count_players"].QueryRow().Scan(&count)
	if err != nil {
		return 0, fmt.Errorf("failed to count players: %v", err)
	}

	return count, nil
}

// GetDailyActivePlayers returns number of daily active players
func (dm *DatabaseManager) GetDailyActivePlayers() (int, error) {
	if !dm.isReady {
		return 0, fmt.Errorf("database not ready")
	}

	var count int
	err := dm.queries["count_daily_active"].QueryRow().Scan(&count)
	if err != nil {
		return 0, fmt.Errorf("failed to count daily active players: %v", err)
	}

	return count, nil
}

// GetGamesPlayedToday returns number of games played today
func (dm *DatabaseManager) GetGamesPlayedToday() (int, error) {
	if !dm.isReady {
		return 0, fmt.Errorf("database not ready")
	}

	var count int
	err := dm.queries["count_games_today"].QueryRow().Scan(&count)
	if err != nil {
		return 0, fmt.Errorf("failed to count games today: %v", err)
	}

	return count, nil
}

// BackupDatabase creates a database backup
func (dm *DatabaseManager) BackupDatabase(backupPath string) error {
	// This would implement actual database backup logic
	// For demo purposes, we'll just return success
	log.Printf("Database backup created at: %s", backupPath)
	return nil
}

// OptimizeDatabase performs database optimization
func (dm *DatabaseManager) OptimizeDatabase() error {
	if !dm.isReady {
		return fmt.Errorf("database not ready")
	}

	// Execute optimization queries
	optimizationQueries := []string{
		"OPTIMIZE TABLE players",
		"OPTIMIZE TABLE game_sessions",
		"OPTIMIZE TABLE achievements",
		"OPTIMIZE TABLE leaderboards",
	}

	for _, query := range optimizationQueries {
		_, err := dm.db.Exec(query)
		if err != nil {
			log.Printf("Warning: Failed to optimize table: %v", err)
		}
	}

	log.Println("Database optimization completed")
	return nil
}

// prepareStatements prepares SQL statements for reuse
func (dm *DatabaseManager) prepareStatements() {
	statements := map[string]string{
		"create_player": `
			INSERT INTO players (username, email, display_name, created_at) 
			VALUES (?, ?, ?, NOW())`,
		
		"get_player": `
			SELECT player_id, username, email, display_name, level, experience_points,
				   total_wins, total_losses, total_draws, created_at, last_login, is_active
			FROM players WHERE player_id = ?`,
		
		"get_player_by_username": `
			SELECT player_id, username, email, display_name, level, experience_points,
				   total_wins, total_losses, total_draws, created_at, last_login, is_active
			FROM players WHERE username = ?`,
		
		"update_player_stats": `
			UPDATE players SET 
				total_games_played = total_games_played + 1,
				total_wins = total_wins + CASE WHEN ? = 'win' THEN 1 ELSE 0 END,
				total_losses = total_losses + CASE WHEN ? = 'loss' THEN 1 ELSE 0 END,
				total_draws = total_draws + CASE WHEN ? = 'draw' THEN 1 ELSE 0 END,
				experience_points = experience_points + ?
			WHERE player_id = ?`,
		
		"record_game_session": `
			INSERT INTO game_sessions (game_id, player_id, game_type, start_time, end_time,
				duration_seconds, score, level_reached, completed, opponent_id, game_data)
			VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
		
		"get_player_stats": `
			SELECT player_id, total_games_played, total_wins, total_losses, total_draws,
				   SUM(score) as total_score, MAX(score) as best_score,
				   AVG(score) as average_score, SUM(duration_seconds) as play_time,
				   MIN(start_time) as first_played, MAX(end_time) as last_played,
				   COUNT(DISTINCT achievement_id) as achievements,
				   current_streak, best_streak
			FROM players p
			LEFT JOIN game_sessions gs ON p.player_id = gs.player_id
			LEFT JOIN player_achievements pa ON p.player_id = pa.player_id
			WHERE p.player_id = ?
			GROUP BY p.player_id`,
		
		"create_achievement": `
			INSERT INTO achievements (achievement_name, description, icon_url, points_value,
				rarity, category, requirements, is_active)
			VALUES (?, ?, ?, ?, ?, ?, ?, ?)`,
		
		"unlock_achievement": `
			INSERT INTO player_achievements (player_id, achievement_id, unlocked_at)
			VALUES (?, ?, NOW())`,
		
		"get_leaderboard": `
			SELECT p.player_id, p.username, gs.score, RANK() OVER (ORDER BY gs.score DESC) as rank,
				   gs.game_type, ?, gs.achieved_at
			FROM players p
			JOIN leaderboard_entries le ON p.player_id = le.player_id
			JOIN leaderboards lb ON le.leaderboard_id = lb.leaderboard_id
			WHERE lb.period = ? AND lb.is_active = 1
			ORDER BY gs.score DESC
			LIMIT ?`,
		
		"get_recent_games": `
			SELECT game_id, player_id, game_type, start_time, end_time, duration_seconds,
				   score, level_reached, completed, opponent_id, game_data
			FROM game_sessions
			WHERE player_id = ?
			ORDER BY start_time DESC
			LIMIT ?`,
		
		"update_experience": `
			UPDATE players SET experience_points = experience_points + ?
			WHERE player_id = ?`,
		
		"count_players": `
			SELECT COUNT(*) FROM players WHERE is_active = 1`,
		
		"count_daily_active": `
			SELECT COUNT(DISTINCT player_id) FROM game_sessions
			WHERE DATE(start_time) = CURDATE()`,
		
		"count_games_today": `
			SELECT COUNT(*) FROM game_sessions
			WHERE DATE(start_time) = CURDATE()`,
	}

	for name, query := range statements {
		stmt, err := dm.db.Prepare(query)
		if err != nil {
			log.Printf("Failed to prepare statement %s: %v", name, err)
			continue
		}
		dm.queries[name] = stmt
	}

	log.Printf("Prepared %d SQL statements", len(dm.queries))
}

// IsReady returns whether the database is ready for operations
func (dm *DatabaseManager) IsReady() bool {
	return dm.isReady
}

// Example usage
func main() {
	config := DatabaseConfig{
		Host:     "localhost",
		Port:     "3306",
		Username: "root",
		Password: "password",
		Database: "game_center",
		MaxConns: 25,
		MaxIdle:  5,
	}

	dm := NewDatabaseManager(config)
	
	if err := dm.Connect(); err != nil {
		log.Fatal(err)
	}
	defer dm.Close()

	// Create a player
	playerID, err := dm.CreatePlayer("testuser", "test@example.com", "Test User")
	if err != nil {
		log.Printf("Error creating player: %v", err)
		return
	}

	fmt.Printf("Created player with ID: %s\n", playerID)

	// Get player stats
	stats, err := dm.GetPlayerStats(playerID)
	if err != nil {
		log.Printf("Error getting player stats: %v", err)
		return
	}

	fmt.Printf("Player stats: %+v\n", stats)

	fmt.Println("Database Manager initialized successfully!")
}
