package main

import (
	"fmt"
	"sync"
	"time"
)

// PlayerManager handles player registration, authentication, and profile management
type PlayerManager struct {
	players    map[string]*Player
	usernames  map[string]string // username -> playerID
	sessions   map[string]*PlayerSession
	mutex      sync.RWMutex
}

// PlayerSession represents an active player session
type PlayerSession struct {
	PlayerID    string
	Token       string
	CreatedAt   time.Time
	LastActive  time.Time
	IPAddress   string
	GameContext string
}

// Player represents a game player
type Player struct {
	ID           string
	Username     string
	Email        string
	DisplayName  string
	AvatarURL    string
	Level        int
	Experience   int
	TotalWins    int
	TotalLosses  int
	TotalDraws   int
	CreatedAt    time.Time
	LastLogin    time.Time
	IsActive     bool
	Preferences  map[string]interface{}
	Statistics   map[string]int
}

// NewPlayerManager creates a new player manager
func NewPlayerManager() *PlayerManager {
	return &PlayerManager{
		players:   make(map[string]*Player),
		usernames: make(map[string]string),
		sessions:  make(map[string]*PlayerSession),
	}
}

// RegisterPlayer creates a new player account
func (pm *PlayerManager) RegisterPlayer(username, email, displayName string) (string, error) {
	pm.mutex.Lock()
	defer pm.mutex.Unlock()

	// Check if username already exists
	if _, exists := pm.usernames[username]; exists {
		return "", fmt.Errorf("username already exists: %s", username)
	}

	playerID := fmt.Sprintf("player_%d", time.Now().Unix())
	
	player := &Player{
		ID:          playerID,
		Username:    username,
		Email:       email,
		DisplayName: displayName,
		Level:       1,
		Experience:  0,
		CreatedAt:   time.Now(),
		IsActive:    true,
		Preferences: make(map[string]interface{}),
		Statistics:  make(map[string]int),
	}

	pm.players[playerID] = player
	pm.usernames[username] = playerID

	return playerID, nil
}

// AuthenticatePlayer validates player credentials
func (pm *PlayerManager) AuthenticatePlayer(username, password string) (string, error) {
	pm.mutex.RLock()
	playerID, exists := pm.usernames[username]
	pm.mutex.RUnlock()

	if !exists {
		return "", fmt.Errorf("player not found: %s", username)
	}

	pm.mutex.RLock()
	player := pm.players[playerID]
	pm.mutex.RUnlock()

	// In a real implementation, you'd verify the password hash
	// For demo purposes, we'll just check if player exists and is active
	if !player.IsActive {
		return "", fmt.Errorf("player account is deactivated")
	}

	// Create session
	sessionToken := pm.createSession(playerID, "")
	
	// Update last login
	pm.mutex.Lock()
	player.LastLogin = time.Now()
	pm.mutex.Unlock()

	return sessionToken, nil
}

// GetPlayer retrieves a player by ID
func (pm *PlayerManager) GetPlayer(playerID string) (*Player, error) {
	pm.mutex.RLock()
	defer pm.mutex.RUnlock()

	player, exists := pm.players[playerID]
	if !exists {
		return nil, fmt.Errorf("player not found: %s", playerID)
	}

	return player, nil
}

// GetPlayerByUsername retrieves a player by username
func (pm *PlayerManager) GetPlayerByUsername(username string) (*Player, error) {
	pm.mutex.RLock()
	defer pm.mutex.RUnlock()

	playerID, exists := pm.usernames[username]
	if !exists {
		return nil, fmt.Errorf("player not found: %s", username)
	}

	player := pm.players[playerID]
	return player, nil
}

// UpdatePlayerExperience adds experience points and handles level ups
func (pm *PlayerManager) UpdatePlayerExperience(playerID string, expGained int) error {
	pm.mutex.Lock()
	defer pm.mutex.Unlock()

	player, exists := pm.players[playerID]
	if !exists {
		return fmt.Errorf("player not found: %s", playerID)
	}

	oldLevel := player.Level
	player.Experience += expGained

	// Calculate new level (every 1000 exp = 1 level)
	newLevel := (player.Experience / 1000) + 1
	
	if newLevel > oldLevel {
		player.Level = newLevel
		player.Statistics["level_ups"]++
	}

	return nil
}

// UpdatePlayerStats updates player game statistics
func (pm *PlayerManager) UpdatePlayerStats(playerID string, gameResult string) error {
	pm.mutex.Lock()
	defer pm.mutex.Unlock()

	player, exists := pm.players[playerID]
	if !exists {
		return fmt.Errorf("player not found: %s", playerID)
	}

	switch gameResult {
	case "win":
		player.TotalWins++
		player.Statistics["wins"]++
	case "loss":
		player.TotalLosses++
		player.Statistics["losses"]++
	case "draw":
		player.TotalDraws++
		player.Statistics["draws"]++
	}

	// Update win rate
	totalGames := player.TotalWins + player.TotalLosses + player.TotalDraws
	if totalGames > 0 {
		winRate := float64(player.TotalWins) / float64(totalGames) * 100
		player.Statistics["win_rate"] = int(winRate)
	}

	return nil
}

// CreateSession creates a new player session
func (pm *PlayerManager) CreateSession(playerID, ipAddress string) (string, error) {
	pm.mutex.Lock()
	defer pm.mutex.Unlock()

	if _, exists := pm.players[playerID]; !exists {
		return "", fmt.Errorf("player not found: %s", playerID)
	}

	sessionToken := fmt.Sprintf("session_%d_%s", time.Now().Unix(), playerID)
	
	session := &PlayerSession{
		PlayerID:   playerID,
		Token:      sessionToken,
		CreatedAt:  time.Now(),
		LastActive: time.Now(),
		IPAddress:  ipAddress,
	}

	pm.sessions[sessionToken] = session
	return sessionToken, nil
}

// ValidateSession checks if a session token is valid
func (pm *PlayerManager) ValidateSession(token string) (*PlayerSession, error) {
	pm.mutex.RLock()
	session, exists := pm.sessions[token]
	pm.mutex.RUnlock()

	if !exists {
		return nil, fmt.Errorf("invalid session token")
	}

	// Check if session is expired (24 hours)
	if time.Since(session.CreatedAt) > time.Hour*24 {
		pm.mutex.Lock()
		delete(pm.sessions, token)
		pm.mutex.Unlock()
		return nil, fmt.Errorf("session expired")
	}

	// Update last active time
	pm.mutex.Lock()
	session.LastActive = time.Now()
	pm.mutex.Unlock()

	return session, nil
}

// InvalidateSession removes a session token
func (pm *PlayerManager) InvalidateSession(token string) error {
	pm.mutex.Lock()
	defer pm.mutex.Unlock()

	if _, exists := pm.sessions[token]; !exists {
		return fmt.Errorf("session not found")
	}

	delete(pm.sessions, token)
	return nil
}

// GetPlayerRank calculates player rank based on experience
func (pm *PlayerManager) GetPlayerRank(playerID string) (int, error) {
	pm.mutex.RLock()
	defer pm.mutex.RUnlock()

	player, exists := pm.players[playerID]
	if !exists {
		return 0, fmt.Errorf("player not found: %s", playerID)
	}

	rank := 1
	for _, otherPlayer := range pm.players {
		if otherPlayer.Experience > player.Experience && otherPlayer.IsActive {
			rank++
		}
	}

	return rank, nil
}

// GetLeaderboard returns top players by experience
func (pm *PlayerManager) GetLeaderboard(limit int) []*Player {
	pm.mutex.RLock()
	defer pm.mutex.RUnlock()

	players := make([]*Player, 0, len(pm.players))
	for _, player := range pm.players {
		if player.IsActive {
			players = append(players, player)
		}
	}

	// Sort by experience (descending)
	for i := 0; i < len(players); i++ {
		for j := i + 1; j < len(players); j++ {
			if players[i].Experience < players[j].Experience {
				players[i], players[j] = players[j], players[i]
			}
		}
	}

	// Return top N players
	if len(players) > limit {
		players = players[:limit]
	}

	return players
}

// GetOnlinePlayers returns currently active players
func (pm *PlayerManager) GetOnlinePlayers() []*Player {
	pm.mutex.RLock()
	defer pm.mutex.RUnlock()

	onlinePlayers := make([]*Player, 0)
	for _, session := range pm.sessions {
		if player, exists := pm.players[session.PlayerID]; exists {
			if time.Since(session.LastActive) < time.Minute*5 {
				onlinePlayers = append(onlinePlayers, player)
			}
		}
	}

	return onlinePlayers
}

// UpdatePlayerPreferences updates player preferences
func (pm *PlayerManager) UpdatePlayerPreferences(playerID string, preferences map[string]interface{}) error {
	pm.mutex.Lock()
	defer pm.mutex.Unlock()

	player, exists := pm.players[playerID]
	if !exists {
		return fmt.Errorf("player not found: %s", playerID)
	}

	for key, value := range preferences {
		player.Preferences[key] = value
	}

	return nil
}

// DeactivatePlayer deactivates a player account
func (pm *PlayerManager) DeactivatePlayer(playerID string) error {
	pm.mutex.Lock()
	defer pm.mutex.Unlock()

	player, exists := pm.players[playerID]
	if !exists {
		return fmt.Errorf("player not found: %s", playerID)
	}

	player.IsActive = false

	// Invalidate all sessions for this player
	for token, session := range pm.sessions {
		if session.PlayerID == playerID {
			delete(pm.sessions, token)
		}
	}

	return nil
}

// GetPlayerCount returns total number of registered players
func (pm *PlayerManager) GetPlayerCount() int {
	pm.mutex.RLock()
	defer pm.mutex.RUnlock()

	return len(pm.players)
}

// CleanupExpiredSessions removes expired sessions
func (pm *PlayerManager) CleanupExpiredSessions() {
	pm.mutex.Lock()
	defer pm.mutex.Unlock()

	now := time.Now()
	for token, session := range pm.sessions {
		if now.Sub(session.CreatedAt) > time.Hour*24 {
			delete(pm.sessions, token)
		}
	}
}

// Helper function to create session (internal)
func (pm *PlayerManager) createSession(playerID, ipAddress string) string {
	sessionToken := fmt.Sprintf("session_%d_%s", time.Now().Unix(), playerID)
	
	session := &PlayerSession{
		PlayerID:   playerID,
		Token:      sessionToken,
		CreatedAt:  time.Now(),
		LastActive: time.Now(),
		IPAddress:  ipAddress,
	}

	pm.sessions[sessionToken] = session
	return sessionToken
}

// Example usage
func main() {
	pm := NewPlayerManager()

	// Register players
	playerID1, err := pm.RegisterPlayer("alice", "alice@example.com", "Alice")
	if err != nil {
		fmt.Printf("Error registering Alice: %v\n", err)
		return
	}

	playerID2, err := pm.RegisterPlayer("bob", "bob@example.com", "Bob")
	if err != nil {
		fmt.Printf("Error registering Bob: %v\n", err)
		return
	}

	// Authenticate players
	session1, err := pm.AuthenticatePlayer("alice", "password123")
	if err != nil {
		fmt.Printf("Error authenticating Alice: %v\n", err)
		return
	}

	session2, err := pm.AuthenticatePlayer("bob", "password123")
	if err != nil {
		fmt.Printf("Error authenticating Bob: %v\n", err)
		return
	}

	// Update player statistics
	pm.UpdatePlayerExperience(playerID1, 1500)
	pm.UpdatePlayerStats(playerID1, "win")
	pm.UpdatePlayerStats(playerID1, "win")

	pm.UpdatePlayerExperience(playerID2, 800)
	pm.UpdatePlayerStats(playerID2, "loss")

	// Get leaderboard
	leaderboard := pm.GetLeaderboard(10)
	fmt.Println("=== Leaderboard ===")
	for i, player := range leaderboard {
		rank, _ := pm.GetPlayerRank(player.ID)
		fmt.Printf("%d. %s (Level %d, %d XP, %d wins)\n", 
			rank, player.Username, player.Level, player.Experience, player.TotalWins)
	}

	// Get online players
	online := pm.GetOnlinePlayers()
	fmt.Printf("\nOnline Players: %d\n", len(online))

	fmt.Println("\nPlayer Manager initialized successfully!")
}
