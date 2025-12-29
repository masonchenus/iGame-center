package main

import (
	"fmt"
	"sync"
	"time"
)

// GameManager handles overall game coordination
type GameManager struct {
	games     map[string]*Game
	players   map[string]*Player
	gameRooms map[string]*GameRoom
	mutex     sync.RWMutex
}

// Game represents a game instance
type Game struct {
	ID          string
	Name        string
	Players     []*Player
	GameState   GameState
	CreatedAt   time.Time
	MaxPlayers  int
	GameType    string
}

// Player represents a player in the game
type Player struct {
	ID          string
	Username    string
	Score       int
	Level       int
	Connected   bool
	LastActive  time.Time
}

// GameState represents the current state of a game
type GameState struct {
	Phase       string
	Board       [][]int
	Turn        string
	Winner      string
	StartTime   time.Time
}

// GameRoom represents a game room for multiplayer
type GameRoom struct {
	ID          string
	Game        *Game
	Players     []*Player
	IsActive    bool
	CreatedAt   time.Time
}

// NewGameManager creates a new game manager
func NewGameManager() *GameManager {
	return &GameManager{
		games:     make(map[string]*Game),
		players:   make(map[string]*Player),
		gameRooms: make(map[string]*GameRoom),
	}
}

// CreateGame creates a new game
func (gm *GameManager) CreateGame(name string, gameType string, maxPlayers int) (*Game, error) {
	gm.mutex.Lock()
	defer gm.mutex.Unlock()

	gameID := fmt.Sprintf("game_%d", time.Now().Unix())
	
	game := &Game{
		ID:         gameID,
		Name:       name,
		Players:    make([]*Player, 0, maxPlayers),
		GameState:  GameState{},
		CreatedAt:  time.Now(),
		MaxPlayers: maxPlayers,
		GameType:   gameType,
	}

	gm.games[gameID] = game
	return game, nil
}

// JoinGame allows a player to join a game
func (gm *GameManager) JoinGame(gameID string, player *Player) error {
	gm.mutex.Lock()
	defer gm.mutex.Unlock()

	game, exists := gm.games[gameID]
	if !exists {
		return fmt.Errorf("game not found: %s", gameID)
	}

	if len(game.Players) >= game.MaxPlayers {
		return fmt.Errorf("game is full")
	}

	game.Players = append(game.Players, player)
	player.Connected = true
	player.LastActive = time.Now()

	return nil
}

// LeaveGame removes a player from a game
func (gm *GameManager) LeaveGame(gameID string, playerID string) error {
	gm.mutex.Lock()
	defer gm.mutex.Unlock()

	game, exists := gm.games[gameID]
	if !exists {
		return fmt.Errorf("game not found: %s", gameID)
	}

	for i, p := range game.Players {
		if p.ID == playerID {
			game.Players = append(game.Players[:i], game.Players[i+1:]...)
			p.Connected = false
			break
		}
	}

	return nil
}

// GetGame retrieves a game by ID
func (gm *GameManager) GetGame(gameID string) (*Game, bool) {
	gm.mutex.RLock()
	defer gm.mutex.RUnlock()

	game, exists := gm.games[gameID]
	return game, exists
}

// UpdatePlayerScore updates a player's score
func (gm *GameManager) UpdatePlayerScore(gameID string, playerID string, score int) error {
	gm.mutex.Lock()
	defer gm.mutex.Unlock()

	game, exists := gm.games[gameID]
	if !exists {
		return fmt.Errorf("game not found: %s", gameID)
	}

	for _, player := range game.Players {
		if player.ID == playerID {
			player.Score += score
			player.Level = player.Score / 1000 // Level up every 1000 points
			player.LastActive = time.Now()
			break
		}
	}

	return nil
}

// GetActiveGames returns all active games
func (gm *GameManager) GetActiveGames() []*Game {
	gm.mutex.RLock()
	defer gm.mutex.RUnlock()

	games := make([]*Game, 0, len(gm.games))
	for _, game := range gm.games {
		if len(game.Players) > 0 {
			games = append(games, game)
		}
	}
	return games
}

// CleanupInactiveGames removes games with no players for more than 1 hour
func (gm *GameManager) CleanupInactiveGames() {
	gm.mutex.Lock()
	defer gm.mutex.Unlock()

	now := time.Now()
	for gameID, game := range gm.games {
		if len(game.Players) == 0 && now.Sub(game.CreatedAt) > time.Hour {
			delete(gm.games, gameID)
		}
	}
}

// GetPlayerCount returns the total number of connected players
func (gm *GameManager) GetPlayerCount() int {
	gm.mutex.RLock()
	defer gm.mutex.RUnlock()

	count := 0
	for _, player := range gm.players {
		if player.Connected {
			count++
		}
	}
	return count
}

// StartGameLoop starts the main game loop
func (gm *GameManager) StartGameLoop() {
	ticker := time.NewTicker(time.Second * 5)
	defer ticker.Stop()

	for range ticker.C {
		gm.CleanupInactiveGames()
	}
}

// NewPlayer creates a new player
func NewPlayer(id, username string) *Player {
	return &Player{
		ID:         id,
		Username:   username,
		Score:      0,
		Level:      1,
		Connected:  false,
		LastActive: time.Now(),
	}
}

// GetPlayerStats returns player statistics
func (p *Player) GetPlayerStats() map[string]interface{} {
	return map[string]interface{}{
		"id":       p.ID,
		"username": p.Username,
		"score":    p.Score,
		"level":    p.Level,
		"connected": p.Connected,
	}
}

// IsActive checks if a player is still active
func (p *Player) IsActive() bool {
	return p.Connected && time.Since(p.LastActive) < time.Minute*5
}

// Example usage
func main() {
	gm := NewGameManager()
	
	// Create a new game
	game, err := gm.CreateGame("Tic-Tac-Toe", "puzzle", 2)
	if err != nil {
		fmt.Printf("Error creating game: %v\n", err)
		return
	}

	// Create players
	player1 := NewPlayer("player_1", "Alice")
	player2 := NewPlayer("player_2", "Bob")

	// Join players to game
	err = gm.JoinGame(game.ID, player1)
	if err != nil {
		fmt.Printf("Error joining game: %v\n", err)
	}

	err = gm.JoinGame(game.ID, player2)
	if err != nil {
		fmt.Printf("Error joining game: %v\n", err)
	}

	// Update scores
	gm.UpdatePlayerScore(game.ID, player1.ID, 100)
	gm.UpdatePlayerScore(game.ID, player2.ID, 50)

	// Print game info
	fmt.Printf("Game ID: %s\n", game.ID)
	fmt.Printf("Players: %d/%d\n", len(game.Players), game.MaxPlayers)
	
	for _, player := range game.Players {
		fmt.Printf("Player: %s, Score: %d, Level: %d\n", 
			player.Username, player.Score, player.Level)
	}

	// Start game loop (would run in production)
	fmt.Println("Game Manager initialized successfully!")
}
