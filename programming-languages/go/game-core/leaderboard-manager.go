package main

import (
	"fmt"
	"sort"
	"sync"
	"time"
)

// LeaderboardManager handles leaderboard operations and rankings
type LeaderboardManager struct {
	leaderboards map[string]*Leaderboard
	entries      map[string][]*LeaderboardEntry
	mutex        sync.RWMutex
}

// Leaderboard represents a leaderboard
type Leaderboard struct {
	ID          string
	Name        string
	GameType    string
	Period      string // "daily", "weekly", "monthly", "all_time"
	IsActive    bool
	MaxEntries  int
	CreatedAt   time.Time
	ResetDate   time.Time
	Description string
}

// LeaderboardEntry represents an entry in a leaderboard
type LeaderboardEntry struct {
	ID          string
	LeaderboardID string
	PlayerID    string
	Username    string
	Score       int
	Rank        int
	GameType    string
	Period      string
	AchievedAt  time.Time
	Metadata    map[string]interface{}
}

// LeaderboardStats represents leaderboard statistics
type LeaderboardStats struct {
	TotalEntries  int
	TopPlayer     string
	TopScore      int
	AverageScore  float64
	RecentChanges int
	ActivePlayers int
}

// NewLeaderboardManager creates a new leaderboard manager
func NewLeaderboardManager() *LeaderboardManager {
	return &LeaderboardManager{
		leaderboards: make(map[string]*Leaderboard),
		entries:      make(map[string][]*LeaderboardEntry),
	}
}

// CreateLeaderboard creates a new leaderboard
func (lm *LeaderboardManager) CreateLeaderboard(config LeaderboardConfig) (*Leaderboard, error) {
	lm.mutex.Lock()
	defer lm.mutex.Unlock()

	leaderboardID := fmt.Sprintf("lb_%d", time.Now().Unix())
	
	leaderboard := &Leaderboard{
		ID:         leaderboardID,
		Name:       config.Name,
		GameType:   config.GameType,
		Period:     config.Period,
		IsActive:   config.IsActive,
		MaxEntries: config.MaxEntries,
		CreatedAt:  time.Now(),
		ResetDate:  lm.calculateResetDate(config.Period),
		Description: config.Description,
	}

	lm.leaderboards[leaderboardID] = leaderboard
	lm.entries[leaderboardID] = make([]*LeaderboardEntry, 0)

	return leaderboard, nil
}

// LeaderboardConfig contains configuration for creating leaderboards
type LeaderboardConfig struct {
	Name        string
	GameType    string
	Period      string
	IsActive    bool
	MaxEntries  int
	Description string
}

// UpdatePlayerScore updates a player's score on a leaderboard
func (lm *LeaderboardManager) UpdatePlayerScore(leaderboardID, playerID, username string, score int) error {
	lm.mutex.Lock()
	defer lm.mutex.Unlock()

	leaderboard, exists := lm.leaderboards[leaderboardID]
	if !exists {
		return fmt.Errorf("leaderboard not found: %s", leaderboardID)
	}

	if !leaderboard.IsActive {
		return fmt.Errorf("leaderboard is not active: %s", leaderboardID)
	}

	// Check if period has reset
	if lm.shouldResetLeaderboard(leaderboard) {
		lm.resetLeaderboard(leaderboardID)
		leaderboard.ResetDate = lm.calculateResetDate(leaderboard.Period)
	}

	entries := lm.entries[leaderboardID]
	
	// Find existing entry for this player
	var existingEntry *LeaderboardEntry
	for _, entry := range entries {
		if entry.PlayerID == playerID {
			existingEntry = entry
			break
		}
	}

	now := time.Now()
	
	if existingEntry != nil {
		// Update existing entry if score is better
		if score > existingEntry.Score {
			oldScore := existingEntry.Score
			existingEntry.Score = score
			existingEntry.AchievedAt = now
			
			// Re-sort entries
			lm.recalculateRanks(leaderboardID)
			
			return nil
		}
	} else {
		// Create new entry
		newEntry := &LeaderboardEntry{
			ID:           fmt.Sprintf("entry_%d", now.Unix()),
			LeaderboardID: leaderboardID,
			PlayerID:     playerID,
			Username:     username,
			Score:        score,
			GameType:     leaderboard.GameType,
			Period:       leaderboard.Period,
			AchievedAt:   now,
			Metadata:     make(map[string]interface{}),
		}

		entries = append(entries, newEntry)
		lm.entries[leaderboardID] = entries
		
		// Sort and recalculate ranks
		lm.recalculateRanks(leaderboardID)
		
		// Limit entries to max
		if len(entries) > leaderboard.MaxEntries {
			entries = entries[:leaderboard.MaxEntries]
			lm.entries[leaderboardID] = entries
		}
	}

	return nil
}

// GetTopPlayers returns top players from a leaderboard
func (lm *LeaderboardManager) GetTopPlayers(leaderboardID string, limit int) ([]*LeaderboardEntry, error) {
	lm.mutex.RLock()
	defer lm.mutex.RUnlock()

	if _, exists := lm.leaderboards[leaderboardID]; !exists {
		return nil, fmt.Errorf("leaderboard not found: %s", leaderboardID)
	}

	entries := lm.entries[leaderboardID]
	
	// Sort by score descending
	sort.Slice(entries, func(i, j int) bool {
		return entries[i].Score > entries[j].Score
	})

	// Return top N entries
	if len(entries) > limit {
		entries = entries[:limit]
	}

	return entries, nil
}

// GetPlayerRank returns a player's rank on a leaderboard
func (lm *LeaderboardManager) GetPlayerRank(leaderboardID, playerID string) (int, error) {
	lm.mutex.RLock()
	defer lm.mutex.RUnlock()

	entries := lm.entries[leaderboardID]
	
	for i, entry := range entries {
		if entry.PlayerID == playerID {
			return entry.Rank, nil
		}
	}

	return 0, fmt.Errorf("player not found on leaderboard")
}

// GetPlayerScore returns a player's score on a leaderboard
func (lm *LeaderboardManager) GetPlayerScore(leaderboardID, playerID string) (int, error) {
	lm.mutex.RLock()
	defer lm.mutex.RUnlock()

	entries := lm.entries[leaderboardID]
	
	for _, entry := range entries {
		if entry.PlayerID == playerID {
			return entry.Score, nil
		}
	}

	return 0, fmt.Errorf("player not found on leaderboard")
}

// GetLeaderboard returns a leaderboard by ID
func (lm *LeaderboardManager) GetLeaderboard(leaderboardID string) (*Leaderboard, error) {
	lm.mutex.RLock()
	defer lm.mutex.RUnlock()

	leaderboard, exists := lm.leaderboards[leaderboardID]
	if !exists {
		return nil, fmt.Errorf("leaderboard not found: %s", leaderboardID)
	}

	return leaderboard, nil
}

// GetAllLeaderboards returns all leaderboards
func (lm *LeaderboardManager) GetAllLeaderboards() []*Leaderboard {
	lm.mutex.RLock()
	defer lm.mutex.RUnlock()

	leaderboards := make([]*Leaderboard, 0, len(lm.leaderboards))
	for _, leaderboard := range lm.leaderboards {
		leaderboards = append(leaderboards, leaderboard)
	}

	return leaderboards
}

// GetLeaderboardEntries returns all entries for a leaderboard
func (lm *LeaderboardManager) GetLeaderboardEntries(leaderboardID string) ([]*LeaderboardEntry, error) {
	lm.mutex.RLock()
	defer lm.mutex.RUnlock()

	if _, exists := lm.leaderboards[leaderboardID]; !exists {
		return nil, fmt.Errorf("leaderboard not found: %s", leaderboardID)
	}

	entries := make([]*LeaderboardEntry, len(lm.entries[leaderboardID]))
	copy(entries, lm.entries[leaderboardID])
	
	return entries, nil
}

// GetPlayerLeaderboards returns all leaderboards where a player has entries
func (lm *LeaderboardManager) GetPlayerLeaderboards(playerID string) []*PlayerLeaderboardInfo {
	lm.mutex.RLock()
	defer lm.mutex.RUnlock()

	playerLeaderboards := make([]*PlayerLeaderboardInfo, 0)
	
	for leaderboardID, entries := range lm.entries {
		for _, entry := range entries {
			if entry.PlayerID == playerID {
				leaderboard := lm.leaderboards[leaderboardID]
				playerLeaderboards = append(playerLeaderboards, &PlayerLeaderboardInfo{
					Leaderboard: leaderboard,
					Entry:       entry,
				})
				break
			}
		}
	}

	return playerLeaderboards
}

// GetLeaderboardStats returns statistics for a leaderboard
func (lm *LeaderboardManager) GetLeaderboardStats(leaderboardID string) (*LeaderboardStats, error) {
	lm.mutex.RLock()
	defer lm.mutex.RUnlock()

	if _, exists := lm.leaderboards[leaderboardID]; !exists {
		return nil, fmt.Errorf("leaderboard not found: %s", leaderboardID)
	}

	entries := lm.entries[leaderboardID]
	
	if len(entries) == 0 {
		return &LeaderboardStats{
			TotalEntries: 0,
		}, nil
	}

	// Calculate statistics
	totalScore := 0
	for _, entry := range entries {
		totalScore += entry.Score
	}

	averageScore := float64(totalScore) / float64(len(entries))
	
	stats := &LeaderboardStats{
		TotalEntries: len(entries),
		TopPlayer:    entries[0].Username,
		TopScore:     entries[0].Score,
		AverageScore: averageScore,
	}

	return stats, nil
}

// GetGlobalStats returns global leaderboard statistics
func (lm *LeaderboardManager) GetGlobalStats() *GlobalLeaderboardStats {
	lm.mutex.RLock()
	defer lm.mutex.RUnlock()

	totalLeaderboards := len(lm.leaderboards)
	totalEntries := 0
	activeLeaderboards := 0
	
	uniquePlayers := make(map[string]bool)
	
	for _, leaderboard := range lm.leaderboards {
		if leaderboard.IsActive {
			activeLeaderboards++
		}
		
		for _, entry := range lm.entries[leaderboard.ID] {
			totalEntries++
			uniquePlayers[entry.PlayerID] = true
		}
	}

	return &GlobalLeaderboardStats{
		TotalLeaderboards:   totalLeaderboards,
		ActiveLeaderboards:  activeLeaderboards,
		TotalEntries:        totalEntries,
		UniquePlayers:       len(uniquePlayers),
		AverageEntriesPerLB: float64(totalEntries) / float64(totalLeaderboards),
	}
}

// GlobalLeaderboardStats represents global statistics
type GlobalLeaderboardStats struct {
	TotalLeaderboards   int
	ActiveLeaderboards  int
	TotalEntries        int
	UniquePlayers       int
	AverageEntriesPerLB float64
}

// PlayerLeaderboardInfo represents a player's info on a leaderboard
type PlayerLeaderboardInfo struct {
	Leaderboard *Leaderboard
	Entry       *LeaderboardEntry
}

// DeactivateLeaderboard deactivates a leaderboard
func (lm *LeaderboardManager) DeactivateLeaderboard(leaderboardID string) error {
	lm.mutex.Lock()
	defer lm.mutex.Unlock()

	leaderboard, exists := lm.leaderboards[leaderboardID]
	if !exists {
		return fmt.Errorf("leaderboard not found: %s", leaderboardID)
	}

	leaderboard.IsActive = false
	return nil
}

// ResetLeaderboard resets a leaderboard
func (lm *LeaderboardManager) ResetLeaderboard(leaderboardID string) error {
	lm.mutex.Lock()
	defer lm.mutex.Unlock()

	if _, exists := lm.leaderboards[leaderboardID]; !exists {
		return fmt.Errorf("leaderboard not found: %s", leaderboardID)
	}

	lm.resetLeaderboard(leaderboardID)
	return nil
}

// BulkUpdateScores updates multiple players' scores at once
func (lm *LeaderboardManager) BulkUpdateScores(updates []ScoreUpdate) []ScoreUpdateResult {
	lm.mutex.Lock()
	defer lm.mutex.Unlock()

	results := make([]ScoreUpdateResult, 0, len(updates))
	
	for _, update := range updates {
		result := ScoreUpdateResult{
			PlayerID:    update.PlayerID,
			LeaderboardID: update.LeaderboardID,
			Success:     false,
		}

		leaderboard, exists := lm.leaderboards[update.LeaderboardID]
		if !exists {
			result.Error = "Leaderboard not found"
			results = append(results, result)
			continue
		}

		if !leaderboard.IsActive {
			result.Error = "Leaderboard not active"
			results = append(results, result)
			continue
		}

		// Check if period has reset
		if lm.shouldResetLeaderboard(leaderboard) {
			lm.resetLeaderboard(update.LeaderboardID)
			leaderboard.ResetDate = lm.calculateResetDate(leaderboard.Period)
		}

		err := lm.updatePlayerScoreInternal(update.LeaderboardID, update.PlayerID, update.Username, update.Score)
		if err != nil {
			result.Error = err.Error()
		} else {
			result.Success = true
		}

		results = append(results, result)
	}

	// Recalculate all ranks
	for _, leaderboard := range lm.leaderboards {
		lm.recalculateRanks(leaderboard.ID)
	}

	return results
}

// ScoreUpdate represents a score update request
type ScoreUpdate struct {
	LeaderboardID string
	PlayerID      string
	Username      string
	Score         int
}

// ScoreUpdateResult represents the result of a score update
type ScoreUpdateResult struct {
	LeaderboardID string
	PlayerID      string
	Success       bool
	Error         string
}

// Helper functions

// shouldResetLeaderboard checks if a leaderboard should be reset
func (lm *LeaderboardManager) shouldResetLeaderboard(leaderboard *Leaderboard) bool {
	if leaderboard.Period == "all_time" {
		return false
	}

	return time.Now().After(leaderboard.ResetDate)
}

// calculateResetDate calculates when a leaderboard should reset
func (lm *LeaderboardManager) calculateResetDate(period string) time.Time {
	now := time.Now()
	
	switch period {
	case "daily":
		return now.AddDate(0, 0, 1)
	case "weekly":
		return now.AddDate(0, 0, 7)
	case "monthly":
		return now.AddDate(0, 1, 0)
	default:
		return now.AddDate(10, 0, 0) // Very far future for all_time
	}
}

// resetLeaderboard clears a leaderboard
func (lm *LeaderboardManager) resetLeaderboard(leaderboardID string) {
	lm.entries[leaderboardID] = make([]*LeaderboardEntry, 0)
}

// recalculateRanks recalculates ranks for all entries
func (lm *LeaderboardManager) recalculateRanks(leaderboardID string) {
	entries := lm.entries[leaderboardID]
	
	// Sort by score descending
	sort.Slice(entries, func(i, j int) bool {
		return entries[i].Score > entries[j].Score
	})

	// Update ranks
	for i, entry := range entries {
		entry.Rank = i + 1
	}
}

// updatePlayerScoreInternal updates a player's score (internal method)
func (lm *LeaderboardManager) updatePlayerScoreInternal(leaderboardID, playerID, username string, score int) error {
	entries := lm.entries[leaderboardID]
	
	// Find existing entry
	var existingEntry *LeaderboardEntry
	for _, entry := range entries {
		if entry.PlayerID == playerID {
			existingEntry = entry
			break
		}
	}

	now := time.Now()
	
	if existingEntry != nil {
		if score > existingEntry.Score {
			existingEntry.Score = score
			existingEntry.AchievedAt = now
		}
	} else {
		newEntry := &LeaderboardEntry{
			ID:           fmt.Sprintf("entry_%d", now.Unix()),
			LeaderboardID: leaderboardID,
			PlayerID:     playerID,
			Username:     username,
			Score:        score,
			GameType:     lm.leaderboards[leaderboardID].GameType,
			Period:       lm.leaderboard[leaderboardID].Period,
			AchievedAt:   now,
			Metadata:     make(map[string]interface{}),
		}

		entries = append(entries, newEntry)
		lm.entries[leaderboardID] = entries
	}

	return nil
}

// CleanupExpiredLeaderboards removes expired leaderboard entries
func (lm *LeaderboardManager) CleanupExpiredLeaderboards() {
	lm.mutex.Lock()
	defer lm.mutex.Unlock()

	for leaderboardID, leaderboard := range lm.leaderboards {
		if lm.shouldResetLeaderboard(leaderboard) {
			lm.resetLeaderboard(leaderboardID)
			leaderboard.ResetDate = lm.calculateResetDate(leaderboard.Period)
		}
	}
}

// Example usage
func main() {
	lm := NewLeaderboardManager()

	// Create leaderboards
	configs := []LeaderboardConfig{
		{
			Name:        "Daily Puzzle High Scores",
			GameType:    "puzzle",
			Period:      "daily",
			IsActive:    true,
			MaxEntries:  100,
			Description: "Daily high scores for puzzle games",
		},
		{
			Name:        "Weekly Action Champions",
			GameType:    "action",
			Period:      "weekly",
			IsActive:    true,
			MaxEntries:  50,
			Description: "Weekly champions for action games",
		},
		{
			Name:        "All-Time Legends",
			GameType:    "all",
			Period:      "all_time",
			IsActive:    true,
			MaxEntries:  1000,
			Description: "All-time legendary players",
		},
	}

	for _, config := range configs {
		_, err := lm.CreateLeaderboard(config)
		if err != nil {
			fmt.Printf("Error creating leaderboard: %v\n", err)
		}
	}

	// Update player scores
	updates := []ScoreUpdate{
		{lb.ID, "player_1", "Alice", 1500},
		{lb.ID, "player_2", "Bob", 2000},
		{lb.ID, "player_3", "Charlie", 1200},
		{lb.ID, "lb_1", "player_1", "Alice", 800},
		{lb.ID, "lb_1", "player_2", "Bob", 1200},
	}

	// Update scores for all leaderboards
	lbIDs := []string{"lb_1", "lb_2", "lb_3"} // Get actual IDs from creation
	
	for _, lbID := range lbIDs {
		updates = []ScoreUpdate{
			{lbID, "player_1", "Alice", 1500},
			{lbID, "player_2", "Bob", 2000},
			{lbID, "player_3", "Charlie", 1200},
		}
		
		results := lm.BulkUpdateScores(updates)
		fmt.Printf("Updated %d scores for leaderboard %s\n", len(results), lbID)
	}

	// Get top players from daily puzzle leaderboard
	topPlayers, err := lm.GetTopPlayers("lb_1", 10)
	if err != nil {
		fmt.Printf("Error getting top players: %v\n", err)
	} else {
		fmt.Println("\nTop Players - Daily Puzzle:")
		for i, player := range topPlayers {
			fmt.Printf("%d. %s - %d points\n", player.Rank, player.Username, player.Score)
		}
	}

	// Get player rank
	rank, err := lm.GetPlayerRank("lb_1", "player_2")
	if err != nil {
		fmt.Printf("Error getting rank: %v\n", err)
	} else {
		fmt.Printf("Bob's rank: %d\n", rank)
	}

	// Get global stats
	stats := lm.GetGlobalStats()
	fmt.Printf("\nGlobal Stats:\n")
	fmt.Printf("Total Leaderboards: %d\n", stats.TotalLeaderboards)
	fmt.Printf("Active Leaderboards: %d\n", stats.ActiveLeaderboards)
	fmt.Printf("Total Entries: %d\n", stats.TotalEntries)
	fmt.Printf("Unique Players: %d\n", stats.UniquePlayers)

	fmt.Println("\nLeaderboard Manager initialized successfully!")
}
