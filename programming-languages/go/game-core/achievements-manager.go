package main

import (
	"fmt"
	"sort"
	"sync"
	"time"
)

// AchievementManager handles achievement tracking and unlocking
type AchievementManager struct {
	achievements map[string]*Achievement
	playerData   map[string]map[string]*PlayerAchievement
	mutex        sync.RWMutex
}

// Achievement represents a game achievement
type Achievement struct {
	ID             string
	Name           string
	Description    string
	IconURL        string
	Points         int
	Rarity         string
	Category       string
	Requirements   AchievementRequirements
	IsActive       bool
	Hidden         bool
	ParentIDs      []string // Prerequisites
	CreatedAt      time.Time
}

// AchievementRequirements defines what needs to be done to unlock an achievement
type AchievementRequirements struct {
	Type        string                 // "wins", "games_played", "score", "time_played", "streak", etc.
	Target      int                    // Target value
	GameType    string                 // Specific game type (empty for all games)
	Period      string                 // "all_time", "daily", "weekly", "monthly"
	Consecutive bool                   // Must be consecutive (for streaks)
	Metadata    map[string]interface{} // Additional requirements
}

// PlayerAchievement represents a player's unlocked achievement
type PlayerAchievement struct {
	PlayerID       string
	AchievementID  string
	UnlockedAt     time.Time
	Progress       float64 // 0.0 to 1.0
	ProgressData   map[string]interface{}
	Notified       bool
}

// AchievementNotification represents an achievement unlock notification
type AchievementNotification struct {
	PlayerID      string
	Achievement   *Achievement
	Progress      float64
	PointsEarned  int
	NewLevel      int
	RankChange    int
	Timestamp     time.Time
}

// NewAchievementManager creates a new achievement manager
func NewAchievementManager() *AchievementManager {
	return &AchievementManager{
		achievements: make(map[string]*Achievement),
		playerData:   make(map[string]map[string]*PlayerAchievement),
	}
}

// AddAchievement adds a new achievement
func (am *AchievementManager) AddAchievement(achievement *Achievement) error {
	am.mutex.Lock()
	defer am.mutex.Unlock()

	if _, exists := am.achievements[achievement.ID]; exists {
		return fmt.Errorf("achievement already exists: %s", achievement.ID)
	}

	am.achievements[achievement.ID] = achievement
	return nil
}

// GetAchievement retrieves an achievement by ID
func (am *AchievementManager) GetAchievement(achievementID string) (*Achievement, error) {
	am.mutex.RLock()
	defer am.mutex.RUnlock()

	achievement, exists := am.achievements[achievementID]
	if !exists {
		return nil, fmt.Errorf("achievement not found: %s", achievementID)
	}

	return achievement, nil
}

// GetAllAchievements returns all achievements
func (am *AchievementManager) GetAllAchievements() []*Achievement {
	am.mutex.RLock()
	defer am.mutex.RUnlock()

	achievements := make([]*Achievement, 0, len(am.achievements))
	for _, achievement := range am.achievements {
		achievements = append(achievements, achievement)
	}
	
	return achievements
}

// GetPlayerAchievements returns all achievements for a player
func (am *AchievementManager) GetPlayerAchievements(playerID string) []*PlayerAchievement {
	am.mutex.RLock()
	defer am.mutex.RUnlock()

	playerAchievements := am.playerData[playerID]
	if playerAchievements == nil {
		return make([]*PlayerAchievement, 0)
	}

	achievements := make([]*PlayerAchievement, 0, len(playerAchievements))
	for _, playerAchievement := range playerAchievements {
		achievements = append(achievements, playerAchievement)
	}
	
	return achievements
}

// UpdatePlayerProgress updates a player's progress towards achievements
func (am *AchievementManager) UpdatePlayerProgress(playerID string, stats PlayerGameStats) []AchievementNotification {
	am.mutex.Lock()
	defer am.mutex.Unlock()

	notifications := make([]AchievementNotification, 0)
	
	// Get or create player data
	playerData := am.playerData[playerID]
	if playerData == nil {
		playerData = make(map[string]*PlayerAchievement)
		am.playerData[playerID] = playerData
	}

	// Check each achievement
	for achievementID, achievement := range am.achievements {
		if !achievement.IsActive {
			continue
		}

		// Check prerequisites
		if !am.checkPrerequisites(playerID, achievement) {
			continue
		}

		// Get or create player achievement record
		playerAchievement := playerData[achievementID]
		if playerAchievement == nil {
			playerAchievement = &PlayerAchievement{
				PlayerID:       playerID,
				AchievementID:  achievementID,
				Progress:       0.0,
				ProgressData:   make(map[string]interface{}),
				Notified:       false,
			}
			playerData[achievementID] = playerAchievement
		}

		// Calculate progress
		progress := am.calculateProgress(achievement, stats, playerAchievement)
		oldProgress := playerAchievement.Progress
		
		playerAchievement.Progress = progress
		playerAchievement.UnlockedAt = time.Now()
		playerAchievement.ProgressData = am.updateProgressData(achievement, stats, playerAchievement.ProgressData)

		// Check if achievement was just unlocked
		if oldProgress < 1.0 && progress >= 1.0 {
			notification := am.createAchievementNotification(playerID, achievement, playerAchievement)
			notifications = append(notifications, notification)
		}
	}

	return notifications
}

// checkPrerequisites verifies if player has met all prerequisite achievements
func (am *AchievementManager) checkPrerequisites(playerID string, achievement *Achievement) bool {
	if len(achievement.ParentIDs) == 0 {
		return true
	}

	playerData := am.playerData[playerID]
	if playerData == nil {
		return false
	}

	for _, parentID := range achievement.ParentIDs {
		parentAchievement := playerData[parentID]
		if parentAchievement == nil || parentAchievement.Progress < 1.0 {
			return false
		}
	}

	return true
}

// calculateProgress calculates the current progress towards an achievement
func (am *AchievementManager) calculateProgress(achievement *Achievement, stats PlayerGameStats, playerAchievement *PlayerAchievement) float64 {
	req := achievement.Requirements
	
	switch req.Type {
	case "wins":
		if req.GameType == "" || req.GameType == stats.GameType {
			return float64(stats.Wins) / float64(req.Target)
		}
	case "games_played":
		if req.GameType == "" || req.GameType == stats.GameType {
			return float64(stats.GamesPlayed) / float64(req.Target)
		}
	case "total_score":
		if req.GameType == "" || req.GameType == stats.GameType {
			return float64(stats.TotalScore) / float64(req.Target)
		}
	case "best_score":
		if req.GameType == "" || req.GameType == stats.GameType {
			return float64(stats.BestScore) / float64(req.Target)
		}
	case "time_played":
		playTime := stats.TotalPlayTime / time.Second
		return float64(playTime) / float64(req.Target)
	case "win_streak":
		if req.Consecutive {
			return float64(stats.CurrentStreak) / float64(req.Target)
		}
	case "perfect_games":
		return float64(stats.PerfectGames) / float64(req.Target)
	case "comeback_wins":
		return float64(stats.ComebackWins) / float64(req.Target)
	case "speed_runs":
		return float64(stats.SpeedRuns) / float64(req.Target)
	}

	// Default to current progress if no matching type
	return playerAchievement.Progress
}

// updateProgressData updates additional progress tracking data
func (am *AchievementManager) updateProgressData(achievement *Achievement, stats PlayerGameStats, currentData map[string]interface{}) map[string]interface{} {
	if currentData == nil {
		currentData = make(map[string]interface{})
	}

	// Store specific metadata based on achievement type
	switch achievement.Requirements.Type {
	case "wins", "games_played":
		if achievement.Requirements.GameType != "" {
			currentData["game_type"] = achievement.Requirements.GameType
			currentData["games"] = stats.GamesByType[achievement.Requirements.GameType]
		}
	case "time_played":
		currentData["total_seconds"] = int(stats.TotalPlayTime.Seconds())
	case "win_streak":
		currentData["current_streak"] = stats.CurrentStreak
		currentData["best_streak"] = stats.BestStreak
	case "perfect_games":
		currentData["perfect_games"] = stats.PerfectGames
	}

	return currentData
}

// createAchievementNotification creates a notification for unlocked achievement
func (am *AchievementManager) createAchievementNotification(playerID string, achievement *Achievement, playerAchievement *PlayerAchievement) AchievementNotification {
	return AchievementNotification{
		PlayerID:      playerID,
		Achievement:   achievement,
		Progress:      playerAchievement.Progress,
		PointsEarned:  achievement.Points,
		Timestamp:     time.Now(),
	}
}

// GetPlayerProgress returns a player's progress towards all achievements
func (am *AchievementManager) GetPlayerProgress(playerID string) map[string]float64 {
	am.mutex.RLock()
	defer am.mutex.RUnlock()

	progress := make(map[string]float64)
	playerData := am.playerData[playerID]

	if playerData != nil {
		for achievementID, playerAchievement := range playerData {
			progress[achievementID] = playerAchievement.Progress
		}
	}

	return progress
}

// GetUnlockedAchievements returns all unlocked achievements for a player
func (am *AchievementManager) GetUnlockedAchievements(playerID string) []*Achievement {
	am.mutex.RLock()
	defer am.mutex.RUnlock()

	playerData := am.playerData[playerID]
	if playerData == nil {
		return make([]*Achievement, 0)
	}

	unlocked := make([]*Achievement, 0)
	for achievementID, playerAchievement := range playerData {
		if playerAchievement.Progress >= 1.0 {
			if achievement, exists := am.achievements[achievementID]; exists {
				unlocked = append(unlocked, achievement)
			}
		}
	}

	return unlocked
}

// GetAvailableAchievements returns achievements a player can work towards
func (am *AchievementManager) GetAvailableAchievements(playerID string) []*Achievement {
	am.mutex.RLock()
	defer am.mutex.RUnlock()

	available := make([]*Achievement, 0)
	playerData := am.playerData[playerID]

	for _, achievement := range am.achievements {
		if !achievement.IsActive {
			continue
		}

		// Check if already unlocked
		if playerData != nil {
			playerAchievement := playerData[achievement.ID]
			if playerAchievement != nil && playerAchievement.Progress >= 1.0 {
				continue
			}
		}

		// Check prerequisites
		if !am.checkPrerequisites(playerID, achievement) {
			continue
		}

		available = append(available, achievement)
	}

	return available
}

// GetAchievementLeaderboard returns players ranked by achievement points
func (am *AchievementManager) GetAchievementLeaderboard(limit int) []*PlayerAchievementSummary {
	am.mutex.RLock()
	defer am.mutex.RUnlock()

	type playerSummary struct {
		PlayerID    string
		Username    string
		TotalPoints int
		Unlocked    int
		Hidden      int
	}

	summaries := make([]playerSummary, 0)
	
	for playerID, playerData := range am.playerData {
		var totalPoints, unlocked, hidden int
		
		for achievementID, playerAchievement := range playerData {
			if achievement, exists := am.achievements[achievementID]; exists {
				if playerAchievement.Progress >= 1.0 {
					totalPoints += achievement.Points
					unlocked++
					if achievement.Hidden {
						hidden++
					}
				}
			}
		}
		
		if unlocked > 0 {
			summaries = append(summaries, playerSummary{
				PlayerID:    playerID,
				TotalPoints: totalPoints,
				Unlocked:    unlocked,
				Hidden:      hidden,
			})
		}
	}

	// Sort by total points descending
	sort.Slice(summaries, func(i, j int) bool {
		return summaries[i].TotalPoints > summaries[j].TotalPoints
	})

	// Convert to result type and limit
	result := make([]*PlayerAchievementSummary, 0, len(summaries))
	for i, summary := range summaries {
		if i >= limit {
			break
		}
		result = append(result, &PlayerAchievementSummary{
			PlayerID:       summary.PlayerID,
			TotalPoints:    summary.TotalPoints,
			UnlockedCount:  summary.Unlocked,
			HiddenCount:    summary.Hidden,
			Rank:           i + 1,
		})
	}

	return result
}

// PlayerGameStats represents game statistics for achievement calculation
type PlayerGameStats struct {
	PlayerID       string
	GameType       string
	GamesPlayed    int
	GamesByType    map[string]int
	Wins           int
	TotalScore     int
	BestScore      int
	CurrentStreak  int
	BestStreak     int
	TotalPlayTime  time.Duration
	PerfectGames   int
	ComebackWins   int
	SpeedRuns      int
	FirstGameAt    time.Time
	LastGameAt     time.Time
}

// PlayerAchievementSummary represents achievement summary for leaderboards
type PlayerAchievementSummary struct {
	PlayerID       string
	TotalPoints    int
	UnlockedCount  int
	HiddenCount    int
	Rank           int
}

// BulkUpdateAchievements updates multiple players' achievement progress
func (am *AchievementManager) BulkUpdateAchievements(playerStats []PlayerGameStats) map[string][]AchievementNotification {
	am.mutex.Lock()
	defer am.mutex.Unlock()

	allNotifications := make(map[string][]AchievementNotification)
	
	for _, stats := range playerStats {
		notifications := make([]AchievementNotification, 0)
		
		// Get player data
		playerData := am.playerData[stats.PlayerID]
		if playerData == nil {
			playerData = make(map[string]*PlayerAchievement)
			am.playerData[stats.PlayerID] = playerData
		}

		// Check each achievement
		for achievementID, achievement := range am.achievements {
			if !achievement.IsActive {
				continue
			}

			// Check prerequisites
			if !am.checkPrerequisites(stats.PlayerID, achievement) {
				continue
			}

			// Get or create player achievement record
			playerAchievement := playerData[achievementID]
			if playerAchievement == nil {
				playerAchievement = &PlayerAchievement{
					PlayerID:       stats.PlayerID,
					AchievementID:  achievementID,
					Progress:       0.0,
					ProgressData:   make(map[string]interface{}),
					Notified:       false,
				}
				playerData[achievementID] = playerAchievement
			}

			// Calculate progress
			progress := am.calculateProgress(achievement, stats, playerAchievement)
			oldProgress := playerAchievement.Progress
			
			playerAchievement.Progress = progress
			playerAchievement.UnlockedAt = time.Now()
			playerAchievement.ProgressData = am.updateProgressData(achievement, stats, playerAchievement.ProgressData)

			// Check if achievement was just unlocked
			if oldProgress < 1.0 && progress >= 1.0 {
				notification := am.createAchievementNotification(stats.PlayerID, achievement, playerAchievement)
				notifications = append(notifications, notification)
			}
		}
		
		if len(notifications) > 0 {
			allNotifications[stats.PlayerID] = notifications
		}
	}

	return allNotifications
}

// Example usage
func main() {
	am := NewAchievementManager()

	// Add sample achievements
	achievements := []*Achievement{
		{
			ID:          "first_win",
			Name:        "First Victory",
			Description: "Win your first game",
			Points:      10,
			Rarity:      "common",
			Category:    "games",
			Requirements: AchievementRequirements{
				Type:   "wins",
				Target: 1,
			},
			IsActive:  true,
			Hidden:    false,
			CreatedAt: time.Now(),
		},
		{
			ID:          "speed_demon",
			Name:        "Speed Demon",
			Description: "Complete a game in under 30 seconds",
			Points:      50,
			Rarity:      "rare",
			Category:    "speed",
			Requirements: AchievementRequirements{
				Type:   "speed_runs",
				Target: 1,
			},
			IsActive:  true,
			Hidden:    false,
			CreatedAt: time.Now(),
		},
		{
			ID:          "perfectionist",
			Name:        "Perfectionist",
			Description: "Complete 10 perfect games",
			Points:      100,
			Rarity:      "epic",
			Category:    "skill",
			Requirements: AchievementRequirements{
				Type:   "perfect_games",
				Target: 10,
			},
			IsActive:  true,
			Hidden:    false,
			CreatedAt: time.Now(),
		},
	}

	for _, achievement := range achievements {
		err := am.AddAchievement(achievement)
		if err != nil {
			fmt.Printf("Error adding achievement: %v\n", err)
		}
	}

	// Create sample player stats
	stats := PlayerGameStats{
		PlayerID:      "player_1",
		GameType:      "puzzle",
		GamesPlayed:   5,
		GamesByType:   map[string]int{"puzzle": 5},
		Wins:          3,
		TotalScore:    1500,
		BestScore:     500,
		CurrentStreak: 2,
		BestStreak:    3,
		PerfectGames:  1,
		SpeedRuns:     2,
		TotalPlayTime: time.Hour,
		FirstGameAt:   time.Now().Add(-time.Hour),
		LastGameAt:    time.Now(),
	}

	// Update player progress
	notifications := am.UpdatePlayerProgress("player_1", stats)
	
	fmt.Printf("Processed %d achievement updates for player_1\n", len(notifications))
	
	for _, notification := range notifications {
		fmt.Printf("Achievement Unlocked: %s (+%d points)\n", 
			notification.Achievement.Name, notification.PointsEarned)
	}

	// Get player progress
	progress := am.GetPlayerProgress("player_1")
	fmt.Println("\nPlayer Progress:")
	for achievementID, prog := range progress {
		fmt.Printf("%s: %.1f%%\n", achievementID, prog*100)
	}

	// Get achievement leaderboard
	leaderboard := am.GetAchievementLeaderboard(10)
	fmt.Println("\nAchievement Leaderboard:")
	for i, entry := range leaderboard {
		fmt.Printf("%d. Player %s: %d points (%d unlocked)\n", 
			entry.Rank, entry.PlayerID, entry.TotalPoints, entry.UnlockedCount)
	}

	fmt.Println("\nAchievement Manager initialized successfully!")
}
