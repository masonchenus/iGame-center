/*
 * Game Statistics Analyzer - D Language Implementation
 * Advanced game statistics with D's powerful range and analysis features
 */

import std.stdio;
import std.algorithm;
import std.range;
import std.array;
import std.csv;
import std.stdio;
import std.typecons;
import std.conv;
import std.math;
import std.datetime;
import std.format;
import std.regex;

/**
 * Game event types
 */
enum GameEventType {
    GameStart,
    GameEnd,
    PlayerMove,
    PlayerAction,
    ScoreUpdate,
    LevelComplete,
    Achievement,
    Error
}

/**
 * Player statistics
 */
struct PlayerStats {
    string playerId;
    int gamesPlayed;
    int gamesWon;
    int totalScore;
    int bestScore;
    int totalTime;
    int levelReached;
    string[] achievements;
    
    @property {
        double winRate() const {
            return gamesPlayed > 0 ? cast(double)gamesWon / gamesPlayed : 0.0;
        }
        
        double averageScore() const {
            return gamesPlayed > 0 ? cast(double)totalScore / gamesPlayed : 0.0;
        }
        
        double averageTime() const {
            return gamesPlayed > 0 ? cast(double)totalTime / gamesPlayed : 0.0;
        }
    }
    
    void updateStats(int score, int time, int level) {
        gamesPlayed++;
        totalScore += score;
        totalTime += time;
        levelReached = max(levelReached, level);
        
        if (score > bestScore) {
            bestScore = score;
        }
    }
}

/**
 * Game session data
 */
struct GameSession {
    string sessionId;
    string playerId;
    SysTime startTime;
    SysTime endTime;
    int finalScore;
    int levelReached;
    GameEventType[] events;
    
    @property {
        Duration duration() const {
            return endTime - startTime;
        }
        
        int timeInSeconds() const {
            return cast(int)duration.total!"seconds";
        }
    }
}

/**
 * Game event for detailed analysis
 */
struct GameEvent {
    string sessionId;
    string playerId;
    SysTime timestamp;
    GameEventType type;
    string data;
    int value;
}

/**
 * Statistics analyzer with D's powerful features
 */
class StatisticsAnalyzer {
    private {
        GameSession[] sessions;
        GameEvent[] events;
        PlayerStats[string] playerStats;
    }
    
    this() {
        // Initialize with sample data
        initializeSampleData();
    }
    
    /**
     * Load game data from CSV
     */
    void loadFromCSV(string filename) {
        import std.file : readText;
        
        try {
            auto content = readText(filename);
            auto records = content.parseCSV!(string, string, string, int, int, string);
            
            foreach (record; records) {
                auto session = GameSession(
                    record.field0, // sessionId
                    record.field1, // playerId
                    SysTime.fromISOExtString(record.field2), // startTime
                    SysTime.fromISOExtString(record.field3), // endTime
                    record.field4, // finalScore
                    record.field5, // levelReached
                    [] // events
                );
                sessions ~= session;
            }
            
            calculatePlayerStatistics();
            writeln("Loaded ", sessions.length, " game sessions from ", filename);
        } catch (Exception e) {
            writeln("Error loading CSV: ", e.msg);
        }
    }
    
    /**
     * Calculate comprehensive player statistics
     */
    void calculatePlayerStatistics() {
        playerStats = null;
        
        foreach (session; sessions) {
            if (session.playerId !in playerStats) {
                playerStats[session.playerId] = PlayerStats(session.playerId);
            }
            
            auto stats = playerStats[session.playerId];
            stats.updateStats(session.finalScore, session.timeInSeconds(), session.levelReached);
            playerStats[session.playerId] = stats;
        }
    }
    
    /**
     * Get top players by various metrics
     */
    auto getTopPlayers(string metric, int count = 10) {
        final switch (metric) {
            case "gamesWon":
                return playerStats.byValue()
                    .array
                    .sort!((a, b) => a.gamesWon > b.gamesWon)
                    .take(count);
                    
            case "winRate":
                return playerStats.byValue()
                    .filter!(stats => stats.gamesPlayed >= 5) // Minimum games
                    .array
                    .sort!((a, b) => a.winRate > b.winRate)
                    .take(count);
                    
            case "averageScore":
                return playerStats.byValue()
                    .filter!(stats => stats.gamesPlayed >= 3)
                    .array
                    .sort!((a, b) => a.averageScore > b.averageScore)
                    .take(count);
                    
            case "bestScore":
                return playerStats.byValue()
                    .array
                    .sort!((a, b) => a.bestScore > b.bestScore)
                    .take(count);
                    
            case "totalTime":
                return playerStats.byValue()
                    .array
                    .sort!((a, b) => a.totalTime > b.totalTime)
                    .take(count);
        }
    }
    
    /**
     * Analyze game performance patterns
     */
    void analyzePerformancePatterns() {
        writeln("\n=== GAME PERFORMANCE ANALYSIS ===");
        
        // Score distribution analysis
        auto scores = sessions.map!(s => s.finalScore).array;
        auto sortedScores = scores.sort();
        
        writeln("Score Statistics:");
        writeln("  Mean: ", sortedScores.reduce!"a + b" / cast(double)sortedScores.length);
        writeln("  Median: ", sortedScores[$ / 2]);
        writeln("  Min: ", sortedScores[0]);
        writeln("  Max: ", sortedScores[$ - 1]);
        writeln("  Standard Deviation: ", calculateStandardDeviation(scores));
        
        // Time analysis
        auto times = sessions.map!(s => s.timeInSeconds()).array;
        auto avgTime = times.reduce!"a + b" / cast(double)times.length;
        writeln("\nTime Analysis:");
        writeln("  Average game duration: ", avgTime, " seconds");
        writeln("  Shortest game: ", times.minElement, " seconds");
        writeln("  Longest game: ", times.maxElement, " seconds");
        
        // Level progression analysis
        auto levels = sessions.map!(s => s.levelReached).array;
        auto avgLevel = levels.reduce!"a + b" / cast(double)levels.length;
        writeln("\nLevel Progression:");
        writeln("  Average level reached: ", avgLevel);
        writeln("  Highest level: ", levels.maxElement);
        writeln("  Level 1 completion rate: ", 
                cast(double)sessions.filter!(s => s.levelReached >= 1).count / sessions.length * 100, "%");
    }
    
    /**
     * Identify player behavior patterns
     */
    void analyzePlayerBehavior() {
        writeln("\n=== PLAYER BEHAVIOR ANALYSIS ===");
        
        // Player engagement patterns
        auto playerEngagement = playerStats.byValue()
            .map!(stats => tuple(stats.playerId, stats.gamesPlayed, stats.averageTime))
            .array;
        
        writeln("Player Engagement Tiers:");
        writeln("  High Engagement (20+ games): ", 
                playerEngagement.filter!(t => t[1] >= 20).count);
        writeln("  Medium Engagement (5-19 games): ", 
                playerEngagement.filter!(t => t[1] >= 5 && t[1] < 20).count);
        writeln("  Low Engagement (1-4 games): ", 
                playerEngagement.filter!(t => t[1] >= 1 && t[1] < 5).count);
        
        // Performance vs Engagement correlation
        auto correlation = calculateCorrelation(
            playerEngagement.map!(t => t[1]).array, // games played
            playerEngagement.map!(t => t[2]).array  // average time
        );
        writeln("  Games vs Time Correlation: ", correlation);
    }
    
    /**
     * Generate detailed player report
     */
    void generatePlayerReport(string playerId) {
        if (playerId !in playerStats) {
            writeln("Player ", playerId, " not found!");
            return;
        }
        
        auto stats = playerStats[playerId];
        auto playerSessions = sessions.filter!(s => s.playerId == playerId).array;
        
        writeln("\n=== PLAYER REPORT: ", playerId, " ===");
        writeln("Games Played: ", stats.gamesPlayed);
        writeln("Games Won: ", stats.gamesWon);
        writeln("Win Rate: ", format("%.1f%%", stats.winRate * 100));
        writeln("Total Score: ", stats.totalScore);
        writeln("Best Score: ", stats.bestScore);
        writeln("Average Score: ", format("%.1f", stats.averageScore));
        writeln("Average Game Time: ", format("%.1f", stats.averageTime), " seconds");
        writeln("Highest Level: ", stats.levelReached);
        writeln("Total Play Time: ", stats.totalTime, " seconds");
        
        if (playerSessions.length > 0) {
            writeln("\nRecent Performance:");
            auto recent = playerSessions
                .sort!"a.endTime > b.endTime"
                .take(5);
            
            foreach (session; recent) {
                writeln("  ", session.endTime.toISOExtString()[0..10], 
                       " - Score: ", session.finalScore, 
                       " - Level: ", session.levelReached);
            }
        }
    }
    
    /**
     * Export analysis results
     */
    void exportAnalysis(string filename) {
        import std.file : write;
        
        auto output = appender!string();
        output.put("=== GAME STATISTICS ANALYSIS ===\n\n");
        
        output.put("Overall Statistics:\n");
        output.put("Total Sessions: " ~ to!string(sessions.length) ~ "\n");
        output.put("Total Players: " ~ to!string(playerStats.length) ~ "\n");
        output.put("Average Score: " ~ to!string(sessions.map!(s => s.finalScore).reduce!"a + b" / cast(double)sessions.length) ~ "\n");
        output.put("Average Game Time: " ~ to!string(sessions.map!(s => s.timeInSeconds()).reduce!"a + b" / cast(double)sessions.length) ~ " seconds\n\n");
        
        output.put("Top Players by Games Won:\n");
        foreach (i, stats; getTopPlayers("gamesWon", 5).enumerate) {
            output.put(to!string(i + 1) ~ ". " ~ stats.playerId ~ " - " ~ to!string(stats.gamesWon) ~ " wins\n");
        }
        
        write(filename, output.data);
        writeln("Analysis exported to ", filename);
    }
    
    /**
     * Utility functions
     */
    private {
        double calculateStandardDeviation(int[] values) {
            if (values.length == 0) return 0;
            
            auto mean = values.reduce!"a + b" / cast(double)values.length;
            auto variance = values.map!(x => (x - mean)^^2).reduce!"a + b" / values.length;
            return sqrt(variance);
        }
        
        double calculateCorrelation(int[] x, int[] y) {
            if (x.length != y.length || x.length == 0) return 0;
            
            auto n = cast(double)x.length;
            auto sumX = x.reduce!"a + b";
            auto sumY = y.reduce!"a + b";
            auto sumXY = zip(x, y).map!(t => t[0] * t[1]).reduce!"a + b";
            auto sumX2 = x.map!(x => x^^2).reduce!"a + b";
            auto sumY2 = y.map!(y => y^^2).reduce!"a + b";
            
            auto numerator = n * sumXY - sumX * sumY;
            auto denominator = sqrt((n * sumX2 - sumX^^2) * (n * sumY2 - sumY^^2));
            
            return denominator != 0 ? numerator / denominator : 0;
        }
        
        void initializeSampleData() {
            // Create sample game sessions
            auto sampleSessions = [
                GameSession("session1", "player1", SysTime(2024, 1, 1, 10, 0), 
                           SysTime(2024, 1, 1, 10, 15), 1500, 5),
                GameSession("session2", "player2", SysTime(2024, 1, 1, 11, 0), 
                           SysTime(2024, 1, 1, 11, 20), 2200, 7),
                GameSession("session3", "player1", SysTime(2024, 1, 1, 12, 0), 
                           SysTime(2024, 1, 1, 12, 10), 1800, 6),
                GameSession("session4", "player3", SysTime(2024, 1, 1, 13, 0), 
                           SysTime(2024, 1, 1, 13, 25), 3200, 8),
                GameSession("session5", "player2", SysTime(2024, 1, 1, 14, 0), 
                           SysTime(2024, 1, 1, 14, 12), 1950, 6)
            ];
            
            sessions = sampleSessions;
            calculatePlayerStatistics();
        }
    }
}

/**
 * Real-time analytics dashboard
 */
class AnalyticsDashboard {
    private {
        StatisticsAnalyzer analyzer;
        SysTime lastUpdate;
    }
    
    this() {
        analyzer = new StatisticsAnalyzer();
        lastUpdate = Clock.currTime();
    }
    
    /**
     * Update dashboard with new data
     */
    void updateDashboard() {
        auto currentTime = Clock.currTime();
        auto timeDiff = currentTime - lastUpdate;
        
        writeln("\n=== REAL-TIME ANALYTICS DASHBOARD ===");
        writeln("Last Update: ", lastUpdate.toISOExtString());
        writeln("Current Time: ", currentTime.toISOExtString());
        writeln("Update Interval: ", timeDiff);
        
        // Display current metrics
        auto activePlayers = playerStatsCount();
        auto totalSessions = analyzer.sessions.length;
        auto avgScore = analyzer.sessions.map!(s => s.finalScore).reduce!"a + b" / cast(double)totalSessions;
        
        writeln("\nCurrent Metrics:");
        writeln("  Active Players: ", activePlayers);
        writeln("  Total Sessions: ", totalSessions);
        writeln("  Average Score: ", avgScore);
        
        // Performance alerts
        checkPerformanceAlerts();
        
        lastUpdate = currentTime;
    }
    
    /**
     * Generate performance alerts
     */
    void checkPerformanceAlerts() {
        auto scores = analyzer.sessions.map!(s => s.finalScore).array;
        auto avgScore = scores.reduce!"a + b" / cast(double)scores.length;
        var minScore = scores.minElement;
        var maxScore = scores.maxElement;
        
        writeln("\nPerformance Alerts:");
        
        if (maxScore > avgScore * 2) {
            writeln("  HIGH SCORE DETECTED: ", maxScore, " (", maxScore / avgScore, "x average)");
        }
        
        if (minScore < avgScore * 0.5) {
            writeln("  LOW PERFORMANCE: Score of ", minScore, " significantly below average");
        }
    }
    
    private int playerStatsCount() {
        return cast(int)analyzer.playerStats.length;
    }
}

/**
 * Main demonstration program
 */
void main() {
    writeln("=== D Language Game Statistics Analyzer ===\n");
    
    auto analyzer = new StatisticsAnalyzer();
    
    // Perform comprehensive analysis
    analyzer.analyzePerformancePatterns();
    analyzer.analyzePlayerBehavior();
    
    // Generate reports for top players
    writeln("\n=== TOP PLAYERS ANALYSIS ===");
    writeln("By Games Won:");
    foreach (i, stats; analyzer.getTopPlayers("gamesWon", 3).enumerate) {
        writeln(i + 1, ". ", stats.playerId, " - ", stats.gamesWon, " wins");
    }
    
    writeln("\nBy Win Rate (min 5 games):");
    foreach (i, stats; analyzer.getTopPlayers("winRate", 3).enumerate) {
        writeln(i + 1, ". ", stats.playerId, " - ", format("%.1f%%", stats.winRate * 100));
    }
    
    // Generate detailed player report
    if (analyzer.playerStats.length > 0) {
        auto firstPlayer = analyzer.playerStats.byKey().front;
        analyzer.generatePlayerReport(firstPlayer);
    }
    
    // Export analysis
    analyzer.exportAnalysis("game_analysis.txt");
    
    // Real-time dashboard demo
    writeln("\n=== REAL-TIME DASHBOARD ===");
    auto dashboard = new AnalyticsDashboard();
    dashboard.updateDashboard();
    
    writeln("\n=== D Language Analytics Features ===");
    writeln("✓ Range-based data processing");
    writeln("✓ Compile-time and runtime analytics");
    writeln("✓ Statistical calculations and correlations");
    writeln("✓ Real-time performance monitoring");
    writeln("✓ Pattern recognition and alerting");
    writeln("✓ High-performance data aggregation");
    writeln("✓ Type-safe statistical operations");
    
    writeln("\nStatistics analyzer demo completed!");
}
