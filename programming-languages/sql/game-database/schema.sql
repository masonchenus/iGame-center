-- Game Center Database Schema
-- Comprehensive database design for game management, player data, and statistics

-- Create database
CREATE DATABASE IF NOT EXISTS game_center;
USE game_center;

-- Players table
CREATE TABLE players (
    player_id INT PRIMARY KEY AUTO_INCREMENT,
    username VARCHAR(50) UNIQUE NOT NULL,
    email VARCHAR(100) UNIQUE NOT NULL,
    password_hash VARCHAR(255) NOT NULL,
    display_name VARCHAR(50),
    avatar_url VARCHAR(255),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    last_login TIMESTAMP NULL,
    is_active BOOLEAN DEFAULT TRUE,
    level INT DEFAULT 1,
    experience_points BIGINT DEFAULT 0,
    total_games_played INT DEFAULT 0,
    total_wins INT DEFAULT 0,
    total_losses INT DEFAULT 0,
    total_draws INT DEFAULT 0,
    INDEX idx_username (username),
    INDEX idx_email (email),
    INDEX idx_level (level)
);

-- Games table
CREATE TABLE games (
    game_id INT PRIMARY KEY AUTO_INCREMENT,
    game_name VARCHAR(100) NOT NULL,
    game_type ENUM('puzzle', 'action', 'strategy', 'simulation', 'sports') NOT NULL,
    description TEXT,
    max_players INT DEFAULT 1,
    min_players INT DEFAULT 1,
    is_active BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    INDEX idx_game_type (game_type),
    INDEX idx_active (is_active)
);

-- Game sessions table
CREATE TABLE game_sessions (
    session_id INT PRIMARY KEY AUTO_INCREMENT,
    game_id INT NOT NULL,
    player_id INT NOT NULL,
    start_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    end_time TIMESTAMP NULL,
    duration_seconds INT NULL,
    score BIGINT DEFAULT 0,
    level_reached INT DEFAULT 1,
    completed BOOLEAN DEFAULT FALSE,
    FOREIGN KEY (game_id) REFERENCES games(game_id) ON DELETE CASCADE,
    FOREIGN KEY (player_id) REFERENCES players(player_id) ON DELETE CASCADE,
    INDEX idx_game_player (game_id, player_id),
    INDEX idx_start_time (start_time),
    INDEX idx_score (score DESC)
);

-- Leaderboards table
CREATE TABLE leaderboards (
    leaderboard_id INT PRIMARY KEY AUTO_INCREMENT,
    game_id INT NOT NULL,
    leaderboard_name VARCHAR(100) NOT NULL,
    period ENUM('daily', 'weekly', 'monthly', 'all_time') NOT NULL,
    reset_date DATE,
    is_active BOOLEAN DEFAULT TRUE,
    FOREIGN KEY (game_id) REFERENCES games(game_id) ON DELETE CASCADE,
    INDEX idx_game_period (game_id, period),
    INDEX idx_active (is_active)
);

-- Leaderboard entries table
CREATE TABLE leaderboard_entries (
    entry_id INT PRIMARY KEY AUTO_INCREMENT,
    leaderboard_id INT NOT NULL,
    player_id INT NOT NULL,
    score BIGINT NOT NULL,
    rank INT NOT NULL,
    achieved_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (leaderboard_id) REFERENCES leaderboards(leaderboard_id) ON DELETE CASCADE,
    FOREIGN KEY (player_id) REFERENCES players(player_id) ON DELETE CASCADE,
    INDEX idx_leaderboard_rank (leaderboard_id, rank),
    INDEX idx_player_score (player_id, score DESC),
    UNIQUE KEY unique_player_leaderboard (leaderboard_id, player_id)
);

-- Achievements table
CREATE TABLE achievements (
    achievement_id INT PRIMARY KEY AUTO_INCREMENT,
    achievement_name VARCHAR(100) NOT NULL,
    description TEXT,
    icon_url VARCHAR(255),
    points_value INT DEFAULT 10,
    rarity ENUM('common', 'uncommon', 'rare', 'epic', 'legendary') DEFAULT 'common',
    is_active BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    INDEX idx_rarity (rarity),
    INDEX idx_active (is_active)
);

-- Player achievements table
CREATE TABLE player_achievements (
    player_achievement_id INT PRIMARY KEY AUTO_INCREMENT,
    player_id INT NOT NULL,
    achievement_id INT NOT NULL,
    unlocked_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    progress_percentage DECIMAL(5,2) DEFAULT 100.00,
    FOREIGN KEY (player_id) REFERENCES players(player_id) ON DELETE CASCADE,
    FOREIGN KEY (achievement_id) REFERENCES achievements(achievement_id) ON DELETE CASCADE,
    INDEX idx_player_achievement (player_id, achievement_id),
    UNIQUE KEY unique_player_achievement (player_id, achievement_id)
);

-- Game statistics table
CREATE TABLE game_statistics (
    stat_id INT PRIMARY KEY AUTO_INCREMENT,
    game_id INT NOT NULL,
    stat_name VARCHAR(50) NOT NULL,
    stat_value_type ENUM('integer', 'decimal', 'boolean', 'string') DEFAULT 'integer',
    description TEXT,
    FOREIGN KEY (game_id) REFERENCES games(game_id) ON DELETE CASCADE,
    INDEX idx_game_stat (game_id, stat_name),
    UNIQUE KEY unique_game_stat (game_id, stat_name)
);

-- Player statistics table
CREATE TABLE player_statistics (
    player_stat_id INT PRIMARY KEY AUTO_INCREMENT,
    player_id INT NOT NULL,
    stat_id INT NOT NULL,
    stat_value VARCHAR(255) NOT NULL,
    last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    FOREIGN KEY (player_id) REFERENCES players(player_id) ON DELETE CASCADE,
    FOREIGN KEY (stat_id) REFERENCES game_statistics(stat_id) ON DELETE CASCADE,
    INDEX idx_player_stat (player_id, stat_id),
    UNIQUE KEY unique_player_stat (player_id, stat_id)
);

-- Friends table
CREATE TABLE friends (
    friendship_id INT PRIMARY KEY AUTO_INCREMENT,
    player1_id INT NOT NULL,
    player2_id INT NOT NULL,
    friendship_status ENUM('pending', 'accepted', 'blocked') DEFAULT 'pending',
    requested_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    accepted_at TIMESTAMP NULL,
    FOREIGN KEY (player1_id) REFERENCES players(player_id) ON DELETE CASCADE,
    FOREIGN KEY (player2_id) REFERENCES players(player_id) ON DELETE CASCADE,
    INDEX idx_player1_status (player1_id, friendship_status),
    INDEX idx_player2_status (player2_id, friendship_status),
    CHECK (player1_id < player2_id) -- Prevent duplicate friendships
);

-- Chat messages table
CREATE TABLE chat_messages (
    message_id INT PRIMARY KEY AUTO_INCREMENT,
    sender_id INT NOT NULL,
    receiver_id INT NULL, -- NULL for global messages
    game_session_id INT NULL,
    message_text TEXT NOT NULL,
    message_type ENUM('text', 'system', 'game_event') DEFAULT 'text',
    sent_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (sender_id) REFERENCES players(player_id) ON DELETE CASCADE,
    FOREIGN KEY (receiver_id) REFERENCES players(player_id) ON DELETE CASCADE,
    FOREIGN KEY (game_session_id) REFERENCES game_sessions(session_id) ON DELETE CASCADE,
    INDEX idx_sender_time (sender_id, sent_at),
    INDEX idx_receiver_time (receiver_id, sent_at),
    INDEX idx_session_time (game_session_id, sent_at)
);

-- Game assets table
CREATE TABLE game_assets (
    asset_id INT PRIMARY KEY AUTO_INCREMENT,
    game_id INT NOT NULL,
    asset_name VARCHAR(100) NOT NULL,
    asset_type ENUM('texture', 'sound', 'model', 'script', 'level') NOT NULL,
    file_path VARCHAR(255) NOT NULL,
    file_size BIGINT,
    uploaded_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (game_id) REFERENCES games(game_id) ON DELETE CASCADE,
    INDEX idx_game_type (game_id, asset_type),
    INDEX idx_asset_name (asset_name)
);

-- Create indexes for performance
CREATE INDEX idx_sessions_duration ON game_sessions(duration_seconds);
CREATE INDEX idx_sessions_completed ON game_sessions(completed);
CREATE INDEX idx_entries_achieved ON leaderboard_entries(achieved_at);
CREATE INDEX idx_achievements_unlocked ON player_achievements(unlocked_at);
CREATE INDEX idx_messages_type ON chat_messages(message_type);
CREATE INDEX idx_assets_uploaded ON game_assets(uploaded_at);

-- Insert sample data
INSERT INTO games (game_name, game_type, description, max_players, min_players) VALUES
('Space Invaders', 'action', 'Classic arcade shooter game', 1, 1),
('Tetris', 'puzzle', 'Block stacking puzzle game', 1, 1),
('Chess', 'strategy', 'Classic board game', 2, 2),
('Snake', 'action', 'Classic snake game', 1, 1),
('Memory Match', 'puzzle', 'Card matching memory game', 1, 1);

INSERT INTO achievements (achievement_name, description, points_value, rarity) VALUES
('First Win', 'Win your first game', 10, 'common'),
('Speed Demon', 'Complete a game in under 30 seconds', 50, 'rare'),
('Perfectionist', 'Complete a game with perfect score', 100, 'epic'),
('Social Butterfly', 'Add 10 friends', 25, 'uncommon'),
('Dedicated Player', 'Play for 100 hours total', 200, 'legendary');

-- Create views for common queries
CREATE VIEW player_rankings AS
SELECT
    p.player_id,
    p.username,
    p.display_name,
    p.level,
    p.experience_points,
    p.total_games_played,
    p.total_wins,
    ROUND((p.total_wins / GREATEST(p.total_games_played, 1)) * 100, 2) AS win_percentage,
    RANK() OVER (ORDER BY p.experience_points DESC) AS global_rank
FROM players p
WHERE p.is_active = TRUE;

CREATE VIEW game_popularity AS
SELECT
    g.game_name,
    g.game_type,
    COUNT(gs.session_id) AS total_sessions,
    AVG(gs.duration_seconds) AS avg_duration,
    MAX(gs.score) AS highest_score,
    COUNT(DISTINCT gs.player_id) AS unique_players
FROM games g
LEFT JOIN game_sessions gs ON g.game_id = gs.game_id
GROUP BY g.game_id, g.game_name, g.game_type
ORDER BY total_sessions DESC;

-- Create stored procedures
DELIMITER //

CREATE PROCEDURE update_player_stats(IN p_player_id INT, IN p_game_id INT, IN p_score BIGINT, IN p_completed BOOLEAN)
BEGIN
    DECLARE v_games_played INT DEFAULT 0;
    DECLARE v_wins INT DEFAULT 0;

    -- Update game session
    UPDATE game_sessions
    SET end_time = CURRENT_TIMESTAMP,
        duration_seconds = TIMESTAMPDIFF(SECOND, start_time, CURRENT_TIMESTAMP),
        score = p_score,
        completed = p_completed
    WHERE player_id = p_player_id AND game_id = p_game_id AND end_time IS NULL
    ORDER BY start_time DESC LIMIT 1;

    -- Update player totals
    SELECT total_games_played, total_wins INTO v_games_played, v_wins
    FROM players WHERE player_id = p_player_id;

    UPDATE players
    SET total_games_played = v_games_played + 1,
        total_wins = IF(p_completed, v_wins + 1, v_wins),
        experience_points = experience_points + IF(p_completed, p_score / 10, p_score / 20)
    WHERE player_id = p_player_id;
END //

CREATE PROCEDURE get_leaderboard(IN p_game_id INT, IN p_period VARCHAR(20), IN p_limit INT)
BEGIN
    SELECT
        le.rank,
        p.username,
        p.display_name,
        le.score,
        le.achieved_at
    FROM leaderboard_entries le
    JOIN leaderboards lb ON le.leaderboard_id = lb.leaderboard_id
    JOIN players p ON le.player_id = p.player_id
    WHERE lb.game_id = p_game_id
      AND lb.period = p_period
      AND lb.is_active = TRUE
    ORDER BY le.rank ASC
    LIMIT p_limit;
END //

DELIMITER ;

-- Create triggers for automatic updates
DELIMITER //

CREATE TRIGGER update_game_updated_at
    BEFORE UPDATE ON games
    FOR EACH ROW
BEGIN
    SET NEW.updated_at = CURRENT_TIMESTAMP;
END //

CREATE TRIGGER check_friendship_uniqueness
    BEFORE INSERT ON friends
    FOR EACH ROW
BEGIN
    IF NEW.player1_id > NEW.player2_id THEN
        SET @temp = NEW.player1_id;
        SET NEW.player1_id = NEW.player2_id;
        SET NEW.player2_id = @temp;
    END IF;
END //

DELIMITER ;
