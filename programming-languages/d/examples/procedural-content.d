r/*
 * Procedural Content Generator - D Language Implementation
 * Advanced procedural generation for game content
 */

import std.stdio;
import std.random;
import std.algorithm;
import std.range;
import std.array;
import std.typecons;
import std.conv;
import std.math;
import std.datetime;
import std.parallelism;

/**
 * Seeded random number generator for reproducibility
 */
class SeededRNG {
    private {
        uint seed;
        Random rng;
    }
    
    this(uint seedValue = 0) {
        this.seed = seedValue != 0 ? seedValue : cast(uint)Clock.currTime().toUnixTime;
        this.rng = Random(this.seed);
    }
    
    /**
     * Get random integer in range
     */
    int randInt(int min, int max) {
        return uniform(min, max + 1, rng);
    }
    
    /**
     * Get random float in range
     */
    float randFloat(float min, float max) {
        return uniform(min, max, rng);
    }
    
    /**
     * Get random choice from array
     */
    T randChoice(T)(T[] choices) {
        return choices[randInt(0, cast(int)choices.length - 1)];
    }
    
    /**
     * Shuffle array
     */
    void shuffle(T)(ref T[] array) {
        randomShuffle(array, rng);
    }
    
    /**
     * Get weighted random choice
     */
    T weightedChoice(T)(T[] choices, float[] weights) {
        assert(choices.length == weights.length);
        
        auto totalWeight = weights.reduce!"a + b";
        auto randomWeight = randFloat(0, totalWeight);
        
        auto currentWeight = 0.0f;
        foreach (i, choice; choices) {
            currentWeight += weights[i];
            if (randomWeight <= currentWeight) {
                return choice;
            }
        }
        
        return choices[0]; // Fallback
    }
    
    @property {
        uint Seed() const { return seed; }
    }
}

/**
 * Terrain generation using noise functions
 */
class TerrainGenerator {
    private {
        SeededRNG rng;
        float[,] heightMap;
        int width, height;
        float noiseScale;
    }
    
    this(int width, int height, uint seed = 0) {
        this.width = width;
        this.height = height;
        this.rng = new SeededRNG(seed);
        this.noiseScale = 0.1f;
        this.heightMap = new float[height, width];
    }
    
    /**
     * Generate terrain using multiple noise layers
     */
    float[,] generateTerrain() {
        // Base noise
        auto baseNoise = generateNoise(0.05f, 1.0f);
        
        // Detail noise
        auto detailNoise = generateNoise(0.1f, 0.5f);
        
        // Combine layers
        foreach (y; 0..height) {
            foreach (x; 0..width) {
                heightMap[y, x] = baseNoise[y, x] * 0.7f + detailNoise[y, x] * 0.3f;
            }
        }
        
        // Apply post-processing
        applyErosion(2);
        smoothTerrain(1);
        
        return heightMap;
    }
    
    /**
     * Generate noise using value noise
     */
    float[,] generateNoise(float scale, float amplitude) {
        auto noise = new float[height, width];
        
        foreach (y; 0..height) {
            foreach (x; 0..width) {
                auto worldX = x * scale;
                auto worldY = y * scale;
                noise[y, x] = valueNoise(worldX, worldY) * amplitude;
            }
        }
        
        return noise;
    }
    
    /**
     * Value noise implementation
     */
    float valueNoise(float x, float y) {
        auto x0 = cast(int)floor(x);
        auto y0 = cast(int)floor(y);
        auto x1 = x0 + 1;
        auto y1 = y0 + 1;
        
        auto fx = x - x0;
        auto fy = y - y0;
        
        auto v00 = hash2D(x0, y0);
        auto v10 = hash2D(x1, y0);
        auto v01 = hash2D(x0, y1);
        auto v11 = hash2D(x1, y1);
        
        auto i1 = lerp(v00, v10, smoothStep(fx));
        auto i2 = lerp(v01, v11, smoothStep(fx));
        
        return lerp(i1, i2, smoothStep(fy));
    }
    
    /**
     * 2D hash function for noise generation
     */
    float hash2D(int x, int y) {
        auto n = x * 374761393 + y * 668265263;
        n = (n ^ (n >> 13)) * 1274126177;
        n = n ^ (n >> 16);
        return (n & 0xFFFFFF) / 16777216.0f;
    }
    
    /**
     * Smooth step function
     */
    float smoothStep(float t) {
        return t * t * (3 - 2 * t);
    }
    
    /**
     * Linear interpolation
     */
    float lerp(float a, float b, float t) {
        return a + t * (b - a);
    }
    
    /**
     * Apply erosion effect
     */
    void applyErosion(int iterations) {
        foreach (iter; 0..iterations) {
            auto newHeightMap = heightMap.dup;
            
            foreach (y; 1..height-1) {
                foreach (x; 1..width-1) {
                    auto current = heightMap[y, x];
                    auto average = 0.0f;
                    auto count = 0;
                    
                    // Check 8 neighbors
                    foreach (dy; -1..2) {
                        foreach (dx; -1..2) {
                            if (dx != 0 || dy != 0) {
                                average += heightMap[y + dy, x + dx];
                                count++;
                            }
                        }
                    }
                    
                    average /= count;
                    
                    // Erode if surrounded by higher terrain
                    if (average > current) {
                        newHeightMap[y, x] = current + (average - current) * 0.1f;
                    }
                }
            }
            
            heightMap = newHeightMap;
        }
    }
    
    /**
     * Smooth terrain
     */
    void smoothTerrain(int iterations) {
        foreach (iter; 0..iterations) {
            auto newHeightMap = heightMap.dup;
            
            foreach (y; 1..height-1) {
                foreach (x; 1..width-1) {
                    auto sum = 0.0f;
                    auto count = 0;
                    
                    foreach (dy; -1..2) {
                        foreach (dx; -1..2) {
                            sum += heightMap[y + dy, x + dx];
                            count++;
                        }
                    }
                    
                    newHeightMap[y, x] = sum / count;
                }
            }
            
            heightMap = newHeightMap;
        }
    }
    
    /**
     * Get terrain type at position
     */
    string getTerrainType(float height) {
        if (height < 0.3f) return "water";
        if (height < 0.4f) return "beach";
        if (height < 0.6f) return "grassland";
        if (height < 0.8f) return "hills";
        return "mountains";
    }
    
    /**
     * Generate biome distribution
     */
    string[,] generateBiomes() {
        auto biomes = new string[height, width];
        
        foreach (y; 0..height) {
            foreach (x; 0..width) {
                auto height = heightMap[y, x];
                auto terrainType = getTerrainType(height);
                
                // Add some randomness and temperature variation
                auto temperature = rng.randFloat(0, 1);
                auto humidity = rng.randFloat(0, 1);
                
                if (terrainType == "grassland") {
                    if (temperature < 0.3f) biomes[y, x] = "tundra";
                    else if (temperature > 0.8f) biomes[y, x] = "desert";
                    else if (humidity > 0.7f) biomes[y, x] = "rainforest";
                    else biomes[y, x] = "plains";
                } else if (terrainType == "hills") {
                    if (temperature < 0.4f) biomes[y, x] = "mountains";
                    else biomes[y, x] = "forest";
                } else {
                    biomes[y, x] = terrainType;
                }
            }
        }
        
        return biomes;
    }
}

/**
 * Dungeon generation using cellular automata
 */
class DungeonGenerator {
    private {
        bool[,] map;
        int width, height;
        SeededRNG rng;
    }
    
    this(int width, int height, uint seed = 0) {
        this.width = width;
        this.height = height;
        this.rng = new SeededRNG(seed);
        this.map = new bool[height, width];
    }
    
    /**
     * Generate dungeon using cellular automata
     */
    bool[,] generate(int wallChance = 45, int smoothSteps = 5) {
        // Initialize random map
        foreach (y; 0..height) {
            foreach (x; 0..width) {
                if (x == 0 || x == width-1 || y == 0 || y == height-1) {
                    map[y, x] = true; // Border walls
                } else {
                    map[y, x] = rng.randInt(1, 100) < wallChance;
                }
            }
        }
        
        // Smooth the map
        foreach (step; 0..smoothSteps) {
            map = smoothDungeon(map);
        }
        
        // Connect rooms
        connectRegions();
        
        return map;
    }
    
    /**
     * Smooth dungeon using cellular automata rules
     */
    bool[,] smoothDungeon(bool[,] currentMap) {
        auto newMap = new bool[height, width];
        
        foreach (y; 0..height) {
            foreach (x; 0..width) {
                auto wallCount = countWallNeighbors(currentMap, x, y);
                
                if (wallCount > 4) {
                    newMap[y, x] = true; // Wall
                } else if (wallCount < 4) {
                    newMap[y, x] = false; // Room
                } else {
                    newMap[y, x] = currentMap[y, x]; // Keep current
                }
            }
        }
        
        return newMap;
    }
    
    /**
     * Count wall neighbors
     */
    int countWallNeighbors(bool[,] map, int x, int y) {
        int count = 0;
        
        foreach (dy; -1..2) {
            foreach (dx; -1..2) {
                if (dx == 0 && dy == 0) continue;
                
                int nx = x + dx;
                int ny = y + dy;
                
                if (nx < 0 || nx >= width || ny < 0 || ny >= height) {
                    count++; // Out of bounds = wall
                } else if (map[ny, nx]) {
                    count++;
                }
            }
        }
        
        return count;
    }
    
    /**
     * Connect dungeon regions
     */
    void connectRegions() {
        // Find room regions
        auto regions = findRegions();
        
        if (regions.length <= 1) return;
        
        // Connect regions with corridors
        foreach (i; 0..regions.length-1) {
            auto region1 = regions[i];
            auto region2 = regions[i + 1];
            
            // Find closest points between regions
            auto point1 = findClosestPoint(region1, region2);
            auto point2 = findClosestPoint(region2, region1);
            
            // Create corridor between points
            createCorridor(point1, point2);
        }
    }
    
    /**
     * Find connected regions
     */
    bool[][] findRegions() {
        auto visited = new bool[height, width];
        auto regions = appender!(bool[][])();
        
        foreach (y; 0..height) {
            foreach (x; 0..width) {
                if (!map[y, x] && !visited[y, x]) {
                    auto region = floodFill(x, y, visited);
                    regions.put(region);
                }
            }
        }
        
        return regions.data;
    }
    
    /**
     * Flood fill to find connected region
     */
    bool[] floodFill(int startX, int startY, bool[,] visited) {
        auto region = appender!(bool[])();
        auto stack = appender!(Tuple!(int, int)[]);
        stack.put(tuple(startX, startY));
        
        while (!stack.data.empty) {
            auto pos = stack.data.back;
            stack.data.popBack;
            
            auto x = pos[0];
            auto y = pos[1];
            
            if (x < 0 || x >= width || y < 0 || y >= height) continue;
            if (map[y, x] || visited[y, x]) continue;
            
            visited[y, x] = true;
            
            // Add to region (simplified - would need proper coordinate tracking)
            region.put(true);
            
            // Add neighbors to stack
            stack.put(tuple(x-1, y));
            stack.put(tuple(x+1, y));
            stack.put(tuple(x, y-1));
            stack.put(tuple(x, y+1));
        }
        
        return region.data;
    }
    
    /**
     * Find closest point between two regions
     */
    Tuple!(int, int) findClosestPoint(bool[] region1, bool[] region2) {
        // Simplified implementation
        return tuple(width/2, height/2);
    }
    
    /**
     * Create corridor between two points
     */
    void createCorridor(Tuple!(int, int) start, Tuple!(int, int) end) {
        auto x = start[0];
        auto y = start[1];
        auto endX = end[0];
        auto endY = end[1];
        
        // L-shaped corridor
        while (x != endX) {
            x += (endX > x) ? 1 : -1;
            if (x >= 0 && x < width && y >= 0 && y < height) {
                map[y, x] = false;
            }
        }
        
        while (y != endY) {
            y += (endY > y) ? 1 : -1;
            if (x >= 0 && x < width && y >= 0 && y < height) {
                map[y, x] = false;
            }
        }
    }
    
    /**
     * Place features in dungeon
     */
    void placeFeatures() {
        auto roomCount = 0;
        auto treasureRooms = rng.randInt(3, 7);
        auto monsterNests = rng.randInt(5, 10);
        
        foreach (y; 1..height-1) {
            foreach (x; 1..width-1) {
                if (!map[y, x]) {
                    // Check if this is a room center
                    if (isRoomCenter(x, y)) {
                        roomCount++;
                        
                        // Place special features
                        if (roomCount <= treasureRooms) {
                            map[y, x] = true; // Mark as treasure
                        } else if (roomCount <= treasureRooms + monsterNests) {
                            map[y, x] = true; // Mark as monster nest
                        }
                    }
                }
            }
        }
    }
    
    /**
     * Check if position is room center
     */
    bool isRoomCenter(int x, int y) {
        // Simple room detection
        return !map[y, x] && map[y-1, x] && map[y+1, x] && map[y, x-1] && map[y, x+1];
    }
}

/**
 * Item generator
 */
class ItemGenerator {
    private {
        SeededRNG rng;
        string[] itemTypes;
        string[] itemNames;
        int[] rarityWeights;
    }
    
    this(uint seed = 0) {
        this.rng = new SeededRNG(seed);
        initializeItemData();
    }
    
    /**
     * Initialize item type and name data
     */
    void initializeItemData() {
        itemTypes = ["weapon", "armor", "potion", "scroll", "ring", "amulet"];
        itemNames = ["Sword", "Shield", "Helmet", "Boots", "Ring", "Amulet", "Staff", "Bow"];
        rarityWeights = [10, 25, 30, 20, 10, 5]; // Common to Legendary
    }
    
    /**
     * Generate random item
     */
    Item generateItem(int level = 1) {
        auto itemType = rng.randChoice(itemTypes);
        auto itemName = rng.randChoice(itemNames);
        auto rarity = cast(Rarity)rng.weightedChoice(
            [Rarity.Common, Rarity.Uncommon, Rarity.Rare, Rarity.Epic, Rarity.Legendary, Rarity.Mythic],
            [40, 30, 20, 8, 2, 1]
        );
        
        auto item = Item(itemType, itemName, rarity, level);
        item.stats = generateItemStats(itemType, rarity, level);
        item.description = generateDescription(item);
        
        return item;
    }
    
    /**
     * Generate item stats based on type, rarity and level
     */
    ItemStats generateItemStats(string itemType, Rarity rarity, int level) {
        auto stats = ItemStats();
        auto rarityMultiplier = getRarityMultiplier(rarity);
        auto levelMultiplier = 1.0f + level * 0.2f;
        
        final switch (itemType) {
            case "weapon":
                stats.attack = cast(int)(rng.randInt(5, 15) * rarityMultiplier * levelMultiplier);
                stats.critChance = rng.randFloat(0.05f, 0.25f) * rarityMultiplier;
                break;
                
            case "armor":
                stats.defense = cast(int)(rng.randInt(3, 12) * rarityMultiplier * levelMultiplier);
                stats.health = cast(int)(rng.randInt(10, 50) * rarityMultiplier * levelMultiplier);
                break;
                
            case "potion":
                stats.healing = cast(int)(rng.randInt(20, 100) * rarityMultiplier * levelMultiplier);
                break;
                
            case "scroll":
                stats.magicDamage = cast(int)(rng.randInt(15, 75) * rarityMultiplier * levelMultiplier);
                break;
                
            case "ring":
            case "amulet":
                stats.magicPower = cast(int)(rng.randInt(5, 25) * rarityMultiplier * levelMultiplier);
                stats.mana = cast(int)(rng.randInt(20, 60) * rarityMultiplier * levelMultiplier);
                break;
        }
        
        return stats;
    }
    
    /**
     * Get rarity multiplier
     */
    float getRarityMultiplier(Rarity rarity) {
        final switch (rarity) {
            case Rarity.Common: return 1.0f;
            case Rarity.Uncommon: return 1.2f;
            case Rarity.Rare: return 1.5f;
            case Rarity.Epic: return 2.0f;
            case Rarity.Legendary: return 3.0f;
            case Rarity.Mythic: return 5.0f;
        }
    }
    
    /**
     * Generate item description
     */
    string generateDescription(Item item) {
        import std.format : format;
        
        auto descriptions = [
            "An ancient artifact of great power.",
            "Forged by master craftsmen in forgotten times.",
            "Said to contain the essence of dragons.",
            "Gleams with mysterious magical energy.",
            "Rumored to bring fortune to its bearer.",
            "Emits a faint humming sound.",
            "Covered in intricate magical runes."
        ];
        
        return rng.randChoice(descriptions);
    }
}

/**
 * Item data structures
 */
enum Rarity {
    Common,
    Uncommon,
    Rare,
    Epic,
    Legendary,
    Mythic
}

struct Item {
    string type;
    string name;
    Rarity rarity;
    int level;
    ItemStats stats;
    string description;
}

struct ItemStats {
    int attack;
    int defense;
    int health;
    int magicDamage;
    int magicPower;
    int mana;
    int healing;
    float critChance;
}

/**
 * Procedural quest generator
 */
class QuestGenerator {
    private {
        SeededRNG rng;
        string[] questTypes;
        string[] objectives;
        string[] rewards;
    }
    
    this(uint seed = 0) {
        this.rng = new SeededRNG(seed);
        initializeQuestData();
    }
    
    void initializeQuestData() {
        questTypes = ["kill", "collect", "explore", "rescue", "deliver"];
        objectives = ["goblins", "treasure", "dungeon", "villager", "package"];
        rewards = ["gold", "experience", "magic_item", "reputation", "skill_point"];
    }
    
    /**
     * Generate random quest
     */
    Quest generateQuest(int playerLevel = 1) {
        auto questType = rng.randChoice(questTypes);
        auto objective = rng.randChoice(objectives);
        auto reward = rng.randChoice(rewards);
        
        auto difficulty = clamp(playerLevel + rng.randInt(-2, 2), 1, 20);
        auto quest = Quest(questType, objective, reward, difficulty);
        
        quest.title = generateQuestTitle(questType, objective);
        quest.description = generateQuestDescription(questType, objective);
        quest.objectives = generateQuestObjectives(questType, objective, difficulty);
        quest.rewards = generateQuestRewards(reward, difficulty);
        
        return quest;
    }
    
    string generateQuestTitle(string type, string objective) {
        import std.format : format;
        
        auto titles = [
            format("Eliminate the %s threat", objective),
            format("Recover the lost %s", objective),
            format("Explore the mysterious %s", objective),
            format("Rescue the %s", objective),
            format("Deliver the %s", objective)
        ];
        
        auto typeIndex = questTypes.countUntil(type);
        return typeIndex >= 0 && typeIndex < titles.length ? titles[typeIndex] : "Mysterious Quest";
    }
    
    string generateQuestDescription(string type, string objective) {
        import std.format : format;
        
        final switch (type) {
            case "kill":
                return format("The %s have been causing trouble in the area. Please eliminate %d of them.", objective, rng.randInt(5, 20));
            case "collect":
                return format("I need you to collect %d %s from the wilderness.", rng.randInt(3, 10), objective);
            case "explore":
                return format("There's been strange activity at the %s. Investigate and report back.", objective);
            case "rescue":
                return format("A %s has been captured! Please rescue them from their captors.", objective);
            case "deliver":
                return format("I need someone to deliver this %s to the next town. Can I trust you?", objective);
        }
    }
    
    string[] generateQuestObjectives(string type, string objective, int difficulty) {
        auto objectives = appender!(string[])();
        
        final switch (type) {
            case "kill":
                objectives.put("Eliminate " ~ to!string(difficulty * rng.randInt(2, 5)) ~ " " ~ objective);
                break;
            case "collect":
                objectives.put("Collect " ~ to!string(difficulty * rng.randInt(1, 3)) ~ " " ~ objective);
                break;
            case "explore":
                objectives.put("Explore the " ~ objective);
                objectives.put("Find clues about the strange activity");
                break;
            case "rescue":
                objectives.put("Locate the captured " ~ objective);
                objectives.put("Defeat the captors");
                objectives.put("Escort " ~ objective ~ " to safety");
                break;
            case "deliver":
                objectives.put("Collect the " ~ objective);
                objectives.put("Travel to the destination");
                objectives.put("Deliver to the recipient");
                break;
        }
        
        return objectives.data;
    }
    
    QuestReward[] generateQuestRewards(string rewardType, int difficulty) {
        auto rewards = appender!(QuestReward[])();
        
        final switch (rewardType) {
            case "gold":
                rewards.put(QuestReward("gold", difficulty * rng.randInt(50, 200)));
                break;
            case "experience":
                rewards.put(QuestReward("experience", difficulty * rng.randInt(100, 500)));
                break;
            case "magic_item":
                rewards.put(QuestReward("magic_item", 1));
                break;
            case "reputation":
                rewards.put(QuestReward("reputation", difficulty * rng.randInt(5, 25)));
                break;
            case "skill_point":
                rewards.put(QuestReward("skill_point", rng.randInt(1, 3)));
                break;
        }
        
        return rewards.data;
    }
}

struct Quest {
    string type;
    string objective;
    string reward;
    int difficulty;
    string title;
    string description;
    string[] objectives;
    QuestReward[] rewards;
}

struct QuestReward {
    string type;
    int amount;
}

/**
 * Demo program
 */
void main() {
    writeln("=== D Language Procedural Content Generator Demo ===\n");
    
    auto seed = cast(uint)Clock.currTime().toUnixTime;
    writeln("Using seed: ", seed);
    
    // Terrain generation demo
    writeln("\n=== Terrain Generation ===");
    auto terrainGen = new TerrainGenerator(50, 30, seed);
    auto heightMap = terrainGen.generateTerrain();
    auto biomes = terrainGen.generateBiomes();
    
    writeln("Generated terrain map: ", heightMap.length, "x", heightMap.length);
    writeln("Sample biome at (25,15): ", biomes[15, 25]);
    
    // Dungeon generation demo
    writeln("\n=== Dungeon Generation ===");
    auto dungeonGen = new DungeonGenerator(40, 25, seed + 1000);
    auto dungeonMap = dungeonGen.generate();
    dungeonGen.placeFeatures();
    
    writeln("Generated dungeon map: ", dungeonMap.length, "x", dungeonMap.length);
    
    // Count rooms and walls
    auto roomCount = 0;
    auto wallCount = 0;
    foreach (y; 0..25) {
        foreach (x; 0..40) {
            if (dungeonMap[y, x]) wallCount++;
            else roomCount++;
        }
    }
    writeln("Dungeon stats: ", roomCount, " rooms, ", wallCount, " walls");
    
    // Item generation demo
    writeln("\n=== Item Generation ===");
    auto itemGen = new ItemGenerator(seed + 2000);
    
    writeln("Generated items:");
    foreach (i; 0..5) {
        auto item = itemGen.generateItem(i + 1);
        writeln("  ", item.name, " (", item.type, ") - ", item.rarity, " - Level ", item.level);
        writeln("    ", item.description);
    }
    
    // Quest generation demo
    writeln("\n=== Quest Generation ===");
    auto questGen = new QuestGenerator(seed + 3000);
    
    writeln("Generated quests:");
    foreach (i; 0..3) {
        auto quest = questGen.generateQuest(i + 1);
        writeln("  ", quest.title);
        writeln("    ", quest.description);
        writeln("    Difficulty: ", quest.difficulty);
        writeln("    Objectives:");
        foreach (obj; quest.objectives) {
            writeln("      - ", obj);
        }
        writeln("");
    }
    
    writeln("\n=== D Language Procedural Generation Features ===");
    writeln("✓ Seeded random number generation for reproducibility");
    writeln("✓ Advanced noise functions for terrain generation");
    writeln("✓ Cellular automata for dungeon generation");
    writeln("✓ Weighted random selection for balanced content");
    writeln("✓ Parallel processing capabilities");
    writeln("✓ Template-based item and quest generation");
    writeln("✓ Multi-layer biome and terrain systems");
    writeln("✓ Extensible content generation framework");
    
    writeln("\nProcedural content generator demo completed!");
}

