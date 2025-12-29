package main

import (
	"fmt"
	"sync"
	"time"
)

// AudioManager handles all audio operations for the game
type AudioManager struct {
	sounds     map[string]*Sound
	music      map[string]*MusicTrack
	master     *AudioChannel
	sfx        *AudioChannel
	musicChan  *AudioChannel
	isMuted    bool
	volume     float32
	mutex      sync.RWMutex
}

// Sound represents a sound effect
type Sound struct {
	ID          string
	Name        string
	Data        []byte
	Format      AudioFormat
	Duration    time.Duration
	Volume      float32
	IsLooping   bool
	Is3D        bool
	Position    Vector3
	MaxDistance float32
	CreatedAt   time.Time
	hZ          int
}

// MusicTrack represents background music
type MusicTrack struct {
	ID          string
	Name        string
	Data        []byte
	Format      AudioFormat
	Duration    time.Duration
	Volume      float32
	IsLooping   bool
	FadeIn      bool
	FadeOut     bool
	CreatedAt   time.Time
}

// AudioChannel represents an audio channel with volume control
type AudioChannel struct {
	Name     string
	Volume   float32
	IsMuted  bool
	Effects  []AudioEffect
}

// AudioFormat represents audio format information
type AudioFormat struct {
	SampleRate int
	Channels   int
	BitDepth   int
	Encoding   string
}

// AudioEffect represents an audio effect
type AudioEffect struct {
	Type     EffectType
	Params   map[string]float32
	Enabled  bool
}

// Vector3 represents 3D position
type Vector3 struct {
	X, Y, Z float32
}

// AudioSettings contains audio manager configuration
type AudioSettings struct {
	MasterVolume float32
	SFXVolume    float32
	MusicVolume  float32
	Enable3D     bool
	SampleRate   int
	BufferSize   int
	MaxSounds    int
}

// PlaySoundRequest represents a request to play a sound
type PlaySoundRequest struct {
	SoundID     string
	Volume      float32
	Position    *Vector3
	Loop        bool
	Priority    int
	Callback    func()
}

// NewAudioManager creates a new audio manager
func NewAudioManager(settings AudioSettings) *AudioManager {
	return &AudioManager{
		sounds:   make(map[string]*Sound),
		music:    make(map[string]*MusicTrack),
		master:   &AudioChannel{Name: "master", Volume: settings.MasterVolume},
		sfx:      &AudioChannel{Name: "sfx", Volume: settings.SFXVolume},
		musicChan: &AudioChannel{Name: "music", Volume: settings.MusicVolume},
		volume:   settings.MasterVolume,
	}
}

// Start initializes the audio manager
func (am *AudioManager) Start() error {
	am.mutex.Lock()
	defer am.mutex.Unlock()

	// Initialize audio system
	// In a real implementation, this would initialize the audio library
	fmt.Println("Audio Manager started")

	return nil
}

// Stop stops the audio manager
func (am *AudioManager) Stop() {
	am.mutex.Lock()
	defer am.mutex.Unlock()

	// Stop all playing sounds
	am.StopAllSounds()
	am.StopAllMusic()

	fmt.Println("Audio Manager stopped")
}

// LoadSound loads a sound effect
func (am *AudioManager) LoadSound(sound *Sound) error {
	am.mutex.Lock()
	defer am.mutex.Unlock()

	if _, exists := am.sounds[sound.ID]; exists {
		return fmt.Errorf("sound already exists: %s", sound.ID)
	}

	// Validate sound data
	if len(sound.Data) == 0 {
		return fmt.Errorf("sound data is empty: %s", sound.ID)
	}

	// Set default values
	if sound.Volume == 0 {
		sound.Volume = 1.0
	}

	am.sounds[sound.ID] = sound
	fmt.Printf("Loaded sound: %s (%s)\n", sound.Name, sound.ID)

	return nil
}

// LoadMusic loads a music track
func (am *AudioManager) LoadMusic(track *MusicTrack) error {
	am.mutex.Lock()
	defer am.mutex.Unlock()

	if _, exists := am.music[track.ID]; exists {
		return fmt.Errorf("music already exists: %s", track.ID)
	}

	// Validate music data
	if len(track.Data) == 0 {
		return fmt.Errorf("music data is empty: %s", track.ID)
	}

	// Set default values
	if track.Volume == 0 {
		track.Volume = 0.8
	}

	am.music[track.ID] = track
	fmt.Printf("Loaded music: %s (%s)\n", track.Name, track.ID)

	return nil
}

// PlaySound plays a sound effect
func (am *AudioManager) PlaySound(request PlaySoundRequest) error {
	am.mutex.RLock()
	sound, exists := am.sounds[request.SoundID]
	am.mutex.RUnlock()

	if !exists {
		return fmt.Errorf("sound not found: %s", request.SoundID)
	}

	// Check if audio is muted
	if am.isMuted || am.sfx.IsMuted {
		return nil
	}

	// Calculate final volume
	volume := request.Volume * sound.Volume * am.sfx.Volume * am.master.Volume
	
	// In a real implementation, this would actually play the sound
	fmt.Printf("Playing sound: %s at volume %.2f\n", sound.Name, volume)
	
	if request.Position != nil && sound.Is3D {
		fmt.Printf("  3D Position: (%.1f, %.1f, %.1f)\n", request.Position.X, request.Position.Y, request.Position.Z)
	}

	// Execute callback if provided
	if request.Callback != nil {
		go request.Callback()
	}

	return nil
}

// PlayMusic plays background music
func (am *AudioManager) PlayMusic(musicID string, fadeIn bool) error {
	am.mutex.RLock()
	track, exists := am.music[musicID]
	am.mutex.RUnlock()

	if !exists {
		return fmt.Errorf("music not found: %s", musicID)
	}

	// Check if audio is muted
	if am.isMuted || am.musicChan.IsMuted {
		return nil
	}

	// Calculate final volume
	volume := track.Volume * am.musicChan.Volume * am.master.Volume
	
	// In a real implementation, this would actually play the music
	fmt.Printf("Playing music: %s at volume %.2f\n", track.Name, volume)
	
	if fadeIn {
		fmt.Println("  Fading in...")
	}

	return nil
}

// StopSound stops a specific sound
func (am *AudioManager) StopSound(soundID string) error {
	am.mutex.RLock()
	_, exists := am.sounds[soundID]
	am.mutex.RUnlock()

	if !exists {
		return fmt.Errorf("sound not found: %s", soundID)
	}

	// In a real implementation, this would stop the specific sound
	fmt.Printf("Stopped sound: %s\n", soundID)

	return nil
}

// StopMusic stops the current music
func (am *AudioManager) StopMusic(fadeOut bool) error {
	// In a real implementation, this would stop the current music
	if fadeOut {
		fmt.Println("Music fading out...")
	} else {
		fmt.Println("Music stopped immediately")
	}

	return nil
}

// StopAllSounds stops all playing sounds
func (am *AudioManager) StopAllSounds() {
	// In a real implementation, this would stop all sound effects
	fmt.Println("Stopped all sounds")
}

// StopAllMusic stops all music
func (am *AudioManager) StopAllMusic() {
	// In a real implementation, this would stop all music
	fmt.Println("Stopped all music")
}

// SetMasterVolume sets the master volume
func (am *AudioManager) SetMasterVolume(volume float32) error {
	if volume < 0 || volume > 1 {
		return fmt.Errorf("volume must be between 0.0 and 1.0, got: %.2f", volume)
	}

	am.mutex.Lock()
	defer am.mutex.Unlock()

	am.master.Volume = volume
	am.volume = volume
	fmt.Printf("Master volume set to: %.2f\n", volume)

	return nil
}

// SetSFXVolume sets the sound effects volume
func (am *AudioManager) SetSFXVolume(volume float32) error {
	if volume < 0 || volume > 1 {
		return fmt.Errorf("volume must be between 0.0 and 1.0, got: %.2f", volume)
	}

	am.mutex.Lock()
	defer am.mutex.Unlock()

	am.sfx.Volume = volume
	fmt.Printf("SFX volume set to: %.2f\n", volume)

	return nil
}

// SetMusicVolume sets the music volume
func (am *AudioManager) SetMusicVolume(volume float32) error {
	if volume < 0 || volume > 1 {
		return fmt.Errorf("volume must be between 0.0 and 1.0, got: %.2f", volume)
	}

	am.mutex.Lock()
	defer am.mutex.Unlock()

	am.musicChan.Volume = volume
	fmt.Printf("Music volume set to: %.2f\n", volume)

	return nil
}

// Mute mutes or unmutes all audio
func (am *AudioManager) Mute(mute bool) {
	am.mutex.Lock()
	defer am.mutex.Unlock()

	am.isMuted = mute
	
	if mute {
		fmt.Println("Audio muted")
		am.StopAllSounds()
		am.StopAllMusic()
	} else {
		fmt.Println("Audio unmuted")
	}
}

// MuteSFX mutes or unmutes sound effects
func (am *AudioManager) MuteSFX(mute bool) {
	am.mutex.Lock()
	defer am.mutex.Unlock()

	am.sfx.IsMuted = mute
	
	if mute {
		fmt.Println("SFX muted")
		am.StopAllSounds()
	} else {
		fmt.Println("SFX unmuted")
	}
}

// MuteMusic mutes or unmutes music
func (am *AudioManager) MuteMusic(mute bool) {
	am.mutex.Lock()
	defer am.mutex.Unlock()

	am.musicChan.IsMuted = mute
	
	if mute {
		fmt.Println("Music muted")
		am.StopAllMusic()
	} else {
		fmt.Println("Music unmuted")
	}
}

// GetMasterVolume returns the master volume
func (am *AudioManager) GetMasterVolume() float32 {
	am.mutex.RLock()
	defer am.mutex.RUnlock()
	return am.master.Volume
}

// GetSFXVolume returns the sound effects volume
func (am *AudioManager) GetSFXVolume() float32 {
	am.mutex.RLock()
	defer am.mutex.RUnlock()
	return am.sfx.Volume
}

// GetMusicVolume returns the music volume
func (am *AudioManager) GetMusicVolume() float32 {
	am.mutex.RLock()
	defer am.mutex.RUnlock()
	return am.musicChan.Volume
}

// IsMuted returns whether audio is muted
func (am *AudioManager) IsMuted() bool {
	am.mutex.RLock()
	defer am.mutex.RUnlock()
	return am.isMuted
}

// GetSoundCount returns the number of loaded sounds
func (am *AudioManager) GetSoundCount() int {
	am.mutex.RLock()
	defer am.mutex.RUnlock()
	return len(am.sounds)
}

// GetMusicCount returns the number of loaded music tracks
func (am *AudioManager) GetMusicCount() int {
	am.mutex.RLock()
	defer am.mutex.RUnlock()
	return len(am.music)
}

// GetSound returns a sound by ID
func (am *AudioManager) GetSound(soundID string) (*Sound, error) {
	am.mutex.RLock()
	defer am.mutex.RUnlock()

	sound, exists := am.sounds[soundID]
	if !exists {
		return nil, fmt.Errorf("sound not found: %s", soundID)
	}

	return sound, nil
}

// GetMusic returns a music track by ID
func (am *AudioManager) GetMusic(musicID string) (*MusicTrack, error) {
	am.mutex.RLock()
	defer am.mutex.RUnlock()

	track, exists := am.music[musicID]
	if !exists {
		return nil, fmt.Errorf("music not found: %s", musicID)
	}

	return track, nil
}

// UpdateListenerPosition updates the audio listener position for 3D audio
func (am *AudioManager) UpdateListenerPosition(position, forward, up Vector3) {
	// In a real implementation, this would update the audio listener
	fmt.Printf("Listener position updated: (%.1f, %.1f, %.1f)\n", position.X, position.Y, position.Z)
	fmt.Printf("Forward: (%.1f, %.1f, %.1f), Up: (%.1f, %.1f, %.1f)\n", 
		forward.X, forward.Y, forward.Z, up.X, up.Y, up.Z)
}

// ApplyEffect applies an audio effect to a channel
func (am *AudioManager) ApplyEffect(channelName string, effect AudioEffect) error {
	am.mutex.Lock()
	defer am.mutex.Unlock()

	var channel *AudioChannel
	switch channelName {
	case "master":
		channel = am.master
	case "sfx":
		channel = am.sfx
	case "music":
		channel = am.musicChan
	default:
		return fmt.Errorf("unknown channel: %s", channelName)
	}

	channel.Effects = append(channel.Effects, effect)
	fmt.Printf("Applied %s effect to %s channel\n", effect.Type, channelName)

	return nil
}

// RemoveEffect removes an audio effect from a channel
func (am *AudioManager) RemoveEffect(channelName string, effectType EffectType) error {
	am.mutex.Lock()
	defer am.mutex.Unlock()

	var channel *AudioChannel
	switch channelName {
	case "master":
		channel = am.master
	case "sfx":
		channel = am.sfx
	case "music":
		channel = am.musicChan
	default:
		return fmt.Errorf("unknown channel: %s", channelName)
	}

	// Remove effect
	filtered := make([]AudioEffect, 0)
	for _, effect := range channel.Effects {
		if effect.Type != effectType {
			filtered = append(filtered, effect)
		}
	}
	channel.Effects = filtered

	fmt.Printf("Removed %s effect from %s channel\n", effectType, channelName)

	return nil
}

// GetAudioStats returns audio system statistics
func (am *AudioManager) GetAudioStats() AudioStats {
	am.mutex.RLock()
	defer am.mutex.RUnlock()

	return AudioStats{
		SoundsLoaded:    len(am.sounds),
		MusicLoaded:     len(am.music),
		MasterVolume:    am.master.Volume,
		SFXVolume:       am.sfx.Volume,
		MusicVolume:     am.musicChan.Volume,
		IsMuted:         am.isMuted,
		SFXMuted:        am.sfx.IsMuted,
		MusicMuted:      am.musicChan.IsMuted,
		ActiveEffects:   len(am.master.Effects) + len(am.sfx.Effects) + len(am.musicChan.Effects),
	}
}

// AudioStats represents audio system statistics
type AudioStats struct {
	SoundsLoaded   int
	MusicLoaded    int
	MasterVolume   float32
	SFXVolume      float32
	MusicVolume    float32
	IsMuted        bool
	SFXMuted       bool
	MusicMuted     bool
	ActiveEffects  int
}

// Effect types
type EffectType string

const (
	ReverbEffect   EffectType = "reverb"
	CompressorEffect EffectType = "compressor"
	EQEffect       EffectType = "eq"
	ChorusEffect   EffectType = "chorus"
	DistortionEffect EffectType = "distortion"
)

// CreateSampleSound creates a sample sound for testing
func CreateSampleSound(id, name string, duration time.Duration) *Sound {
	return &Sound{
		ID:        id,
		Name:      name,
		Data:      make([]byte, 44100*duration.Seconds()*2), // 44.1kHz, 16-bit
		Format:    AudioFormat{SampleRate: 44100, Channels: 2, BitDepth: 16, Encoding: "PCM"},
		Duration:  duration,
		Volume:    1.0,
		IsLooping: false,
		Is3D:      false,
		CreatedAt: time.Now(),
	}
}

// CreateSampleMusic creates a sample music track for testing
func CreateSampleMusic(id, name string, duration time.Duration) *MusicTrack {
	return &MusicTrack{
		ID:        id,
		Name:      name,
		Data:      make([]byte, 44100*duration.Seconds()*4), // 44.1kHz, 16-bit, stereo
		Format:    AudioFormat{SampleRate: 44100, Channels: 2, BitDepth: 16, Encoding: "PCM"},
		Duration:  duration,
		Volume:    0.8,
		IsLooping: true,
		FadeIn:    true,
		FadeOut:   true,
		CreatedAt: time.Now(),
	}
}

// Example usage
func main() {
	settings := AudioSettings{
		MasterVolume: 1.0,
		SFXVolume:    0.8,
		MusicVolume:  0.6,
		Enable3D:     true,
		SampleRate:   44100,
		BufferSize:   1024,
		MaxSounds:    64,
	}

	am := NewAudioManager(settings)
	
	// Start audio manager
	if err := am.Start(); err != nil {
		fmt.Printf("Error starting audio manager: %v\n", err)
		return
	}

	// Load sample sounds
	buttonClick := CreateSampleSound("button_click", "Button Click", time.Millisecond*200)
	jumpSound := CreateSampleSound("jump", "Jump Sound", time.Millisecond*500)
	explosion := CreateSampleSound("explosion", "Explosion", time.Second*2)

	am.LoadSound(buttonClick)
	am.LoadSound(jumpSound)
	am.LoadSound(explosion)

	// Load sample music
	backgroundMusic := CreateSampleMusic("bg_music", "Background Music", time.Minute*3)
	ambientMusic := CreateSampleMusic("ambient", "Ambient Music", time.Minute*5)

	am.LoadMusic(backgroundMusic)
	am.LoadMusic(ambientMusic)

	// Play some sounds
	am.PlaySound(PlaySoundRequest{
		SoundID:  "button_click",
		Volume:   0.7,
		Loop:     false,
		Priority: 1,
	})

	am.PlaySound(PlaySoundRequest{
		SoundID:  "jump",
		Volume:   1.0,
		Position: &Vector3{X: 10, Y: 5, Z: -5},
		Loop:     false,
		Priority: 2,
	})

	// Play background music
	am.PlayMusic("bg_music", true)

	// Test volume controls
	am.SetSFXVolume(0.5)
	am.SetMusicVolume(0.4)

	// Test 3D audio
	listenerPos := Vector3{X: 0, Y: 0, Z: 0}
	forward := Vector3{X: 0, Y: 0, Z: -1}
	up := Vector3{X: 0, Y: 1, Z: 0}
	am.UpdateListenerPosition(listenerPos, forward, up)

	// Test effects
	reverb := AudioEffect{
		Type:    ReverbEffect,
		Params:  map[string]float32{"room_size": 0.5, "decay": 0.8},
		Enabled: true,
	}
	am.ApplyEffect("master", reverb)

	// Get statistics
	stats := am.GetAudioStats()
	fmt.Printf("\nAudio Statistics:\n")
	fmt.Printf("Sounds loaded: %d\n", stats.SoundsLoaded)
	fmt.Printf("Music loaded: %d\n", stats.MusicLoaded)
	fmt.Printf("Master volume: %.2f\n", stats.MasterVolume)
	fmt.Printf("SFX volume: %.2f\n", stats.SFXVolume)
	fmt.Printf("Music volume: %.2f\n", stats.MusicVolume)
	fmt.Printf("Active effects: %d\n", stats.ActiveEffects)

	// Test muting
	am.MuteSFX(true)
	am.PlaySound(PlaySoundRequest{
		SoundID: "button_click",
		Volume:  1.0,
	})

	am.MuteSFX(false)
	am.MuteMusic(true)

	// Stop audio
	time.Sleep(time.Second)
	am.StopMusic(true)
	am.Stop()

	fmt.Println("\nAudio Manager initialized successfully!")
}
