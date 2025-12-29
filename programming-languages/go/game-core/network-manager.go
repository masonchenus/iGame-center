package main

import (
	"fmt"
	"log"
	"net"
	"sync"
	"time"
)

// NetworkManager handles all network communications
type NetworkManager struct {
	clients    map[string]*ClientConnection
	broadcast  chan NetworkMessage
	listener   net.Listener
	serverAddr string
	mutex      sync.RWMutex
	isRunning  bool
}

// ClientConnection represents a connected client
type ClientConnection struct {
	ID          string
	Conn        net.Conn
	PlayerID    string
	GameID      string
	ConnectedAt time.Time
	LastPing    time.Time
	Latency     time.Duration
}

// NetworkMessage represents a message to be sent over the network
type NetworkMessage struct {
	Type      string
	From      string
	To        string
	GameID    string
	Data      interface{}
	Timestamp time.Time
}

// NetworkEvent represents network events
type NetworkEvent struct {
	Type      string
	ClientID  string
	Data      interface{}
	Timestamp time.Time
}

// NewNetworkManager creates a new network manager
func NewNetworkManager(serverAddr string) *NetworkManager {
	return &NetworkManager{
		clients:   make(map[string]*ClientConnection),
		broadcast: make(chan NetworkMessage, 100),
		serverAddr: serverAddr,
		isRunning:  false,
	}
}

// Start starts the network manager and begins listening for connections
func (nm *NetworkManager) Start() error {
	listener, err := net.Listen("tcp", nm.serverAddr)
	if err != nil {
		return fmt.Errorf("failed to start server: %v", err)
	}

	nm.listener = listener
	nm.isRunning = true

	log.Printf("Network Manager started on %s", nm.serverAddr)

	// Start goroutines
	go nm.acceptConnections()
	go nm.handleBroadcasts()
	go nm.pingClients()
	go nm.cleanupDisconnected()

	return nil
}

// Stop stops the network manager
func (nm *NetworkManager) Stop() {
	nm.isRunning = false
	if nm.listener != nil {
		nm.listener.Close()
	}

	nm.mutex.Lock()
	defer nm.mutex.Unlock()

	// Close all client connections
	for _, client := range nm.clients {
		client.Conn.Close()
	}
	nm.clients = make(map[string]*ClientConnection)

	close(nm.broadcast)
}

// Accept connections
func (nm *NetworkManager) acceptConnections() {
	for nm.isRunning {
		conn, err := nm.listener.Accept()
		if err != nil {
			if nm.isRunning {
				log.Printf("Error accepting connection: %v", err)
			}
			continue
		}

		go nm.handleConnection(conn)
	}
}

// Handle a new client connection
func (nm *NetworkManager) handleConnection(conn net.Conn) {
	clientID := fmt.Sprintf("client_%d", time.Now().Unix())
	
	client := &ClientConnection{
		ID:          clientID,
		Conn:        conn,
		ConnectedAt: time.Now(),
		LastPing:    time.Now(),
	}

	nm.addClient(client)
	log.Printf("New client connected: %s", clientID)

	// Handle client communication
	nm.handleClientCommunication(client)
}

// Handle communication with a specific client
func (nm *NetworkManager) handleClientCommunication(client *ClientConnection) {
	defer func() {
		nm.removeClient(client.ID)
		client.Conn.Close()
		log.Printf("Client disconnected: %s", client.ID)
	}()

	for {
		// Set read timeout
		client.Conn.SetReadDeadline(time.Now().Add(time.Minute * 5))
		
		// Read message
		message, err := nm.readMessage(client.Conn)
		if err != nil {
			return
		}

		// Process message
		nm.processMessage(client, message)
	}
}

// Read message from connection
func (nm *NetworkManager) readMessage(conn net.Conn) (*NetworkMessage, error) {
	var message NetworkMessage
	
	// Simple message format: type|from|to|gameID|data
	// In a real implementation, you'd use JSON or protobuf
	buffer := make([]byte, 1024)
	n, err := conn.Read(buffer)
	if err != nil {
		return nil, err
	}

	// Parse message (simplified)
	// This would be replaced with proper deserialization
	message = NetworkMessage{
		Type:      "data",
		From:      clientIDFromConnection(conn),
		To:        "server",
		Timestamp: time.Now(),
		Data:      string(buffer[:n]),
	}

	return &message, nil
}

// Process incoming message
func (nm *NetworkManager) processMessage(client *ClientConnection, message *NetworkMessage) {
	// Update client last ping time
	client.LastPing = time.Now()

	switch message.Type {
	case "join_game":
		nm.handleJoinGame(client, message)
	case "leave_game":
		nm.handleLeaveGame(client, message)
	case "game_move":
		nm.handleGameMove(client, message)
	case "chat_message":
		nm.handleChatMessage(client, message)
	case "ping":
		nm.handlePing(client)
	default:
		log.Printf("Unknown message type: %s", message.Type)
	}
}

// Handle join game request
func (nm *NetworkManager) handleJoinGame(client *ClientConnection, message *NetworkMessage) {
	gameID, ok := message.Data.(string)
	if !ok {
		nm.sendError(client, "Invalid game ID")
		return
	}

	client.GameID = gameID
	nm.sendToClient(client, "join_confirmed", map[string]interface{}{
		"game_id": gameID,
		"client_id": client.ID,
	})

	log.Printf("Client %s joined game %s", client.ID, gameID)
}

// Handle leave game request
func (nm *NetworkManager) handleLeaveGame(client *ClientConnection, message *NetworkMessage) {
	if client.GameID != "" {
		log.Printf("Client %s left game %s", client.ID, client.GameID)
		client.GameID = ""
	}

	nm.sendToClient(client, "leave_confirmed", map[string]interface{}{
		"message": "Successfully left game",
	})
}

// Handle game move
func (nm *NetworkManager) handleGameMove(client *ClientConnection, message *NetworkMessage) {
	if client.GameID == "" {
		nm.sendError(client, "Not in a game")
		return
	}

	// Broadcast move to other players in the game
	nm.BroadcastToGame(client.GameID, "game_move", map[string]interface{}{
		"client_id": client.ID,
		"move":      message.Data,
		"timestamp": time.Now(),
	})
}

// Handle chat message
func (nm *NetworkManager) handleChatMessage(client *ClientConnection, message *NetworkMessage) {
	if client.GameID == "" {
		nm.sendError(client, "Not in a game")
		return
	}

	// Broadcast chat message to game
	nm.BroadcastToGame(client.GameID, "chat_message", map[string]interface{}{
		"client_id": client.ID,
		"message":   message.Data,
		"timestamp": time.Now(),
	})
}

// Handle ping
func (nm *NetworkManager) handlePing(client *ClientConnection) {
	nm.sendToClient(client, "pong", map[string]interface{}{
		"timestamp": time.Now(),
	})
}

// Broadcast message to all clients in a game
func (nm *NetworkManager) BroadcastToGame(gameID, messageType string, data interface{}) {
	nm.mutex.RLock()
	defer nm.mutex.RUnlock()

	for _, client := range nm.clients {
		if client.GameID == gameID {
			go nm.sendToClient(client, messageType, data)
		}
	}
}

// Broadcast message to all connected clients
func (nm *NetworkManager) Broadcast(messageType string, data interface{}) {
	nm.mutex.RLock()
	defer nm.mutex.RUnlock()

	for _, client := range nm.clients {
		go nm.sendToClient(client, messageType, data)
	}
}

// Send message to specific client
func (nm *NetworkManager) SendToClient(clientID, messageType string, data interface{}) error {
	nm.mutex.RLock()
	client, exists := nm.clients[clientID]
	nm.mutex.RUnlock()

	if !exists {
		return fmt.Errorf("client not found: %s", clientID)
	}

	return nm.sendToClient(client, messageType, data)
}

// Send message to client (internal)
func (nm *NetworkManager) sendToClient(client *ClientConnection, messageType string, data interface{}) error {
	message := NetworkMessage{
		Type:      messageType,
		From:      "server",
		To:        client.ID,
		Data:      data,
		Timestamp: time.Now(),
	}

	// Serialize message (simplified)
	messageData := fmt.Sprintf("%s|%s|%v\n", messageType, message.To, data)
	
	_, err := client.Conn.Write([]byte(messageData))
	if err != nil {
		log.Printf("Error sending message to client %s: %v", client.ID, err)
		return err
	}

	return nil
}

// Send error message to client
func (nm *NetworkManager) sendError(client *ClientConnection, errorMessage string) {
	nm.sendToClient(client, "error", map[string]interface{}{
		"message": errorMessage,
	})
}

// Handle broadcast messages
func (nm *NetworkManager) handleBroadcasts() {
	for message := range nm.broadcast {
		switch message.To {
		case "all":
			nm.Broadcast(message.Type, message.Data)
		case "game":
			nm.BroadcastToGame(message.GameID, message.Type, message.Data)
		default:
			nm.SendToClient(message.To, message.Type, message.Data)
		}
	}
}

// Ping clients to check connectivity
func (nm *NetworkManager) pingClients() {
	ticker := time.NewTicker(time.Second * 30)
	defer ticker.Stop()

	for range ticker.C {
		nm.mutex.RLock()
		clients := make([]*ClientConnection, 0, len(nm.clients))
		for _, client := range nm.clients {
			clients = append(clients, client)
		}
		nm.mutex.RUnlock()

		for _, client := range clients {
			go func(c *ClientConnection) {
				// Send ping
				err := nm.sendToClient(c, "ping", map[string]interface{}{
					"timestamp": time.Now(),
				})
				if err != nil {
					nm.removeClient(c.ID)
					c.Conn.Close()
				}
			}(client)
		}
	}
}

// Cleanup disconnected clients
func (nm *NetworkManager) cleanupDisconnected() {
	ticker := time.NewTicker(time.Minute)
	defer ticker.Stop()

	for range ticker.C {
		nm.mutex.Lock()
		for clientID, client := range nm.clients {
			if time.Since(client.LastPing) > time.Minute*3 {
				log.Printf("Cleaning up inactive client: %s", clientID)
				client.Conn.Close()
				delete(nm.clients, clientID)
			}
		}
		nm.mutex.Unlock()
	}
}

// Add client to the client map
func (nm *NetworkManager) addClient(client *ClientConnection) {
	nm.mutex.Lock()
	defer nm.mutex.Unlock()
	nm.clients[client.ID] = client
}

// Remove client from the client map
func (nm *NetworkManager) removeClient(clientID string) {
	nm.mutex.Lock()
	defer nm.mutex.Unlock()
	delete(nm.clients, clientID)
}

// Get client count
func (nm *NetworkManager) GetClientCount() int {
	nm.mutex.RLock()
	defer nm.mutex.RUnlock()
	return len(nm.clients)
}

// Get clients in a game
func (nm *NetworkManager) GetGameClients(gameID string) []*ClientConnection {
	nm.mutex.RLock()
	defer nm.mutex.RUnlock()

	clients := make([]*ClientConnection, 0)
	for _, client := range nm.clients {
		if client.GameID == gameID {
			clients = append(clients, client)
		}
	}
	return clients
}

// Utility functions
func clientIDFromConnection(conn net.Conn) string {
	return fmt.Sprintf("client_%d", time.Now().Unix())
}

// Queue message for broadcast
func (nm *NetworkManager) QueueBroadcast(message NetworkMessage) {
	select {
	case nm.broadcast <- message:
	default:
		log.Printf("Broadcast queue full, dropping message")
	}
}

// Example usage
func main() {
	nm := NewNetworkManager(":8080")

	if err := nm.Start(); err != nil {
		log.Fatal(err)
	}

	log.Printf("Network Manager running with %d clients", nm.GetClientCount())
	
	// Keep running
	select {}
}
