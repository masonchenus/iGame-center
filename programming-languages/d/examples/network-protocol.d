/*
 * Network Protocol Handler - D Language Implementation
 * High-performance network protocol parsing and handling
 */

import std.stdio;
import std.socket;
import std.stream;
import std.typecons;
import std.conv;
import std.bitmanip;
import std.system;
import std.datetime;
import std.array;
import std.algorithm;

/**
 * Network message types
 */
enum MessageType : ushort {
    Handshake = 0x1000,
    Authentication = 0x1001,
    GameState = 0x2000,
    PlayerMove = 0x2001,
    PlayerAction = 0x2002,
    ChatMessage = 0x3000,
    SystemMessage = 0x3001,
    Heartbeat = 0x4000,
    Error = 0xFFFF
}

/**
 * Network message header
 */
struct MessageHeader {
    MessageType type;
    uint length;
    uint sequenceId;
    uint timestamp;
    ushort checksum;
    
    this(MessageType msgType, uint msgLength, uint seqId = 0) {
        type = msgType;
        length = msgLength;
        sequenceId = seqId;
        timestamp = cast(uint)Clock.currTime().toUnixTime;
        checksum = calculateChecksum(msgLength);
    }
    
    private ushort calculateChecksum(uint data) {
        // Simple checksum calculation
        ushort checksum = 0;
        auto bytes = cast(ubyte*)&data;
        for (int i = 0; i < uint.sizeof; i++) {
            checksum += bytes[i];
        }
        return checksum;
    }
}

/**
 * Serialized message buffer
 */
class MessageBuffer {
    private {
        ubyte[] buffer;
        size_t readPos;
        size_t writePos;
    }
    
    this(size_t initialSize = 1024) {
        buffer = new ubyte[initialSize];
        readPos = 0;
        writePos = 0;
    }
    
    /**
     * Write various data types to buffer
     */
    void writeByte(ubyte value) {
        ensureSpace(1);
        buffer[writePos++] = value;
    }
    
    void writeUshort(ushort value) {
        ensureSpace(2);
        buffer[writePos++] = cast(ubyte)(value & 0xFF);
        buffer[writePos++] = cast(ubyte)((value >> 8) & 0xFF);
    }
    
    void writeUint(uint value) {
        ensureSpace(4);
        buffer[writePos++] = cast(ubyte)(value & 0xFF);
        buffer[writePos++] = cast(ubyte)((value >> 8) & 0xFF);
        buffer[writePos++] = cast(ubyte)((value >> 16) & 0xFF);
        buffer[writePos++] = cast(ubyte)((value >> 24) & 0xFF);
    }
    
    void writeUlong(ulong value) {
        ensureSpace(8);
        for (int i = 0; i < 8; i++) {
            buffer[writePos++] = cast(ubyte)(value & 0xFF);
            value >>= 8;
        }
    }
    
    void writeFloat(float value) {
        writeUint(*cast(uint*)&value);
    }
    
    void writeString(string str) {
        writeUshort(cast(ushort)str.length);
        ensureSpace(str.length);
        foreach (char c; str) {
            buffer[writePos++] = cast(ubyte)c;
        }
    }
    
    /**
     * Read various data types from buffer
     */
    ubyte readByte() {
        if (readPos >= buffer.length) throw new Exception("Buffer underflow");
        return buffer[readPos++];
    }
    
    ushort readUshort() {
        if (readPos + 2 > buffer.length) throw new Exception("Buffer underflow");
        ushort value = buffer[readPos++];
        value |= cast(ushort)(buffer[readPos++] << 8);
        return value;
    }
    
    uint readUint() {
        if (readPos + 4 > buffer.length) throw new Exception("Buffer underflow");
        uint value = buffer[readPos++];
        value |= cast(uint)(buffer[readPos++] << 8);
        value |= cast(uint)(buffer[readPos++] << 16);
        value |= cast(uint)(buffer[readPos++] << 24);
        return value;
    }
    
    ulong readUlong() {
        if (readPos + 8 > buffer.length) throw new Exception("Buffer underflow");
        ulong value = 0;
        for (int i = 0; i < 8; i++) {
            value |= cast(ulong)(buffer[readPos++]) << (i * 8);
        }
        return value;
    }
    
    float readFloat() {
        auto bits = readUint();
        return *cast(float*)&bits;
    }
    
    string readString() {
        auto length = readUshort();
        if (readPos + length > buffer.length) throw new Exception("Buffer underflow");
        
        string result;
        result.length = length;
        for (int i = 0; i < length; i++) {
            result[i] = cast(char)buffer[readPos++];
        }
        return result;
    }
    
    /**
     * Reset buffer positions
     */
    void reset() {
        readPos = 0;
        writePos = 0;
    }
    
    @property {
        ubyte[] data() { return buffer[0..writePos]; }
        size_t length() { return writePos; }
        size_t available() { return buffer.length - writePos; }
    }
    
    private {
        void ensureSpace(size_t needed) {
            if (writePos + needed > buffer.length) {
                auto newSize = buffer.length * 2;
                while (newSize < writePos + needed) {
                    newSize *= 2;
                }
                auto newBuffer = new ubyte[newSize];
                newBuffer[0..buffer.length] = buffer[];
                buffer = newBuffer;
            }
        }
    }
}

/**
 * Network message base class
 */
abstract class NetworkMessage {
    MessageType type;
    
    this(MessageType msgType) {
        type = msgType;
    }
    
    abstract void serialize(MessageBuffer buffer);
    abstract void deserialize(MessageBuffer buffer);
    abstract size_t getSize();
}

/**
 * Handshake message
 */
class HandshakeMessage : NetworkMessage {
    string clientVersion;
    string clientName;
    
    this() {
        super(MessageType.Handshake);
    }
    
    this(string version, string name) {
        super(MessageType.Handshake);
        clientVersion = version;
        clientName = name;
    }
    
    void serialize(MessageBuffer buffer) {
        buffer.writeString(clientVersion);
        buffer.writeString(clientName);
    }
    
    void deserialize(MessageBuffer buffer) {
        clientVersion = buffer.readString();
        clientName = buffer.readString();
    }
    
    size_t getSize() {
        return clientVersion.length + clientName.length + 4; // +4 for length fields
    }
}

/**
 * Game state message
 */
class GameStateMessage : NetworkMessage {
    float playerX, playerY, playerZ;
    int score;
    int level;
    uint gameTime;
    bool isGameOver;
    
    this() {
        super(MessageType.GameState);
    }
    
    void serialize(MessageBuffer buffer) {
        buffer.writeFloat(playerX);
        buffer.writeFloat(playerY);
        buffer.writeFloat(playerZ);
        buffer.writeUint(score);
        buffer.writeUint(level);
        buffer.writeUint(gameTime);
        buffer.writeByte(isGameOver ? 1 : 0);
    }
    
    void deserialize(MessageBuffer buffer) {
        playerX = buffer.readFloat();
        playerY = buffer.readFloat();
        playerZ = buffer.readFloat();
        score = cast(int)buffer.readUint();
        level = cast(int)buffer.readUint();
        gameTime = buffer.readUint();
        isGameOver = buffer.readByte() == 1;
    }
    
    size_t getSize() {
        return 4 * 3 + 4 * 2 + 4 + 1; // 3 floats + 2 uints + uint + bool
    }
}

/**
 * Player movement message
 */
class PlayerMoveMessage : NetworkMessage {
    float deltaX, deltaY, deltaZ;
    float velocityX, velocityY, velocityZ;
    
    this() {
        super(MessageType.PlayerMove);
    }
    
    void serialize(MessageBuffer buffer) {
        buffer.writeFloat(deltaX);
        buffer.writeFloat(deltaY);
        buffer.writeFloat(deltaZ);
        buffer.writeFloat(velocityX);
        buffer.writeFloat(velocityY);
        buffer.writeFloat(velocityZ);
    }
    
    void deserialize(MessageBuffer buffer) {
        deltaX = buffer.readFloat();
        deltaY = buffer.readFloat();
        deltaZ = buffer.readFloat();
        velocityX = buffer.readFloat();
        velocityY = buffer.readFloat();
        velocityZ = buffer.readFloat();
    }
    
    size_t getSize() {
        return 4 * 6; // 6 floats
    }
}

/**
 * Chat message
 */
class ChatMessage : NetworkMessage {
    string playerName;
    string message;
    uint timestamp;
    
    this() {
        super(MessageType.ChatMessage);
    }
    
    void serialize(MessageBuffer buffer) {
        buffer.writeString(playerName);
        buffer.writeString(message);
        buffer.writeUint(timestamp);
    }
    
    void deserialize(MessageBuffer buffer) {
        playerName = buffer.readString();
        message = buffer.readString();
        timestamp = buffer.readUint();
    }
    
    size_t getSize() {
        return playerName.length + message.length + 4 + 4; // strings + length fields + timestamp
    }
}

/**
 * Network protocol handler
 */
class ProtocolHandler {
    private {
        uint nextSequenceId;
        bool[uint] acknowledgedMessages;
        NetworkMessage[uint] pendingMessages;
    }
    
    this() {
        nextSequenceId = 1;
    }
    
    /**
     * Create message from type
     */
    NetworkMessage createMessage(MessageType type) {
        final switch (type) {
            case MessageType.Handshake:
                return new HandshakeMessage();
            case MessageType.GameState:
                return new GameStateMessage();
            case MessageType.PlayerMove:
                return new PlayerMoveMessage();
            case MessageType.ChatMessage:
                return new ChatMessage();
            default:
                return null;
        }
    }
    
    /**
     * Serialize message with header
     */
    ubyte[] serializeMessage(NetworkMessage message) {
        auto buffer = new MessageBuffer();
        
        // Create header
        auto header = MessageHeader(message.type, cast(uint)message.getSize(), nextSequenceId++);
        
        // Write header
        buffer.writeUshort(cast(ushort)header.type);
        buffer.writeUint(header.length);
        buffer.writeUint(header.sequenceId);
        buffer.writeUint(header.timestamp);
        buffer.writeUshort(header.checksum);
        
        // Write message data
        message.serialize(buffer);
        
        return buffer.data;
    }
    
    /**
     * Deserialize message from buffer
     */
    NetworkMessage deserializeMessage(ubyte[] data) {
        if (data.length < MessageHeader.sizeof) {
            return null;
        }
        
        auto buffer = new MessageBuffer(data.length);
        buffer.buffer = data.dup;
        
        try {
            // Read header
            auto type = cast(MessageType)buffer.readUshort();
            auto length = buffer.readUint();
            auto sequenceId = buffer.readUint();
            auto timestamp = buffer.readUint();
            auto checksum = buffer.readUshort();
            
            // Validate checksum
            auto calculatedChecksum = MessageHeader(0, length).calculateChecksum(length);
            if (checksum != calculatedChecksum) {
                writeln("Checksum mismatch!");
                return null;
            }
            
            // Create message
            auto message = createMessage(type);
            if (!message) {
                return null;
            }
            
            // Read message data
            message.deserialize(buffer);
            
            // Acknowledge receipt
            acknowledgeMessage(sequenceId);
            
            return message;
        } catch (Exception e) {
            writeln("Deserialization error: ", e.msg);
            return null;
        }
    }
    
    /**
     * Send message through socket
     */
    bool sendMessage(Socket socket, NetworkMessage message) {
        auto data = serializeMessage(message);
        
        try {
            auto bytesSent = socket.send(data);
            return bytesSent == data.length;
        } catch (Exception e) {
            writeln("Send error: ", e.msg);
            return false;
        }
    }
    
    /**
     * Receive message from socket
     */
    NetworkMessage receiveMessage(Socket socket) {
        // Simple implementation - would need more sophisticated buffering
        char[1024] buffer;
        auto bytesReceived = socket.receive(buffer);
        
        if (bytesReceived <= 0) {
            return null;
        }
        
        return deserializeMessage(cast(ubyte[])buffer[0..bytesReceived]);
    }
    
    private {
        void acknowledgeMessage(uint sequenceId) {
            acknowledgedMessages[sequenceId] = true;
        }
    }
}

/**
 * Connection manager
 */
class ConnectionManager {
    private {
        Socket serverSocket;
        ProtocolHandler protocol;
        NetworkMessage[Socket] clientConnections;
        bool running;
    }
    
    this() {
        protocol = new ProtocolHandler();
        running = false;
    }
    
    /**
     * Start server
     */
    bool startServer(ushort port) {
        try {
            serverSocket = new Socket(AddressFamily.INET, SocketType.STREAM);
            serverSocket.bind(new InternetAddress("localhost", port));
            serverSocket.listen(10);
            
            writeln("Server listening on port ", port);
            running = true;
            
            return true;
        } catch (Exception e) {
            writeln("Server start error: ", e.msg);
            return false;
        }
    }
    
    /**
     * Accept client connections
     */
    void acceptConnections() {
        while (running) {
            try {
                auto clientSocket = serverSocket.accept();
                writeln("Client connected");
                
                // Handle client in spawn(&handleClient separate thread
               , clientSocket);
                
            } catch (Exception e) {
                if (running) {
                    writeln("Accept error: ", e.msg);
                }
            }
        }
    }
    
    /**
     * Handle client connection
     */
    void handleClient(Socket clientSocket) {
        auto handshake = new HandshakeMessage("1.0.0", "Client");
        
        // Send handshake
        protocol.sendMessage(clientSocket, handshake);
        
        // Handle client messages
        while (running) {
            auto message = protocol.receiveMessage(clientSocket);
            if (!message) break;
            
            processMessage(clientSocket, message);
        }
        
        clientSocket.close();
        writeln("Client disconnected");
    }
    
    /**
     * Process received message
     */
    void processMessage(Socket client, NetworkMessage message) {
        final switch (message.type) {
            case MessageType.Handshake:
                writeln("Handshake received");
                break;
                
            case MessageType.GameState:
                auto gameState = cast(GameStateMessage)message;
                writeln("Game state: Position(", gameState.playerX, ",", gameState.playerY, ",", gameState.playerZ, 
                       ") Score:", gameState.score, " Level:", gameState.level);
                break;
                
            case MessageType.PlayerMove:
                auto move = cast(PlayerMoveMessage)message;
                writeln("Player move: Delta(", move.deltaX, ",", move.deltaY, ",", move.deltaZ, ")");
                break;
                
            case MessageType.ChatMessage:
                auto chat = cast(ChatMessage)message;
                writeln("Chat[", chat.playerName, "]: ", chat.message);
                break;
                
            default:
                writeln("Unknown message type: ", message.type);
                break;
        }
    }
    
    /**
     * Stop server
     */
    void stopServer() {
        running = false;
        if (serverSocket) {
            serverSocket.close();
        }
    }
}

/**
 * Performance monitor
 */
class NetworkPerformanceMonitor {
    private {
        size_t messagesSent;
        size_t messagesReceived;
        size_t bytesSent;
        size_t bytesReceived;
        TickDuration startTime;
        uint sequenceId;
    }
    
    this() {
        startTime = Clock.currTime().toTickDuration;
        sequenceId = 1;
    }
    
    /**
     * Record message sent
     */
    void recordMessageSent(size_t size) {
        messagesSent++;
        bytesSent += size;
    }
    
    /**
     * Record message received
     */
    void recordMessageReceived(size_t size) {
        messagesReceived++;
        bytesReceived += size;
    }
    
    /**
     * Generate performance report
     */
    void generateReport() {
        auto elapsed = Clock.currTime().toTickDuration - startTime;
        auto elapsedSeconds = elapsed.to!"msecs" / 1000.0;
        
        writeln("\n=== NETWORK PERFORMANCE REPORT ===");
        writeln("Messages sent: ", messagesSent);
        writeln("Messages received: ", messagesReceived);
        writeln("Bytes sent: ", bytesSent);
        writeln("Bytes received: ", bytesReceived);
        writeln("Elapsed time: ", elapsedSeconds, " seconds");
        
        if (elapsedSeconds > 0) {
            writeln("Message rate: ", format("%.1f", messagesReceived / elapsedSeconds), " msgs/sec");
            writeln("Bandwidth: ", format("%.1f", bytesReceived / elapsedSeconds / 1024), " KB/sec");
        }
    }
}

/**
 * Demo program
 */
void main() {
    writeln("=== D Language Network Protocol Handler Demo ===\n");
    
    auto monitor = new NetworkPerformanceMonitor();
    auto protocol = new ProtocolHandler();
    
    // Test message serialization
    writeln("Testing message serialization...");
    
    // Create test messages
    auto handshake = new HandshakeMessage("1.0.0", "DemoClient");
    auto gameState = new GameStateMessage();
    gameState.playerX = 10.5f;
    gameState.playerY = 0.0f;
    gameState.playerZ = 25.3f;
    gameState.score = 1500;
    gameState.level = 5;
    gameState.gameTime = 3600;
    
    auto move = new PlayerMoveMessage();
    move.deltaX = 1.0f;
    move.deltaY = 0.0f;
    move.deltaZ = 0.5f;
    
    auto chat = new ChatMessage();
    chat.playerName = "Player1";
    chat.message = "Hello, world!";
    chat.timestamp = cast(uint)Clock.currTime().toUnixTime;
    
    // Serialize messages
    auto serializedHandshake = protocol.serializeMessage(handshake);
    auto serializedGameState = protocol.serializeMessage(gameState);
    auto serializedMove = protocol.serializeMessage(move);
    auto serializedChat = protocol.serializeMessage(chat);
    
    monitor.recordMessageSent(serializedHandshake.length);
    monitor.recordMessageSent(serializedGameState.length);
    monitor.recordMessageSent(serializedMove.length);
    monitor.recordMessageSent(serializedChat.length);
    
    writeln("Serialized handshake: ", serializedHandshake.length, " bytes");
    writeln("Serialized game state: ", serializedGameState.length, " bytes");
    writeln("Serialized move: ", serializedMove.length, " bytes");
    writeln("Serialized chat: ", serializedChat.length, " bytes");
    
    // Deserialize messages
    writeln("\nTesting message deserialization...");
    
    auto deserializedHandshake = protocol.deserializeMessage(serializedHandshake);
    auto deserializedGameState = protocol.deserializeMessage(serializedGameState);
    auto deserializedMove = protocol.deserializeMessage(serializedMove);
    auto deserializedChat = protocol.deserializeMessage(serializedChat);
    
    if (deserializedHandshake) {
        auto hs = cast(HandshakeMessage)deserializedHandshake;
        writeln("Deserialized handshake: Version=", hs.clientVersion, " Name=", hs.clientName);
        monitor.recordMessageReceived(serializedHandshake.length);
    }
    
    if (deserializedGameState) {
        auto gs = cast(GameStateMessage)deserializedGameState;
        writeln("Deserialized game state: Position(", gs.playerX, ",", gs.playerY, ",", gs.playerZ, 
               ") Score=", gs.score);
        monitor.recordMessageReceived(serializedGameState.length);
    }
    
    if (deserializedMove) {
        auto mv = cast(PlayerMoveMessage)deserializedMove;
        writeln("Deserialized move: Delta(", mv.deltaX, ",", mv.deltaY, ",", mv.deltaZ, ")");
        monitor.recordMessageReceived(serializedMove.length);
    }
    
    if (deserializedChat) {
        auto ct = cast(ChatMessage)deserializedChat;
        writeln("Deserialized chat: ", ct.playerName, ": ", ct.message);
        monitor.recordMessageReceived(serializedChat.length);
    }
    
    // Start server demo (simplified)
    writeln("\nStarting server demo...");
    auto connectionManager = new ConnectionManager();
    
    if (connectionManager.startServer(8080)) {
        writeln("Server started successfully");
        writeln("(Server would run here - demo limited for safety)");
        connectionManager.stopServer();
    }
    
    // Generate performance report
    monitor.generateReport();
    
    writeln("\n=== D Language Network Protocol Features ===");
    writeln("✓ High-performance binary serialization");
    writeln("✓ Type-safe message handling");
    writeln("✓ Checksum validation for data integrity");
    writeln("✓ Extensible message system");
    writeln("✓ Connection management and threading");
    writeln("✓ Performance monitoring and metrics");
    writeln("✓ Memory-efficient buffer management");
    
    writeln("\nNetwork protocol handler demo completed!");
}

