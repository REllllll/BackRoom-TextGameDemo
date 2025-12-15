// ============================================================================
// app.js
// ============================================================================
// Frontend JavaScript logic: interact with Prolog HTTP server
// ============================================================================

// API base configuration
// - Local development: use relative path /api (proxied by nginx)
// - Vercel deployment: use /api/proxy (Vercel Serverless Function to backend)
// - Direct connection: use environment variable or explicitly set backend URL
const getApiBase = () => {
    // Detect Vercel environment (by hostname)
    const hostname = window.location.hostname;
    const isVercel = hostname.includes('vercel.app') || hostname.includes('vercel.dev');
    
    // Check whether an environment-provided backend URL exists (global var or meta tag)
    const envApiUrl = window.__ENV__?.API_URL || 
                      document.querySelector('meta[name="api-url"]')?.getAttribute('content');
    
    if (envApiUrl) {
        // Use provided environment value directly
        return envApiUrl.endsWith('/api') ? envApiUrl : `${envApiUrl}/api`;
    } else if (isVercel) {
        // Vercel: use serverless proxy
        return '/api/proxy';
    } else if (hostname === 'localhost' || hostname === '127.0.0.1') {
        // Local dev: use relative path via nginx proxy
        return '/api';
    } else {
        // Fallback: default relative path
        return '/api';
    }
};

const API_BASE = getApiBase();

// ============================================================================
// Global state
// ============================================================================

let gameState = {
    player_location: null,
    entity_location: null,
    sanity: 100,
    holding: [],
    items_here: [],
    game_status: 'playing'
};

let mapData = {
    rooms: [],
    connections: [],
    dark_rooms: [],
    exit_rooms: []
};

// ============================================================================
// DOM elements
// ============================================================================

const elements = {
    commandInput: document.getElementById('command-input'),
    executeBtn: document.getElementById('execute-btn'),
    initBtn: document.getElementById('init-btn'),
    refreshBtn: document.getElementById('refresh-btn'),
    playerLocation: document.getElementById('player-location'),
    entityLocation: document.getElementById('entity-location'),
    sanity: document.getElementById('sanity'),
    sanityBarFill: document.getElementById('sanity-bar-fill'),
    holdingItem: document.getElementById('holding-item'),
    gameStatus: document.getElementById('game-status'),
    roomDescription: document.getElementById('room-description'),
    roomExits: document.getElementById('room-exits'),
    roomItems: document.getElementById('room-items'),
    gameLog: document.getElementById('game-log'),
    mapSvg: document.getElementById('map-svg')
};

// ============================================================================
// Initialization
// ============================================================================

// Initialize app
function initializeApp() {
    setupEventListeners();
    loadMapData();
    refreshGameState();
}

// Check whether DOM is ready
if (document.readyState === 'loading') {
    // DOM still loading, wait for DOMContentLoaded
    document.addEventListener('DOMContentLoaded', initializeApp);
} else {
    // DOM ready, initialize immediately
    initializeApp();
}

// ============================================================================
// Event listeners
// ============================================================================

function setupEventListeners() {
    // Execute command button
    elements.executeBtn.addEventListener('click', executeCommand);
    
    // Execute command on Enter key
    elements.commandInput.addEventListener('keypress', (e) => {
        if (e.key === 'Enter') {
            executeCommand();
        }
    });
    
    // Quick command buttons (exclude take-item because it needs special handling)
    document.querySelectorAll('.cmd-btn').forEach(btn => {
        // Skip take-item button; it has custom logic
        if (btn.id === 'take-item-btn') {
            return;
        }
        btn.addEventListener('click', () => {
            const cmd = btn.getAttribute('data-cmd');
            if (cmd) {
                elements.commandInput.value = cmd;
                executeCommand();
            }
        });
    });
    
    // Take item button (custom handling)
    const takeItemBtn = document.getElementById('take-item-btn');
    if (takeItemBtn) {
        takeItemBtn.addEventListener('click', autoTakeItem);
    }
    
    // Initialize game
    elements.initBtn.addEventListener('click', initGame);
    
    // Refresh status
    elements.refreshBtn.addEventListener('click', refreshGameState);
}

// ============================================================================
// API calls
// ============================================================================

async function apiCall(endpoint, method = 'GET', body = null) {
    try {
        const options = {
            method: method,
            headers: {
                'Content-Type': 'application/json'
            }
        };
        
        if (body) {
            options.body = JSON.stringify(body);
        }
        
        const response = await fetch(`${API_BASE}${endpoint}`, options);
        
        if (!response.ok) {
            throw new Error(`HTTP error! status: ${response.status}`);
        }
        
        return await response.json();
    } catch (error) {
        logMessage(`API error: ${error.message}`, 'error');
        throw error;
    }
}

// ============================================================================
// Game state management
// ============================================================================

async function refreshGameState() {
    try {
        const status = await apiCall('/status');
        updateGameState(status);
        logMessage('Game state updated', 'info');
    } catch (error) {
        logMessage('Failed to fetch game state', 'error');
    }
}

async function initGame() {
    try {
        logMessage('Initializing game...', 'info');
        await apiCall('/init', 'POST');
        logMessage('Game initialized', 'success');
        await refreshGameState();
        await loadMapData();
        drawMap();
    } catch (error) {
        logMessage('Failed to initialize game', 'error');
    }
}

async function executeCommand() {
    const command = elements.commandInput.value.trim();
    if (!command) {
        return;
    }
    
    logMessage(`> ${command}`, 'command');
    elements.commandInput.value = '';
    
    try {
        const result = await apiCall('/command', 'POST', { command: command });
        
        if (result.output) {
            logMessage(result.output, result.success ? 'success' : 'error');
        }
        
        // Always refresh game state (including room info), even if the command failed.
        // This ensures entering a new room updates the UI immediately.
        updateGameState(result);
        
        if (result.game_status) {
            if (result.game_status === 'win') {
                logMessage('🎉 Congrats! You escaped!', 'success');
            } else if (result.game_status === 'lose') {
                logMessage('💀 Game over! You failed...', 'error');
            }
        }
        
        // Update map
        drawMap();
    } catch (error) {
        logMessage(`Command failed: ${error.message}`, 'error');
        // Try refreshing state even on error
        try {
            await refreshGameState();
        } catch (refreshError) {
            // Ignore refresh errors
        }
    }
}

// ============================================================================
// Auto take item
// ============================================================================

// Convert a JS value (usually a string item name) into a safe Prolog atom literal.
// - If it's already a simple atom (e.g. almond_water), return as-is.
// - Otherwise, quote it as a Prolog atom: '...'
function toPrologAtom(value) {
    if (value === null || value === undefined) {
        return "''";
    }
    // Some backends might return objects; try common shapes.
    const raw = (typeof value === 'string')
        ? value
        : (typeof value === 'object' && typeof value.value === 'string')
            ? value.value
            : String(value);
    const s = raw.trim();
    // SWI-Prolog atoms commonly used in this project are lowercase + underscores.
    const isSimpleAtom = /^[a-z][a-zA-Z0-9_]*$/.test(s);
    if (isSimpleAtom) return s;
    // Escape single quotes by doubling them inside a quoted atom.
    const escaped = s.replace(/'/g, "''");
    return `'${escaped}'`;
}

async function autoTakeItem() {
    // Refresh first to ensure latest items
    try {
        await refreshGameState();
    } catch (error) {
        logMessage('Unable to refresh game state', 'error');
        return;
    }
    
    // Loop to take all available items until hands are full or no items left
    while (true) {
        // Check if hands are full (can only carry 2 items)
        if (gameState.holding && gameState.holding.length >= 2) {
            logMessage('Your hands are full. You can only carry 2 items.', 'info');
            break;
        }
        
        // Check if current room has items
        if (!gameState.items_here || gameState.items_here.length === 0) {
            logMessage('No more items to pick up in this room.', 'info');
            break;
        }
        
        // Take the first available item
        const firstItem = gameState.items_here[0];
        // Use the Prolog syntax the backend expects: take(item).
        // Quote/escape item name defensively in case it contains special chars.
        const command = `take(${toPrologAtom(firstItem)}).`;
        
        logMessage(`> ${command}`, 'command');
        
        try {
            const result = await apiCall('/command', 'POST', { command: command });
            
            if (result.output) {
                logMessage(result.output, result.success ? 'success' : 'error');
            }
            
            // Update game state
            updateGameState(result);
            
            // Update map
            drawMap();
            
            // If the command failed (e.g., item not found, hands full), stop trying
            if (!result.success) {
                break;
            }
            
            // Small delay to avoid rapid API calls
            await new Promise(resolve => setTimeout(resolve, 100));
        } catch (error) {
            logMessage(`Failed to pick up item: ${error.message}`, 'error');
            // Try refreshing state even on error
            try {
                await refreshGameState();
            } catch (refreshError) {
                // Ignore refresh errors
            }
            break;
        }
    }
}

// ============================================================================
// Update UI
// ============================================================================

function updateGameState(status) {
    // Normalize holding: array or single value (backward compatible)
    let holdingItems = status.holding;
    if (!holdingItems) {
        holdingItems = [];
    } else if (!Array.isArray(holdingItems)) {
        holdingItems = [holdingItems];
    }
    
    gameState = {
        player_location: status.player_location || null,
        entity_location: status.entity_location || null,
        sanity: status.sanity || 100,
        holding: holdingItems,
        items_here: status.items_here || [],
        game_status: status.game_status || 'playing'
    };
    
    // Update player location
    elements.playerLocation.textContent = gameState.player_location || '-';
    
    // Update entity location
    elements.entityLocation.textContent = gameState.entity_location || '-';
    
    // Update sanity
    elements.sanity.textContent = gameState.sanity;
    elements.sanityBarFill.style.width = `${Math.max(0, Math.min(100, gameState.sanity))}%`;
    
    // Update holding items
    if (holdingItems.length === 0) {
        elements.holdingItem.textContent = 'None';
    } else {
        elements.holdingItem.textContent = holdingItems.join(', ');
    }
    
    // Update game status
    const statusText = {
        'playing': 'In progress',
        'win': 'Won',
        'lose': 'Lost'
    };
    elements.gameStatus.textContent = statusText[gameState.game_status] || 'Unknown';
    elements.gameStatus.className = `value status-${gameState.game_status}`;
    
    // Update room info
    updateRoomInfo(status);
}

function updateRoomInfo(status) {
    if (!status.player_location) {
        elements.roomDescription.textContent = 'Not initialized';
        elements.roomExits.innerHTML = '';
        elements.roomItems.innerHTML = '';
        return;
    }
    
    // Room description
    elements.roomDescription.textContent = `You are in ${status.player_location}.`;
    
    // Exits
    if (status.exits && status.exits.length > 0) {
        const exitsHtml = status.exits.map(exit => {
            const dir = typeof exit === 'object' ? exit.direction : exit.split('-')[0];
            const room = typeof exit === 'object' ? exit.to : exit.split('-')[1];
            return `<span class="exit-item">${dir} → ${room}</span>`;
        }).join('');
        elements.roomExits.innerHTML = `<strong>Exits:</strong> ${exitsHtml}`;
    } else {
        elements.roomExits.innerHTML = '<strong>Exits:</strong> None';
    }
    
    // Items
    if (status.items_here && status.items_here.length > 0) {
        const itemsHtml = status.items_here.map(item => 
            `<span class="item-item">${item}</span>`
        ).join('');
        elements.roomItems.innerHTML = `<strong>Items:</strong> ${itemsHtml}`;
    } else {
        elements.roomItems.innerHTML = '<strong>Items:</strong> None';
    }
}

// ============================================================================
// Map management
// ============================================================================

async function loadMapData() {
    try {
        mapData = await apiCall('/map');
        drawMap();
    } catch (error) {
        logMessage('Failed to load map data', 'error');
    }
}

function drawMap() {
    const svg = elements.mapSvg;
    svg.innerHTML = '';
    
    if (!mapData.rooms || mapData.rooms.length === 0) {
        return;
    }
    
    // Room positions (auto layout based on connections)
    const roomPositions = calculateRoomPositions(mapData);
    const adjustedPositions = adjustPositions(roomPositions, svg);
    
    // Draw connections
    if (mapData.connections) {
        mapData.connections.forEach(conn => {
            const fromPos = adjustedPositions[conn.from];
            const toPos = adjustedPositions[conn.to];
            
            if (fromPos && toPos) {
                const line = document.createElementNS('http://www.w3.org/2000/svg', 'line');
                line.setAttribute('x1', fromPos.x);
                line.setAttribute('y1', fromPos.y);
                line.setAttribute('x2', toPos.x);
                line.setAttribute('y2', toPos.y);
                line.setAttribute('class', 'connection-line');
                svg.appendChild(line);
            }
        });
    }
    
    // Draw room nodes
    mapData.rooms.forEach(room => {
        const pos = adjustedPositions[room];
        if (!pos) return;
        
        const circle = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
        circle.setAttribute('cx', pos.x);
        circle.setAttribute('cy', pos.y);
        circle.setAttribute('r', 30);
        circle.setAttribute('class', getRoomNodeClass(room));
        svg.appendChild(circle);
        
        const text = document.createElementNS('http://www.w3.org/2000/svg', 'text');
        text.setAttribute('x', pos.x);
        text.setAttribute('y', pos.y + 5);
        text.setAttribute('class', 'room-label');
        text.textContent = room.replace(/_/g, ' ');
        svg.appendChild(text);
    });
}

    // Shift positions to center of SVG to keep rooms visible
function adjustPositions(positions, svg) {
    const adjusted = {};
    const coords = Object.values(positions);
    if (coords.length === 0) return adjusted;
    
    // Read viewBox size, default 800x600
    const viewBox = svg.getAttribute('viewBox') || '0 0 800 600';
    const parts = viewBox.split(' ').map(Number);
    const viewWidth = parts[2] || 800;
    const viewHeight = parts[3] || 600;
    
    const minX = Math.min(...coords.map(p => p.x));
    const maxX = Math.max(...coords.map(p => p.x));
    const minY = Math.min(...coords.map(p => p.y));
    const maxY = Math.max(...coords.map(p => p.y));
    
    const padding = 60; // Leave padding to avoid nodes at edges
    const mapWidth = (maxX - minX) + padding * 2;
    const mapHeight = (maxY - minY) + padding * 2;
    
    // Center the map
    const offsetX = padding - minX + (viewWidth - mapWidth) / 2;
    const offsetY = padding - minY + (viewHeight - mapHeight) / 2;
    
    Object.entries(positions).forEach(([room, pos]) => {
        adjusted[room] = {
            x: pos.x + offsetX,
            y: pos.y + offsetY
        };
    });
    
    return adjusted;
}

function calculateRoomPositions(mapData) {
    // Auto layout algorithm based on connections
    const positions = {};
    const rooms = mapData.rooms || [];
    const connections = mapData.connections || [];
    const spacing = 150;
    const svgHeight = 600; // SVG viewBox height
    
    if (rooms.length === 0) {
        return positions;
    }
    
    // Build adjacency list (from each room to its connected rooms and directions)
    const adjacencyList = {};
    rooms.forEach(room => {
        adjacencyList[room] = [];
    });
    
    connections.forEach(conn => {
        if (adjacencyList[conn.from]) {
            adjacencyList[conn.from].push({
                room: conn.to,
                direction: conn.direction
            });
        }
    });
    
    // Direction offsets (in SVG coords, y increases downward)
    const directionOffsets = {
        'north': { x: 0, y: -1 },   // Up (y decreases)
        'south': { x: 0, y: 1 },    // Down (y increases)
        'east': { x: 1, y: 0 },     // Right (x increases)
        'west': { x: -1, y: 0 }     // Left (x decreases)
    };
    
    // Start BFS layout from a starting room
    let startRoom = 'start_point';
    if (!rooms.includes(startRoom)) {
        // If no start_point, use first room
        startRoom = rooms[0];
    }
    
    const queue = [startRoom];
    const visited = new Set();
    
    // Place starting room at center
    positions[startRoom] = { x: 400, y: 300 };
    visited.add(startRoom);
    
    // BFS over all rooms
    while (queue.length > 0) {
        const currentRoom = queue.shift();
        const currentPos = positions[currentRoom];
        
        // Traverse connections of current room
        if (adjacencyList[currentRoom]) {
            adjacencyList[currentRoom].forEach(neighbor => {
                const neighborRoom = neighbor.room;
                const direction = neighbor.direction;
                
                // If neighbor not positioned, calculate its position
                if (!visited.has(neighborRoom)) {
                    const offset = directionOffsets[direction] || { x: 0, y: 0 };
                    positions[neighborRoom] = {
                        x: currentPos.x + offset.x * spacing,
                        y: currentPos.y + offset.y * spacing
                    };
                    visited.add(neighborRoom);
                    queue.push(neighborRoom);
                }
            });
        }
    }
    
    // Handle disconnected rooms (grid layout fallback)
    rooms.forEach((room, index) => {
        if (!positions[room]) {
            const cols = Math.ceil(Math.sqrt(rooms.length));
            const row = Math.floor(index / cols);
            const col = index % cols;
            positions[room] = {
                x: 100 + col * spacing,
                y: 100 + row * spacing
            };
        }
    });
    
    // Do not manually flip y; rely on SVG transform for coordinate system
    // SVG y increases downward, but in the map north should be at the top
    // Keep calculation logic (north = y decrease) so SVG renders north on top
    
    return positions;
}

function getRoomNodeClass(room) {
    let classes = ['room-node'];
    
    if (gameState.player_location === room) {
        classes.push('current');
    }
    
    if (gameState.entity_location === room) {
        classes.push('entity');
    }
    
    if (mapData.dark_rooms && mapData.dark_rooms.includes(room)) {
        classes.push('dark');
    }
    
    if (mapData.exit_rooms && mapData.exit_rooms.includes(room)) {
        classes.push('exit');
    }
    
    return classes.join(' ');
}

// ============================================================================
// Log management
// ============================================================================

function logMessage(message, type = 'info') {
    const logEntry = document.createElement('div');
    logEntry.className = `log-entry ${type}`;
    logEntry.textContent = `[${new Date().toLocaleTimeString()}] ${message}`;
    
    elements.gameLog.appendChild(logEntry);
    elements.gameLog.scrollTop = elements.gameLog.scrollHeight;
    
    // Limit log entries
    const maxEntries = 100;
    while (elements.gameLog.children.length > maxEntries) {
        elements.gameLog.removeChild(elements.gameLog.firstChild);
    }
}

// ============================================================================
// Auto refresh (optional)
// ============================================================================

// Auto refresh state every 5s (optional; can be commented out)
// setInterval(refreshGameState, 5000);

