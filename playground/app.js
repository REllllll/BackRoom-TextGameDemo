// ============================================================================
// app.js
// ============================================================================
// 前端 JavaScript 逻辑：与 Prolog HTTP server 交互
// ============================================================================

// 自动检测 API 地址：如果从远程访问，使用当前主机地址
// 如果从本地访问，使用 localhost
const getApiBase = () => {
    // 如果当前页面是通过 IP 地址访问的，使用相同的 IP
    const hostname = window.location.hostname;
    const port = window.location.port || '8081'; // 默认使用映射后的端口
    return `http://${hostname}:${port}/api`;
};

const API_BASE = getApiBase();

// ============================================================================
// 全局状态
// ============================================================================

let gameState = {
    player_location: null,
    entity_location: null,
    sanity: 100,
    holding: null,
    game_status: 'playing'
};

let mapData = {
    rooms: [],
    connections: [],
    dark_rooms: [],
    exit_rooms: []
};

// ============================================================================
// DOM 元素
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
// 初始化
// ============================================================================

document.addEventListener('DOMContentLoaded', () => {
    setupEventListeners();
    loadMapData();
    refreshGameState();
});

// ============================================================================
// 事件监听器
// ============================================================================

function setupEventListeners() {
    // 执行命令按钮
    elements.executeBtn.addEventListener('click', executeCommand);
    
    // 回车键执行命令
    elements.commandInput.addEventListener('keypress', (e) => {
        if (e.key === 'Enter') {
            executeCommand();
        }
    });
    
    // 快捷命令按钮
    document.querySelectorAll('.cmd-btn').forEach(btn => {
        btn.addEventListener('click', () => {
            const cmd = btn.getAttribute('data-cmd');
            elements.commandInput.value = cmd;
            executeCommand();
        });
    });
    
    // 初始化游戏
    elements.initBtn.addEventListener('click', initGame);
    
    // 刷新状态
    elements.refreshBtn.addEventListener('click', refreshGameState);
}

// ============================================================================
// API 调用
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
        logMessage(`API 错误: ${error.message}`, 'error');
        throw error;
    }
}

// ============================================================================
// 游戏状态管理
// ============================================================================

async function refreshGameState() {
    try {
        const status = await apiCall('/status');
        updateGameState(status);
        logMessage('游戏状态已更新', 'info');
    } catch (error) {
        logMessage('无法获取游戏状态', 'error');
    }
}

async function initGame() {
    try {
        logMessage('正在初始化游戏...', 'info');
        await apiCall('/init', 'POST');
        logMessage('游戏已初始化', 'success');
        await refreshGameState();
        await loadMapData();
        drawMap();
    } catch (error) {
        logMessage('初始化游戏失败', 'error');
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
        
        if (result.game_status) {
            updateGameState(result);
            
            if (result.game_status === 'win') {
                logMessage('🎉 恭喜！你成功逃脱了！', 'success');
            } else if (result.game_status === 'lose') {
                logMessage('💀 游戏结束！你失败了...', 'error');
            }
        }
        
        // 更新地图
        drawMap();
    } catch (error) {
        logMessage(`执行命令失败: ${error.message}`, 'error');
    }
}

// ============================================================================
// 更新 UI
// ============================================================================

function updateGameState(status) {
    gameState = {
        player_location: status.player_location || null,
        entity_location: status.entity_location || null,
        sanity: status.sanity || 100,
        holding: status.holding || null,
        game_status: status.game_status || 'playing'
    };
    
    // 更新玩家位置
    elements.playerLocation.textContent = gameState.player_location || '-';
    
    // 更新实体位置
    elements.entityLocation.textContent = gameState.entity_location || '-';
    
    // 更新理智值
    elements.sanity.textContent = gameState.sanity;
    elements.sanityBarFill.style.width = `${Math.max(0, Math.min(100, gameState.sanity))}%`;
    
    // 更新持有物品
    elements.holdingItem.textContent = gameState.holding || '无';
    
    // 更新游戏状态
    const statusText = {
        'playing': '进行中',
        'win': '胜利',
        'lose': '失败'
    };
    elements.gameStatus.textContent = statusText[gameState.game_status] || '未知';
    elements.gameStatus.className = `value status-${gameState.game_status}`;
    
    // 更新房间信息
    updateRoomInfo(status);
}

function updateRoomInfo(status) {
    if (!status.player_location) {
        elements.roomDescription.textContent = '未初始化';
        elements.roomExits.innerHTML = '';
        elements.roomItems.innerHTML = '';
        return;
    }
    
    // 房间描述
    elements.roomDescription.textContent = `你在 ${status.player_location}。`;
    
    // 出口
    if (status.exits && status.exits.length > 0) {
        const exitsHtml = status.exits.map(exit => {
            const dir = typeof exit === 'object' ? exit.direction : exit.split('-')[0];
            const room = typeof exit === 'object' ? exit.to : exit.split('-')[1];
            return `<span class="exit-item">${dir} → ${room}</span>`;
        }).join('');
        elements.roomExits.innerHTML = `<strong>出口:</strong> ${exitsHtml}`;
    } else {
        elements.roomExits.innerHTML = '<strong>出口:</strong> 无';
    }
    
    // 物品
    if (status.items_here && status.items_here.length > 0) {
        const itemsHtml = status.items_here.map(item => 
            `<span class="item-item">${item}</span>`
        ).join('');
        elements.roomItems.innerHTML = `<strong>物品:</strong> ${itemsHtml}`;
    } else {
        elements.roomItems.innerHTML = '<strong>物品:</strong> 无';
    }
}

// ============================================================================
// 地图管理
// ============================================================================

async function loadMapData() {
    try {
        mapData = await apiCall('/map');
        drawMap();
    } catch (error) {
        logMessage('无法加载地图数据', 'error');
    }
}

function drawMap() {
    const svg = elements.mapSvg;
    svg.innerHTML = '';
    
    if (!mapData.rooms || mapData.rooms.length === 0) {
        return;
    }
    
    // 房间位置（简单的网格布局）
    const roomPositions = calculateRoomPositions(mapData.rooms);
    
    // 绘制连接
    if (mapData.connections) {
        mapData.connections.forEach(conn => {
            const fromPos = roomPositions[conn.from];
            const toPos = roomPositions[conn.to];
            
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
    
    // 绘制房间节点
    mapData.rooms.forEach(room => {
        const pos = roomPositions[room];
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

function calculateRoomPositions(rooms) {
    // 简单的网格布局算法
    const positions = {};
    const cols = Math.ceil(Math.sqrt(rooms.length));
    const spacing = 150;
    const startX = 100;
    const startY = 100;
    
    // 特殊房间的固定位置
    const specialPositions = {
        'start_point': { x: 100, y: 300 },
        'yellow_hallway': { x: 250, y: 300 },
        'dark_corridor': { x: 250, y: 150 },
        'dead_end': { x: 400, y: 150 },
        'the_hub': { x: 400, y: 300 },
        'electrical_room': { x: 400, y: 450 },
        'manila_room': { x: 550, y: 300 },
        'supply_closet': { x: 250, y: 450 }
    };
    
    rooms.forEach((room, index) => {
        if (specialPositions[room]) {
            positions[room] = specialPositions[room];
        } else {
            const row = Math.floor(index / cols);
            const col = index % cols;
            positions[room] = {
                x: startX + col * spacing,
                y: startY + row * spacing
            };
        }
    });
    
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
// 日志管理
// ============================================================================

function logMessage(message, type = 'info') {
    const logEntry = document.createElement('div');
    logEntry.className = `log-entry ${type}`;
    logEntry.textContent = `[${new Date().toLocaleTimeString()}] ${message}`;
    
    elements.gameLog.appendChild(logEntry);
    elements.gameLog.scrollTop = elements.gameLog.scrollHeight;
    
    // 限制日志条数
    const maxEntries = 100;
    while (elements.gameLog.children.length > maxEntries) {
        elements.gameLog.removeChild(elements.gameLog.firstChild);
    }
}

// ============================================================================
// 自动刷新（可选）
// ============================================================================

// 每 5 秒自动刷新一次状态（可选，可以注释掉）
// setInterval(refreshGameState, 5000);

