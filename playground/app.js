// ============================================================================
// app.js
// ============================================================================
// 前端 JavaScript 逻辑：与 Prolog HTTP server 交互
// ============================================================================

// API 地址配置
// - 本地开发：使用相对路径 /api（通过 nginx 代理）
// - Vercel 部署：使用 /api/proxy（通过 Vercel Serverless Function 代理到后端）
// - 直接连接：使用环境变量或直接指定后端地址
const getApiBase = () => {
    // 检查是否在 Vercel 环境（通过 hostname 判断）
    const hostname = window.location.hostname;
    const isVercel = hostname.includes('vercel.app') || hostname.includes('vercel.dev');
    
    // 检查是否有环境变量配置的后端地址（通过全局变量或 meta 标签）
    const envApiUrl = window.__ENV__?.API_URL || 
                      document.querySelector('meta[name="api-url"]')?.getAttribute('content');
    
    if (envApiUrl) {
        // 如果配置了环境变量，直接使用
        return envApiUrl.endsWith('/api') ? envApiUrl : `${envApiUrl}/api`;
    } else if (isVercel) {
        // Vercel 环境：使用 Serverless Function 代理
        return '/api/proxy';
    } else if (hostname === 'localhost' || hostname === '127.0.0.1') {
        // 本地开发：使用相对路径，通过 nginx 代理
        return '/api';
    } else {
        // 其他环境：默认使用相对路径
        return '/api';
    }
};

const API_BASE = getApiBase();

// ============================================================================
// 全局状态
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

// 初始化函数
function initializeApp() {
    setupEventListeners();
    loadMapData();
    refreshGameState();
}

// 检查 DOM 是否已准备好
if (document.readyState === 'loading') {
    // DOM 还在加载中，等待 DOMContentLoaded
    document.addEventListener('DOMContentLoaded', initializeApp);
} else {
    // DOM 已经准备好了，直接初始化
    initializeApp();
}

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
    
    // 快捷命令按钮（排除拾取物品按钮，因为它需要特殊处理）
    document.querySelectorAll('.cmd-btn').forEach(btn => {
        // 跳过拾取物品按钮，它需要特殊处理
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
    
    // 拾取物品按钮（特殊处理）
    const takeItemBtn = document.getElementById('take-item-btn');
    if (takeItemBtn) {
        takeItemBtn.addEventListener('click', autoTakeItem);
    }
    
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
        
        // 总是更新游戏状态（包括房间信息），无论命令是否成功
        // 这样进入新房间时能自动刷新状态
        updateGameState(result);
        
        if (result.game_status) {
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
        // 即使出错也尝试刷新状态
        try {
            await refreshGameState();
        } catch (refreshError) {
            // 忽略刷新错误
        }
    }
}

// ============================================================================
// 自动拾取物品
// ============================================================================

async function autoTakeItem() {
    // 先刷新状态以确保获取最新的物品信息
    try {
        await refreshGameState();
    } catch (error) {
        logMessage('无法刷新游戏状态', 'error');
        return;
    }
    
    // 检查当前房间是否有物品
    if (!gameState.items_here || gameState.items_here.length === 0) {
        logMessage('当前房间没有物品可以拾取。', 'info');
        return;
    }
    
    // 拾取第一个物品
    const firstItem = gameState.items_here[0];
    const command = `take(${firstItem})`;
    
    logMessage(`> ${command}`, 'command');
    
    try {
        const result = await apiCall('/command', 'POST', { command: command });
        
        if (result.output) {
            logMessage(result.output, result.success ? 'success' : 'error');
        }
        
        // 更新游戏状态
        updateGameState(result);
        
        // 更新地图
        drawMap();
    } catch (error) {
        logMessage(`拾取物品失败: ${error.message}`, 'error');
        // 即使出错也尝试刷新状态
        try {
            await refreshGameState();
        } catch (refreshError) {
            // 忽略刷新错误
        }
    }
}

// ============================================================================
// 更新 UI
// ============================================================================

function updateGameState(status) {
    // 处理 holding 字段：可能是数组或单个值（向后兼容）
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
    
    // 更新玩家位置
    elements.playerLocation.textContent = gameState.player_location || '-';
    
    // 更新实体位置
    elements.entityLocation.textContent = gameState.entity_location || '-';
    
    // 更新理智值
    elements.sanity.textContent = gameState.sanity;
    elements.sanityBarFill.style.width = `${Math.max(0, Math.min(100, gameState.sanity))}%`;
    
    // 更新持有物品
    if (holdingItems.length === 0) {
        elements.holdingItem.textContent = '无';
    } else {
        elements.holdingItem.textContent = holdingItems.join(', ');
    }
    
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
    
    // 房间位置（基于连接关系的自动布局）
    const roomPositions = calculateRoomPositions(mapData);
    const adjustedPositions = adjustPositions(roomPositions, svg);
    
    // 绘制连接
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
    
    // 绘制房间节点
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

// 将位置平移到 SVG 容器中心，确保所有房间可见
function adjustPositions(positions, svg) {
    const adjusted = {};
    const coords = Object.values(positions);
    if (coords.length === 0) return adjusted;
    
    // 读取 viewBox 尺寸，默认 800x600
    const viewBox = svg.getAttribute('viewBox') || '0 0 800 600';
    const parts = viewBox.split(' ').map(Number);
    const viewWidth = parts[2] || 800;
    const viewHeight = parts[3] || 600;
    
    const minX = Math.min(...coords.map(p => p.x));
    const maxX = Math.max(...coords.map(p => p.x));
    const minY = Math.min(...coords.map(p => p.y));
    const maxY = Math.max(...coords.map(p => p.y));
    
    const padding = 60; // 预留边距，避免节点贴边
    const mapWidth = (maxX - minX) + padding * 2;
    const mapHeight = (maxY - minY) + padding * 2;
    
    // 将地图居中放置
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
    // 基于连接关系的自动布局算法
    const positions = {};
    const rooms = mapData.rooms || [];
    const connections = mapData.connections || [];
    const spacing = 150;
    const svgHeight = 600; // SVG viewBox 高度
    
    if (rooms.length === 0) {
        return positions;
    }
    
    // 构建邻接表（从每个房间到其连接的房间和方向）
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
    
    // 方向偏移量（在 SVG 坐标系中，y 向下为正）
    const directionOffsets = {
        'north': { x: 0, y: -1 },   // 向上（y 减小）
        'south': { x: 0, y: 1 },    // 向下（y 增加）
        'east': { x: 1, y: 0 },     // 向右（x 增加）
        'west': { x: -1, y: 0 }     // 向左（x 减小）
    };
    
    // 从起点开始 BFS 布局
    let startRoom = 'start_point';
    if (!rooms.includes(startRoom)) {
        // 如果没有起点，使用第一个房间
        startRoom = rooms[0];
    }
    
    const queue = [startRoom];
    const visited = new Set();
    
    // 设置起点位置（居中）
    positions[startRoom] = { x: 400, y: 300 };
    visited.add(startRoom);
    
    // BFS 遍历所有房间
    while (queue.length > 0) {
        const currentRoom = queue.shift();
        const currentPos = positions[currentRoom];
        
        // 遍历当前房间的所有连接
        if (adjacencyList[currentRoom]) {
            adjacencyList[currentRoom].forEach(neighbor => {
                const neighborRoom = neighbor.room;
                const direction = neighbor.direction;
                
                // 如果邻居房间还没有位置，计算其位置
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
    
    // 处理未连接的房间（使用网格布局作为后备）
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
    
    // 不再手动翻转 y 坐标，而是通过 SVG transform 来正确处理坐标系
    // SVG 坐标系中 y 向下为正，但地图中 north 应该在上方
    // 我们保持计算逻辑不变（north = y 减小），这样在 SVG 中 north 自然在上方
    
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

