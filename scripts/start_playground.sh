#!/bin/bash
# ============================================================================
# start_playground.sh
# ============================================================================
# 启动 Prolog HTTP server 用于 playground
# 此脚本可以在主机上运行，但 Prolog 服务器会在开发容器内启动
# ============================================================================

# 获取脚本所在目录
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# 颜色输出
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 容器配置
CONTAINER_NAME="liminal-logic-game-dev"
COMPOSE_FILE="docker-compose.dev.yml"

# 检测是否在容器中运行
IS_IN_CONTAINER=false
if [ -f /.dockerenv ] || [ -n "${DOCKER_CONTAINER:-}" ] || grep -q 'docker\|lxc' /proc/1/cgroup 2>/dev/null; then
    IS_IN_CONTAINER=true
fi

# 默认端口：容器内使用 8081
PORT=${1:-8081}

# 如果已经在容器内，直接运行服务器
if [ "$IS_IN_CONTAINER" = true ]; then
    # 在容器内直接运行
    echo -e "${GREEN}========================================${NC}"
    echo -e "${GREEN}Liminal Logic: Prolog Playground${NC}"
    echo -e "${GREEN}========================================${NC}"
    echo ""
    echo -e "${BLUE}Running in Docker container${NC}"
    echo -e "${BLUE}Container: ${CONTAINER_NAME}${NC}"
    echo ""
    
    # 检查 Prolog 是否安装
    if ! command -v swipl &> /dev/null; then
        echo -e "${RED}Error: SWI-Prolog is not installed.${NC}"
        exit 1
    fi
    
    # 检查必要文件
    if [ ! -f "prolog/http_server.pl" ]; then
        echo -e "${RED}Error: prolog/http_server.pl not found.${NC}"
        exit 1
    fi
    
    if [ ! -d "playground" ]; then
        echo -e "${RED}Error: playground directory not found.${NC}"
        exit 1
    fi
    
    # 显示启动信息
    echo -e "${YELLOW}Starting HTTP server on port ${PORT}...${NC}"
    echo -e "${BLUE}Server will bind to 0.0.0.0:${PORT}${NC}"
    echo -e "${BLUE}Access URL: http://localhost:${PORT}${NC}"
    echo ""
    echo -e "${YELLOW}Press Ctrl+C to stop the server${NC}"
    echo ""
    
    # 启动服务器
    swipl -s prolog/http_server.pl -g "game_http_server:start_server(${PORT}), halt_on_error, halt."
    exit $?
fi

# 在主机上运行：需要在容器内启动服务器
echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}Liminal Logic: Prolog Playground${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""

# 切换到项目根目录
cd "$PROJECT_ROOT"

# 检测是否需要 sudo（通过环境变量或自动检测）
if [ -n "$USE_SUDO" ]; then
    USE_SUDO_FLAG="$USE_SUDO"
elif docker ps &> /dev/null; then
    USE_SUDO_FLAG=""
else
    # 自动检测：如果普通用户无法访问 docker，则使用 sudo
    USE_SUDO_FLAG="sudo"
fi

# 检测 docker compose 命令（支持 docker compose 和 docker-compose）
if command -v docker &> /dev/null && $USE_SUDO_FLAG docker compose version &> /dev/null 2>&1; then
    DOCKER_COMPOSE="$USE_SUDO_FLAG docker compose"
elif command -v docker-compose &> /dev/null; then
    DOCKER_COMPOSE="$USE_SUDO_FLAG docker-compose"
else
    echo -e "${RED}Error: docker compose or docker-compose not found${NC}"
    if [ -n "$USE_SUDO_FLAG" ]; then
        echo -e "${YELLOW}Hint: Detected need for sudo permissions, script will automatically use sudo${NC}"
    fi
    exit 1
fi

# 检查容器是否运行
if ! $DOCKER_COMPOSE -f "$COMPOSE_FILE" ps | grep -q "Up"; then
    echo -e "${YELLOW}Development container is not running. Starting container...${NC}"
    $DOCKER_COMPOSE -f "$COMPOSE_FILE" up -d
    echo -e "${GREEN}Container started. Waiting for it to be ready...${NC}"
    sleep 3
fi

# 检查容器是否真的在运行
if ! $DOCKER_COMPOSE -f "$COMPOSE_FILE" ps | grep -q "Up"; then
    echo -e "${RED}Error: Failed to start development container${NC}"
    exit 1
fi

echo -e "${BLUE}Running Prolog server in container: ${CONTAINER_NAME}${NC}"
echo -e "${BLUE}Server will bind to 0.0.0.0:${PORT} (accessible from outside container)${NC}"
echo -e "${BLUE}Access URLs:${NC}"
echo -e "${BLUE}  - From container: http://localhost:${PORT}${NC}"
echo -e "${BLUE}  - From host: http://localhost:8081 (via port mapping)${NC}"
echo -e "${BLUE}  - From network: http://<container-ip>:${PORT}${NC}"
echo ""
echo -e "${YELLOW}Press Ctrl+C to stop the server${NC}"
echo ""

# 在容器内启动 Prolog HTTP server
# 使用 dev.sh run 在容器内执行命令
echo -e "${GREEN}Launching Prolog HTTP server in container...${NC}"
./scripts/dev.sh run swipl -s prolog/http_server.pl -g "game_http_server:start_server(${PORT}), halt_on_error, halt."

# 检查退出状态
EXIT_CODE=$?
if [ $EXIT_CODE -eq 0 ]; then
    echo ""
    echo -e "${GREEN}Server stopped.${NC}"
else
    echo ""
    echo -e "${RED}Server exited with an error (code: $EXIT_CODE).${NC}"
    exit $EXIT_CODE
fi

