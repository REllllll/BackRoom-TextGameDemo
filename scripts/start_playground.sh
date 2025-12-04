#!/bin/bash
# ============================================================================
# start_playground.sh
# ============================================================================
# 启动 Prolog HTTP server 用于 playground
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

# 默认端口
PORT=${1:-8080}

echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}Liminal Logic: Prolog Playground${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""

# 检查 Prolog 是否安装
if ! command -v swipl &> /dev/null; then
    echo -e "${RED}Error: SWI-Prolog is not installed.${NC}"
    echo "Please install SWI-Prolog: https://www.swi-prolog.org/"
    exit 1
fi

# 进入项目根目录
cd "$PROJECT_ROOT"

# 检查必要文件是否存在
if [ ! -f "prolog/http_server.pl" ]; then
    echo -e "${RED}Error: prolog/http_server.pl not found.${NC}"
    exit 1
fi

if [ ! -d "playground" ]; then
    echo -e "${RED}Error: playground directory not found.${NC}"
    exit 1
fi

# 检查端口是否被占用
if command -v lsof &> /dev/null; then
    if lsof -i :${PORT} &> /dev/null; then
        echo -e "${YELLOW}Port ${PORT} is already in use. Attempting to stop existing server...${NC}"
        pkill -f "swipl.*http_server" || pkill -f "game_http_server" || true
        sleep 2
    fi
elif command -v netstat &> /dev/null; then
    if netstat -tuln 2>/dev/null | grep -q ":${PORT} "; then
        echo -e "${YELLOW}Port ${PORT} is already in use. Attempting to stop existing server...${NC}"
        pkill -f "swipl.*http_server" || pkill -f "game_http_server" || true
        sleep 2
    fi
fi

# 显示启动信息
echo -e "${YELLOW}Starting HTTP server on port ${PORT}...${NC}"
echo -e "${BLUE}If running in Docker container:${NC}"
echo -e "${BLUE}  - From container: http://localhost:${PORT}${NC}"
echo -e "${BLUE}  - From host: http://localhost:8081 (if port is mapped)${NC}"
echo ""
echo -e "${YELLOW}Press Ctrl+C to stop the server${NC}"
echo ""

# 启动 Prolog HTTP server
swipl -s prolog/http_server.pl -g "game_http_server:start_server(${PORT}), halt_on_error, halt."

# 如果服务器正常退出
if [ $? -eq 0 ]; then
    echo ""
    echo -e "${GREEN}Server stopped.${NC}"
else
    echo ""
    echo -e "${RED}Server exited with an error.${NC}"
    exit 1
fi

