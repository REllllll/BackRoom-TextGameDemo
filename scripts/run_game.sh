#!/bin/bash
# ============================================================================
# run_game.sh
# ============================================================================
# 游戏启动脚本
# 用于启动 Prolog 游戏并处理 PDDL 规划器调用
# ============================================================================

# 建议：在主机上运行时，本脚本会自动通过 Docker 开发容器启动游戏，
# 从而避免在主机上安装 SWI-Prolog / Java / PDDL4J。
# 如果你已经在容器内运行，则会直接启动 swipl。

# 获取脚本所在目录
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# 颜色输出
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}Liminal Logic: Escape from Level 0${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""

# 容器配置（与 scripts/dev.sh 和 start_playground.sh 保持一致）
CONTAINER_NAME="liminal-logic-game-dev"
COMPOSE_FILE="docker-compose.dev.yml"

# 检测是否在容器中运行
IS_IN_CONTAINER=false
if [ -f /.dockerenv ] || [ -n "${DOCKER_CONTAINER:-}" ] || grep -q 'docker\|lxc' /proc/1/cgroup 2>/dev/null; then
    IS_IN_CONTAINER=true
fi

# 检查 PDDL 规划器（可选）
# 如果需要使用 PDDL 规划器，请确保已安装（如 Fast-Forward）
# PDDL_PLANNER="ff"  # 或其他规划器路径

# 进入项目根目录
cd "$PROJECT_ROOT"

# 如果已经在容器内，直接运行游戏
if [ "$IS_IN_CONTAINER" = true ]; then
    echo -e "${BLUE}Running in Docker container${NC}"
    echo -e "${BLUE}Container: ${CONTAINER_NAME}${NC}"
    echo ""

    # 检查 Prolog 是否安装
    if ! command -v swipl &> /dev/null; then
        echo -e "${RED}Error: SWI-Prolog is not installed in container.${NC}"
        exit 1
    fi

    # 检查入口文件
    if [ ! -f "prolog/main.pl" ]; then
        echo -e "${RED}Error: prolog/main.pl not found.${NC}"
        exit 1
    fi

    echo -e "${YELLOW}Starting game...${NC}"
    echo ""

    swipl -s prolog/main.pl -g start -t halt

    EXIT_CODE=$?
    if [ $EXIT_CODE -eq 0 ]; then
        echo ""
        echo -e "${GREEN}Game ended.${NC}"
        exit 0
    else
        echo ""
        echo -e "${RED}Game exited with an error (code: $EXIT_CODE).${NC}"
        exit $EXIT_CODE
    fi
fi

# 在主机上运行：优先通过开发容器启动游戏

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
DOCKER_COMPOSE=""
if command -v docker &> /dev/null && $USE_SUDO_FLAG docker compose version &> /dev/null 2>&1; then
    DOCKER_COMPOSE="$USE_SUDO_FLAG docker compose"
elif command -v docker-compose &> /dev/null; then
    DOCKER_COMPOSE="$USE_SUDO_FLAG docker-compose"
fi

# 如果主机没有 Docker（或不可用），则回退到本地 swipl（兼容“本地安装”用法）
if [ -z "$DOCKER_COMPOSE" ]; then
    if command -v swipl &> /dev/null; then
        echo -e "${YELLOW}Docker compose not available; falling back to local SWI-Prolog...${NC}"
        echo ""
        swipl -s prolog/main.pl -g start -t halt
        EXIT_CODE=$?
        if [ $EXIT_CODE -eq 0 ]; then
            echo ""
            echo -e "${GREEN}Game ended.${NC}"
            exit 0
        else
            echo ""
            echo -e "${RED}Game exited with an error (code: $EXIT_CODE).${NC}"
            exit $EXIT_CODE
        fi
    fi

    echo -e "${RED}Error: docker compose (or docker-compose) not found, and local swipl is not installed.${NC}"
    if [ -n "$USE_SUDO_FLAG" ]; then
        echo -e "${YELLOW}Hint: Detected need for sudo permissions, script will automatically use sudo when available.${NC}"
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

# 再次确认容器已运行
if ! $DOCKER_COMPOSE -f "$COMPOSE_FILE" ps | grep -q "Up"; then
    echo -e "${RED}Error: Failed to start development container${NC}"
    exit 1
fi

echo -e "${BLUE}Running game in container: ${CONTAINER_NAME}${NC}"
echo ""
echo -e "${YELLOW}Starting game...${NC}"
echo ""

# 在容器内启动 Prolog 游戏（按仓库约定，使用 dev.sh run 在容器内执行命令）
./scripts/dev.sh run swipl -s prolog/main.pl -g start -t halt

EXIT_CODE=$?
if [ $EXIT_CODE -eq 0 ]; then
    echo ""
    echo -e "${GREEN}Game ended.${NC}"
else
    echo ""
    echo -e "${RED}Game exited with an error (code: $EXIT_CODE).${NC}"
    exit $EXIT_CODE
fi
