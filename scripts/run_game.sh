#!/bin/bash
# ============================================================================
# run_game.sh
# ============================================================================
# Game launcher script
# Starts the Prolog game and handles PDDL planner invocation
# ============================================================================

# Recommendation: when running on the host, this script will automatically run
# the game via the Docker dev container to avoid installing SWI-Prolog / Java /
# PDDL4J locally. If you're already inside the container, it will run swipl
# directly.

# Get the directory of this script
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}Liminal Logic: Escape from Level 0${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""

# Container configuration (keep in sync with scripts/dev.sh and start_playground.sh)
CONTAINER_NAME="liminal-logic-game-dev"
COMPOSE_FILE="docker-compose.dev.yml"

# Detect whether running inside a container
IS_IN_CONTAINER=false
if [ -f /.dockerenv ] || [ -n "${DOCKER_CONTAINER:-}" ] || grep -q 'docker\|lxc' /proc/1/cgroup 2>/dev/null; then
    IS_IN_CONTAINER=true
fi

# PDDL planner (optional)
# If you want PDDL-driven behavior, ensure a planner is installed (e.g. Fast-Forward)
# PDDL_PLANNER="ff"  # or another planner path

# Change to project root
cd "$PROJECT_ROOT"

# If already in container, run the game directly
if [ "$IS_IN_CONTAINER" = true ]; then
    echo -e "${BLUE}Running in Docker container${NC}"
    echo -e "${BLUE}Container: ${CONTAINER_NAME}${NC}"
    echo ""

    # Check whether Prolog is installed
    if ! command -v swipl &> /dev/null; then
        echo -e "${RED}Error: SWI-Prolog is not installed in container.${NC}"
        exit 1
    fi

    # Check entry file
    if [ ! -f "prolog/liminal_logic_game.pl" ]; then
        echo -e "${RED}Error: prolog/liminal_logic_game.pl not found.${NC}"
        exit 1
    fi

    echo -e "${YELLOW}Starting game...${NC}"
    echo ""

    swipl -s prolog/liminal_logic_game.pl -g start -t halt

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

# On host: prefer running via the dev container

# Detect whether sudo is needed (via env var or auto-detection)
if [ -n "$USE_SUDO" ]; then
    USE_SUDO_FLAG="$USE_SUDO"
elif docker ps &> /dev/null; then
    USE_SUDO_FLAG=""
else
    # Auto-detect: if docker isn't accessible as the current user, use sudo
    USE_SUDO_FLAG="sudo"
fi

# Detect docker compose command (supports `docker compose` and `docker-compose`)
DOCKER_COMPOSE=""
if command -v docker &> /dev/null && $USE_SUDO_FLAG docker compose version &> /dev/null 2>&1; then
    DOCKER_COMPOSE="$USE_SUDO_FLAG docker compose"
elif command -v docker-compose &> /dev/null; then
    DOCKER_COMPOSE="$USE_SUDO_FLAG docker-compose"
fi

# If Docker isn't available on the host, fall back to local swipl (for "local install" usage)
if [ -z "$DOCKER_COMPOSE" ]; then
    if command -v swipl &> /dev/null; then
        echo -e "${YELLOW}Docker compose not available; falling back to local SWI-Prolog...${NC}"
        echo ""
        swipl -s prolog/liminal_logic_game.pl -g start -t halt
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

# Check whether the container is running
if ! $DOCKER_COMPOSE -f "$COMPOSE_FILE" ps | grep -q "Up"; then
    echo -e "${YELLOW}Development container is not running. Starting container...${NC}"
    $DOCKER_COMPOSE -f "$COMPOSE_FILE" up -d
    echo -e "${GREEN}Container started. Waiting for it to be ready...${NC}"
    sleep 3
fi

# Confirm the container is running
if ! $DOCKER_COMPOSE -f "$COMPOSE_FILE" ps | grep -q "Up"; then
    echo -e "${RED}Error: Failed to start development container${NC}"
    exit 1
fi

echo -e "${BLUE}Running game in container: ${CONTAINER_NAME}${NC}"
echo ""
echo -e "${YELLOW}Starting game...${NC}"
echo ""

# Run the Prolog game in the container (repo convention: use dev.sh run)
./scripts/dev.sh run swipl -s prolog/liminal_logic_game.pl -g start -t halt

EXIT_CODE=$?
if [ $EXIT_CODE -eq 0 ]; then
    echo ""
    echo -e "${GREEN}Game ended.${NC}"
else
    echo ""
    echo -e "${RED}Game exited with an error (code: $EXIT_CODE).${NC}"
    exit $EXIT_CODE
fi
