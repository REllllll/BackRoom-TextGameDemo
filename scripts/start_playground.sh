#!/bin/bash
# ============================================================================
# start_playground.sh
# ============================================================================
# Start the Prolog HTTP server for the playground
# This script can run on the host, but the Prolog server will run inside the dev container
# ============================================================================

# Get the directory of this script
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Container configuration
CONTAINER_NAME="liminal-logic-game-dev"
COMPOSE_FILE="docker-compose.dev.yml"

# Detect whether running inside a container
IS_IN_CONTAINER=false
if [ -f /.dockerenv ] || [ -n "${DOCKER_CONTAINER:-}" ] || grep -q 'docker\|lxc' /proc/1/cgroup 2>/dev/null; then
    IS_IN_CONTAINER=true
fi

# Default port: use 8081 inside the container
PORT=${1:-8081}

# If already in container, run the server directly
if [ "$IS_IN_CONTAINER" = true ]; then
    # Running inside container
    echo -e "${GREEN}========================================${NC}"
    echo -e "${GREEN}Liminal Logic: Prolog Playground${NC}"
    echo -e "${GREEN}========================================${NC}"
    echo ""
    echo -e "${BLUE}Running in Docker container${NC}"
    echo -e "${BLUE}Container: ${CONTAINER_NAME}${NC}"
    echo ""
    
    # Check whether Prolog is installed
    if ! command -v swipl &> /dev/null; then
        echo -e "${RED}Error: SWI-Prolog is not installed.${NC}"
        exit 1
    fi
    
    # Check required files
    if [ ! -f "prolog/http_server.pl" ]; then
        echo -e "${RED}Error: prolog/http_server.pl not found.${NC}"
        exit 1
    fi
    
    if [ ! -d "playground" ]; then
        echo -e "${RED}Error: playground directory not found.${NC}"
        exit 1
    fi
    
    # Print startup info
    echo -e "${YELLOW}Starting HTTP server on port ${PORT}...${NC}"
    echo -e "${BLUE}Server will bind to 0.0.0.0:${PORT}${NC}"
    echo -e "${BLUE}Access URL: http://localhost:${PORT}${NC}"
    echo ""
    echo -e "${YELLOW}Press Ctrl+C to stop the server${NC}"
    echo ""
    
    # Start server
    swipl -s prolog/http_server.pl -g "game_http_server:start_server(${PORT}), halt_on_error, halt."
    exit $?
fi

# On host: start the server inside the container
echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}Liminal Logic: Prolog Playground${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""

# Change to project root
cd "$PROJECT_ROOT"

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

echo -e "${BLUE}Running Prolog server in container: ${CONTAINER_NAME}${NC}"
echo -e "${BLUE}Server will bind to 0.0.0.0:${PORT} (accessible from outside container)${NC}"
echo -e "${BLUE}Access URLs:${NC}"
echo -e "${BLUE}  - From container: http://localhost:${PORT}${NC}"
echo -e "${BLUE}  - From host: http://localhost:8081 (via port mapping)${NC}"
echo -e "${BLUE}  - From network: http://<container-ip>:${PORT}${NC}"
echo ""
echo -e "${YELLOW}Press Ctrl+C to stop the server${NC}"
echo ""

# Start the Prolog HTTP server in the container
# Repo convention: use dev.sh run to execute commands in the container
echo -e "${GREEN}Launching Prolog HTTP server in container...${NC}"
./scripts/dev.sh run swipl -s prolog/http_server.pl -g "game_http_server:start_server(${PORT}), halt_on_error, halt."

# Check exit status
EXIT_CODE=$?
if [ $EXIT_CODE -eq 0 ]; then
    echo ""
    echo -e "${GREEN}Server stopped.${NC}"
else
    echo ""
    echo -e "${RED}Server exited with an error (code: $EXIT_CODE).${NC}"
    exit $EXIT_CODE
fi

