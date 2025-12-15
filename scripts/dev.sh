#!/bin/bash
# Dev container helper script

set -e

# Get the directory of this script
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

CONTAINER_NAME="liminal-logic-game-dev"
COMPOSE_FILE="docker-compose.dev.yml"

# Detect whether sudo is needed (via env var or auto-detection)
if [ -n "$USE_SUDO" ]; then
    USE_SUDO_FLAG="$USE_SUDO"
elif docker ps &> /dev/null; then
    USE_SUDO_FLAG=""
else
    # Auto-detect: if the current user cannot access Docker, use sudo
    USE_SUDO_FLAG="sudo"
fi

# Detect docker compose command (supports `docker compose` and `docker-compose`)
if command -v docker &> /dev/null && $USE_SUDO_FLAG docker compose version &> /dev/null 2>&1; then
    DOCKER_COMPOSE="$USE_SUDO_FLAG docker compose"
elif command -v docker-compose &> /dev/null; then
    DOCKER_COMPOSE="$USE_SUDO_FLAG docker-compose"
else
    echo "Error: docker compose or docker-compose command not found"
    if [ -n "$USE_SUDO_FLAG" ]; then
        echo "Hint: sudo permissions appear to be required; the script will automatically use sudo"
    fi
    exit 1
fi

# Change to project root
cd "$PROJECT_ROOT"

case "$1" in
    start|up)
        echo "Starting dev container..."
        $DOCKER_COMPOSE -f "$COMPOSE_FILE" up -d --build
        echo "Container started. Use './scripts/dev.sh shell' to enter the container."
        ;;
    stop|down)
        echo "Stopping dev container..."
        $DOCKER_COMPOSE -f "$COMPOSE_FILE" down
        ;;
    shell|bash)
        echo "Entering dev container..."
        $DOCKER_COMPOSE -f "$COMPOSE_FILE" exec game-dev bash
        ;;
    restart)
        echo "Restarting dev container..."
        $DOCKER_COMPOSE -f "$COMPOSE_FILE" restart
        ;;
    logs)
        $DOCKER_COMPOSE -f "$COMPOSE_FILE" logs -f game-dev
        ;;
    status|ps)
        $DOCKER_COMPOSE -f "$COMPOSE_FILE" ps
        ;;
    run)
        shift
        if [ $# -eq 0 ]; then
            echo "Error: please provide a command to run"
            echo "Usage: $0 run <command> [args...]"
            exit 1
        fi
        echo "Running in container: $@"
        $DOCKER_COMPOSE -f "$COMPOSE_FILE" exec game-dev "$@"
        ;;
    *)
        echo "Usage: $0 {start|stop|shell|restart|logs|status|run <command>}"
        echo ""
        echo "Commands:"
        echo "  start    - Start the dev container"
        echo "  stop     - Stop the dev container"
        echo "  shell    - Open a bash shell in the container"
        echo "  restart  - Restart the container"
        echo "  logs     - Follow container logs"
        echo "  status   - Show container status"
        echo "  run      - Run a command in the container (e.g. ./scripts/dev.sh run swipl --version)"
        exit 1
        ;;
esac

