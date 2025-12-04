#!/bin/bash
# 开发容器便捷脚本

set -e

# 获取脚本所在目录
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

CONTAINER_NAME="liminal-logic-game-dev"
COMPOSE_FILE="docker-compose.dev.yml"

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
    echo "错误: 未找到 docker compose 或 docker-compose 命令"
    if [ -n "$USE_SUDO_FLAG" ]; then
        echo "提示: 检测到需要 sudo 权限，脚本将自动使用 sudo"
    fi
    exit 1
fi

# 切换到项目根目录
cd "$PROJECT_ROOT"

case "$1" in
    start|up)
        echo "启动开发容器..."
        $DOCKER_COMPOSE -f "$COMPOSE_FILE" up -d --build
        echo "容器已启动，使用 './scripts/dev.sh shell' 进入容器"
        ;;
    stop|down)
        echo "停止开发容器..."
        $DOCKER_COMPOSE -f "$COMPOSE_FILE" down
        ;;
    shell|bash)
        echo "进入开发容器..."
        $DOCKER_COMPOSE -f "$COMPOSE_FILE" exec game-dev bash
        ;;
    restart)
        echo "重启开发容器..."
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
            echo "错误: 请提供要运行的命令"
            echo "用法: $0 run <command> [args...]"
            exit 1
        fi
        echo "在容器内运行: $@"
        $DOCKER_COMPOSE -f "$COMPOSE_FILE" exec game-dev "$@"
        ;;
    *)
        echo "用法: $0 {start|stop|shell|restart|logs|status|run <command>}"
        echo ""
        echo "命令说明:"
        echo "  start    - 启动开发容器"
        echo "  stop     - 停止开发容器"
        echo "  shell    - 进入容器的 bash shell"
        echo "  restart  - 重启容器"
        echo "  logs     - 查看容器日志"
        echo "  status   - 查看容器状态"
        echo "  run      - 在容器内运行命令（例如: ./scripts/dev.sh run swipl --version）"
        exit 1
        ;;
esac

