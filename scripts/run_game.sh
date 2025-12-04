#!/bin/bash
# ============================================================================
# run_game.sh
# ============================================================================
# 游戏启动脚本
# 用于启动 Prolog 游戏并处理 PDDL 规划器调用
# ============================================================================

# 获取脚本所在目录
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# 颜色输出
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}Liminal Logic: Escape from Level 0${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""

# 检查 Prolog 是否安装
if ! command -v swipl &> /dev/null; then
    echo -e "${RED}Error: SWI-Prolog is not installed.${NC}"
    echo "Please install SWI-Prolog: https://www.swi-prolog.org/"
    exit 1
fi

# 检查 PDDL 规划器（可选）
# 如果需要使用 PDDL 规划器，请确保已安装（如 Fast-Forward）
# PDDL_PLANNER="ff"  # 或其他规划器路径

# 进入项目根目录
cd "$PROJECT_ROOT"

# 启动 Prolog 游戏
echo -e "${YELLOW}Starting game...${NC}"
echo ""

swipl -s prolog/main.pl -g start -t halt

# 如果游戏正常退出
if [ $? -eq 0 ]; then
    echo ""
    echo -e "${GREEN}Game ended.${NC}"
else
    echo ""
    echo -e "${RED}Game exited with an error.${NC}"
    exit 1
fi

