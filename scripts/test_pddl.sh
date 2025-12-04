#!/bin/bash
# ============================================================================
# test_pddl.sh
# ============================================================================
# PDDL 集成测试脚本（Shell版本）
# 用于快速检查 PDDL 环境配置
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

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}PDDL 环境检查${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# 进入项目根目录
cd "$PROJECT_ROOT"

# 检查1: SWI-Prolog是否安装
echo -e "${YELLOW}[检查1] SWI-Prolog${NC}"
if command -v swipl &> /dev/null; then
    SWIPL_VERSION=$(swipl --version 2>&1 | head -n 1)
    echo -e "${GREEN}  ✓ SWI-Prolog 已安装${NC}"
    echo "    版本: $SWIPL_VERSION"
else
    echo -e "${RED}  ✗ SWI-Prolog 未安装${NC}"
    echo "    请安装: https://www.swi-prolog.org/"
    exit 1
fi
echo ""

# 检查2: PDDL规划器是否安装
echo -e "${YELLOW}[检查2] PDDL规划器${NC}"
if command -v ff &> /dev/null; then
    echo -e "${GREEN}  ✓ Fast-Forward (ff) 已安装${NC}"
    FF_PATH=$(which ff)
    echo "    路径: $FF_PATH"
elif command -v fast-downward.py &> /dev/null; then
    echo -e "${GREEN}  ✓ Fast-Downward 已安装${NC}"
    FD_PATH=$(which fast-downward.py)
    echo "    路径: $FD_PATH"
else
    echo -e "${YELLOW}  ⚠ 未找到常见PDDL规划器 (ff, fast-downward)${NC}"
    echo "    提示: 如果使用其他规划器，请在 pddl_interface.pl 中配置"
fi
echo ""

# 检查3: PDDL文件是否存在
echo -e "${YELLOW}[检查3] PDDL文件${NC}"
DOMAIN_FILE="$PROJECT_ROOT/pddl/domains/adversary_domain.pddl"
if [ -f "$DOMAIN_FILE" ]; then
    echo -e "${GREEN}  ✓ Domain文件存在${NC}"
    echo "    路径: $DOMAIN_FILE"
else
    echo -e "${RED}  ✗ Domain文件不存在${NC}"
    echo "    路径: $DOMAIN_FILE"
    exit 1
fi

PROBLEM_DIR="$PROJECT_ROOT/pddl/problems"
if [ -d "$PROBLEM_DIR" ]; then
    echo -e "${GREEN}  ✓ Problem目录存在${NC}"
    echo "    路径: $PROBLEM_DIR"
else
    echo -e "${RED}  ✗ Problem目录不存在${NC}"
    exit 1
fi
echo ""

# 检查4: Prolog模块文件
echo -e "${YELLOW}[检查4] Prolog模块文件${NC}"
PDDL_INTERFACE="$PROJECT_ROOT/prolog/pddl_interface.pl"
if [ -f "$PDDL_INTERFACE" ]; then
    echo -e "${GREEN}  ✓ pddl_interface.pl 存在${NC}"
else
    echo -e "${RED}  ✗ pddl_interface.pl 不存在${NC}"
    exit 1
fi
echo ""

# 检查5: 测试PDDL domain文件语法（如果规划器可用）
echo -e "${YELLOW}[检查5] PDDL Domain文件语法${NC}"
if command -v ff &> /dev/null; then
    # 创建一个简单的测试problem文件
    TEST_PROBLEM="$PROJECT_ROOT/pddl/problems/test_syntax.pddl"
    cat > "$TEST_PROBLEM" << 'EOF'
(define (problem test)
  (:domain adversary)
  (:objects
    howler - entity
    player1 - player
    start_point yellow_hallway - location
  )
  (:init
    (at howler start_point)
    (at_player player1 yellow_hallway)
    (connected start_point yellow_hallway)
    (connected yellow_hallway start_point)
  )
  (:goal (at howler yellow_hallway))
)
EOF
    
    # 测试规划器是否能读取domain文件
    TEST_PLAN="$PROJECT_ROOT/pddl/problems/test_plan.txt"
    if ff -o "$DOMAIN_FILE" -f "$TEST_PROBLEM" > "$TEST_PLAN" 2>&1; then
        echo -e "${GREEN}  ✓ Domain文件语法正确${NC}"
        rm -f "$TEST_PROBLEM" "$TEST_PLAN"
    else
        echo -e "${YELLOW}  ⚠ Domain文件可能有语法问题${NC}"
        echo "    规划器输出:"
        cat "$TEST_PLAN" | head -n 10 | sed 's/^/    /'
        rm -f "$TEST_PROBLEM" "$TEST_PLAN"
    fi
else
    echo -e "${YELLOW}  ⚠ 跳过语法检查（未找到规划器）${NC}"
fi
echo ""

# 运行Prolog集成测试
echo -e "${YELLOW}[检查6] 运行Prolog集成测试${NC}"
echo "  执行: swipl -s scripts/test_pddl_integration.pl -g test_pddl_integration -t halt"
echo ""

if swipl -s scripts/test_pddl_integration.pl -g test_pddl_integration -t halt 2>&1; then
    echo ""
    echo -e "${GREEN}========================================${NC}"
    echo -e "${GREEN}所有检查完成！${NC}"
    echo -e "${GREEN}========================================${NC}"
else
    echo ""
    echo -e "${RED}========================================${NC}"
    echo -e "${RED}测试失败，请检查上述错误信息${NC}"
    echo -e "${RED}========================================${NC}"
    exit 1
fi

