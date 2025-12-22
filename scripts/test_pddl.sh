#!/bin/bash
# ============================================================================
# test_pddl.sh
# ============================================================================
# PDDL integration test script (Shell)
# Quickly checks PDDL environment configuration
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

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}PDDL environment check${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Change to project root
cd "$PROJECT_ROOT"

# Check 1: is SWI-Prolog installed?
echo -e "${YELLOW}[Check 1] SWI-Prolog${NC}"
if command -v swipl &> /dev/null; then
    SWIPL_VERSION=$(swipl --version 2>&1 | head -n 1)
    echo -e "${GREEN}  ✓ SWI-Prolog is installed${NC}"
    echo "    Version: $SWIPL_VERSION"
else
    echo -e "${RED}  ✗ SWI-Prolog is not installed${NC}"
    echo "    Please install: https://www.swi-prolog.org/"
    exit 1
fi
echo ""

# Check 2: is a PDDL planner installed?
echo -e "${YELLOW}[Check 2] PDDL planner${NC}"
if command -v ff &> /dev/null; then
    echo -e "${GREEN}  ✓ Fast-Forward (ff) is installed${NC}"
    FF_PATH=$(which ff)
    echo "    Path: $FF_PATH"
elif command -v fast-downward.py &> /dev/null; then
    echo -e "${GREEN}  ✓ Fast-Downward is installed${NC}"
    FD_PATH=$(which fast-downward.py)
    echo "    Path: $FD_PATH"
else
    echo -e "${YELLOW}  ⚠ No common PDDL planner found (ff, fast-downward)${NC}"
    echo "    Hint: if you use a different planner, configure it in liminal_logic_pddl_interface.pl"
fi
echo ""

# Check 3: do PDDL files exist?
echo -e "${YELLOW}[Check 3] PDDL files${NC}"
DOMAIN_FILE="$PROJECT_ROOT/pddl/domains/adversary_domain.pddl"
if [ -f "$DOMAIN_FILE" ]; then
    echo -e "${GREEN}  ✓ Domain file exists${NC}"
    echo "    Path: $DOMAIN_FILE"
else
    echo -e "${RED}  ✗ Domain file does not exist${NC}"
    echo "    Path: $DOMAIN_FILE"
    exit 1
fi

PROBLEM_DIR="$PROJECT_ROOT/pddl/problems"
if [ -d "$PROBLEM_DIR" ]; then
    echo -e "${GREEN}  ✓ Problem directory exists${NC}"
    echo "    Path: $PROBLEM_DIR"
else
    echo -e "${RED}  ✗ Problem directory does not exist${NC}"
    exit 1
fi
echo ""

# Check 4: Prolog module files
echo -e "${YELLOW}[Check 4] Prolog module files${NC}"
PDDL_INTERFACE="$PROJECT_ROOT/prolog/liminal_logic_pddl_interface.pl"
if [ -f "$PDDL_INTERFACE" ]; then
    echo -e "${GREEN}  ✓ liminal_logic_pddl_interface.pl exists${NC}"
else
    echo -e "${RED}  ✗ liminal_logic_pddl_interface.pl does not exist${NC}"
    exit 1
fi
echo ""

# Check 5: validate PDDL domain syntax (if planner is available)
echo -e "${YELLOW}[Check 5] PDDL domain file syntax${NC}"
if command -v ff &> /dev/null; then
    # Create a minimal test problem file
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
    
    # Test whether the planner can read the domain file
    TEST_PLAN="$PROJECT_ROOT/pddl/problems/test_plan.txt"
    if ff -o "$DOMAIN_FILE" -f "$TEST_PROBLEM" > "$TEST_PLAN" 2>&1; then
        echo -e "${GREEN}  ✓ Domain file syntax looks OK${NC}"
        rm -f "$TEST_PROBLEM" "$TEST_PLAN"
    else
        echo -e "${YELLOW}  ⚠ Domain file may have syntax issues${NC}"
        echo "    Planner output:"
        cat "$TEST_PLAN" | head -n 10 | sed 's/^/    /'
        rm -f "$TEST_PROBLEM" "$TEST_PLAN"
    fi
else
    echo -e "${YELLOW}  ⚠ Skipping syntax check (planner not found)${NC}"
fi
echo ""

# Run Prolog integration test
echo -e "${YELLOW}[Check 6] Run Prolog integration test${NC}"
echo "  Running: swipl -s scripts/test_pddl_integration.pl -g test_pddl_integration -t halt"
echo ""

if swipl -s scripts/test_pddl_integration.pl -g test_pddl_integration -t halt 2>&1; then
    echo ""
    echo -e "${GREEN}========================================${NC}"
    echo -e "${GREEN}All checks completed!${NC}"
    echo -e "${GREEN}========================================${NC}"
else
    echo ""
    echo -e "${RED}========================================${NC}"
    echo -e "${RED}Test failed. Please check the error output above.${NC}"
    echo -e "${RED}========================================${NC}"
    exit 1
fi

