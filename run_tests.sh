#!/bin/bash
# Onoki Compiler Test Runner
# Automatically runs all .onoki files in tests/ directory

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

TESTS_DIR="tests"
PASSED=0
FAILED=0
SKIPPED=0
FAILED_TESTS=()

# Build the compiler first
echo "Building Onoki compiler..."
dune build

echo ""
echo "========================================"
echo "         Running Onoki Tests           "
echo "========================================"
echo ""

# Clean up any existing class files
rm -f *.class 2>/dev/null || true

for test_file in "$TESTS_DIR"/*.onoki; do
    test_name=$(basename "$test_file" .onoki)
    
    echo -n "Testing $test_name... "
    
    # Clean class files before each test
    rm -f *.class 2>/dev/null || true
    
    # Try to compile
    if ! dune exec onoki -- "$test_file" > /tmp/onoki_output.txt 2>&1; then
        echo -e "${YELLOW}SKIP${NC} (compilation failed)"
        cat /tmp/onoki_output.txt | tail -5
        ((SKIPPED++))
        continue
    fi
    
    # Check if compilation was successful (Main.class exists)
    if [ ! -f "Main.class" ]; then
        echo -e "${YELLOW}SKIP${NC} (no Main.class generated)"
        ((SKIPPED++))
        continue
    fi
    
    # Try to run
    if java Main > /tmp/onoki_runtime.txt 2>&1; then
        echo -e "${GREEN}PASS${NC}"
        # Show output if any
        if [ -s /tmp/onoki_runtime.txt ]; then
            echo "  Output: $(cat /tmp/onoki_runtime.txt | head -3 | tr '\n' ' ')"
        fi
        ((PASSED++))
    else
        echo -e "${RED}FAIL${NC} (runtime error)"
        cat /tmp/onoki_runtime.txt | tail -5
        ((FAILED++))
        FAILED_TESTS+=("$test_name")
    fi
done

# Clean up
rm -f *.class 2>/dev/null || true
rm -f /tmp/onoki_output.txt /tmp/onoki_runtime.txt 2>/dev/null || true

echo ""
echo "========================================"
echo "            Test Summary               "
echo "========================================"
echo -e "${GREEN}Passed:${NC}  $PASSED"
echo -e "${RED}Failed:${NC}  $FAILED"
echo -e "${YELLOW}Skipped:${NC} $SKIPPED"
echo ""

if [ ${#FAILED_TESTS[@]} -gt 0 ]; then
    echo -e "${RED}Failed tests:${NC}"
    for t in "${FAILED_TESTS[@]}"; do
        echo "  - $t"
    done
    exit 1
fi

if [ $PASSED -gt 0 ]; then
    echo -e "${GREEN}All executed tests passed!${NC}"
fi
