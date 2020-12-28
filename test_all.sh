#!/bin/bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
    echo "Usage: $0 <testcase_root_folder>"
    exit 1
fi

TEST_DIR=$(realpath "$1")

ANSI_RESET="\033[0m"
ANSI_RED="\033[1;31m"
ANSI_YELLOW="\033[1;33m"
ANSI_GREEN="\033[1;32m"

for f in $(find "$TEST_DIR" -name "*.c"); do
    EXIT_STATUS=0
    msg=$(./test_single.sh "$f" 2>/dev/null) || EXIT_STATUS=$?
    if [[ $EXIT_STATUS -eq 0 ]]; then
        echo -e "${ANSI_GREEN}AC${ANSI_RESET} - $f - $msg"
    elif [[ $EXIT_STATUS -eq 1 ]]; then
        echo -e "${ANSI_RED}WA${ANSI_RESET} - $f - $msg"
    else
        echo -e "${ANSI_YELLOW}ER${ANSI_RESET} - $f - $msg"
    fi
done
